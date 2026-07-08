# =============================================================================
# Random Slope Variance Decomposition
# Identity–Attitudes Slope: Individual vs. Period-Level Heterogeneity
#
# Purpose:
#   Figure 2 shows substantial wave-to-wave variation in the average
#   identity–attitudes slope. This script asks: is there also meaningful
#   between-individual heterogeneity in that slope, net of the period-level
#   pattern? The answer conditions how the modest meso/micro moderation
#   results (Appendices B and A) should be interpreted.
#
# Strategy:
#   For both Britishness and Englishness:
#   (1) Estimate a random-intercept-only model (slope fixed across persons).
#   (2) Estimate a random-slope model (both intercept and identity slope
#       allowed to vary across individuals).
#   (3) LR test (ML estimation) tests whether the slope variance > 0.
#   (4) REML estimation gives the preferred variance components.
#   (5) Compare individual-level slope variance against wave-level slope
#       variance (computed from Figure 2 models) to decompose total
#       heterogeneity into its period vs. individual components.
#
# Output:
#   - Variance component tables (console + saved .csv)
#   - LR test results
#   - Figure: individual-level BLUP slopes plotted against wave-mean slopes
#     to visually decompose the two sources of heterogeneity
#
# Data:
#   Same .rds file and panels as Appendix A (panel_self, panel_self_eng).
#   Run Appendix A setup chunks first, or source the data-building code
#   from appendix_A.qmd before running this script.
#
# Repository:
#   Available at: [GITHUB URL WHEN ACCEPTED]
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(fixest)
library(haven)
library(purrr)
library(lme4)
library(ggplot2)

# =============================================================================
# 0. LOAD DATA AND BUILD PANELS
# =============================================================================
# This reproduces the minimal panel-building code from Appendix A so this
# script is fully self-contained. If you have already run the Appendix A
# setup, you can skip to Section 1 and use panel_self / panel_self_eng directly.

df <- readRDS(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v3_England.rds"
)

DV_BASE   <- "immigSelf"
BRIT_BASE <- "britishness"
ENG_BASE  <- "englishness"
TIME_BASE <- "starttime"
AGE_BASE  <- "age"

get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(df, base) {
  vars <- names(df)[str_detect(names(df), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave_num)))
}

make_long_with_age <- function(df, dv_base, brit_base, time_base, age_base) {
  w_dv   <- waves_available(df, dv_base)
  w_brit <- waves_available(df, brit_base)
  w_time <- waves_available(df, time_base)
  w_age  <- waves_available(df, age_base)
  waves  <- Reduce(intersect, list(w_dv, w_brit, w_time, w_age))
  if (length(waves) < 3) stop("Too few overlapping waves.")
  
  core_cols <- c(
    paste0(dv_base,   "W", waves),
    paste0(brit_base, "W", waves),
    paste0(time_base, "W", waves),
    paste0(age_base,  "W", waves)
  )
  
  df |>
    mutate(across(
      any_of(core_cols),
      ~ as.numeric(haven::zap_labels(.x))
    )) |>
    select(id, any_of(core_cols)) |>
    pivot_longer(
      cols = -id,
      names_to = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) |>
    mutate(
      wave = as.integer(wave),
      time = suppressWarnings(ymd_hms(as.character(.data[[time_base]]), quiet = TRUE)),
      year = year(time)
    ) |>
    rename(dv = all_of(dv_base), brit = all_of(brit_base), age = all_of(age_base)) |>
    filter(!is.na(dv), !is.na(brit), !is.na(age), !is.na(wave))
}

panel_self <- make_long_with_age(df, DV_BASE, BRIT_BASE, TIME_BASE, AGE_BASE)

panel_self_eng <- make_long_with_age(df, DV_BASE, ENG_BASE, TIME_BASE, AGE_BASE) |>
  rename(engl = brit) |>
  mutate(engl = if_else(engl %in% 1:7, engl, NA_real_)) |>
  filter(!is.na(engl))

cat("Britishness panel:", nrow(panel_self), "obs,",
    n_distinct(panel_self$id), "individuals\n")
cat("Englishness panel:", nrow(panel_self_eng), "obs,",
    n_distinct(panel_self_eng$id), "individuals\n")

# =============================================================================
# 1. WAVE-LEVEL SLOPE VARIANCE (PERIOD COMPONENT)
# Reproduces Figure 2: wave-specific identity slopes from a model that
# interacts identity with wave indicators. This is the macro/period component
# of total slope heterogeneity, against which individual-level variance
# (Section 2) will be benchmarked.
# =============================================================================

m_wave_brit <- feols(dv ~ i(wave, brit) | id, data = panel_self,     cluster = ~id)
m_wave_engl <- feols(dv ~ i(wave, engl) | id, data = panel_self_eng, cluster = ~id)

wave_slopes_brit <- coef(m_wave_brit)
wave_slopes_engl <- coef(m_wave_engl)

wave_slope_var_brit <- var(wave_slopes_brit, na.rm = TRUE)
wave_slope_var_engl <- var(wave_slopes_engl, na.rm = TRUE)
wave_slope_sd_brit  <- sd(wave_slopes_brit,  na.rm = TRUE)
wave_slope_sd_engl  <- sd(wave_slopes_engl,  na.rm = TRUE)

cat("\n--- Wave-level slope variance ---\n")
cat("Britishness: variance =", round(wave_slope_var_brit, 6),
    "| SD =", round(wave_slope_sd_brit, 4), "\n")
cat("Englishness: variance =", round(wave_slope_var_engl, 6),
    "| SD =", round(wave_slope_sd_engl, 4), "\n")

# =============================================================================
# 2. RANDOM SLOPE MODELS (INDIVIDUAL COMPONENT)
# NOTE: These models are computationally intensive (~5-15 minutes each on
# panels of ~400,000 obs). The bobyqa optimizer is the most robust default
# for large panels. If you encounter a singular fit warning, uncomment the
# uncorrelated slopes version below as a fallback.
# =============================================================================

lmer_control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl   = list(maxfun = 2e5)
)

# ── BRITISHNESS ───────────────────────────────────────────────────────────────

cat("\n--- Estimating Britishness random slope models ---\n")
cat("(This may take several minutes...)\n")

# Random intercept only — ML for LR test
m_ri_brit_ml <- lmer(
  dv ~ brit + age + factor(wave) + (1 | id),
  data    = panel_self,
  REML    = FALSE,
  control = lmer_control
)

# Random slope — ML for LR test
m_rs_brit_ml <- lmer(
  dv ~ brit + age + factor(wave) + (1 + brit | id),
  data    = panel_self,
  REML    = FALSE,
  control = lmer_control
)

# Random slope — REML for final variance components
m_rs_brit <- lmer(
  dv ~ brit + age + factor(wave) + (1 + brit | id),
  data    = panel_self,
  REML    = TRUE,
  control = lmer_control
)

# Fallback if singular fit: uncorrelated random intercept + slope
# m_rs_brit <- lmer(
#   dv ~ brit + age + factor(wave) + (1 | id) + (0 + brit | id),
#   data = panel_self, REML = TRUE, control = lmer_control
# )

lr_brit <- anova(m_ri_brit_ml, m_rs_brit_ml)
vc_brit <- as.data.frame(VarCorr(m_rs_brit))

cat("Britishness random slope models: done\n")
print(lr_brit)
print(vc_brit)

# ── ENGLISHNESS ───────────────────────────────────────────────────────────────

cat("\n--- Estimating Englishness random slope models ---\n")
cat("(This may take several minutes...)\n")

m_ri_engl_ml <- lmer(
  dv ~ engl + age + factor(wave) + (1 | id),
  data    = panel_self_eng,
  REML    = FALSE,
  control = lmer_control
)

m_rs_engl_ml <- lmer(
  dv ~ engl + age + factor(wave) + (1 + engl | id),
  data    = panel_self_eng,
  REML    = FALSE,
  control = lmer_control
)

m_rs_engl <- lmer(
  dv ~ engl + age + factor(wave) + (1 + engl | id),
  data    = panel_self_eng,
  REML    = TRUE,
  control = lmer_control
)

lr_engl <- anova(m_ri_engl_ml, m_rs_engl_ml)
vc_engl <- as.data.frame(VarCorr(m_rs_engl))

cat("Englishness random slope models: done\n")
print(lr_engl)
print(vc_engl)

# =============================================================================
# 3. VARIANCE DECOMPOSITION TABLE
# Compares individual-level slope variance (random slope models) against
# wave-level slope variance (Figure 2 models) to quantify the relative
# contribution of person vs. period heterogeneity.
# =============================================================================

extract_components <- function(vc_df, lr_test, identity_var,
                               wave_var, wave_sd) {
  
  slope_row <- vc_df[vc_df$var1 == identity_var & is.na(vc_df$var2), ]
  int_row   <- vc_df[vc_df$grp == "id" &
                       vc_df$var1 == "(Intercept)" &
                       is.na(vc_df$var2), ]
  resid_row <- vc_df[vc_df$grp == "Residual", ]
  
  data.frame(
    Component = c(
      "SD: random intercept (between-person, id)",
      "SD: random slope — identity (between-person, id)",
      "Variance: random slope — identity (between-person)",
      "SD: residual (within-person)",
      "SD: wave-specific slopes (between-wave, Figure 2)",
      "Variance: wave-specific slopes (between-wave)",
      "Ratio: individual slope SD / wave slope SD",
      "LR χ² (H0: slope variance = 0)",
      "df",
      "p-value (LR test)"
    ),
    Value = c(
      round(int_row$sdcor[1],    4),
      round(slope_row$sdcor[1],  4),
      round(slope_row$vcov[1],   6),
      round(resid_row$sdcor[1],  4),
      round(wave_sd,             4),
      round(wave_var,            6),
      round(slope_row$sdcor[1] / wave_sd, 3),
      round(lr_test$Chisq[2],    3),
      lr_test$Df[2],                          # <-- fixed: was `Chi Df`
      round(lr_test$`Pr(>Chisq)`[2], 4)
    )
  )
}

decomp_brit <- extract_components(
  vc_brit, lr_brit, "brit",
  wave_slope_var_brit, wave_slope_sd_brit
)

decomp_engl <- extract_components(
  vc_engl, lr_engl, "engl",
  wave_slope_var_engl, wave_slope_sd_engl
)

decomp_table <- data.frame(
  Component   = decomp_brit$Component,
  Britishness = decomp_brit$Value,
  Englishness = decomp_engl$Value
)

print(decomp_table)
write.csv(decomp_table, "random_slope_variance_decomposition.csv", row.names = FALSE)
cat("Saved: random_slope_variance_decomposition.csv\n")

# =============================================================================
# 4. FIGURE: BLUP SLOPES vs. WAVE MEAN SLOPES
# Plots the distribution of individual-level BLUP (Best Linear Unbiased
# Prediction) slopes from the random slope models alongside the wave-specific
# slopes from Figure 2, giving a visual comparison of the two sources
# of heterogeneity.
# =============================================================================

# Individual-level BLUP slopes
blups_brit <- ranef(m_rs_brit)$id
blups_engl <- ranef(m_rs_engl)$id

df_blup <- bind_rows(
  data.frame(
    slope    = blups_brit[[2]],   # second column is the slope BLUP
    source   = "Individual (BLUP)",
    identity = "Britishness"
  ),
  data.frame(
    slope    = blups_engl[[2]],
    source   = "Individual (BLUP)",
    identity = "Englishness"
  ),
  data.frame(
    slope    = wave_slopes_brit,
    source   = "Wave (Figure 2)",
    identity = "Britishness"
  ),
  data.frame(
    slope    = wave_slopes_engl,
    source   = "Wave (Figure 2)",
    identity = "Englishness"
  )
) |>
  mutate(
    identity = factor(identity, levels = c("Britishness", "Englishness")),
    source   = factor(source,   levels = c("Individual (BLUP)", "Wave (Figure 2)"))
  )

p_decomp <- ggplot(df_blup, aes(x = slope, fill = source, colour = source)) +
  geom_density(alpha = 0.35, linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  facet_wrap(~ identity, ncol = 2) +
  scale_fill_manual(values   = c("Individual (BLUP)" = "#1B3A57",
                                 "Wave (Figure 2)"   = "#8B3E2F")) +
  scale_colour_manual(values = c("Individual (BLUP)" = "#1B3A57",
                                 "Wave (Figure 2)"   = "#8B3E2F")) +
  labs(
    title    = "Variance Decomposition: Individual vs. Wave-Level Slope Heterogeneity",
    subtitle = paste(
      "Density of individual-level BLUP slopes (random slope model)",
      "vs. wave-specific slopes (Figure 2 models)"
    ),
    x        = "Identity–attitudes slope",
    y        = "Density",
    fill     = NULL,
    colour   = NULL,
    caption  = paste(
      "BLUP slopes from REML random slope models with individual random effects and wave fixed effects.",
      "Wave slopes from individual FE models interacting identity with wave indicators (no wave FE).",
      "Both sets of slopes are expressed as deviations from their respective reference categories.",
      "A wider individual-level distribution relative to the wave-level distribution indicates",
      "substantial between-person heterogeneity in the identity-attitudes relationship."
    )
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    legend.position  = "top",
    legend.text      = element_text(size = 10),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10),
    plot.caption     = element_text(size = 8, colour = "grey35")
  )

print(p_decomp)

ggsave(
  "random_slope_variance_decomposition.png",
  p_decomp,
  width  = 11,
  height = 5,
  dpi    = 300
)
cat("Saved: random_slope_variance_decomposition.png\n")

cat("\n=== Script complete ===\n")
# ============================================================
# Macro marginal effect plots — Tables 6 and 7 (TERCILE VERSION)
# Britishness and Englishness × macroeconomic context
# Identity entered as terciles (Low / Mid / High), ref = Mid
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(haven)
library(fixest)
library(ggplot2)

# ------------------------------------------------------------
# 0) Load data
# ------------------------------------------------------------

df <- readRDS(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v3_England.rds"
)

DV_BASE   <- "immigSelf"
TIME_BASE <- "starttime"
AGE_BASE  <- "age"

macro_vars <- c("gdp_pc", "unempl_rate", "claimant_k", "cpih_rate")

# ------------------------------------------------------------
# 1) Helpers
# ------------------------------------------------------------

get_wave <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(data, base) {
  vars <- names(data)[str_detect(names(data), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave)))
}

zap_if_labelled <- function(x) {
  if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x
}

make_macro_panel <- function(data, identity_base, identity_label) {
  
  w_dv   <- waves_available(data, DV_BASE)
  w_id   <- waves_available(data, identity_base)
  w_time <- waves_available(data, TIME_BASE)
  w_age  <- waves_available(data, AGE_BASE)
  
  w_macro <- reduce(
    map(macro_vars, ~ waves_available(data, .x)),
    intersect
  )
  
  waves <- reduce(
    list(w_dv, w_id, w_time, w_age, w_macro),
    intersect
  )
  
  if (length(waves) < 3) {
    stop("Too few overlapping waves for ", identity_base)
  }
  
  core_cols <- c(
    paste0(DV_BASE,       "W", waves),
    paste0(identity_base, "W", waves),
    paste0(TIME_BASE,     "W", waves),
    paste0(AGE_BASE,      "W", waves)
  )
  
  panel <- data %>%
    mutate(across(any_of(core_cols), zap_if_labelled)) %>%
    select(id, any_of(core_cols)) %>%
    pivot_longer(
      cols = -id,
      names_to = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) %>%
    mutate(
      id = as.character(id),
      wave = as.integer(wave),
      time = as.character(.data[[TIME_BASE]]),
      time = suppressWarnings(ymd_hms(time, quiet = TRUE)),
      year = year(time)
    ) %>%
    rename(
      dv = all_of(DV_BASE),
      identity = all_of(identity_base),
      age = all_of(AGE_BASE)
    ) %>%
    mutate(
      dv = as.numeric(dv),
      identity = as.numeric(identity),
      age = as.numeric(age),
      identity = na_if(identity, 9999),
      identity_label = identity_label
    ) %>%
    filter(
      !is.na(dv),
      !is.na(identity),
      identity %in% 1:7,
      !is.na(age),
      !is.na(wave)
    )
  
  for (v in macro_vars) {
    macro_long <- data %>%
      select(id, all_of(paste0(v, "W", waves))) %>%
      mutate(id = as.character(id)) %>%
      pivot_longer(
        cols = -id,
        names_to = "wave",
        names_pattern = paste0("^", v, "W(\\d+)$"),
        values_to = v
      ) %>%
      mutate(
        wave = as.integer(wave),
        !!v := as.numeric(.data[[v]])
      )
    
    panel <- panel %>%
      left_join(macro_long, by = c("id", "wave"))
  }
  
  panel
}

# ------------------------------------------------------------
# 1b) NEW: tercile construction
# ------------------------------------------------------------
# Rank-based terciles of the continuous identity variable, computed
# on the pooled person-wave panel for that identity. Mid is the
# reference category throughout, consistent with the appendix.

add_identity_tercile <- function(panel) {
  panel %>%
    mutate(
      identity_tercile_num = ntile(identity, 3),
      identity_tercile = factor(
        identity_tercile_num, levels = 1:3,
        labels = c("Low", "Mid", "High")
      ),
      identity_tercile = relevel(identity_tercile, ref = "Mid")
    )
}

# ------------------------------------------------------------
# NEW: marginal effect function for terciles
# ------------------------------------------------------------
# Unlike the continuous case (where a single "identity" main effect
# is identified), here the macro variable's main effect is absorbed
# by wave FE, but the tercile main effects (Low, High vs. Mid) ARE
# identified, since identity_tercile varies across individuals within
# a wave. The marginal effect of being in the Low (or High) tercile,
# relative to Mid, at a given value of the moderator, is:
#
#   ME_tercile(moderator) = beta_tercile + beta_tercile:moderator * moderator
#
# This is the direct tercile analogue of beta_id + beta_int * moderator
# in the continuous make_me() function above.

make_me_tercile <- function(model, moderator_values, moderator_term,
                            identity_label, macro_label) {
  
  b <- coef(model)
  V <- vcov(model)
  
  map_dfr(c("Low", "High"), function(tc) {
    
    main_term <- paste0("identity_tercile", tc)
    int_term  <- names(b)[
      names(b) %in% c(
        paste0(main_term, ":", moderator_term),
        paste0(moderator_term, ":", main_term)
      )
    ]
    
    if (length(int_term) != 1 || !(main_term %in% names(b))) {
      stop("Coefficient not found for ", identity_label, " / ", tc,
           " × ", moderator_term)
    }
    
    beta_main <- b[[main_term]]
    beta_int  <- b[[int_term]]
    
    var_main <- V[main_term, main_term]
    var_int  <- V[int_term, int_term]
    covar    <- V[main_term, int_term]
    
    tibble(
      moderator = moderator_values,
      marginal_effect = beta_main + beta_int * moderator_values,
      se = sqrt(var_main + moderator_values^2 * var_int + 2 * moderator_values * covar),
      tercile = tc,
      identity = identity_label,
      macro = macro_label
    )
  }) %>%
    mutate(
      conf.low = marginal_effect - 1.96 * se,
      conf.high = marginal_effect + 1.96 * se
    )
}

grid_5_95 <- function(x, n = 100) {
  seq(
    quantile(x, 0.05, na.rm = TRUE),
    quantile(x, 0.95, na.rm = TRUE),
    length.out = n
  )
}

# ------------------------------------------------------------
# 2) Build panels
# ------------------------------------------------------------

panel_brit <- make_macro_panel(df, "britishness", "Britishness") %>%
  add_identity_tercile()

panel_engl <- make_macro_panel(df, "englishness", "Englishness") %>%
  add_identity_tercile()

# ------------------------------------------------------------
# 3) Estimate age-adjusted interaction models
#    Identity now enters as terciles (ref = Mid) rather than continuous
# ------------------------------------------------------------

estimate_macro_models <- function(panel) {
  list(
    GDP = feols(
      dv ~ identity_tercile * gdp_pc + age | id + wave,
      data = panel,
      cluster = "id"
    ),
    Unemployment = feols(
      dv ~ identity_tercile * unempl_rate + age | id + wave,
      data = panel,
      cluster = "id"
    ),
    Claimant = feols(
      dv ~ identity_tercile * claimant_k + age | id + wave,
      data = panel,
      cluster = "id"
    ),
    CPIH = feols(
      dv ~ identity_tercile * cpih_rate + age | id + wave,
      data = panel,
      cluster = "id"
    )
  )
}

models_brit <- estimate_macro_models(panel_brit)
models_engl <- estimate_macro_models(panel_engl)

# ------------------------------------------------------------
# 4) Marginal effect data
# ------------------------------------------------------------

gdp_grid   <- grid_5_95(c(panel_brit$gdp_pc, panel_engl$gdp_pc))
unemp_grid <- grid_5_95(c(panel_brit$unempl_rate, panel_engl$unempl_rate))
claim_grid <- grid_5_95(c(panel_brit$claimant_k, panel_engl$claimant_k))
cpih_grid  <- grid_5_95(c(panel_brit$cpih_rate, panel_engl$cpih_rate))

me_macro <- bind_rows(
  make_me_tercile(models_brit$GDP, gdp_grid, "gdp_pc", "Britishness", "GDP per capita"),
  make_me_tercile(models_engl$GDP, gdp_grid, "gdp_pc", "Englishness", "GDP per capita"),
  
  make_me_tercile(models_brit$Unemployment, unemp_grid, "unempl_rate", "Britishness", "Unemployment rate"),
  make_me_tercile(models_engl$Unemployment, unemp_grid, "unempl_rate", "Englishness", "Unemployment rate"),
  
  make_me_tercile(models_brit$Claimant, claim_grid, "claimant_k", "Britishness", "Claimant count"),
  make_me_tercile(models_engl$Claimant, claim_grid, "claimant_k", "Englishness", "Claimant count"),
  
  make_me_tercile(models_brit$CPIH, cpih_grid, "cpih_rate", "Britishness", "CPIH inflation"),
  make_me_tercile(models_engl$CPIH, cpih_grid, "cpih_rate", "Englishness", "CPIH inflation")
) %>%
  mutate(
    moderator_plot = case_when(
      macro == "GDP per capita" ~ moderator / 1000,
      TRUE ~ moderator
    ),
    macro = factor(
      macro,
      levels = c(
        "GDP per capita",
        "Unemployment rate",
        "Claimant count",
        "CPIH inflation"
      )
    ),
    tercile = factor(tercile, levels = c("Low", "High"))
  )

# ------------------------------------------------------------
# 5) Plot
# ------------------------------------------------------------
# Colour = identity (Britishness / Englishness), linetype = tercile
# (Low vs High, relative to Mid). The implicit Mid line is the
# y = 0 reference line, since Mid is the omitted base category.

p_macro_1 <- ggplot(
  me_macro,
  aes(
    x = moderator_plot,
    y = marginal_effect,
    colour = identity,
    fill = identity,
    linetype = tercile
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "black") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.10,
    colour = NA
  ) +
  geom_line(linewidth = 0.9) +
  facet_wrap(
    ~ macro,
    scales = "free_x",
    nrow = 1,
    labeller = labeller(
      macro = c(
        "GDP per capita" = "GDP per capita (£000s)",
        "Unemployment rate" = "Unemployment rate (%)",
        "Claimant count" = "Claimant count (000s)",
        "CPIH inflation" = "CPIH inflation (%)"
      )
    )
  ) +
  scale_colour_manual(
    values = c(
      "Britishness" = "#1B3A57",
      "Englishness" = "#8B3E2F"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Britishness" = "#1B3A57",
      "Englishness" = "#8B3E2F"
    )
  ) +
  scale_linetype_manual(
    values = c("Low" = "solid", "High" = "dashed")
  ) +
  labs(
    title = "Figure 4 (Tercile Robustness): Macro-level activation by identity tercile",
    subtitle = "Marginal effect of Low and High identity terciles, relative to Mid, across UK macroeconomic indicators",
    x = NULL,
    y = "Marginal effect relative to Mid tercile\non immigration attitudes",
    colour = NULL,
    fill = NULL,
    linetype = "Tercile (vs. Mid)",
    caption = paste(
      "Lower values of the dependent variable indicate more restrictive immigration attitudes.",
      "Shaded areas show 95% confidence intervals. Mid tercile is the reference category (effect = 0).",
      "Models include respondent and wave fixed effects; standard errors clustered by respondent."
    )
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8.5, colour = "grey35"),
    axis.title = element_text(size = 10.5),
    axis.text = element_text(size = 9)
  )

print(p_macro_1)

# ------------------------------------------------------------
# 6) Save
# ------------------------------------------------------------

ggsave(
  "h4.1_macro_marginal_effects_tercile.png",
  p_macro_1,
  width = 13,
  height = 5.2,
  dpi = 300
)


# ------------------------------------------------------------
# 1b) Tercile construction — VALUE-BASED (primary specification)
# ------------------------------------------------------------
# Identity scales are heavily concentrated at the top (values 6-7
# account for ~59% of Britishness and ~61% of Englishness responses),
# so a strict rank-based tercile split would arbitrarily divide
# respondents who gave the identical answer into different groups.
# We instead use fixed value-based cut points that never split a tied
# group: Low = 1-5, Mid = 6 (reference), High = 7. This also ensures
# "High" corresponds exactly to the "very strongly identified"
# reference category used in the 7-point categorical specifications
# elsewhere in this appendix. Group sizes are therefore unequal by
# construction (see caption for exact proportions).

add_identity_tercile <- function(panel) {
  panel %>%
    mutate(
      identity_tercile = case_when(
        identity %in% 1:4 ~ "Low",
        identity %in% 5:6     ~ "Mid",
        identity == 7     ~ "High"
      ),
      identity_tercile = factor(identity_tercile, levels = c("Low", "Mid", "High")),
      identity_tercile = relevel(identity_tercile, ref = "Mid")
    )
}
# ------------------------------------------------------------
# NEW: marginal effect function for terciles
# ------------------------------------------------------------
# Unlike the continuous case (where a single "identity" main effect
# is identified), here the macro variable's main effect is absorbed
# by wave FE, but the tercile main effects (Low, High vs. Mid) ARE
# identified, since identity_tercile varies across individuals within
# a wave. The marginal effect of being in the Low (or High) tercile,
# relative to Mid, at a given value of the moderator, is:
#
#   ME_tercile(moderator) = beta_tercile + beta_tercile:moderator * moderator
#
# This is the direct tercile analogue of beta_id + beta_int * moderator
# in the continuous make_me() function above.

make_me_tercile <- function(model, moderator_values, moderator_term,
                            identity_label, macro_label) {
  
  b <- coef(model)
  V <- vcov(model)
  
  map_dfr(c("Low", "High"), function(tc) {
    
    main_term <- paste0("identity_tercile", tc)
    int_term  <- names(b)[
      names(b) %in% c(
        paste0(main_term, ":", moderator_term),
        paste0(moderator_term, ":", main_term)
      )
    ]
    
    if (length(int_term) != 1 || !(main_term %in% names(b))) {
      stop("Coefficient not found for ", identity_label, " / ", tc,
           " × ", moderator_term)
    }
    
    beta_main <- b[[main_term]]
    beta_int  <- b[[int_term]]
    
    var_main <- V[main_term, main_term]
    var_int  <- V[int_term, int_term]
    covar    <- V[main_term, int_term]
    
    tibble(
      moderator = moderator_values,
      marginal_effect = beta_main + beta_int * moderator_values,
      se = sqrt(var_main + moderator_values^2 * var_int + 2 * moderator_values * covar),
      tercile = tc,
      identity = identity_label,
      macro = macro_label
    )
  }) %>%
    mutate(
      conf.low = marginal_effect - 1.96 * se,
      conf.high = marginal_effect + 1.96 * se
    )
}

grid_5_95 <- function(x, n = 100) {
  seq(
    quantile(x, 0.05, na.rm = TRUE),
    quantile(x, 0.95, na.rm = TRUE),
    length.out = n
  )
}

# ------------------------------------------------------------
# 2) Build panels
# ------------------------------------------------------------

panel_brit <- make_macro_panel(df, "britishness", "Britishness") %>%
  add_identity_tercile()

panel_engl <- make_macro_panel(df, "englishness", "Englishness") %>%
  add_identity_tercile()

# ------------------------------------------------------------
# 3) Estimate age-adjusted interaction models
#    Identity now enters as terciles (ref = Mid) rather than continuous
# ------------------------------------------------------------

estimate_macro_models <- function(panel) {
  list(
    GDP = feols(
      dv ~ identity_tercile * gdp_pc + age | id + wave,
      data = panel,
      cluster = "id"
    ),
    Unemployment = feols(
      dv ~ identity_tercile * unempl_rate + age | id + wave,
      data = panel,
      cluster = "id"
    ),
    Claimant = feols(
      dv ~ identity_tercile * claimant_k + age | id + wave,
      data = panel,
      cluster = "id"
    ),
    CPIH = feols(
      dv ~ identity_tercile * cpih_rate + age | id + wave,
      data = panel,
      cluster = "id"
    )
  )
}

models_brit <- estimate_macro_models(panel_brit)
models_engl <- estimate_macro_models(panel_engl)

# ------------------------------------------------------------
# 4) Marginal effect data
# ------------------------------------------------------------

gdp_grid   <- grid_5_95(c(panel_brit$gdp_pc, panel_engl$gdp_pc))
unemp_grid <- grid_5_95(c(panel_brit$unempl_rate, panel_engl$unempl_rate))
claim_grid <- grid_5_95(c(panel_brit$claimant_k, panel_engl$claimant_k))
cpih_grid  <- grid_5_95(c(panel_brit$cpih_rate, panel_engl$cpih_rate))

me_macro <- bind_rows(
  make_me_tercile(models_brit$GDP, gdp_grid, "gdp_pc", "Britishness", "GDP per capita"),
  make_me_tercile(models_engl$GDP, gdp_grid, "gdp_pc", "Englishness", "GDP per capita"),
  
  make_me_tercile(models_brit$Unemployment, unemp_grid, "unempl_rate", "Britishness", "Unemployment rate"),
  make_me_tercile(models_engl$Unemployment, unemp_grid, "unempl_rate", "Englishness", "Unemployment rate"),
  
  make_me_tercile(models_brit$Claimant, claim_grid, "claimant_k", "Britishness", "Claimant count"),
  make_me_tercile(models_engl$Claimant, claim_grid, "claimant_k", "Englishness", "Claimant count"),
  
  make_me_tercile(models_brit$CPIH, cpih_grid, "cpih_rate", "Britishness", "CPIH inflation"),
  make_me_tercile(models_engl$CPIH, cpih_grid, "cpih_rate", "Englishness", "CPIH inflation")
) %>%
  mutate(
    moderator_plot = case_when(
      macro == "GDP per capita" ~ moderator / 1000,
      TRUE ~ moderator
    ),
    macro = factor(
      macro,
      levels = c(
        "GDP per capita",
        "Unemployment rate",
        "Claimant count",
        "CPIH inflation"
      )
    ),
    tercile = factor(tercile, levels = c("Low", "High"))
  )

# ------------------------------------------------------------
# 5) Plot
# ------------------------------------------------------------
# Colour = identity (Britishness / Englishness), linetype = tercile
# (Low vs High, relative to Mid). The implicit Mid line is the
# y = 0 reference line, since Mid is the omitted base category.

p_macro <- ggplot(
  me_macro,
  aes(
    x = moderator_plot,
    y = marginal_effect,
    colour = identity,
    fill = identity,
    linetype = tercile
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "black") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.10,
    colour = NA
  ) +
  geom_line(linewidth = 0.9) +
  facet_wrap(
    ~ macro,
    scales = "free_x",
    nrow = 1,
    labeller = labeller(
      macro = c(
        "GDP per capita" = "GDP per capita (£000s)",
        "Unemployment rate" = "Unemployment rate (%)",
        "Claimant count" = "Claimant count (000s)",
        "CPIH inflation" = "CPIH inflation (%)"
      )
    )
  ) +
  scale_colour_manual(
    values = c(
      "Britishness" = "#1B3A57",
      "Englishness" = "#8B3E2F"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Britishness" = "#1B3A57",
      "Englishness" = "#8B3E2F"
    )
  ) +
  scale_linetype_manual(
    values = c("Low" = "solid", "High" = "dashed")
  ) +
  labs(
    title = "Figure 4 (Tercile Robustness): Macro-level activation by identity tercile",
    subtitle = "Marginal effect of Low and High identity terciles, relative to Mid, across UK macroeconomic indicators",
    x = NULL,
    y = "Marginal effect relative to Mid tercile\non immigration attitudes",
    colour = NULL,
    fill = NULL,
    linetype = "Tercile (vs. Mid)",
    caption = paste(
      "Lower values of the dependent variable indicate more restrictive immigration attitudes.",
      "Tercile defined by fixed cut points (Low = 1-5, Mid = 5-6, High = 7) to avoid splitting tied",
      "respondents; group sizes approx. 22/42/37% (Britishness) and 25/32/43% (Englishness).",
      "Shaded areas show 95% confidence intervals. Mid tercile is the reference category (effect = 0).",
      "Models include respondent and wave fixed effects; standard errors clustered by respondent."
    )
  ) + theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8.5, colour = "grey35"),
    axis.title = element_text(size = 10.5),
    axis.text = element_text(size = 9)
  )

print(p_macro)

# ------------------------------------------------------------
# 6) Save
# ------------------------------------------------------------

ggsave(
  "h4_macro_marginal_effects_tercile.png",
  p_macro,
  width = 13,
  height = 5.2,
  dpi = 300
)

# =============================================================================
# TERCILE COMPARISON PLOT 
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# =============================================================================
# 1. BUILD COMPARISON DATA
# We need three versions of each identity:
#   A) Raw 1-7 distribution
#   B) Rank-based terciles (ntile)
#   C) Value-based terciles (fixed cut points)
# =============================================================================

# Pull raw identity from the already-built panels (before any tercile added)
raw_brit <- make_macro_panel(df, "britishness", "Britishness")
raw_engl <- make_macro_panel(df, "englishness", "Englishness")

# ── A) Original 1-7 distributions ────────────────────────────────────────────
dist_original <- bind_rows(
  raw_brit %>%
    count(identity) %>%
    mutate(pct = 100 * n / sum(n), id_label = "Britishness"),
  raw_engl %>%
    count(identity) %>%
    mutate(pct = 100 * n / sum(n), id_label = "Englishness")
) %>%
  mutate(identity = factor(identity, levels = 1:7))

# ── B) Rank-based terciles ────────────────────────────────────────────────────
dist_rank_brit <- raw_brit %>%
  mutate(
    tercile = case_when(
      dplyr::ntile(identity, 3) == 1 ~ "Low",
      dplyr::ntile(identity, 3) == 2 ~ "Mid",
      dplyr::ntile(identity, 3) == 3 ~ "High"
    ),
    tercile = factor(tercile, levels = c("Low", "Mid", "High"))
  ) %>%
  count(identity, tercile) %>%
  mutate(pct = 100 * n / sum(n), id_label = "Britishness")

dist_rank_engl <- raw_engl %>%
  mutate(
    tercile = case_when(
      dplyr::ntile(identity, 3) == 1 ~ "Low",
      dplyr::ntile(identity, 3) == 2 ~ "Mid",
      dplyr::ntile(identity, 3) == 3 ~ "High"
    ),
    tercile = factor(tercile, levels = c("Low", "Mid", "High"))
  ) %>%
  count(identity, tercile) %>%
  mutate(pct = 100 * n / sum(n), id_label = "Englishness")

dist_rank <- bind_rows(dist_rank_brit, dist_rank_engl) %>%
  mutate(identity = factor(identity, levels = 1:7))

# ── C) Value-based terciles ───────────────────────────────────────────────────
dist_value_brit <- raw_brit %>%
  mutate(
    tercile = case_when(
      identity %in% 1:4 ~ "Low",
      identity %in% 5:6     ~ "Mid",
      identity == 7     ~ "High"
    ),
    tercile = factor(tercile, levels = c("Low", "Mid", "High"))
  ) %>%
  count(identity, tercile) %>%
  mutate(pct = 100 * n / sum(n), id_label = "Britishness")

dist_value_engl <- raw_engl %>%
  mutate(
    tercile = case_when(
      identity %in% 1:4 ~ "Low",
      identity %in% 5:6     ~ "Mid",
      identity == 7     ~ "High"
    ),
    tercile = factor(tercile, levels = c("Low", "Mid", "High"))
  ) %>%
  count(identity, tercile) %>%
  mutate(pct = 100 * n / sum(n), id_label = "Englishness")

dist_value <- bind_rows(dist_value_brit, dist_value_engl) %>%
  mutate(identity = factor(identity, levels = 1:7))

# ── Group size summaries for caption text ─────────────────────────────────────
summary_rank <- dist_rank %>%
  group_by(id_label, tercile) %>%
  summarise(pct_total = round(sum(pct), 1), .groups = "drop")

summary_value <- dist_value %>%
  group_by(id_label, tercile) %>%
  summarise(pct_total = round(sum(pct), 1), .groups = "drop")

cat("Rank-based group sizes:\n")
print(summary_rank)
cat("\nValue-based group sizes:\n")
print(summary_value)

# =============================================================================
# 2. COLOUR PALETTES
# =============================================================================

tercile_colours <- c(
  "Low"  = "#A8C5DA",   # light blue
  "Mid"  = "#4A7FA5",   # mid blue
  "High" = "#1B3A57"    # dark blue (matches paper palette)
)

identity_colours <- c(
  "Britishness" = "#1B3A57",
  "Englishness" = "#8B3E2F"
)

# =============================================================================
# 3. PLOTS
# =============================================================================

base_theme <- theme_classic(base_size = 11, base_family = "Times New Roman") +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "top",
    legend.title     = element_blank(),
    axis.title       = element_text(size = 9.5),
    axis.text        = element_text(size = 8.5),
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, colour = "grey30")
  )

# ── Plot A: Original distribution ─────────────────────────────────────────────
p_original <- ggplot(
  dist_original,
  aes(x = identity, y = pct, fill = id_label)
) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.4, size = 2.5
  ) +
  scale_fill_manual(values = identity_colours) +
  scale_x_discrete(
    labels = c("1\nNot at all", "2", "3", "4", "5", "6", "7\nVery\nstrongly")
  ) +
  scale_y_continuous(limits = c(0, 50), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "A. Original identity distribution (1–7 scale)",
    subtitle = "Both identities heavily concentrated at values 6–7",
    x        = "Identity strength",
    y        = "% of person-wave observations"
  ) +
  base_theme

# ── Plot B: Rank-based terciles ───────────────────────────────────────────────
p_rank <- ggplot(
  dist_rank,
  aes(x = identity, y = pct, fill = tercile, alpha = id_label)
) +
  geom_col(position = "stack", width = 0.7) +
  geom_vline(
    xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
    linetype = "dotted", colour = "grey60", linewidth = 0.3
  ) +
  # Highlight the split values — where ntile cuts through ties
  annotate("rect", xmin = 4.6, xmax = 5.4, ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.08) +
  annotate("rect", xmin = 6.6, xmax = 7.4, ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.08) +
  annotate("text", x = 5, y = 47, label = "Split\n(tied values\ndivided)", 
           colour = "red", size = 2.5, fontface = "italic") +
  annotate("text", x = 7, y = 47, label = "Split\n(tied values\ndivided)", 
           colour = "red", size = 2.5, fontface = "italic") +
  scale_fill_manual(values = tercile_colours) +
  scale_alpha_manual(values = c("Britishness" = 1, "Englishness" = 0.55),
                     guide = "none") +
  scale_x_discrete(
    labels = c("1", "2", "3", "4", "5*", "6", "7*")
  ) +
  scale_y_continuous(limits = c(0, 52), labels = function(x) paste0(x, "%")) +
  facet_wrap(~id_label) +
  labs(
    title    = "B. Rank-based terciles (ntile — equal group sizes)",
    subtitle = "* Values 5 and 7 are arbitrarily split across tercile boundaries (red shading)",
    x        = "Identity strength",
    y        = "% of person-wave observations"
  ) +
  base_theme

# ── Plot C: Value-based terciles ──────────────────────────────────────────────
p_value <- ggplot(
  dist_value,
  aes(x = identity, y = pct, fill = tercile, alpha = id_label)
) +
  geom_col(position = "stack", width = 0.7) +
  # Clean boundary lines
  geom_vline(xintercept = 5.5, linetype = "solid", colour = "grey30",
             linewidth = 0.6) +
  geom_vline(xintercept = 6.5, linetype = "solid", colour = "grey30",
             linewidth = 0.6) +
  annotate("text", x = 3,   y = 47, label = "Low\n(1–5)",
           colour = "#1B3A57", size = 3, fontface = "bold") +
  annotate("text", x = 6,   y = 47, label = "Mid\n(6)",
           colour = "#4A7FA5", size = 3, fontface = "bold") +
  annotate("text", x = 7,   y = 47, label = "High\n(7)",
           colour = "#A8C5DA", size = 3, fontface = "bold") +
  scale_fill_manual(values = tercile_colours) +
  scale_alpha_manual(values = c("Britishness" = 1, "Englishness" = 0.55),
                     guide = "none") +
  scale_x_discrete(
    labels = c("1\nNot at all", "2", "3", "4", "5", "6", "7\nVery\nstrongly")
  ) +
  scale_y_continuous(limits = c(0, 52), labels = function(x) paste0(x, "%")) +
  facet_wrap(~id_label) +
  labs(
    title    = "C. Value-based terciles (fixed cut points: Low=1–5, Mid=6, High=7)",
    subtitle = "No tied values are split; High = 'very strongly identified' (matches 7-point categorical reference elsewhere)",
    x        = "Identity strength",
    y        = "% of person-wave observations"
  ) +
  base_theme

# =============================================================================
# 4. COMBINE AND SAVE
# =============================================================================

p_combined <- p_original / p_rank / p_value +
  plot_annotation(
    title   = "Comparison of identity tercile constructions",
    subtitle = paste(
      "Britishness panel: 398,803 person-wave obs | Englishness panel: 398,301 person-wave obs",
      "\nRank-based group sizes (Britishness): Low 40.8% / Mid 22.4% / High 36.8%",
      "| Value-based: identical proportions by design",
      "\nRank-based group sizes (Englishness): Low 39.0% / Mid 17.8% / High 43.2%",
      "| Value-based: identical proportions by design"
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13,
                                   family = "Times New Roman"),
      plot.subtitle = element_text(size = 8.5, colour = "grey30",
                                   family = "Times New Roman")
    )
  )

print(p_combined)

ggsave(
  "tercile_comparison_supervisors.png",
  p_combined,
  width  = 12,
  height = 14,
  dpi    = 300
)

cat("\nPlot saved: tercile_comparison_supervisors.png\n")

# =============================================================================
# DIAGNOSTIC: Cumulative frequency distribution
# Verifies whether ntile() cut points respect whole ordinal categories
# or split tied values across tercile boundaries.
#
# The reviewer's criterion: tertile boundaries should fall BETWEEN
# ordinal categories (i.e., cumulative % crosses 33.3% and 66.6%
# at a category boundary, not within a category). If ntile() splits
# respondents who gave the SAME answer into different terciles,
# the cut points do NOT respect the ordinal scale.
# =============================================================================

library(kableExtra)

make_cumulative_table <- function(raw_panel, ntile_panel, identity_name) {
  
  # Step 1: raw frequency distribution with cumulative percentages
  freq_table <- raw_panel %>%
    count(identity) %>%
    arrange(identity) %>%
    mutate(
      pct     = 100 * n / sum(n),
      cum_pct = cumsum(pct)
    )
  
  # Step 2: how did ntile() actually assign terciles to each value?
  ntile_check <- ntile_panel %>%
    mutate(
      tercile_ntile = case_when(
        dplyr::ntile(identity, 3) == 1 ~ "Low",
        dplyr::ntile(identity, 3) == 2 ~ "Mid",
        dplyr::ntile(identity, 3) == 3 ~ "High"
      )
    ) %>%
    group_by(identity, tercile_ntile) %>%
    summarise(n_in_tercile = n(), .groups = "drop") %>%
    arrange(identity)
  
  # Step 3: flag any identity value that appears in MORE THAN ONE tercile
  # (this is the smoking gun for within-category splitting)
  split_check <- ntile_check %>%
    group_by(identity) %>%
    summarise(
      terciles_assigned = paste(sort(unique(tercile_ntile)), collapse = " + "),
      is_split = n_distinct(tercile_ntile) > 1,
      .groups = "drop"
    )
  
  # Step 4: combine into one diagnostic table
  combined <- freq_table %>%
    left_join(split_check, by = "identity") %>%
    mutate(
      tercile_boundary = case_when(
        cum_pct <= 33.3 ~ "→ Low",
        cum_pct <= 66.6 ~ "→ Mid",
        TRUE            ~ "→ High"
      ),
      split_flag = ifelse(is_split, "⚠ SPLIT", "")
    ) %>%
    select(
      `Value` = identity,
      `N`     = n,
      `%`     = pct,
      `Cum. %` = cum_pct,
      `Theoretical tercile\n(cumulative threshold)` = tercile_boundary,
      `ntile() assignment` = terciles_assigned,
      `Split?` = split_flag
    ) %>%
    mutate(
      `%`      = round(`%`, 1),
      `Cum. %` = round(`Cum. %`, 1)
    )
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat(identity_name, ": Cumulative frequency diagnostic\n")
  cat(rep("=", 60), "\n", sep = "")
  cat("Tertile thresholds: Low = up to 33.3% | Mid = 33.3–66.6% | High = above 66.6%\n\n")
  print(combined, n = 20)
  
  if (any(split_check$is_split, na.rm = TRUE)) {
    split_vals <- split_check$identity[split_check$is_split]
    cat("\n⚠  WARNING: ntile() splits the following values across tercile boundaries:\n")
    cat("   Values:", paste(split_vals, collapse = ", "), "\n")
    cat("   Respondents who gave IDENTICAL answers are assigned to DIFFERENT terciles.\n")
    cat("   This violates the ordinal-scale criterion described by the reviewer.\n")
  } else {
    cat("\n✓  ntile() cut points fall cleanly between ordinal categories.\n")
    cat("   No tied values are split across tercile boundaries.\n")
  }
  
  invisible(combined)
}

# Run diagnostic for both identities
# Note: raw_brit and raw_engl must already exist (built above)
diag_brit <- make_cumulative_table(raw_brit, raw_brit, "BRITISHNESS")
diag_engl <- make_cumulative_table(raw_engl, raw_engl, "ENGLISHNESS")

# =============================================================================
# VISUALISATION: cumulative percentage plot with tercile thresholds
# Shows clearly where the 33.3% and 66.6% lines fall relative to categories
# =============================================================================

cum_plot_data <- bind_rows(
  raw_brit %>%
    count(identity) %>%
    arrange(identity) %>%
    mutate(
      cum_pct  = cumsum(100 * n / sum(n)),
      id_label = "Britishness"
    ),
  raw_engl %>%
    count(identity) %>%
    arrange(identity) %>%
    mutate(
      cum_pct  = cumsum(100 * n / sum(n)),
      id_label = "Englishness"
    )
) %>%
  mutate(identity = as.integer(identity))

p_cumulative <- ggplot(cum_plot_data,
                       aes(x = identity, y = cum_pct, colour = id_label)) +
  geom_hline(yintercept = 33.3, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  geom_hline(yintercept = 66.6, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  annotate("text", x = 1.2, y = 35, label = "33.3% (Low | Mid boundary)",
           hjust = 0, size = 3, colour = "grey30") +
  annotate("text", x = 1.2, y = 68.6, label = "66.6% (Mid | High boundary)",
           hjust = 0, size = 3, colour = "grey30") +
  geom_step(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0(round(cum_pct, 1), "%")),
    vjust = -0.6, size = 2.8
  ) +
  scale_colour_manual(values = c(
    "Britishness" = "#1B3A57",
    "Englishness" = "#8B3E2F"
  )) +
  scale_x_continuous(
    breaks = 1:7,
    labels = c("1\nNot at all", "2", "3", "4", "5", "6", "7\nVery strongly")
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    labels = function(x) paste0(x, "%")
  ) +
  facet_wrap(~id_label) +
  labs(
    title    = "Cumulative frequency distribution: where do tercile thresholds fall?",
    subtitle = paste(
      "Dashed lines mark the 33.3% and 66.6% thresholds.",
      "If a threshold falls WITHIN a bar (i.e., the cumulative % jumps OVER the line at one value),",
      "ntile() must split that category across two terciles."
    ),
    x        = "Identity strength (1–7)",
    y        = "Cumulative % of person-wave observations",
    colour   = NULL
  ) +
  theme_classic(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position  = "none",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 8.5, colour = "grey30"),
    axis.text        = element_text(size = 8.5)
  )

print(p_cumulative)

# =============================================================================
# CUMULATIVE FREQUENCY TABLE (to send to reviewer)
# =============================================================================

cum_table <- cum_plot_data %>%
  group_by(id_label) %>%
  mutate(
    pct     = 100 * n / sum(n),
    tercile_theoretical = case_when(
      cum_pct <= 33.3 ~ "Low",
      cum_pct <= 66.6 ~ "Mid",
      TRUE            ~ "High"
    ),
    # Flag values where the cumulative % crosses a threshold MID-category
    # i.e., the previous cumulative % was below the threshold
    crossed_33 = lag(cum_pct, default = 0) < 33.3 & cum_pct > 33.3,
    crossed_66 = lag(cum_pct, default = 0) < 66.6 & cum_pct > 66.6,
    flag = case_when(
      crossed_33 ~ "⚠ 33.3% threshold crossed within this category",
      crossed_66 ~ "⚠ 66.6% threshold crossed within this category",
      TRUE        ~ ""
    )
  ) %>%
  ungroup() %>%
  select(
    Identity    = id_label,
    Value       = identity,
    N           = n,
    `%`         = pct,
    `Cum. %`    = cum_pct,
    `Theoretical tercile` = tercile_theoretical,
    `Note`      = flag
  ) %>%
  mutate(
    `%`      = round(`%`, 1),
    `Cum. %` = round(`Cum. %`, 1)
  )

# Print to console
print(cum_table, n = 20)

# Save as CSV to share
write.csv(cum_table, "cumulative_frequency_table.csv", row.names = FALSE)
cat("Table saved: cumulative_frequency_table.csv\n")



ggsave(
  "tercile_cumulative_diagnostic.png",
  p_cumulative,
  width  = 11,
  height = 5,
  dpi    = 300
)

cat("\nDiagnostic plot saved: tercile_cumulative_diagnostic.png\n")

# =============================================================================
# SIMPLEST POSSIBLE EXPLANATION
# One stacked bar per identity. Threshold lines cut through the bars visually.
# =============================================================================

simple_stack <- bind_rows(
  raw_brit %>%
    count(identity) %>%
    mutate(pct = 100 * n / sum(n), id_label = "Britishness"),
  raw_engl %>%
    count(identity) %>%
    mutate(pct = 100 * n / sum(n), id_label = "Englishness")
) %>%
  mutate(
    identity = factor(identity, levels = 1:7),
    # Value-based tercile label for each bar segment
    group = case_when(
      identity %in% c("1","2","3","4","5") ~ "Low (1–5)",
      identity == "6"                      ~ "Mid (6)",
      identity == "7"                      ~ "High (7)"
    ),
    group = factor(group, levels = c("Low (1–5)", "Mid (6)", "High (7)"))
  )

p_stack <- ggplot(simple_stack,
                  aes(x = id_label, y = pct,
                      fill = group, group = identity)) +
  
  # Stacked bars — each segment is one scale value
  geom_col(width = 0.5, colour = "white", linewidth = 0.4) +
  
  # Label each segment with its scale value and %
  geom_text(
    aes(label = paste0(identity, "\n(", round(pct, 1), "%)")),
    position = position_stack(vjust = 0.5),
    size = 3, colour = "white", fontface = "bold"
  ) +
  
  # 33.3% threshold line
  geom_hline(yintercept = 33.3, colour = "#CC0000",
             linewidth = 1, linetype = "dashed") +
  annotate("text", x = 2.35, y = 35,
           label = "33.3% — ntile() Low | Mid boundary",
           colour = "#CC0000", size = 3.2, hjust = 0, fontface = "bold") +
  
  # 66.6% threshold line
  geom_hline(yintercept = 66.6, colour = "#CC0000",
             linewidth = 1, linetype = "dashed") +
  annotate("text", x = 2.35, y = 68.6,
           label = "66.6% — ntile() Mid | High boundary",
           colour = "#CC0000", size = 3.2, hjust = 0, fontface = "bold") +
  
  # Arrows pointing to the problem segments
  annotate("segment",
           x = 2.32, xend = 2.27, y = 37, yend = 39,
           arrow = arrow(length = unit(0.2, "cm")),
           colour = "#CC0000", linewidth = 0.7) +
  annotate("text", x = 2.33, y = 36.5,
           label = "Line cuts through\nvalue 5 (19.2% wide)\n→ ntile() splits it",
           colour = "#CC0000", size = 2.8, hjust = 0, lineheight = 1.2) +
  
  annotate("segment",
           x = 2.32, xend = 2.27, y = 70, yend = 75,
           arrow = arrow(length = unit(0.2, "cm")),
           colour = "#CC0000", linewidth = 0.7) +
  annotate("text", x = 2.33, y = 69.5,
           label = "Line cuts through\nvalue 7 (36.8% wide)\n→ ntile() splits it",
           colour = "#CC0000", size = 2.8, hjust = 0, lineheight = 1.2) +
  
  scale_fill_manual(values = c(
    "Low (1–5)" = "#A8C5DA",
    "Mid (6)"   = "#4A7FA5",
    "High (7)"  = "#1B3A57"
  )) +
  
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 108),
    expand = c(0, 0)
  ) +
  
  coord_flip() +
  
  labs(
    title   = "Where do the tercile thresholds land?",
    subtitle = paste(
      "Each coloured segment = one scale position (1–7).",
      "Red dashed lines show where ntile() tries to cut at 33.3% and 66.6%.",
      "\nBoth lines cut through a segment rather than between segments",
      "— so ntile() splits respondents who gave the same answer into different groups."
    ),
    x       = NULL,
    y       = "Cumulative % of respondents",
    fill    = "Value-based tercile",
    caption = paste(
      "Solution: assign whole segments to terciles.",
      "Low = 1–5 (ends at 40.8%), Mid = 6 (ends at 63.2%), High = 7 (ends at 100%).",
      "Groups are unequal in size — but no respondent with the same answer",
      "is ever assigned to a different group."
    )
  ) +
  
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    legend.position  = "top",
    legend.text      = element_text(size = 10),
    legend.title     = element_text(size = 10, face = "bold"),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9.5, colour = "grey20",
                                    lineheight = 1.3),
    plot.caption     = element_text(size = 8.5, colour = "grey35",
                                    lineheight = 1.3),
    axis.text.y      = element_text(size = 11, face = "bold"),
    axis.text.x      = element_text(size = 9),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3)
  )

print(p_stack)

ggsave(
  "tercile_simplest_explanation.png",
  p_stack,
  width  = 11,
  height = 5,
  dpi    = 300
)
cat("Saved: tercile_simplest_explanation.png\n")
# ============================================================
# Marginal effect plots for Tables 4 and 5
# H3: Meso-level constituency economic context
#
# Plots:
#   1. Marginal effect of Britishness across meso indicators
#   2. Marginal effect of Englishness across meso indicators
#
# Models:
#   Table 4: Britishness × constituency unemployment / claimant / income
#   Table 5: Englishness × constituency unemployment / claimant / income
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(haven)
library(purrr)
library(fixest)
library(ggplot2)
library(here)
library(patchwork)

# ------------------------------------------------------------
# 0) Load data
# ------------------------------------------------------------

df <- readRDS(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v3_England.rds"
)

DV_BASE   <- "immigSelf"
TIME_BASE <- "starttime"
AGE_BASE  <- "age"

MESO_UNEMP_BASE  <- "unemp_rate_mean"
MESO_CLAIM_BASE  <- "claimant_mean"
MESO_INCOME_BASE <- "income"

# ------------------------------------------------------------
# 1) Helper functions
# ------------------------------------------------------------

get_wave <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(data, base) {
  vars <- names(data)[str_detect(names(data), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave)))
}

make_core_panel <- function(data, identity_base) {
  
  w_dv   <- waves_available(data, DV_BASE)
  w_id   <- waves_available(data, identity_base)
  w_time <- waves_available(data, TIME_BASE)
  w_age  <- waves_available(data, AGE_BASE)
  
  waves <- Reduce(intersect, list(w_dv, w_id, w_time, w_age))
  
  if (length(waves) < 3) {
    stop("Too few overlapping waves for ", identity_base)
  }
  
  core_cols <- c(
    paste0(DV_BASE,      "W", waves),
    paste0(identity_base,"W", waves),
    paste0(TIME_BASE,    "W", waves),
    paste0(AGE_BASE,     "W", waves)
  )
  
  data %>%
    mutate(across(any_of(core_cols), ~ {
      x <- .x
      if (inherits(x, "haven_labelled")) x <- zap_labels(x)
      x
    })) %>%
    select(id, any_of(core_cols)) %>%
    pivot_longer(
      cols = -id,
      names_to = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) %>%
    mutate(
      id = as.character(id),
      wave = as.integer(wave),
      starttime_chr = as.character(.data[[TIME_BASE]]),
      starttime_parsed = suppressWarnings(ymd_hms(starttime_chr, quiet = TRUE)),
      starttime_parsed = coalesce(
        starttime_parsed,
        suppressWarnings(ymd(starttime_chr, quiet = TRUE))
      ),
      year = year(starttime_parsed)
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
      identity = na_if(identity, 9999)
    ) %>%
    filter(
      !is.na(dv),
      !is.na(identity),
      identity %in% 1:7,
      !is.na(age),
      !is.na(wave)
    )
}

make_meso_long <- function(data, base, out_name, waves_keep) {
  
  cols <- names(data) %>%
    str_subset(paste0("^", base, "W\\d+$"))
  
  if (length(cols) == 0) return(NULL)
  
  tmp <- tibble(col = cols) %>%
    mutate(wave = as.integer(str_extract(col, "\\d+$"))) %>%
    filter(wave %in% waves_keep)
  
  if (nrow(tmp) == 0) return(NULL)
  
  use_cols <- tmp$col
  
  data %>%
    select(id, all_of(use_cols)) %>%
    mutate(
      id = as.character(id),
      across(
        all_of(use_cols),
        ~ as.numeric(if (inherits(.x, "haven_labelled")) zap_labels(.x) else .x)
      )
    ) %>%
    pivot_longer(
      cols = -id,
      names_to = "wave",
      values_to = out_name,
      names_pattern = paste0("^", base, "W(\\d+)$")
    ) %>%
    mutate(wave = as.integer(wave))
}

add_meso_vars <- function(core_panel, data) {
  
  waves_keep <- sort(unique(core_panel$wave))
  
  meso_unemp <- make_meso_long(data, MESO_UNEMP_BASE,  "unemp_rate_mean", waves_keep)
  meso_claim <- make_meso_long(data, MESO_CLAIM_BASE,  "claimant_mean",   waves_keep)
  meso_inc   <- make_meso_long(data, MESO_INCOME_BASE, "income_meso",     waves_keep)
  
  out <- core_panel
  
  if (!is.null(meso_unemp)) out <- left_join(out, meso_unemp, by = c("id", "wave"))
  if (!is.null(meso_claim)) out <- left_join(out, meso_claim, by = c("id", "wave"))
  if (!is.null(meso_inc))   out <- left_join(out, meso_inc,   by = c("id", "wave"))
  
  out
}

make_marginal_effect_df <- function(model,
                                    moderator_values,
                                    identity_term,
                                    moderator_term,
                                    label,
                                    moderator_name) {
  
  b <- coef(model)
  V <- vcov(model)
  
  interaction_term <- names(b)[
    names(b) %in% c(
      paste0(identity_term, ":", moderator_term),
      paste0(moderator_term, ":", identity_term)
    )
  ]
  
  if (length(interaction_term) != 1) {
    stop(
      "Could not identify interaction term for ",
      label, " × ", moderator_term,
      "\nTerms are: ", paste(names(b), collapse = ", ")
    )
  }
  
  beta_id  <- b[[identity_term]]
  beta_int <- b[[interaction_term]]
  
  var_id  <- V[identity_term, identity_term]
  var_int <- V[interaction_term, interaction_term]
  covar   <- V[identity_term, interaction_term]
  
  tibble(
    moderator = moderator_values,
    marginal_effect = beta_id + beta_int * moderator,
    se = sqrt(var_id + moderator^2 * var_int + 2 * moderator * covar),
    identity = label,
    moderator_name = moderator_name
  ) %>%
    mutate(
      conf.low = marginal_effect - 1.96 * se,
      conf.high = marginal_effect + 1.96 * se
    )
}

# ------------------------------------------------------------
# 2) Build Britishness and Englishness meso panels
# ------------------------------------------------------------

panel_brit <- make_core_panel(df, "britishness") %>%
  add_meso_vars(df)

panel_engl <- make_core_panel(df, "englishness") %>%
  add_meso_vars(df)

# ------------------------------------------------------------
# 3) Estimate age-adjusted interaction models
#    These correspond to the main interaction columns in Tables 4 and 5
# ------------------------------------------------------------

# Britishness models — Table 4
panel_u_brit <- panel_brit %>% filter(!is.na(unemp_rate_mean))
panel_c_brit <- panel_brit %>% filter(!is.na(claimant_mean))
panel_i_brit <- panel_brit %>% filter(!is.na(income_meso))

m_u_brit <- feols(
  dv ~ identity * unemp_rate_mean + age | id + wave,
  data = panel_u_brit,
  cluster = "id"
)

m_c_brit <- feols(
  dv ~ identity * claimant_mean + age | id + wave,
  data = panel_c_brit,
  cluster = "id"
)

m_i_brit <- feols(
  dv ~ identity * income_meso + age | id + wave,
  data = panel_i_brit,
  cluster = "id"
)

# Englishness models — Table 5
panel_u_engl <- panel_engl %>% filter(!is.na(unemp_rate_mean))
panel_c_engl <- panel_engl %>% filter(!is.na(claimant_mean))
panel_i_engl <- panel_engl %>% filter(!is.na(income_meso))

m_u_engl <- feols(
  dv ~ identity * unemp_rate_mean + age | id + wave,
  data = panel_u_engl,
  cluster = "id"
)

m_c_engl <- feols(
  dv ~ identity * claimant_mean + age | id + wave,
  data = panel_c_engl,
  cluster = "id"
)

m_i_engl <- feols(
  dv ~ identity * income_meso + age | id + wave,
  data = panel_i_engl,
  cluster = "id"
)

# ------------------------------------------------------------
# 4) Create moderator grids
#    Use 5th–95th percentiles to avoid plotting unsupported tails
# ------------------------------------------------------------

grid_5_95 <- function(x, n = 100) {
  seq(
    quantile(x, 0.05, na.rm = TRUE),
    quantile(x, 0.95, na.rm = TRUE),
    length.out = n
  )
}

unemp_grid <- grid_5_95(c(panel_u_brit$unemp_rate_mean, panel_u_engl$unemp_rate_mean))
claim_grid <- grid_5_95(c(panel_c_brit$claimant_mean,   panel_c_engl$claimant_mean))
inc_grid   <- grid_5_95(c(panel_i_brit$income_meso,     panel_i_engl$income_meso))

# ------------------------------------------------------------
# 5) Compute marginal effects
# ------------------------------------------------------------

me_meso <- bind_rows(
  make_marginal_effect_df(
    m_u_brit, unemp_grid,
    identity_term = "identity",
    moderator_term = "unemp_rate_mean",
    label = "Britishness",
    moderator_name = "Constituency unemployment rate"
  ),
  make_marginal_effect_df(
    m_u_engl, unemp_grid,
    identity_term = "identity",
    moderator_term = "unemp_rate_mean",
    label = "Englishness",
    moderator_name = "Constituency unemployment rate"
  ),
  make_marginal_effect_df(
    m_c_brit, claim_grid,
    identity_term = "identity",
    moderator_term = "claimant_mean",
    label = "Britishness",
    moderator_name = "Claimant count"
  ),
  make_marginal_effect_df(
    m_c_engl, claim_grid,
    identity_term = "identity",
    moderator_term = "claimant_mean",
    label = "Englishness",
    moderator_name = "Claimant count"
  ),
  make_marginal_effect_df(
    m_i_brit, inc_grid,
    identity_term = "identity",
    moderator_term = "income_meso",
    label = "Britishness",
    moderator_name = "Constituency median weekly earnings"
  ),
  make_marginal_effect_df(
    m_i_engl, inc_grid,
    identity_term = "identity",
    moderator_term = "income_meso",
    label = "Englishness",
    moderator_name = "Constituency median weekly earnings"
  )
) %>%
  mutate(
    moderator_plot = case_when(
      moderator_name == "Claimant count" ~ moderator / 1000,
      moderator_name == "Constituency median weekly earnings" ~ moderator,
      TRUE ~ moderator
    ),
    moderator_axis = case_when(
      moderator_name == "Constituency unemployment rate" ~ "Unemployment rate (%)",
      moderator_name == "Claimant count" ~ "Claimant count (000s)",
      moderator_name == "Constituency median weekly earnings" ~ "Median weekly earnings (£)"
    )
  )

# ------------------------------------------------------------
# 6) General plotting function
# ------------------------------------------------------------

plot_meso_me <- function(data, mod_name, xlab) {
  
  ggplot(
    data %>% filter(moderator_name == mod_name),
    aes(
      x = moderator_plot,
      y = marginal_effect,
      colour = identity,
      fill = identity
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = "black") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.12,
      colour = NA
    ) +
    geom_line(linewidth = 0.9) +
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
    labs(
      x = xlab,
      y = "Marginal effect of identity on immigration attitudes",
      colour = NULL,
      fill = NULL
    ) +
    theme_classic(base_size = 12, base_family = "Times New Roman") +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 10.5),
      axis.text = element_text(size = 9.5),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10)
    )
}

# ------------------------------------------------------------
# 7) Produce individual plots
# ------------------------------------------------------------

p_unemp <- plot_meso_me(
  me_meso,
  "Constituency unemployment rate",
  "Constituency unemployment rate (%)"
) +
  labs(
    title = "Unemployment",
    subtitle = "Local labour-market stress"
  )

p_claim <- plot_meso_me(
  me_meso,
  "Claimant count",
  "Claimant count (000s)"
) +
  labs(
    title = "Claimant count",
    subtitle = "Local welfare dependency"
  )

p_income <- plot_meso_me(
  me_meso,
  "Constituency median weekly earnings",
  "Median weekly earnings (£)"
) +
  labs(
    title = "Constituency earnings",
    subtitle = "Local prosperity"
  )

# ------------------------------------------------------------
# 8) Combined figure for slide
# ------------------------------------------------------------

p_meso_combined <- (
  p_unemp + p_claim + p_income
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Figure 3: Meso-level activation: local economic context conditions identity effects",
    subtitle = "Marginal effect of national identity across constituency-level economic indicators",
    caption = paste(
      "Lower values of the dependent variable indicate more restrictive immigration attitudes.",
      "Shaded areas show 95% confidence intervals.",
      "Models include respondent and wave fixed effects; standard errors clustered by respondent."
    ),
    theme = theme(
      plot.title = element_text(
        family = "Times New Roman",
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(
        family = "Times New Roman",
        size = 11
      ),
      plot.caption = element_text(
        family = "Times New Roman",
        size = 8.5,
        colour = "grey35"
      )
    )
  ) &
  theme(
    legend.position = "top",
    legend.text = element_text(family = "Times New Roman", size = 10)
  )

print(p_meso_combined)

# ------------------------------------------------------------
# 9) Optional: separate Britishness and Englishness figures
# ------------------------------------------------------------

plot_meso_by_identity <- function(data, identity_name) {
  
  ggplot(
    data %>% filter(identity == identity_name),
    aes(
      x = moderator_plot,
      y = marginal_effect
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = "black") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.15,
      fill = ifelse(identity_name == "Britishness", "#1B3A57", "#8B3E2F"),
      colour = NA
    ) +
    geom_line(
      linewidth = 0.9,
      colour = ifelse(identity_name == "Britishness", "#1B3A57", "#8B3E2F")
    ) +
    facet_wrap(
      ~ moderator_axis,
      scales = "free_x",
      nrow = 1
    ) +
    labs(
      title = paste0(identity_name, ": marginal effects across local economic context"),
      x = NULL,
      y = "Marginal effect on immigration attitudes",
      caption = "Lower values of the dependent variable indicate more restrictive immigration attitudes. Shaded areas show 95% confidence intervals."
    ) +
    theme_classic(base_size = 12, base_family = "Times New Roman") +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.caption = element_text(size = 8.5, colour = "grey35"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(size = 8.5)
    )
}

p_meso_brit <- plot_meso_by_identity(me_meso, "Britishness")
p_meso_engl <- plot_meso_by_identity(me_meso, "Englishness")

print(p_meso_brit)
print(p_meso_engl)

# ------------------------------------------------------------
# 10) Save figures
# ------------------------------------------------------------

dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  here("h3_meso_marginal_effects_combined.png"),
  p_meso_combined,
  width = 13,
  height = 5.2,
  dpi = 300
)

ggsave(
  here("outputs", "figures", "h3_meso_marginal_effects_britishness.png"),
  p_meso_brit,
  width = 11,
  height = 4.8,
  dpi = 300
)

ggsave(
  here("outputs", "figures", "h3_meso_marginal_effects_englishness.png"),
  p_meso_engl,
  width = 11,
  height = 4.8,
  dpi = 300
)
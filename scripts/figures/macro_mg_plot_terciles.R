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
        identity %in% 1:5 ~ "Low",
        identity == 6     ~ "Mid",
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
      "Tercile defined by fixed cut points (Low = 1-5, Mid = 6, High = 7) to avoid splitting tied",
      "respondents; group sizes approx. 41/22/37% (Britishness) and 39/18/43% (Englishness).",
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
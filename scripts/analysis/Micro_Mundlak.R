# ============================================================
# MUNDLAK MICRO PIPELINE
# Correlated random effects for all micro-level interaction models
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(haven)
library(purrr)
library(lme4)
library(lmerTest)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
df <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v3_England.rds")
eq_long <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/equivalised_income_long.rds")

eq_long <- eq_long %>%
  mutate(
    id   = as.character(id),
    wave = as.integer(wave)
  )

DV_BASE    <- "immigSelf"
BRIT_BASE  <- "britishness"
ENG_BASE   <- "englishness"
TIME_BASE  <- "starttime"
AGE_BASE   <- "age"
INCOME_BASE <- "income"
RISK_BASE   <- "riskUnemployment"

# ------------------------------------------------------------
# Helpers 
# ------------------------------------------------------------
get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(df, base){
  vars <- names(df)[str_detect(names(df), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave_num)))
}

make_long_with_income <- function(df,
                                  dv_base,
                                  brit_base,
                                  time_base,
                                  income_base,
                                  age_base = NULL){
  
  w_dv     <- waves_available(df, dv_base)
  w_brit   <- waves_available(df, brit_base)
  w_time   <- waves_available(df, time_base)
  w_income <- waves_available(df, income_base)
  
  waves <- Reduce(intersect, list(w_dv, w_brit, w_time, w_income))
  
  if(!is.null(age_base)){
    w_age <- waves_available(df, age_base)
    waves <- intersect(waves, w_age)
  }
  
  if(length(waves) < 3){
    stop("Too few overlapping waves across DV, identity, time, income/risk (and age).")
  }
  
  dv_cols     <- paste0(dv_base,     "W", waves)
  brit_cols   <- paste0(brit_base,   "W", waves)
  time_cols   <- paste0(time_base,   "W", waves)
  income_cols <- paste0(income_base, "W", waves)
  age_cols    <- if(!is.null(age_base)) paste0(age_base, "W", waves) else NULL
  
  df2 <- df |>
    mutate(
      across(all_of(dv_cols),     ~ as.numeric(zap_labels(.x))),
      across(all_of(brit_cols),   ~ as.numeric(zap_labels(.x))),
      across(all_of(income_cols), ~ as.numeric(zap_labels(.x))),
      across(all_of(age_cols),    ~ as.numeric(zap_labels(.x)))
    )
  
  df2 |>
    select(id,
           all_of(dv_cols),
           all_of(brit_cols),
           all_of(time_cols),
           all_of(income_cols),
           all_of(age_cols)) |>
    pivot_longer(
      cols = -id,
      names_to = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) |>
    mutate(
      id = as.character(id),
      wave = as.integer(wave),
      starttime_chr = as.character(.data[[time_base]]),
      starttime_parsed = suppressWarnings(ymd_hms(starttime_chr, quiet = TRUE)),
      starttime_parsed = coalesce(
        starttime_parsed,
        suppressWarnings(ymd(starttime_chr, quiet = TRUE))
      ),
      year = year(starttime_parsed)
    ) |>
    rename(
      dv     = all_of(dv_base),
      ident  = all_of(brit_base),
      econ   = all_of(income_base),
      age    = if(!is.null(age_base)) all_of(age_base)
    ) |>
    filter(!is.na(dv), !is.na(ident), !is.na(econ), !is.na(wave))
}

# ------------------------------------------------------------
# Build identity-specific panels
# ------------------------------------------------------------
build_identity_econ_panel <- function(identity_base,
                                      identity_name = c("brit","engl"),
                                      econ_base,
                                      age = FALSE){
  
  identity_name <- match.arg(identity_name)
  
  out <- make_long_with_income(
    df = df,
    dv_base = DV_BASE,
    brit_base = identity_base,
    time_base = TIME_BASE,
    income_base = econ_base,
    age_base = if(age) AGE_BASE else NULL
  )
  
  # rename identity/economic variables cleanly
  names(out)[names(out) == "ident"] <- identity_name
  
  econ_name <- if(econ_base == INCOME_BASE) "income" else
    if(econ_base == RISK_BASE) "risk_unemp" else econ_base
  
  names(out)[names(out) == "econ"] <- econ_name
  
  # clean valid identity scale
  out <- out %>%
    mutate(
      !!identity_name := if_else(.data[[identity_name]] %in% 1:7, .data[[identity_name]], NA_real_)
    ) %>%
    filter(!is.na(.data[[identity_name]]))
  
  # clean risk scale if relevant
  if(econ_name == "risk_unemp"){
    out <- out %>%
      mutate(risk_unemp = if_else(between(risk_unemp, 0, 10), risk_unemp, NA_real_)) %>%
      filter(!is.na(risk_unemp))
  }
  
  out
}

# ------------------------------------------------------------
# Add equivalised income to an identity panel
# ------------------------------------------------------------
add_eq_income <- function(panel){
  panel %>%
    left_join(eq_long, by = c("id", "wave"))
}

# ------------------------------------------------------------
# Mundlak helper:
# for every time-varying regressor, create person mean and within deviation
# ------------------------------------------------------------
add_mundlak_terms <- function(data, vars, id_var = "id"){
  out <- data %>%
    group_by(.data[[id_var]]) %>%
    mutate(
      across(
        all_of(vars),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_mean"
      )
    ) %>%
    ungroup()
  
  for (v in vars) {
    out[[paste0(v, "_w")]] <- out[[v]] - out[[paste0(v, "_mean")]]
  }
  
  out
}
# ------------------------------------------------------------
# Estimation helper
# ------------------------------------------------------------
ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e5)
)

fit_mundlak_interaction <- function(data, identity_var, econ_var, include_age = FALSE){
  vars_needed <- c(identity_var, econ_var, if(include_age) "age")
  d <- add_mundlak_terms(data, vars = vars_needed, id_var = "id")
  
  if(include_age){
    fml <- as.formula(
      paste0(
        "dv ~ ",
        identity_var, "_w * ", econ_var, "_w + ",
        identity_var, "_mean + ", econ_var, "_mean + ",
        "age_w + age_mean + ",
        "(1 | id) + (1 | wave)"
      )
    )
  } else {
    fml <- as.formula(
      paste0(
        "dv ~ ",
        identity_var, "_w * ", econ_var, "_w + ",
        identity_var, "_mean + ", econ_var, "_mean + ",
        "(1 | id) + (1 | wave)"
      )
    )
  }
  
  lmer(
    formula = fml,
    data = d,
    REML = TRUE,
    control = ctrl
  )
}

# ============================================================
# BUILD ALL MICRO PANELS
# ============================================================

# -------------------------
# Britishness × raw income
# -------------------------
panel_brit_income_noage <- build_identity_econ_panel(
  identity_base = BRIT_BASE,
  identity_name = "brit",
  econ_base = INCOME_BASE,
  age = FALSE
)

panel_brit_income_age <- build_identity_econ_panel(
  identity_base = BRIT_BASE,
  identity_name = "brit",
  econ_base = INCOME_BASE,
  age = TRUE
)

# -------------------------
# Britishness × unemployment risk
# -------------------------
panel_brit_risk_noage <- build_identity_econ_panel(
  identity_base = BRIT_BASE,
  identity_name = "brit",
  econ_base = RISK_BASE,
  age = FALSE
)

panel_brit_risk_age <- build_identity_econ_panel(
  identity_base = BRIT_BASE,
  identity_name = "brit",
  econ_base = RISK_BASE,
  age = TRUE
)

# -------------------------
# Englishness × raw income
# -------------------------
panel_engl_income_noage <- build_identity_econ_panel(
  identity_base = ENG_BASE,
  identity_name = "engl",
  econ_base = INCOME_BASE,
  age = FALSE
)

panel_engl_income_age <- build_identity_econ_panel(
  identity_base = ENG_BASE,
  identity_name = "engl",
  econ_base = INCOME_BASE,
  age = TRUE
)

# -------------------------
# Englishness × unemployment risk
# -------------------------
panel_engl_risk_noage <- build_identity_econ_panel(
  identity_base = ENG_BASE,
  identity_name = "engl",
  econ_base = RISK_BASE,
  age = FALSE
)

panel_engl_risk_age <- build_identity_econ_panel(
  identity_base = ENG_BASE,
  identity_name = "engl",
  econ_base = RISK_BASE,
  age = TRUE
)

# -------------------------
# Add equivalised income
# -------------------------
panel_brit_eq_noage <- add_eq_income(panel_brit_income_noage)
panel_brit_eq_age   <- add_eq_income(panel_brit_income_age)

panel_engl_eq_noage <- add_eq_income(panel_engl_income_noage)
panel_engl_eq_age   <- add_eq_income(panel_engl_income_age)

# -------------------------
# Restrict to non-missing eq03 / eq05
# -------------------------
panel_brit_eq03_noage <- panel_brit_eq_noage %>% filter(!is.na(income_eq_child03))
panel_brit_eq03_age   <- panel_brit_eq_age   %>% filter(!is.na(income_eq_child03))
panel_brit_eq05_noage <- panel_brit_eq_noage %>% filter(!is.na(income_eq_child05))
panel_brit_eq05_age   <- panel_brit_eq_age   %>% filter(!is.na(income_eq_child05))

panel_engl_eq03_noage <- panel_engl_eq_noage %>% filter(!is.na(income_eq_child03))
panel_engl_eq03_age   <- panel_engl_eq_age   %>% filter(!is.na(income_eq_child03))
panel_engl_eq05_noage <- panel_engl_eq_noage %>% filter(!is.na(income_eq_child05))
panel_engl_eq05_age   <- panel_engl_eq_age   %>% filter(!is.na(income_eq_child05))

# ============================================================
# ESTIMATE ALL MUNDLAK MICRO INTERACTION MODELS
# ============================================================

# -------------------------
# A) BRITISHNESS
# -------------------------

# Raw income
m_mundlak_brit_income_noage <- fit_mundlak_interaction(
  data = panel_brit_income_noage,
  identity_var = "brit",
  econ_var = "income",
  include_age = FALSE
)

m_mundlak_brit_income_age <- fit_mundlak_interaction(
  data = panel_brit_income_age,
  identity_var = "brit",
  econ_var = "income",
  include_age = TRUE
)

# Equivalised income 0.3
m_mundlak_brit_eq03_noage <- fit_mundlak_interaction(
  data = panel_brit_eq03_noage,
  identity_var = "brit",
  econ_var = "income_eq_child03",
  include_age = FALSE
)

m_mundlak_brit_eq03_age <- fit_mundlak_interaction(
  data = panel_brit_eq03_age,
  identity_var = "brit",
  econ_var = "income_eq_child03",
  include_age = TRUE
)

# Equivalised income 0.5
m_mundlak_brit_eq05_noage <- fit_mundlak_interaction(
  data = panel_brit_eq05_noage,
  identity_var = "brit",
  econ_var = "income_eq_child05",
  include_age = FALSE
)

m_mundlak_brit_eq05_age <- fit_mundlak_interaction(
  data = panel_brit_eq05_age,
  identity_var = "brit",
  econ_var = "income_eq_child05",
  include_age = TRUE
)

# Unemployment risk
m_mundlak_brit_risk_noage <- fit_mundlak_interaction(
  data = panel_brit_risk_noage,
  identity_var = "brit",
  econ_var = "risk_unemp",
  include_age = FALSE
)

m_mundlak_brit_risk_age <- fit_mundlak_interaction(
  data = panel_brit_risk_age,
  identity_var = "brit",
  econ_var = "risk_unemp",
  include_age = TRUE
)

# -------------------------
# B) ENGLISHNESS
# -------------------------

# Raw income
m_mundlak_engl_income_noage <- fit_mundlak_interaction(
  data = panel_engl_income_noage,
  identity_var = "engl",
  econ_var = "income",
  include_age = FALSE
)

m_mundlak_engl_income_age <- fit_mundlak_interaction(
  data = panel_engl_income_age,
  identity_var = "engl",
  econ_var = "income",
  include_age = TRUE
)

# Equivalised income 0.3
m_mundlak_engl_eq03_noage <- fit_mundlak_interaction(
  data = panel_engl_eq03_noage,
  identity_var = "engl",
  econ_var = "income_eq_child03",
  include_age = FALSE
)

m_mundlak_engl_eq03_age <- fit_mundlak_interaction(
  data = panel_engl_eq03_age,
  identity_var = "engl",
  econ_var = "income_eq_child03",
  include_age = TRUE
)

# Equivalised income 0.5
m_mundlak_engl_eq05_noage <- fit_mundlak_interaction(
  data = panel_engl_eq05_noage,
  identity_var = "engl",
  econ_var = "income_eq_child05",
  include_age = FALSE
)

m_mundlak_engl_eq05_age <- fit_mundlak_interaction(
  data = panel_engl_eq05_age,
  identity_var = "engl",
  econ_var = "income_eq_child05",
  include_age = TRUE
)

# Unemployment risk
m_mundlak_engl_risk_noage <- fit_mundlak_interaction(
  data = panel_engl_risk_noage,
  identity_var = "engl",
  econ_var = "risk_unemp",
  include_age = FALSE
)

m_mundlak_engl_risk_age <- fit_mundlak_interaction(
  data = panel_engl_risk_age,
  identity_var = "engl",
  econ_var = "risk_unemp",
  include_age = TRUE
)

# ============================================================
# COLLECT MODELS
# ============================================================

models_micro_mundlak <- list(
  # Britishness
  brit_income_noage_mundlak = m_mundlak_brit_income_noage,
  brit_income_age_mundlak   = m_mundlak_brit_income_age,
  brit_eq03_noage_mundlak   = m_mundlak_brit_eq03_noage,
  brit_eq03_age_mundlak     = m_mundlak_brit_eq03_age,
  brit_eq05_noage_mundlak   = m_mundlak_brit_eq05_noage,
  brit_eq05_age_mundlak     = m_mundlak_brit_eq05_age,
  brit_risk_noage_mundlak   = m_mundlak_brit_risk_noage,
  brit_risk_age_mundlak     = m_mundlak_brit_risk_age,
  
  # Englishness
  engl_income_noage_mundlak = m_mundlak_engl_income_noage,
  engl_income_age_mundlak   = m_mundlak_engl_income_age,
  engl_eq03_noage_mundlak   = m_mundlak_engl_eq03_noage,
  engl_eq03_age_mundlak     = m_mundlak_engl_eq03_age,
  engl_eq05_noage_mundlak   = m_mundlak_engl_eq05_noage,
  engl_eq05_age_mundlak     = m_mundlak_engl_eq05_age,
  engl_risk_noage_mundlak   = m_mundlak_engl_risk_noage,
  engl_risk_age_mundlak     = m_mundlak_engl_risk_age
)

# Quick inspection
lapply(models_micro_mundlak, summary)

# ============================================================
# PRINT SUMMARIES OF ALL MUNDLAK MODELS
# ============================================================

for(i in seq_along(models_micro_mundlak)) {
  
  cat("\n====================================================\n")
  cat("MODEL", i, "\n")
  cat("====================================================\n")
  
  print(summary(models_micro_mundlak[[i]]))
}

# ============================================================
# MINIMAL MUNDLAK CHECKS
# ============================================================

# 1) Verify the helper once on one representative dataset
tmp_check <- add_mundlak_terms(panel_brit_income_noage, vars = c("brit", "income"))

stopifnot(
  max(abs(tmp_check$brit   - (tmp_check$brit_mean   + tmp_check$brit_w)),   na.rm = TRUE) < 1e-8,
  max(abs(tmp_check$income - (tmp_check$income_mean + tmp_check$income_w)), na.rm = TRUE) < 1e-8
)

# 2) Put all Mundlak models in one list
models_micro_mundlak <- list(
  m_mundlak_brit_income_noage,
  m_mundlak_brit_income_age,
  m_mundlak_brit_eq03_noage,
  m_mundlak_brit_eq03_age,
  m_mundlak_brit_eq05_noage,
  m_mundlak_brit_eq05_age,
  m_mundlak_brit_risk_noage,
  m_mundlak_brit_risk_age,
  m_mundlak_engl_income_noage,
  m_mundlak_engl_income_age,
  m_mundlak_engl_eq03_noage,
  m_mundlak_engl_eq03_age,
  m_mundlak_engl_eq05_noage,
  m_mundlak_engl_eq05_age,
  m_mundlak_engl_risk_noage,
  m_mundlak_engl_risk_age
)

# 3) Check singularity + convergence for all models
singular_flags <- sapply(models_micro_mundlak, isSingular)

conv_flags <- sapply(models_micro_mundlak, function(m) {
  is.null(summary(m)$optinfo$conv$lme4$messages)
})

stopifnot(all(!singular_flags), all(conv_flags))

cat("All Mundlak checks passed.\n")

# ============================================================
# SAVE MUNDLAK MODELS
# ============================================================

out_dir_mundlak <- "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/models_micro_mundlak"
dir.create(out_dir_mundlak, recursive = TRUE, showWarnings = FALSE)

saveRDS(
  models_micro_mundlak,
  file.path(out_dir_mundlak, "models_micro_mundlak_all.rds")
)

cat("Saved Mundlak MICRO models to:", out_dir_mundlak, "\n")
cat("Total Mundlak models saved:", length(models_micro_mundlak), "\n")

# ============================================================
# MUNDLAK MICRO PIPELINE
# Correlated random effects for all micro-level interaction models
#
# Changes from v1:
#   1. Wave effects modelled as fixed dummies (+ factor(wave)) rather than
#      (1 | wave) random intercept — consistent with FE identification strategy
#      and avoids reliance on ~11-20 wave-level units for variance estimation
#   2. Single panel per identity × economic variable (age variant uses the same
#      rows; no duplicate data objects)
#   3. dv validity filter added (immigSelf must be in 1:10)
#   4. Safer column renaming after pivot_longer; identity column renamed to
#      "brit"/"engl" (not left as "ident") so fit_mundlak can find it by name
#   5. eq_long join guarded against column name collisions
#   6. econ_name lookup uses switch() instead of if/else chain
#   7. Named model list preserved through to saveRDS
#   8. add_mundlak_terms uses .SD-style subtraction, not get(), for robustness
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
df <- readRDS(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v3_England.rds"
)
eq_long <- readRDS(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/equivalised_income_long.rds"
)

eq_long <- eq_long %>%
  mutate(
    id   = as.character(id),
    wave = as.integer(wave)
  )

# Guard against column name collisions on join
# (eq_long must not carry an 'income' column that would clash with the raw income column)
stopifnot(!"income" %in% names(eq_long))

# ------------------------------------------------------------
# Variable name constants
# ------------------------------------------------------------
DV_BASE     <- "immigSelf"
BRIT_BASE   <- "britishness"
ENG_BASE    <- "englishness"
TIME_BASE   <- "starttime"
AGE_BASE    <- "age"
INCOME_BASE <- "income"
RISK_BASE   <- "riskUnemployment"

DV_VALID_RANGE   <- 1:10   # valid range for immigSelf
IDENT_VALID_RANGE <- 1:7   # valid range for britishness / englishness

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(df, base) {
  vars <- names(df)[str_detect(names(df), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave_num)))
}

# Build a long panel for one identity × one economic variable.
# Age is always included in the pivot when present; the caller decides
# whether to pass it to the model formula.
make_long_panel <- function(df,
                            dv_base,
                            identity_base,
                            time_base,
                            econ_base,
                            age_base = AGE_BASE) {
  
  w_dv     <- waves_available(df, dv_base)
  w_ident  <- waves_available(df, identity_base)
  w_time   <- waves_available(df, time_base)
  w_econ   <- waves_available(df, econ_base)
  w_age    <- waves_available(df, age_base)
  
  # Intersect over all five variable families
  waves <- Reduce(intersect, list(w_dv, w_ident, w_time, w_econ, w_age))
  
  if (length(waves) < 3)
    stop("Too few overlapping waves across dv / identity / time / econ / age.")
  
  all_cols <- c(
    paste0(dv_base,       "W", waves),
    paste0(identity_base, "W", waves),
    paste0(time_base,     "W", waves),
    paste0(econ_base,     "W", waves),
    paste0(age_base,      "W", waves)
  )
  
  df %>%
    mutate(across(
      all_of(all_cols),
      ~ as.numeric(zap_labels(.x))
    )) %>%
    select(id, all_of(all_cols)) %>%
    pivot_longer(
      cols          = -id,
      names_to      = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) %>%
    # pivot_longer uses the base name as column name, so rename to stable names
    rename(
      dv       = all_of(dv_base),
      ident    = all_of(identity_base),
      econ     = all_of(econ_base),
      age      = all_of(age_base),
      startime = all_of(time_base)      # renamed to avoid collision with time_base arg
    ) %>%
    mutate(
      id   = as.character(id),
      wave = as.integer(wave),
      starttime_parsed = coalesce(
        suppressWarnings(ymd_hms(as.character(startime), quiet = TRUE)),
        suppressWarnings(ymd(as.character(startime),     quiet = TRUE))
      ),
      year = year(starttime_parsed)
    ) %>%
    # validity filters
    filter(
      dv    %in% DV_VALID_RANGE,
      ident %in% IDENT_VALID_RANGE,
      !is.na(econ),
      !is.na(wave)
    )
}

# Attach equivalised income columns (income_eq_child03, income_eq_child05)
add_eq_income <- function(panel) {
  panel %>%
    left_join(eq_long, by = c("id", "wave"))
}

# Mundlak decomposition: for every variable in `vars`, add
# a person-mean (_mean) and within-deviation (_w) column
add_mundlak_terms <- function(data, vars, id_var = "id") {
  # Step 1: person means
  out <- data %>%
    group_by(.data[[id_var]]) %>%
    mutate(across(
      all_of(vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    )) %>%
    ungroup()
  
  # Step 2: within deviations — explicit loop avoids get() scoping fragility
  for (v in vars) {
    out[[paste0(v, "_w")]] <- out[[v]] - out[[paste0(v, "_mean")]]
  }
  
  out
}

# Resolve a human-readable economic variable name from its base string
econ_label <- function(econ_base) {
  switch(econ_base,
         income           = "income",
         riskUnemployment = "risk_unemp",
         income_eq_child03 = "eq03",
         income_eq_child05 = "eq05",
         econ_base   # fallback: use the base name as-is
  )
}

# ------------------------------------------------------------
# Estimation
# ------------------------------------------------------------
ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl   = list(maxfun = 2e5)
)

# Fit one Mundlak interaction model.
#
# Wave effects are entered as fixed dummies (factor(wave)) rather than
# as a random intercept. This replicates the within-wave identification
# of the FE models and avoids the instability of estimating a wave-level
# variance from ~11-20 units.
#
# include_age = FALSE simply omits age_w / age_mean from the formula;
# both columns are always present in the data.
fit_mundlak <- function(data,
                        identity_var,
                        econ_var,
                        include_age = FALSE) {
  
  mundlak_vars <- c(identity_var, econ_var, "age")
  d <- add_mundlak_terms(data, vars = mundlak_vars, id_var = "id")
  
  age_terms <- if (include_age) "+ age_w + age_mean" else ""
  
  fml <- as.formula(paste0(
    "dv ~ ",
    identity_var, "_w * ", econ_var, "_w + ",
    identity_var, "_mean + ", econ_var, "_mean + ",
    age_terms,
    " + factor(wave) + (1 | id)"
  ))
  
  lmer(fml, data = d, REML = TRUE, control = ctrl)
}

# ============================================================
# BUILD PANELS
# One panel per (identity × economic variable).
# Age is always in the data; model formula controls whether it enters.
# ============================================================

build_panel <- function(identity_base, econ_base) {
  p <- make_long_panel(
    df             = df,
    dv_base        = DV_BASE,
    identity_base  = identity_base,
    time_base      = TIME_BASE,
    econ_base      = econ_base,
    age_base       = AGE_BASE
  )
  
  # Rename identity column from generic "ident" to "brit" or "engl"
  # so fit_mundlak can reference it by name
  identity_label <- switch(identity_base,
                           britishness = "brit",
                           englishness = "engl",
                           identity_base   # fallback
  )
  p <- p %>% rename(!!identity_label := ident)
  
  # Clean risk scale before renaming (column is still called "econ" here)
  if (econ_base == RISK_BASE) {
    p <- p %>%
      mutate(econ = if_else(between(econ, 0, 10), econ, NA_real_)) %>%
      filter(!is.na(econ))
  }
  
  # Rename econ column to its human-readable label
  label <- econ_label(econ_base)
  p <- p %>% rename(!!label := econ)
  
  p
}

# --- Britishness panels ---
panel_brit_income   <- build_panel(BRIT_BASE, INCOME_BASE)
panel_brit_risk     <- build_panel(BRIT_BASE, RISK_BASE)

# Attach equivalised income and filter to non-missing
panel_brit_eq_base  <- add_eq_income(panel_brit_income)
panel_brit_eq03     <- panel_brit_eq_base %>% filter(!is.na(income_eq_child03))
panel_brit_eq05     <- panel_brit_eq_base %>% filter(!is.na(income_eq_child05))

# --- Englishness panels ---
panel_engl_income   <- build_panel(ENG_BASE, INCOME_BASE)
panel_engl_risk     <- build_panel(ENG_BASE, RISK_BASE)

panel_engl_eq_base  <- add_eq_income(panel_engl_income)
panel_engl_eq03     <- panel_engl_eq_base %>% filter(!is.na(income_eq_child03))
panel_engl_eq05     <- panel_engl_eq_base %>% filter(!is.na(income_eq_child05))

# ============================================================
# ESTIMATE ALL MODELS
# For each panel we fit four variants:
#   noage / age  (whether age enters the formula)
# ============================================================

# Helper: fit both age variants for one panel, return named list of two models
fit_pair <- function(panel, identity_var, econ_var) {
  list(
    noage = fit_mundlak(panel, identity_var, econ_var, include_age = FALSE),
    age   = fit_mundlak(panel, identity_var, econ_var, include_age = TRUE)
  )
}

models_micro_mundlak <- list(
  
  # ---- Britishness ----
  brit_income = fit_pair(panel_brit_income, "brit",  "income"),
  brit_eq03   = fit_pair(panel_brit_eq03,   "brit",  "income_eq_child03"),
  brit_eq05   = fit_pair(panel_brit_eq05,   "brit",  "income_eq_child05"),
  brit_risk   = fit_pair(panel_brit_risk,   "brit",  "risk_unemp"),
  
  # ---- Englishness ----
  engl_income = fit_pair(panel_engl_income, "engl",  "income"),
  engl_eq03   = fit_pair(panel_engl_eq03,   "engl",  "income_eq_child03"),
  engl_eq05   = fit_pair(panel_engl_eq05,   "engl",  "income_eq_child05"),
  engl_risk   = fit_pair(panel_engl_risk,   "engl",  "risk_unemp")
)

# Flatten to a named list of individual model objects for easy iteration
models_flat <- purrr::imap(models_micro_mundlak, function(pair, nm) {
  setNames(
    list(pair$noage, pair$age),
    c(paste0(nm, "_noage"), paste0(nm, "_age"))
  )
}) %>% purrr::list_flatten()

# ============================================================
# CHECKS
# ============================================================

# 1) Arithmetic identity of Mundlak decomposition on one representative dataset
tmp_check <- add_mundlak_terms(panel_brit_income, vars = c("brit", "income"))

stopifnot(
  max(abs(tmp_check$brit   - (tmp_check$brit_mean   + tmp_check$brit_w)),   na.rm = TRUE) < 1e-8,
  max(abs(tmp_check$income - (tmp_check$income_mean + tmp_check$income_w)), na.rm = TRUE) < 1e-8
)

cat("Mundlak decomposition check passed.\n")

# 2) Singularity and convergence for all models
singular_flags <- sapply(models_flat, isSingular)
conv_flags     <- sapply(models_flat, function(m) {
  is.null(summary(m)$optinfo$conv$lme4$messages)
})

if (any(singular_flags)) {
  warning("Singular fit detected in: ",
          paste(names(models_flat)[singular_flags], collapse = ", "))
}
if (any(!conv_flags)) {
  warning("Convergence issues in: ",
          paste(names(models_flat)[!conv_flags], collapse = ", "))
}

cat("Mundlak checks complete.\n")
cat("  Singular:          ", sum(singular_flags),  "of", length(singular_flags), "\n")
cat("  Convergence issues:", sum(!conv_flags), "of", length(conv_flags),  "\n")

# ============================================================
# PRINT SUMMARIES
# ============================================================

for (nm in names(models_flat)) {
  cat("\n====================================================\n")
  cat("MODEL:", nm, "\n")
  cat("====================================================\n")
  print(summary(models_flat[[nm]]))
}

# ============================================================
# SAVE
# ============================================================

out_dir <- "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/models_micro_mundlak"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save the nested list (preserves the noage/age structure)
saveRDS(
  models_micro_mundlak,
  file.path(out_dir, "models_micro_mundlak_nested.rds")
)

# Save the flat named list (convenient for extraction / tabling)
saveRDS(
  models_flat,
  file.path(out_dir, "models_micro_mundlak_flat.rds")
)

cat("\nSaved to:", out_dir, "\n")
cat("Total models:", length(models_flat), "\n")
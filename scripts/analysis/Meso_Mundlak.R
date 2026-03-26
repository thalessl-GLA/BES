# ============================================================
# MUNDLAK MESO PIPELINE
# Correlated random effects for all meso-level interaction models
#
# Symmetric with mundlak_micro_pipeline.R. Key design choices:
#
#   1. Wave effects as fixed dummies (+ factor(wave)), not (1|wave),
#      consistent with FE identification strategy
#   2. BOTH the identity variable and the meso indicator are time-varying
#      at the individual level, so both receive Mundlak decomposition
#      (_mean + _w). The key interaction is identity_w * meso_w.
#   3. Age is always pivoted into the panel; include_age controls whether
#      age_w + age_mean enter the formula (no duplicate panels)
#   4. Categorical identity robustness (ref = 7) estimated as a separate
#      model set, matching the original RE pipeline
#   5. Both Britishness and Englishness estimated; meso panels built once
#      per indicator and reused across identities
#   6. Validity filters applied: dv in 1:10, identity in 1:7
#   7. Named nested list preserved through to saveRDS; flat list also saved
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

# ------------------------------------------------------------
# Variable name constants
# ------------------------------------------------------------
DV_BASE   <- "immigSelf"
BRIT_BASE <- "britishness"
ENG_BASE  <- "englishness"
TIME_BASE <- "starttime"
AGE_BASE  <- "age"

# Meso indicator base names (wide-format columns in df: e.g. unemp_rate_meanW3)
MESO_UNEMP_BASE  <- "unemp_rate_mean"
MESO_CLAIM_BASE  <- "claimant_mean"
MESO_INCOME_BASE <- "income"        # ASHE local earnings

DV_VALID_RANGE    <- 1:10
IDENT_VALID_RANGE <- 1:7

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(df, base) {
  vars <- names(df)[str_detect(names(df), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave_num)))
}

# Resolve identity short-label from base name
identity_label <- function(identity_base) {
  switch(identity_base,
         britishness = "brit",
         englishness = "engl",
         identity_base
  )
}

# Resolve meso indicator column name after pivoting
meso_label <- function(meso_base) {
  switch(meso_base,
         unemp_rate_mean = "unemp",
         claimant_mean   = "claimant",
         income          = "income_meso",
         meso_base
  )
}

# ------------------------------------------------------------
# Panel builder
#
# Builds a long panel for one (identity × meso indicator) combination.
# All five variable families (dv, identity, time, meso, age) are pivoted
# together so every row has complete information on all of them.
# The meso indicator lives in the same wide-format structure as the
# individual-level variables (e.g. unemp_rate_meanW3, unemp_rate_meanW5).
# ------------------------------------------------------------
make_meso_panel <- function(df,
                            dv_base,
                            identity_base,
                            time_base,
                            meso_base,
                            age_base = AGE_BASE) {
  
  w_dv    <- waves_available(df, dv_base)
  w_ident <- waves_available(df, identity_base)
  w_time  <- waves_available(df, time_base)
  w_meso  <- waves_available(df, meso_base)
  w_age   <- waves_available(df, age_base)
  
  waves <- Reduce(intersect, list(w_dv, w_ident, w_time, w_meso, w_age))
  
  if (length(waves) < 3)
    stop(paste0(
      "Too few overlapping waves for identity=", identity_base,
      " meso=", meso_base, " (found ", length(waves), ")"
    ))
  
  all_cols <- c(
    paste0(dv_base,       "W", waves),
    paste0(identity_base, "W", waves),
    paste0(time_base,     "W", waves),
    paste0(meso_base,     "W", waves),
    paste0(age_base,      "W", waves)
  )
  
  ident_lbl <- identity_label(identity_base)
  meso_lbl  <- meso_label(meso_base)
  
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
    # Rename to stable, non-colliding names immediately after pivot
    rename(
      dv      = all_of(dv_base),
      ident   = all_of(identity_base),
      meso    = all_of(meso_base),
      age     = all_of(age_base),
      startime = all_of(time_base)
    ) %>%
    mutate(
      id   = as.character(id),
      wave = as.integer(wave),
      starttime_parsed = coalesce(
        suppressWarnings(ymd_hms(as.character(startime), quiet = TRUE)),
        suppressWarnings(ymd(as.character(startime),     quiet = TRUE))
      ),
      year = year(starttime_parsed),
      # Clean out-of-range identity values (e.g. 9999 sentinels)
      ident = if_else(ident %in% IDENT_VALID_RANGE, ident, NA_real_)
    ) %>%
    filter(
      dv %in% DV_VALID_RANGE,
      !is.na(ident),
      !is.na(meso),
      !is.na(wave)
    ) %>%
    # Rename identity and meso to their human-readable labels so
    # fit_mundlak_meso can reference them by name
    rename(
      !!ident_lbl := ident,
      !!meso_lbl  := meso
    )
}

# ------------------------------------------------------------
# Mundlak decomposition
# Adds _mean (person mean) and _w (within deviation) for each var.
# Explicit loop avoids get() scoping fragility.
# ------------------------------------------------------------
add_mundlak_terms <- function(data, vars, id_var = "id") {
  out <- data %>%
    group_by(.data[[id_var]]) %>%
    mutate(across(
      all_of(vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    )) %>%
    ungroup()
  
  for (v in vars) {
    out[[paste0(v, "_w")]] <- out[[v]] - out[[paste0(v, "_mean")]]
  }
  
  out
}

# ------------------------------------------------------------
# Estimation
#
# Both identity_var and meso_var are time-varying at the individual
# level, so both are Mundlak-decomposed. The key causal quantity is
# the within-person interaction: identity_w * meso_w.
#
# Mundlak correction terms included:
#   identity_mean, meso_mean  (controls for between-person sorting)
#
# Wave fixed effects entered as factor(wave) dummies — not (1|wave) —
# for consistency with the FE pipeline and to avoid estimating a
# wave-level variance from a handful of time periods.
#
# include_age = FALSE omits age_w + age_mean from the formula;
# age is always present in the data.
# ------------------------------------------------------------
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

fit_mundlak_meso <- function(data,
                             identity_var,
                             meso_var,
                             include_age = FALSE) {
  
  mundlak_vars <- c(identity_var, meso_var, "age")
  d <- add_mundlak_terms(data, vars = mundlak_vars, id_var = "id")
  
  age_terms <- if (include_age) "+ age_w + age_mean" else ""
  
  fml <- as.formula(paste0(
    "dv ~ ",
    identity_var, "_w * ", meso_var, "_w + ",
    identity_var, "_mean + ", meso_var, "_mean + ",
    age_terms,
    " + factor(wave) + (1 | id)"
  ))
  
  lmer(fml, data = d, REML = TRUE, control = ctrl)
}

# Categorical identity variant.
# Identity entered as factor (ref = 7) interacted with meso_w.
# Mundlak correction: meso_mean still included; no mean/within split
# for a categorical variable — the factor levels already capture
# between-person differences, so only meso gets decomposed.
fit_mundlak_meso_cat <- function(data,
                                 identity_var,
                                 meso_var,
                                 include_age = FALSE) {
  
  # Decompose only meso and age (identity is categorical)
  mundlak_vars <- c(meso_var, "age")
  d <- add_mundlak_terms(data, vars = mundlak_vars, id_var = "id")
  
  # Build categorical identity column (ref = 7)
  cat_col <- paste0(identity_var, "_cat")
  d[[cat_col]] <- relevel(factor(d[[identity_var]], levels = 1:7), ref = "7")
  
  age_terms <- if (include_age) "+ age_w + age_mean" else ""
  
  fml <- as.formula(paste0(
    "dv ~ ",
    cat_col, " * ", meso_var, "_w + ",
    meso_var, "_mean + ",
    age_terms,
    " + factor(wave) + (1 | id)"
  ))
  
  lmer(fml, data = d, REML = TRUE, control = ctrl)
}

# Helper: fit both age variants (noage / age) for one panel.
# Returns a named list of two models.
fit_pair <- function(panel, identity_var, meso_var, categorical = FALSE) {
  fit_fn <- if (categorical) fit_mundlak_meso_cat else fit_mundlak_meso
  list(
    noage = fit_fn(panel, identity_var, meso_var, include_age = FALSE),
    age   = fit_fn(panel, identity_var, meso_var, include_age = TRUE)
  )
}

# ============================================================
# BUILD PANELS
# One panel per (identity × meso indicator).
# Britishness and Englishness panels are built separately because
# wave coverage may differ between the two identity variables.
# ============================================================

cat("Building panels...\n")

# --- Britishness ---
panel_brit_unemp   <- make_meso_panel(df, DV_BASE, BRIT_BASE, TIME_BASE, MESO_UNEMP_BASE)
panel_brit_claim   <- make_meso_panel(df, DV_BASE, BRIT_BASE, TIME_BASE, MESO_CLAIM_BASE)
panel_brit_income  <- make_meso_panel(df, DV_BASE, BRIT_BASE, TIME_BASE, MESO_INCOME_BASE)

# --- Englishness ---
panel_engl_unemp   <- make_meso_panel(df, DV_BASE, ENG_BASE, TIME_BASE, MESO_UNEMP_BASE)
panel_engl_claim   <- make_meso_panel(df, DV_BASE, ENG_BASE, TIME_BASE, MESO_CLAIM_BASE)
panel_engl_income  <- make_meso_panel(df, DV_BASE, ENG_BASE, TIME_BASE, MESO_INCOME_BASE)

cat("Panels built. Wave/obs counts:\n")
for (nm in c("panel_brit_unemp","panel_brit_claim","panel_brit_income",
             "panel_engl_unemp","panel_engl_claim","panel_engl_income")) {
  p <- get(nm)
  cat(sprintf("  %-25s  N=%d  waves=%s\n",
              nm, nrow(p),
              paste(sort(unique(p$wave)), collapse=",")))
}

# ============================================================
# ESTIMATE ALL MODELS
#
# Structure mirrors the micro pipeline:
#   models_meso_mundlak$brit$unemp$cont$noage  — continuous, no age
#   models_meso_mundlak$brit$unemp$cont$age    — continuous, with age
#   models_meso_mundlak$brit$unemp$cat$noage   — categorical, no age
#   models_meso_mundlak$brit$unemp$cat$age     — categorical, with age
#
# Flat list produced for iteration / checks / saving.
# ============================================================

cat("Estimating models...\n")

models_meso_mundlak <- list(
  
  brit = list(
    
    unemp = list(
      cont = fit_pair(panel_brit_unemp,  "brit", "unemp",       categorical = FALSE),
      cat  = fit_pair(panel_brit_unemp,  "brit", "unemp",       categorical = TRUE)
    ),
    
    claimant = list(
      cont = fit_pair(panel_brit_claim,  "brit", "claimant",    categorical = FALSE),
      cat  = fit_pair(panel_brit_claim,  "brit", "claimant",    categorical = TRUE)
    ),
    
    income = list(
      cont = fit_pair(panel_brit_income, "brit", "income_meso", categorical = FALSE),
      cat  = fit_pair(panel_brit_income, "brit", "income_meso", categorical = TRUE)
    )
  ),
  
  engl = list(
    
    unemp = list(
      cont = fit_pair(panel_engl_unemp,  "engl", "unemp",       categorical = FALSE),
      cat  = fit_pair(panel_engl_unemp,  "engl", "unemp",       categorical = TRUE)
    ),
    
    claimant = list(
      cont = fit_pair(panel_engl_claim,  "engl", "claimant",    categorical = FALSE),
      cat  = fit_pair(panel_engl_claim,  "engl", "claimant",    categorical = TRUE)
    ),
    
    income = list(
      cont = fit_pair(panel_engl_income, "engl", "income_meso", categorical = FALSE),
      cat  = fit_pair(panel_engl_income, "engl", "income_meso", categorical = TRUE)
    )
  )
)

# ============================================================
# FLATTEN
# Produces a named list of individual lmer objects for iteration,
# checks, printing, and downstream tabling.
# Naming convention: {identity}_{indicator}_{cont|cat}_{noage|age}
# e.g. brit_unemp_cont_noage
# ============================================================

flatten_models <- function(nested) {
  out <- list()
  for (ident in names(nested)) {
    for (indic in names(nested[[ident]])) {
      for (spec in names(nested[[ident]][[indic]])) {
        for (age_v in names(nested[[ident]][[indic]][[spec]])) {
          key <- paste(ident, indic, spec, age_v, sep = "_")
          out[[key]] <- nested[[ident]][[indic]][[spec]][[age_v]]
        }
      }
    }
  }
  out
}

models_flat <- flatten_models(models_meso_mundlak)

cat("Total models estimated:", length(models_flat), "\n")

# ============================================================
# CHECKS
# ============================================================

# 1) Arithmetic identity of Mundlak decomposition
tmp_check <- add_mundlak_terms(panel_brit_unemp, vars = c("brit", "unemp"))

stopifnot(
  max(abs(tmp_check$brit  - (tmp_check$brit_mean  + tmp_check$brit_w)),  na.rm = TRUE) < 1e-8,
  max(abs(tmp_check$unemp - (tmp_check$unemp_mean + tmp_check$unemp_w)), na.rm = TRUE) < 1e-8
)
cat("Mundlak decomposition check passed.\n")

# 2) Singularity and convergence
singular_flags <- sapply(models_flat, isSingular)
conv_flags     <- sapply(models_flat, function(m) {
  is.null(summary(m)$optinfo$conv$lme4$messages)
})

if (any(singular_flags)) {
  warning("Singular fit in: ",
          paste(names(models_flat)[singular_flags], collapse = ", "))
}
if (any(!conv_flags)) {
  warning("Convergence issues in: ",
          paste(names(models_flat)[!conv_flags], collapse = ", "))
}

cat("Mundlak checks complete.\n")
cat("  Singular:          ", sum(singular_flags),  "of", length(singular_flags), "\n")
cat("  Convergence issues:", sum(!conv_flags), "of", length(conv_flags), "\n")

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

out_dir <- "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/models_meso_mundlak"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Nested list: preserves brit/engl > indicator > cont/cat > noage/age structure
saveRDS(
  models_meso_mundlak,
  file.path(out_dir, "models_meso_mundlak_nested.rds")
)

# Flat named list: convenient for modelsummary / tabling
saveRDS(
  models_flat,
  file.path(out_dir, "models_meso_mundlak_flat.rds")
)

cat("\nSaved to:", out_dir, "\n")
cat("Total models saved:", length(models_flat), "\n")
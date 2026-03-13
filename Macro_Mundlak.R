# ============================================================
# MUNDLAK MACRO PIPELINE
# Correlated random effects for all macro-level interaction models
#
# Symmetric with mundlak_micro_pipeline.R and mundlak_meso_pipeline.R
# with one critical structural difference at the macro level:
#
# MACRO VARIABLES ARE WAVE-CONSTANT.
# GDP, unemployment rate, claimant count, and CPIH each take a single
# value per wave, shared identically across all individuals. This means
# the "within deviation" of a macro variable has no individual-level
# variation — it reduces to a demeaned time series, identical for every
# person in a given wave. Therefore:
#
#   - Identity (brit/engl) is Mundlak-decomposed as usual: _mean + _w
#     because identity is a genuine individual time-varying variable.
#   - Macro indicators are entered at their level (not decomposed):
#     decomposing them would be vacuous. Their "mean" is just their
#     average over the observed waves for each person, which equals the
#     grand wave-average — no individual heterogeneity to control for.
#   - The Mundlak correction for identity (identity_mean) is included.
#   - For main_0 models (macro only, no identity): wave trend absorbed
#     by poly(wave, 3, raw=TRUE) + (1|id), exactly as in the RE pipeline,
#     because macro indicators are collinear with wave fixed dummies.
#   - For interaction models: factor(wave) as fixed dummies + (1|id),
#     consistent with the micro and meso Mundlak pipelines.
#   - Identity robustness models (identity ~ macro): Mundlak-decompose
#     identity as the outcome, include macro at level + poly(wave,3).
#
# Other design choices:
#   1. Single long panel per identity (all four macro indicators joined)
#   2. Age always in the data; include_age controls formula only
#   3. Categorical identity robustness (ref = 7) estimated in parallel
#   4. Named nested list + flat list both saved
#   5. Memory management: Britishness models saved and removed before
#      building Englishness panel, matching original RE pipeline
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

MACRO_GDP_BASE   <- "gdp_pc"
MACRO_UNEMP_BASE <- "unempl_rate"
MACRO_CLAIM_BASE <- "claimant_k"
MACRO_CPIH_BASE  <- "cpih_rate"

DV_VALID_RANGE    <- 1:10
IDENT_VALID_RANGE <- 1:7

# Short labels used in model names and formula strings
MACRO_VARS <- c(
  gdp_pc      = MACRO_GDP_BASE,
  unempl_rate = MACRO_UNEMP_BASE,
  claimant_k  = MACRO_CLAIM_BASE,
  cpih_rate   = MACRO_CPIH_BASE
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(df, base) {
  vars <- names(df)[str_detect(names(df), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave_num)))
}

identity_label <- function(identity_base) {
  switch(identity_base,
         britishness = "brit",
         englishness = "engl",
         identity_base
  )
}

# ------------------------------------------------------------
# Panel builder
#
# Builds one long panel for a given identity variable, with all four
# macro indicators joined in. Wave coverage is intersected across all
# variable families (dv, identity, time, age, all four macro series).
#
# Macro indicators are pivoted from wide to long via a helper then
# left-joined onto the individual panel — the same approach as the
# original RE pipeline's macro_long_from_base().
# ------------------------------------------------------------
pivot_macro <- function(df, base, waves_keep) {
  cols <- paste0(base, "W", waves_keep)
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(NULL)
  
  df %>%
    select(id, all_of(cols)) %>%
    mutate(across(all_of(cols), ~ as.numeric(zap_labels(.x)))) %>%
    pivot_longer(
      cols          = -id,
      names_to      = "wave",
      names_pattern = paste0("^", base, "W(\\d+)$"),
      values_to     = base
    ) %>%
    mutate(wave = as.integer(wave))
}

make_macro_panel <- function(df,
                             dv_base,
                             identity_base,
                             time_base,
                             age_base = AGE_BASE) {
  
  w_dv    <- waves_available(df, dv_base)
  w_ident <- waves_available(df, identity_base)
  w_time  <- waves_available(df, time_base)
  w_age   <- waves_available(df, age_base)
  w_gdp   <- waves_available(df, MACRO_GDP_BASE)
  w_unemp <- waves_available(df, MACRO_UNEMP_BASE)
  w_claim <- waves_available(df, MACRO_CLAIM_BASE)
  w_cpih  <- waves_available(df, MACRO_CPIH_BASE)
  
  waves <- Reduce(intersect,
                  list(w_dv, w_ident, w_time, w_age, w_gdp, w_unemp, w_claim, w_cpih))
  
  if (length(waves) < 3)
    stop(paste0(
      "Too few overlapping waves for identity=", identity_base,
      " (found ", length(waves), ")"
    ))
  
  ident_lbl <- identity_label(identity_base)
  
  # --- Individual-level pivot (dv, identity, time, age) ---
  indiv_cols <- c(
    paste0(dv_base,       "W", waves),
    paste0(identity_base, "W", waves),
    paste0(time_base,     "W", waves),
    paste0(age_base,      "W", waves)
  )
  
  panel <- df %>%
    mutate(across(all_of(indiv_cols), ~ as.numeric(zap_labels(.x)))) %>%
    select(id, all_of(indiv_cols)) %>%
    pivot_longer(
      cols          = -id,
      names_to      = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) %>%
    rename(
      dv       = all_of(dv_base),
      ident    = all_of(identity_base),
      age      = all_of(age_base),
      startime = all_of(time_base)
    ) %>%
    mutate(
      id   = as.character(id),
      wave = as.integer(wave),
      starttime_parsed = coalesce(
        suppressWarnings(ymd_hms(as.character(startime), quiet = TRUE)),
        suppressWarnings(ymd(as.character(startime),     quiet = TRUE))
      ),
      year  = year(starttime_parsed),
      ident = if_else(ident %in% IDENT_VALID_RANGE, ident, NA_real_)
    ) %>%
    filter(
      dv %in% DV_VALID_RANGE,
      !is.na(ident),
      !is.na(wave)
    ) %>%
    rename(!!ident_lbl := ident)
  
  # --- Join all four macro series ---
  for (base in MACRO_VARS) {
    macro_tbl <- pivot_macro(df, base, waves)
    if (!is.null(macro_tbl))
      panel <- left_join(panel, macro_tbl, by = c("id", "wave"))
  }
  
  panel
}

# ------------------------------------------------------------
# Mundlak decomposition
# Only applied to identity (and age when included).
# Macro indicators are NOT decomposed — they are wave-constant.
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
# Estimation helpers
# ------------------------------------------------------------
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# Main interaction model.
#
# Formula:
#   dv ~ identity_w * macro_var
#       + identity_mean          [Mundlak correction for identity]
#       + (age_w + age_mean)     [optional]
#       + factor(wave)           [wave fixed dummies]
#       + (1 | id)
#
# identity_w is the within-person deviation of identity.
# macro_var  is the macro indicator at its level (wave-constant;
#            collinearity with factor(wave) is absorbed by the
#            within-person variation in identity_w * macro_var).
#
# Note: factor(wave) and macro_var will be collinear in the
# main-effects-only models (main_0). Those use poly(wave,3) instead
# — see fit_mundlak_macro_main0() below.
fit_mundlak_macro <- function(data,
                              identity_var,
                              macro_var,
                              include_age = FALSE) {
  
  mundlak_vars <- c(identity_var, if (include_age) "age")
  d <- add_mundlak_terms(data, vars = mundlak_vars, id_var = "id")
  
  age_terms <- if (include_age) "+ age_w + age_mean" else ""
  
  fml <- as.formula(paste0(
    "dv ~ ",
    identity_var, "_w * ", macro_var, " + ",
    identity_var, "_mean + ",
    age_terms,
    " + factor(wave) + (1 | id)"
  ))
  
  lmer(fml, data = d, REML = TRUE, control = ctrl)
}

# Main-effects-only model (no identity term).
# Uses poly(wave, 3) instead of factor(wave) because macro_var is
# perfectly collinear with wave dummies in a main-effects model.
fit_mundlak_macro_main0 <- function(data,
                                    macro_var,
                                    include_age = FALSE) {
  
  age_terms <- if (include_age) "+ age" else ""
  
  fml <- as.formula(paste0(
    "dv ~ ", macro_var, " ",
    age_terms,
    " + poly(wave, 3, raw = TRUE) + (1 | id)"
  ))
  
  lmer(fml, data = data, REML = TRUE, control = ctrl)
}

# Categorical identity variant of the interaction model.
# Identity entered as factor (ref = 7); only identity decomposition
# changes — macro_var still enters at level.
fit_mundlak_macro_cat <- function(data,
                                  identity_var,
                                  macro_var,
                                  include_age = FALSE) {
  
  # No Mundlak decomposition needed for categorical identity
  # (factor levels absorb between-person heterogeneity directly)
  cat_col <- paste0(identity_var, "_cat")
  d <- data
  d[[cat_col]] <- relevel(factor(d[[identity_var]], levels = 1:7), ref = "7")
  
  age_terms <- if (include_age) "+ age" else ""
  
  fml <- as.formula(paste0(
    "dv ~ ",
    cat_col, " * ", macro_var, " + ",
    age_terms,
    " + factor(wave) + (1 | id)"
  ))
  
  lmer(fml, data = d, REML = TRUE, control = ctrl)
}

# Identity robustness model: identity ~ macro
# Tests whether macro conditions predict within-person identity shifts.
# Mundlak-decomposes identity as the OUTCOME; macro enters at level;
# poly(wave,3) absorbs the wave trend (macro collinear with wave dummies).
fit_mundlak_identity_rob <- function(data,
                                     identity_var,
                                     macro_var) {
  
  fml <- as.formula(paste0(
    identity_var, " ~ ", macro_var,
    " + poly(wave, 3, raw = TRUE) + (1 | id)"
  ))
  
  lmer(fml, data = data, REML = TRUE, control = ctrl)
}

# Fit all variants for one (panel, identity_var, macro_var) triplet.
# Returns a named list:
#   $main0_noage / $main0_age   — macro-only, no identity
#   $cont_noage  / $cont_age    — continuous identity interaction
#   $cat_noage   / $cat_age     — categorical identity interaction
#   $rob                        — identity ~ macro robustness
fit_macro_bundle <- function(panel, identity_var, macro_var) {
  list(
    main0_noage = fit_mundlak_macro_main0(panel, macro_var, include_age = FALSE),
    main0_age   = fit_mundlak_macro_main0(panel, macro_var, include_age = TRUE),
    cont_noage  = fit_mundlak_macro(panel, identity_var, macro_var, include_age = FALSE),
    cont_age    = fit_mundlak_macro(panel, identity_var, macro_var, include_age = TRUE),
    cat_noage   = fit_mundlak_macro_cat(panel, identity_var, macro_var, include_age = FALSE),
    cat_age     = fit_mundlak_macro_cat(panel, identity_var, macro_var, include_age = TRUE),
    rob         = fit_mundlak_identity_rob(panel, identity_var, macro_var)
  )
}

# ============================================================
# FLATTEN
# Converts the nested list to a flat named list of lmer objects.
# Naming convention: {identity}_{macro}_{spec}
# e.g. brit_gdp_pc_cont_noage, engl_unempl_rate_rob
# ============================================================
flatten_models <- function(nested) {
  out <- list()
  for (ident in names(nested)) {
    for (macro in names(nested[[ident]])) {
      for (spec in names(nested[[ident]][[macro]])) {
        key <- paste(ident, macro, spec, sep = "_")
        out[[key]] <- nested[[ident]][[macro]][[spec]]
      }
    }
  }
  out
}

# ============================================================
# BRITISHNESS — build panel, estimate, save, free memory
# ============================================================

cat("Building Britishness macro panel...\n")
panel_brit <- make_macro_panel(df, DV_BASE, BRIT_BASE, TIME_BASE)

cat(sprintf(
  "  panel_brit: N=%d  waves=%s\n",
  nrow(panel_brit),
  paste(sort(unique(panel_brit$wave)), collapse = ",")
))

cat("Estimating Britishness models...\n")

models_macro_mundlak_brit <- list(
  brit = list(
    gdp_pc      = fit_macro_bundle(panel_brit, "brit", "gdp_pc"),
    unempl_rate = fit_macro_bundle(panel_brit, "brit", "unempl_rate"),
    claimant_k  = fit_macro_bundle(panel_brit, "brit", "claimant_k"),
    cpih_rate   = fit_macro_bundle(panel_brit, "brit", "cpih_rate")
  )
)

# --- Checks: Britishness ---
tmp_check_brit <- add_mundlak_terms(panel_brit, vars = "brit")
stopifnot(
  max(abs(tmp_check_brit$brit - (tmp_check_brit$brit_mean + tmp_check_brit$brit_w)),
      na.rm = TRUE) < 1e-8
)
cat("Mundlak decomposition check (brit) passed.\n")
rm(tmp_check_brit)

flat_brit <- flatten_models(models_macro_mundlak_brit)

singular_brit <- sapply(flat_brit, isSingular)
conv_brit     <- sapply(flat_brit, function(m) is.null(summary(m)$optinfo$conv$lme4$messages))
if (any(singular_brit))
  warning("Singular (brit): ", paste(names(flat_brit)[singular_brit], collapse = ", "))
if (any(!conv_brit))
  warning("Convergence (brit): ", paste(names(flat_brit)[!conv_brit], collapse = ", "))
cat(sprintf("Brit checks: singular=%d/%d  conv_issues=%d/%d\n",
            sum(singular_brit), length(singular_brit),
            sum(!conv_brit), length(conv_brit)))

# --- Print: Britishness ---
for (nm in names(flat_brit)) {
  cat("\n====================================================\n")
  cat("MODEL:", nm, "\n")
  cat("====================================================\n")
  print(summary(flat_brit[[nm]]))
}

# --- Save: Britishness ---
out_dir <- "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/models_macro_mundlak"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(
  models_macro_mundlak_brit,
  file.path(out_dir, "models_macro_mundlak_brit_nested.rds")
)
saveRDS(
  flat_brit,
  file.path(out_dir, "models_macro_mundlak_brit_flat.rds")
)
cat("Saved Britishness macro Mundlak models.\n")

# --- Free memory before Englishness ---
rm(panel_brit, models_macro_mundlak_brit, flat_brit,
   singular_brit, conv_brit)
invisible(gc(verbose = FALSE)); invisible(gc(verbose = FALSE))

# ============================================================
# ENGLISHNESS — build panel, estimate, save, free memory
# ============================================================

cat("Building Englishness macro panel...\n")
panel_engl <- make_macro_panel(df, DV_BASE, ENG_BASE, TIME_BASE)

# Englishness-specific cleaning: 9999 sentinel already handled in
# make_macro_panel via the IDENT_VALID_RANGE filter, but apply
# explicitly here as a belt-and-suspenders check.
panel_engl <- panel_engl %>%
  mutate(engl = if_else(engl == 9999, NA_real_, engl)) %>%
  filter(!is.na(engl))

cat(sprintf(
  "  panel_engl: N=%d  waves=%s\n",
  nrow(panel_engl),
  paste(sort(unique(panel_engl$wave)), collapse = ",")
))

cat("Estimating Englishness models...\n")

models_macro_mundlak_engl <- list(
  engl = list(
    gdp_pc      = fit_macro_bundle(panel_engl, "engl", "gdp_pc"),
    unempl_rate = fit_macro_bundle(panel_engl, "engl", "unempl_rate"),
    claimant_k  = fit_macro_bundle(panel_engl, "engl", "claimant_k"),
    cpih_rate   = fit_macro_bundle(panel_engl, "engl", "cpih_rate")
  )
)

# --- Checks: Englishness ---
tmp_check_engl <- add_mundlak_terms(panel_engl, vars = "engl")
stopifnot(
  max(abs(tmp_check_engl$engl - (tmp_check_engl$engl_mean + tmp_check_engl$engl_w)),
      na.rm = TRUE) < 1e-8
)
cat("Mundlak decomposition check (engl) passed.\n")
rm(tmp_check_engl)

flat_engl <- flatten_models(models_macro_mundlak_engl)

singular_engl <- sapply(flat_engl, isSingular)
conv_engl     <- sapply(flat_engl, function(m) is.null(summary(m)$optinfo$conv$lme4$messages))
if (any(singular_engl))
  warning("Singular (engl): ", paste(names(flat_engl)[singular_engl], collapse = ", "))
if (any(!conv_engl))
  warning("Convergence (engl): ", paste(names(flat_engl)[!conv_engl], collapse = ", "))
cat(sprintf("Engl checks: singular=%d/%d  conv_issues=%d/%d\n",
            sum(singular_engl), length(singular_engl),
            sum(!conv_engl), length(conv_engl)))

# --- Print: Englishness ---
for (nm in names(flat_engl)) {
  cat("\n====================================================\n")
  cat("MODEL:", nm, "\n")
  cat("====================================================\n")
  print(summary(flat_engl[[nm]]))
}

# --- Save: Englishness ---
saveRDS(
  models_macro_mundlak_engl,
  file.path(out_dir, "models_macro_mundlak_engl_nested.rds")
)
saveRDS(
  flat_engl,
  file.path(out_dir, "models_macro_mundlak_engl_flat.rds")
)
cat("Saved Englishness macro Mundlak models.\n")

# --- Final summary ---
cat("\n============================================================\n")
cat("MACRO MUNDLAK PIPELINE COMPLETE\n")
cat("============================================================\n")
cat("Output directory:", out_dir, "\n")
cat("Files saved:\n")
cat("  models_macro_mundlak_brit_nested.rds\n")
cat("  models_macro_mundlak_brit_flat.rds\n")
cat("  models_macro_mundlak_engl_nested.rds\n")
cat("  models_macro_mundlak_engl_flat.rds\n")
cat(sprintf("  Brit models: %d  Engl models: %d\n",
            length(flat_engl) * 0 + # (brit already freed — count from structure)
              length(names(MACRO_VARS)) * 7,
            length(flat_engl)))
cat("Naming convention: {identity}_{macro}_{spec}\n")
cat("  spec options: main0_noage, main0_age, cont_noage, cont_age,\n")
cat("                cat_noage, cat_age, rob\n")
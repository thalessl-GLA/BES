# ============================================================
# 01_make_equivalised_income.R
# Purpose: Create OECD-style equivalised household income for BES
# Output: equivalised_income_by_id.rds (compact merge file)
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(haven)


# ------------------------------
# 1) Load raw BES data
# ------------------------------
# Justification: we build the variable once, reproducibly, and avoid
# mixing "data engineering" with modelling documents.

bes_raw <- read_sav("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES2024_W30_Panel_v30.0.sav")

# ------------------------------
# 2) Detect overlapping waves 
# ------------------------------
hh_size_base   <- "p_hh_size"
hh_child_base  <- "p_hh_children"
hh_income_base <- "p_gross_household"

get_wave_num <- function(x) as.integer(str_extract(x, "(?<=W)\\d+$"))

waves_available <- function(df, base){
  cols <- names(df)[str_detect(names(df), paste0("^", base, "W\\d+$"))]
  sort(unique(get_wave_num(cols)))
}

waves <- Reduce(intersect, list(
  waves_available(bes_raw, hh_size_base),
  waves_available(bes_raw, hh_child_base),
  waves_available(bes_raw, hh_income_base)
))

if(length(waves) == 0) stop("No overlapping waves for p_hh_size / p_hh_children / p_gross_household.")

size_cols   <- paste0(hh_size_base, "W", waves)
child_cols  <- paste0(hh_child_base, "W", waves)
income_cols <- paste0(hh_income_base, "W", waves)

# ------------------------------
# 3) Create a clean long HH dataset (id-wave) 
# ------------------------------
# Justification: id-wave rows with three raw inputs.
# This is the minimal representation needed for equivalisation and merging.
# ------------------------------

hh_long <- bes_raw %>%
  select(id, all_of(size_cols), all_of(child_cols), all_of(income_cols)) %>%
  mutate(across(-id, ~ as.numeric(zap_labels(.x)))) %>%   # avoid labelled-class pivot issues
  pivot_longer(
    cols = -id,
    names_to = c("base", "wave"),
    names_pattern = "(.+)W(\\d+)$",
    values_to = "value"
  ) %>%
  mutate(wave = as.integer(wave)) %>%
  # Keep only the three bases we care about (hard filter = safer than broad regex)
  filter(base %in% c(hh_size_base, hh_child_base, hh_income_base)) %>%
  pivot_wider(names_from = base, values_from = value) %>%
  rename(
    hh_size_raw     = !!hh_size_base,
    hh_children_raw = !!hh_child_base,
    hh_income_raw   = !!hh_income_base
  )

# ------------------------------
# 4) Recode HH size and children
# ------------------------------
# Justification: BES uses categorical codes with top-codes and DK/PNTA.
# I apply conservative recodes and set DK/PNTA to missing.
#
# NOTE: These DK/PNTA codes are based on BES profile conventions:
# p_hh_size: 8="8+", 9=DK, 10=PNTA
# p_hh_children: 6="5+", 7="6+", 8=DK, 9=PNTA
# ------------------------------
hh_long <- hh_long %>%
  mutate(
    hh_size = case_when(
      hh_size_raw %in% 1:7 ~ hh_size_raw,
      hh_size_raw == 8     ~ 8,          # "8 or more" -> 8 (conservative)
      hh_size_raw %in% c(9, 10) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    hh_children = case_when(
      hh_children_raw %in% 0:5 ~ hh_children_raw,
      hh_children_raw == 6     ~ 5,       # "5 or more" -> 5 (conservative)
      hh_children_raw == 7     ~ 6,       # "6 or more" -> 6 (conservative)
      hh_children_raw %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    # Adults inferred; minimum 1 adult in household for equivalence scale
    hh_adults = if_else(!is.na(hh_size) & !is.na(hh_children),
                        pmax(hh_size - hh_children, 1),
                        NA_real_)
  )

# ------------------------------
# 5) Convert income bands to annual £ midpoints
# ------------------------------
# Justification: equivalisation requires numeric income. With banded income,
# midpoints are the standard least-assumptive conversion.
#
# Coding assumed for p_gross_household:
# 1 Under 5k; 2 5–10k; ... ; 15 150k+; 16 DK; 17 PNTA
# ------------------------------
income_band_midpoint_annual <- function(x){
  case_when(
    x == 1  ~  2500,
    x == 2  ~  7500,
    x == 3  ~ 12500,
    x == 4  ~ 17500,
    x == 5  ~ 22500,
    x == 6  ~ 27500,
    x == 7  ~ 32500,
    x == 8  ~ 37500,
    x == 9  ~ 42500,
    x == 10 ~ 47500,
    x == 11 ~ 55000,
    x == 12 ~ 65000,
    x == 13 ~ 85000,
    x == 14 ~ 125000,
    x == 15 ~ 175000,      # top-coded; conservative midpoint assumption
    x %in% c(16, 17) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

hh_long <- hh_long %>%
  mutate(
    hh_income_annual_mid = income_band_midpoint_annual(hh_income_raw)
  )

# ------------------------------
# 6) OECD-modified equivalence scale + equivalised income
# ------------------------------
# Justification:
# ONS uses OECD-modified equivalence scale:
#  first adult=1.0; additional adults=0.5; children 0–13=0.3; 14+=0.5.
# BUT BES children variable is "under 18" (no 0–13 vs 14–17 split).
# So I bracket with:
#  - main: all children weight=0.3 (lower bound)
#  - sensitivity: all children weight=0.5 (upper bound)
# ------------------------------
hh_long <- hh_long %>%
  mutate(
    eq_scale_child03 = if_else(
      !is.na(hh_adults) & !is.na(hh_children),
      1.0 + 0.5 * pmax(hh_adults - 1, 0) + 0.3 * hh_children,
      NA_real_
    ),
    eq_scale_child05 = if_else(
      !is.na(hh_adults) & !is.na(hh_children),
      1.0 + 0.5 * pmax(hh_adults - 1, 0) + 0.5 * hh_children,
      NA_real_
    ),
    income_eq_child03 = hh_income_annual_mid / eq_scale_child03,
    income_eq_child05 = hh_income_annual_mid / eq_scale_child05
  )

# ------------------------------
# 7) Save compact merge file (id-wave)
# ------------------------------
equivalised_income_long <- hh_long %>%
  select(
    id, wave,
    hh_size, hh_children, hh_adults,
    hh_income_raw, hh_income_annual_mid,
    income_eq_child03, income_eq_child05
  )

saveRDS(
  equivalised_income_long,
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/equivalised_income_long.rds"
)


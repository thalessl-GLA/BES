# =============================================================================
# GDP PER CAPITA BY LOCAL AUTHORITY — FULL PIPELINE
# Source: ONS Regional gross domestic product: local authorities
#         Tables 5 (GDP £m), 6 (population), 7 (GDP per head)
#         Coverage: 1998–2023, England LAs
# =============================================================================

library(readxl)
library(tidyverse)

path_gdp <- "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/regionalgrossdomesticproductgdplocalauthorities.xlsx"

# =============================================================================
# 1. TABLE 7: GDP PER HEAD (pre-computed by ONS — primary variable)
# =============================================================================

gdp_perhead_raw <- read_excel(path_gdp, sheet = "Table 7", skip = 1)

gdp_percap <- gdp_perhead_raw %>%
  rename(
    itl1_region = `ITL1 Region`,
    la_code     = `LA code`,
    la_name     = `LA name`
  ) %>%
  # England only; exclude City of London and Westminster
  # (workplace-based GDP produces meaningless per-head values
  #  for these two LAs: ~£7m and ~£459k respectively in 2023)
  filter(
    str_starts(la_code, "E"),
    !la_code %in% c("E09000001", "E09000033")
  ) %>%
  mutate(across(-c(itl1_region, la_code, la_name), as.character)) %>%
  pivot_longer(
    cols      = -c(itl1_region, la_code, la_name),
    names_to  = "year_chr",
    values_to = "gdp_percap"
  ) %>%
  mutate(
    year       = as.integer(year_chr),
    gdp_percap = parse_number(gdp_percap)
  ) %>%
  filter(
    !is.na(year),
    !is.na(gdp_percap),
    gdp_percap > 0,
    year >= 2014
  ) %>%
  select(la_code, la_name, itl1_region, year, gdp_percap)

# =============================================================================
# 2. TABLE 6: RESIDENT POPULATION (used for weighting and verification)
# =============================================================================

pop_raw <- read_excel(path_gdp, sheet = "Table 6", skip = 1)

pop_la <- pop_raw %>%
  rename(
    itl1_region = `ITL1 Region`,
    la_code     = `LA code`,
    la_name     = `LA name`
  ) %>%
  filter(str_starts(la_code, "E")) %>%
  mutate(across(-c(itl1_region, la_code, la_name), as.character)) %>%
  pivot_longer(
    cols      = -c(itl1_region, la_code, la_name),
    names_to  = "year_chr",
    values_to = "population"
  ) %>%
  mutate(
    year       = as.integer(year_chr),
    population = parse_number(population)
  ) %>%
  filter(
    !is.na(year),
    !is.na(population),
    population > 0,
    year >= 2014
  ) %>%
  select(la_code, la_name, year, population)

# =============================================================================
# 3. TABLE 5: GDP AT CURRENT MARKET PRICES £m (for constructing own per capita
#    as cross-check against Table 7 — optional but useful for verification)
# =============================================================================

gdp_levels_raw <- read_excel(path_gdp, sheet = "Table 5", skip = 1)

gdp_levels <- gdp_levels_raw %>%
  rename(
    itl1_region = `ITL1 Region`,
    la_code     = `LA code`,
    la_name     = `LA name`
  ) %>%
  filter(
    str_starts(la_code, "E"),
    !la_code %in% c("E09000001", "E09000033")
  ) %>%
  mutate(across(-c(itl1_region, la_code, la_name), as.character)) %>%
  pivot_longer(
    cols      = -c(itl1_region, la_code, la_name),
    names_to  = "year_chr",
    values_to = "gdp_millions"
  ) %>%
  mutate(
    year         = as.integer(year_chr),
    gdp_millions = parse_number(gdp_millions)
  ) %>%
  filter(!is.na(year), !is.na(gdp_millions), year >= 2014) %>%
  select(la_code, la_name, year, gdp_millions)

# =============================================================================
# 4. COMBINE AND VERIFY
# =============================================================================

gdp_la_panel <- gdp_percap %>%
  left_join(
    pop_la     %>% select(la_code, year, population),
    by = c("la_code", "year")
  ) %>%
  left_join(
    gdp_levels %>% select(la_code, year, gdp_millions),
    by = c("la_code", "year")
  ) %>%
  # Construct own per-capita as verification cross-check
  mutate(
    gdp_percap_check = (gdp_millions * 1e6) / population
  )

# =============================================================================
# 5. SANITY CHECKS
# =============================================================================

# One row per LA-year?
gdp_la_panel %>%
  group_by(la_code, year) %>%
  tally() %>%
  filter(n > 1)    # must be 0

# Coverage consistent across years?
gdp_la_panel %>%
  group_by(year) %>%
  summarise(
    n_la             = n_distinct(la_code),
    n_missing_gdp    = sum(is.na(gdp_percap)),
    n_missing_pop    = sum(is.na(population)),
    .groups          = "drop"
  )

# Distribution by year
gdp_la_panel %>%
  group_by(year) %>%
  summarise(
    mean_gdp   = round(mean(gdp_percap,   na.rm = TRUE)),
    median_gdp = round(median(gdp_percap, na.rm = TRUE)),
    p10        = round(quantile(gdp_percap, 0.10, na.rm = TRUE)),
    p90        = round(quantile(gdp_percap, 0.90, na.rm = TRUE)),
    .groups    = "drop"
  )

# Cross-check: Table 7 vs own construction — should be very close
# Small differences expected due to ONS rounding in published per-head figures
gdp_la_panel %>%
  filter(!is.na(gdp_percap_check)) %>%
  mutate(pct_diff = abs(gdp_percap - gdp_percap_check) / gdp_percap * 100) %>%
  summarise(
    mean_pct_diff   = round(mean(pct_diff,   na.rm = TRUE), 2),
    median_pct_diff = round(median(pct_diff, na.rm = TRUE), 2),
    max_pct_diff    = round(max(pct_diff,    na.rm = TRUE), 2)
  )
# If max difference > 5%, something is wrong — investigate those LAs

# Known plausibility anchors
gdp_la_panel %>%
  filter(
    la_name %in% c("Leeds", "Birmingham", "Cornwall",
                   "Hartlepool", "Kensington and Chelsea"),
    year %in% c(2014, 2019, 2023)
  ) %>%
  arrange(la_name, year) %>%
  select(la_name, year, gdp_percap, population, gdp_millions)

# =============================================================================
# 6. SAVE
# =============================================================================

saveRDS(
  gdp_la_panel,
  file = here("gdp_la_panel.rds")
)

write_csv(
  gdp_la_panel,
  file = here("gdp_la_panel.csv")
)

cat("GDP LA panel built successfully\n")
cat("LAs:", n_distinct(gdp_la_panel$la_code), "\n")
cat("Years:", range(gdp_la_panel$year), "\n")
cat("Rows:", nrow(gdp_la_panel), "\n")
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(janitor)
library(tidyverse)



BES_subset_panel <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel.rds")

# Checking BES constituencies 

# 1. extract all starttime variables in the BES dataframe
start_vars <- grep("^starttimeW\\d+$", names(BES_subset_panel), value = TRUE)

# 2. pivot them long
wave_years_long <- BES_subset_panel %>%
  select(all_of(start_vars)) %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = all_of(start_vars),
    names_to = "wave_var",
    values_to = "starttime"
  ) %>%
  mutate(
    wave = as.numeric(str_extract(wave_var, "\\d+")),
    year = year(ymd_hms(starttime))
  ) %>%
  filter(!is.na(year)) %>%
  group_by(wave) %>%
  summarise(
    fieldwork_year = median(year),   # median is robust to some rows being NA
    .groups = "drop"
  ) %>%
  arrange(wave)

pcon_vars <- grep("^pcon_codeW\\d+$", names(BES_subset_panel), value = TRUE)

bes_const_long <- BES_subset_panel %>%
  mutate(id = row_number()) %>%
  select(id, all_of(pcon_vars)) %>%
  pivot_longer(
    cols = all_of(pcon_vars),
    names_to = "wave_var",
    values_to = "pcon_code"
  ) %>%
  mutate(
    wave = as.numeric(str_extract(wave_var, "\\d+"))
  ) %>%
  filter(!is.na(pcon_code))

bes_const_with_year <- bes_const_long %>%
  left_join(wave_years_long, by = "wave")

bes_const_summary <- bes_const_with_year %>%
  group_by(wave, fieldwork_year) %>%
  summarise(
    n_obs = n(),
    n_constituencies = n_distinct(pcon_code),
    .groups = "drop"
  )

###############################################################################

### Meso level data integration

## Income

# https://www.ons.gov.uk/datasets/ashe-tables-9-and-10/editions/time-series/versions/7#id-dimensions

meso_income <- read_csv("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Meso_variables/ashe-tables-9-and-10-time-series-v7.csv")

ashe_clean <- meso_income %>%
  filter(
    AveragesAndPercentiles == "Median",
    HoursAndEarnings == "Weekly pay - Gross",
    WorkplaceOrResidence == "Residence",
    WorkingPattern == "All",
    Sex == "All"                 # <- this removes separate Male/Female rows
  ) %>%
  group_by(`parliamentary-constituencies`, Time) %>%
  summarise(
    income = mean(v4_2, na.rm = TRUE),  # in case there's still >1 row, collapse
    .groups = "drop"
  ) %>%
  rename(
    constituency = `parliamentary-constituencies`,
    year         = Time
  )


ashe_consts <- unique(ashe_clean$constituency)
bes_consts  <- unique(bes_const_with_year$pcon_code)

length(intersect(ashe_consts, bes_consts))

common_years <- intersect(ashe_clean$year, bes_const_with_year$fieldwork_year)

matchability <- bes_const_with_year %>%
  left_join(
    ashe_clean,
    by = c("pcon_code" = "constituency",
           "fieldwork_year" = "year")
  ) %>%
  group_by(wave, fieldwork_year) %>%
  summarise(
    respondents_in_wave      = n(),
    matched_to_income        = sum(!is.na(income)),
    n_constituencies_in_wave = n_distinct(pcon_code),
    n_matched_constituencies = n_distinct(pcon_code[!is.na(income)]),
    match_rate               = matched_to_income / respondents_in_wave,
    .groups = "drop"
  )

matchability

###############################################################################

# Checks

# 1. All unique constituency codes in BES (across all waves)
bes_pcon_all <- bes_const_with_year %>%
  filter(!is.na(pcon_code)) %>%
  distinct(pcon_code) %>%
  arrange(pcon_code) %>%
  pull()

length(bes_pcon_all)       # how many unique in BES?

# 2. All unique constituency codes in ASHE (after your cleaning)
ashe_pcon_all <- ashe_clean %>%
  distinct(constituency) %>%
  arrange(constituency) %>%
  pull()

length(ashe_pcon_all)      # how many unique in ASHE?

# 3. How many are in common?
length(intersect(bes_pcon_all, ashe_pcon_all))

# 4. Constituencies only in BES (no ASHE match)
only_in_bes <- setdiff(bes_pcon_all, ashe_pcon_all)

# 5. Constituencies only in ASHE (no BES match)
only_in_ashe <- setdiff(ashe_pcon_all, bes_pcon_all)

only_in_bes
only_in_ashe

###############################################################################

# Some plots 

# Year-level summary
ashe_year <- ashe_clean %>%
  group_by(year) %>%
  summarise(
    median_income = median(income, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(ashe_year, aes(x = year, y = median_income)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Median Weekly Earnings Across Parliamentary Constituencies",
    x = "Year",
    y = "Median weekly earnings (£)"
  ) +
  theme_minimal(base_size = 14)

# Compute inequality measures per year
ashe_ineq <- ashe_clean %>%
  group_by(year) %>%
  summarise(
    p90 = quantile(income, 0.90, na.rm = TRUE),
    p10 = quantile(income, 0.10, na.rm = TRUE),
    ratio_90_10 = p90 / p10,
    .groups = "drop"
  )

# Plot 90/10 ratio
ggplot(ashe_ineq, aes(x = year, y = ratio_90_10)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Income Inequality Across UK Constituencies (ASHE)",
    subtitle = "90/10 ratio of median weekly earnings",
    x = "Year",
    y = "P90 / P10 ratio"
  ) +
  theme_minimal(base_size = 14)

###############################################################################

## Merge ASHE

## 1. Build constituency–year income panel from ASHE -------------------------

ashe_panel <- meso_income %>%
  filter(
    AveragesAndPercentiles == "Median",
    HoursAndEarnings       == "Weekly pay - Gross",
    WorkplaceOrResidence   == "Residence",
    WorkingPattern         == "All"
  ) %>%
  group_by(`parliamentary-constituencies`, `calendar-years`) %>%
  summarise(
    income = mean(v4_2, na.rm = TRUE),  # collapse any remaining duplicates
    .groups = "drop"
  ) %>%
  rename(
    constituency = `parliamentary-constituencies`,
    year         = `calendar-years`
  )

## 2. Attach income to the BES id×wave panel (bes_const_with_year) ----------

# bes_const_with_year should already have: id, wave, pcon_code, fieldwork_year
bes_const_income <- bes_const_with_year %>%
  left_join(
    ashe_panel,
    by = c(
      "pcon_code"      = "constituency",
      "fieldwork_year" = "year"
    )
  )
# Now: one row per id×wave, with an `income` column for that pcon-year where available.

## 3. Reshape to wide: incomeW1, incomeW2, ..., incomeW30 -------------------

bes_income_wide <- bes_const_income %>%
  select(id, wave, income) %>%
  mutate(wave_var = paste0("incomeW", wave)) %>%
  select(-wave) %>%
  pivot_wider(
    names_from  = wave_var,
    values_from = income
  )

## 4. Make sure BES_subset_panel has an id -----------------------------------

if (!"id" %in% names(BES_subset_panel)) {
  BES_subset_panel <- BES_subset_panel %>%
    mutate(id = row_number())
}

## 5. Merge the meso income into your BES subset -----------------------------

BES_subset_panel_meso <- BES_subset_panel %>%
  left_join(bes_income_wide, by = "id")

## Checks ----

BES_subset_panel_meso %>%
  mutate(any_income = if_any(starts_with("incomeW"), ~ !is.na(.))) %>%
  summarise(
    n_obs          = n(),
    n_with_income  = sum(any_income),
    pct_with_income = mean(any_income)
  )

bes_income_by_wave <- bes_const_income %>%
  group_by(wave) %>%
  summarise(
    n_obs_wave      = n(),
    matched_income  = sum(!is.na(income)),
    pct_matched     = matched_income / n_obs_wave
  )

###############################################################################

### Labour market indicators

##Claimant count 

# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/claimantcountbyparliamentaryconstituencyexperimental

### 1. Read raw claimant CSV 
claimant_raw <- read_csv(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Meso_variables/calimant_count.csv",
  skip = 6,
  col_names = TRUE,
  show_col_types = FALSE
)

# Rename columns to something usable
claimant_raw <- claimant_raw %>%
  rename(
    constituency_raw = `measure    :`,
    values_raw       = `Claimant count`
  )

### 2. Drop obvious non-constituency rows -----------------------------------

claimant_clean <- claimant_raw %>%
  filter(
    !constituency_raw %in% c("Column Total", "-", "", NA),
    !str_detect(constituency_raw, "parliamentary constituency")  # extra safety
  )

### 3. Split comma-separated monthly values into columns ---------------------

claimant_split <- claimant_clean %>%
  mutate(values_raw = str_split(values_raw, ",")) %>%
  unnest_wider(values_raw, names_sep = "_m")

# Number of month columns (everything except the name)
n_months <- ncol(claimant_split) - 1

# Create monthly Date sequence: Jan 2014 onwards
start_date <- ymd("2014-01-01")
date_seq   <- seq(start_date, by = "month", length.out = n_months)

colnames(claimant_split)[2:(n_months + 1)] <- as.character(date_seq)

### 4. Pivot to long: constituency × month ----------------------------------

claimant_long <- claimant_split %>%
  pivot_longer(
    cols      = -constituency_raw,
    names_to  = "date",
    values_to = "claimants"
  ) %>%
  mutate(
    date      = as.Date(date),
    claimants = as.numeric(claimants),
    year      = year(date),
    month     = month(date)
  ) %>%
  rename(
    constituency = constituency_raw
  )

# Quick sanity check
summary(claimant_long$claimants)
n_distinct(claimant_long$constituency)
range(claimant_long$date)

### 5. Build constituency code ↔ name lookup from ASHE (meso_income) --------
# (assuming meso_income is your raw ASHE file with constituency codes)

con_lookup <- meso_income %>%
  select(
    pcon_code     = `parliamentary-constituencies`,  # e.g. E1400...
    pcon_name_raw = Geography                        # constituency label
  ) %>%
  distinct() %>%
  mutate(
    pcon_name_clean = str_squish(str_to_title(pcon_name_raw))
  )

### 6. Attach pcon_code to claimant data (GB only) ---------------------------

claimant_with_code <- claimant_long %>%
  mutate(
    constituency_clean = str_squish(str_to_title(constituency)),
    # Fix known mismatch: Ynys Mon -> Ynys Môn
    constituency_clean = case_when(
      constituency_clean == "Ynys Mon" ~ "Ynys Môn",
      TRUE ~ constituency_clean
    )
  ) %>%
  left_join(
    con_lookup %>% select(pcon_code, pcon_name_clean),
    by = c("constituency_clean" = "pcon_name_clean")
  )

# Drop NI constituencies (codes starting with "N") – BES does not cover NI
claimant_with_code_gb <- claimant_with_code %>%
  filter(is.na(pcon_code) | !str_starts(pcon_code, "N"))

### 7. Collapse to yearly constituency-level claimant measures --------------

claimant_by_year <- claimant_with_code_gb %>%
  filter(!is.na(pcon_code)) %>%
  group_by(pcon_code, year) %>%
  summarise(
    claimant_mean   = mean(claimants, na.rm = TRUE),
    claimant_median = median(claimants, na.rm = TRUE),
    n_months        = n(),
    .groups         = "drop"
  )

# Sanity check
claimant_by_year %>%
  summarise(
    n_constituencies = n_distinct(pcon_code),
    min_year         = min(year),
    max_year         = max(year)
  )

### 8. Merge claimant data into BES (via pcon_code + fieldwork_year) --------
# Here we assume you already have bes_const_with_year with:
# id, wave, pcon_code, fieldwork_year

bes_claimant_long <- bes_const_with_year %>%
  left_join(
    claimant_by_year,
    by = c(
      "pcon_code"      = "pcon_code",
      "fieldwork_year" = "year"
    )
  )

### 9. Pivot claimant to wide by wave, then merge into BES_subset_panel_meso -

bes_claimant_wide <- bes_claimant_long %>%
  select(id, wave, claimant_mean, claimant_median, n_months) %>%
  arrange(id, wave) %>%
  pivot_wider(
    id_cols    = id,
    names_from = wave,
    values_from = c(claimant_mean, claimant_median, n_months),
    names_glue = "{.value}W{wave}"
  )

BES_subset_panel_full <- BES_subset_panel_meso %>%
  left_join(bes_claimant_wide, by = "id")

### 10. Example coverage check: Wave 11 -------------------------------------

BES_subset_panel_full %>%
  mutate(has_claimantW11 = !is.na(claimant_medianW11)) %>%
  summarise(
    n_obs              = n(),
    n_with_claim_W11   = sum(has_claimantW11),
    pct_with_claim_W11 = mean(has_claimantW11)
  )

###############################################################################

### Labour market indicators

## unemployment

# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/locallabourmarketindicatorsforparliamentaryconstituenciesli02/current

unemployment_raw <- read_csv(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Meso_variables/labour_mkt_indicators.csv",
  skip = 6,
  col_names = TRUE,
  show_col_types = FALSE
)

library(dplyr)
library(stringr)
library(purrr)
library(readr)

## 0. Start from unemployment_raw (CSV you already loaded)
# str(unemployment_raw)
# names(unemployment_raw)

## 1. Rename constituency column and drop obvious non-constituency rows (if any)
unemp_clean <- unemployment_raw %>%
  rename(constituency_raw = `parliamentary constituency 2010`) %>%
  filter(
    !is.na(constituency_raw),
    constituency_raw != "",
    constituency_raw != "Column Total"   # sometimes present in these releases
  )

## 2. Build meta with rate / numerator / denominator columns (you already have this)
# Example pattern (adapt to whatever you actually used for meta):
rate_cols <- names(unemp_clean)[str_detect(names(unemp_clean), "^[A-Z][a-z]{2} 20[0-9]{2}-")]
num_cols  <- names(unemp_clean)[str_detect(names(unemp_clean), "^Numerator")]
den_cols  <- names(unemp_clean)[str_detect(names(unemp_clean), "^Denominator")]

meta <- tibble(
  rate_col = rate_cols,
  num_col  = num_cols,
  den_col  = den_cols
)

## 3. Long format: keep your logic, but parse numbers safely

unemp_long <- meta %>%
  pmap_dfr(function(rate_col, num_col, den_col) {
    unemp_clean %>%
      select(
        constituency_raw,
        numerator   = all_of(num_col),
        denominator = all_of(den_col),
        rate        = all_of(rate_col)
      ) %>%
      mutate(period = rate_col)
  }) %>%
  mutate(
    # robust numeric parsing; ignores text footnotes, keeps real numbers
    numerator   = parse_number(numerator),
    denominator = parse_number(denominator),
    rate        = parse_number(rate),
    
    # year from period label, using last year in "Jan 2019-Dec 2019" etc.
    year        = as.numeric(str_extract(period, "[0-9]{4}$")),
    
    # clean constituency name (for later matching)
    constituency_clean = str_squish(str_to_title(constituency_raw)),
    constituency_clean = case_when(
      constituency_clean == "Ynys Mon" ~ "Ynys Môn",
      TRUE                             ~ constituency_clean
    ),
    
    # kill clearly impossible rates created by footnotes
    rate = ifelse(rate > 100, NA_real_, rate)
  )

library(dplyr)
library(stringr)
library(tidyr)

# 4. Attach pcon_code to unemployment (via constituency name)

unemp_with_code <- unemp_long %>%
  # drop any rows with missing cleaned name
  filter(!is.na(constituency_clean)) %>%
  left_join(
    con_lookup %>% select(pcon_code, pcon_name_clean),
    by = c("constituency_clean" = "pcon_name_clean")
  )

unemp_with_code %>%
  summarise(
    n_rows      = n(),
    n_with_code = sum(!is.na(pcon_code)),
    n_no_code   = sum(is.na(pcon_code)),
    n_const     = n_distinct(pcon_code, na.rm = TRUE)
  )

# 5. Drop Northern Ireland constituencies (not in BES, codes start with "N")

unemp_with_code_gb <- unemp_with_code %>%
  filter(is.na(pcon_code) | !str_starts(pcon_code, "N"))

# 6. Collapse to yearly constituency-level stats

unemp_yearly <- unemp_with_code_gb %>%
  filter(!is.na(pcon_code), !is.na(year)) %>%
  group_by(pcon_code, year) %>%
  summarise(
    # APS rate (percentage) across all rolling windows in that year
    unemp_rate_mean   = mean(rate, na.rm = TRUE),
    unemp_rate_median = median(rate, na.rm = TRUE),
    
    # underlying counts (averaged across windows)
    unemp_num_mean    = mean(numerator,   na.rm = TRUE),
    unemp_den_mean    = mean(denominator, na.rm = TRUE),
    
    # how many rolling windows contributed
    n_periods         = n(),
    .groups           = "drop"
  ) %>%
  arrange(pcon_code, year) %>%
  group_by(pcon_code) %>%
  mutate(
    # backward-looking 3-year rolling average:
    # year t uses t, t-1, t-2
    unemp_rate_roll3 = if_else(
      row_number() >= 3,
      (unemp_rate_mean +
         lag(unemp_rate_mean, 1) +
         lag(unemp_rate_mean, 2)) / 3,
      NA_real_
    ),
    # optional: reconstructed rate from counts (in %)
    unemp_rate_from_counts = if_else(
      !is.na(unemp_num_mean) & !is.na(unemp_den_mean) & unemp_den_mean > 0,
      100 * unemp_num_mean / unemp_den_mean,
      NA_real_
    )
  ) %>%
  ungroup()

# sanity check
unemp_yearly %>%
  summarise(
    n_constituencies = n_distinct(pcon_code),
    min_year         = min(year, na.rm = TRUE),
    max_year         = max(year, na.rm = TRUE)
  )

# 7. Attach unemployment to BES by pcon_code × year

bes_unemp_long <- bes_const_with_year %>%
  left_join(
    unemp_yearly,
    by = c(
      "pcon_code"      = "pcon_code",   # constituency code
      "fieldwork_year" = "year"         # BES fieldwork year ↔ APS year
    )
  )

# optional quick coverage check (example: any wave)
bes_unemp_long %>%
  group_by(wave) %>%
  summarise(
    n_obs           = n(),
    pct_with_unemp  = mean(!is.na(unemp_rate_mean))
  )

# 8. Pivot to wide: unemployment vars per wave (one row per respondent)

bes_unemp_wide <- bes_unemp_long %>%
  select(
    id, wave,
    unemp_rate_mean,
    unemp_rate_median,
    unemp_rate_roll3,
    unemp_rate_from_counts,
    unemp_num_mean,
    unemp_den_mean,
    n_periods
  ) %>%
  arrange(id, wave) %>%
  pivot_wider(
    id_cols    = id,
    names_from = wave,
    values_from = c(
      unemp_rate_mean,
      unemp_rate_median,
      unemp_rate_roll3,
      unemp_rate_from_counts,
      unemp_num_mean,
      unemp_den_mean,
      n_periods
    ),
    names_glue = "{.value}W{wave}"
  )

### 6. Merge into your main analysis dataset

BES_subset_panel_full_v1 <- BES_subset_panel_full %>%
  left_join(bes_unemp_wide, by = "id")

# final quick check (example: wave 11)
BES_subset_panel_full_v1 %>%
  summarise(
    n_obs               = n(),
    pct_with_unempW11   = mean(!is.na(unemp_rate_meanW11))
  )

saveRDS(
  BES_subset_panel_full_v1,
  file = "BES_subset_panel_full_v1.rds"
)

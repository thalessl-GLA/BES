library(readxl)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(dplyr)

# 0. Load dataset 
BES_subset_panel_full_v5_England <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v4_England.rds")

# =============================================================================
# BUILD LOOKUP FROM ONS 2021 LA REFERENCE FILE
# =============================================================================

la_lookup_2021 <- read_csv(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Meso_variables/LAD21_LPA21_UK_LU_84e80c61120748b68f9d2b8c19c64888_7318599518562124250.csv",
  show_col_types = FALSE
) %>%
  select(
    la_code = LAD21CD,
    la_name = LAD21NM
  ) %>%
  filter(str_starts(la_code, "E")) %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  ) %>%
  distinct(la_code, la_name, la_name_clean)

# Verify
la_lookup_2021 %>% summarise(n_la = n_distinct(la_code))
la_lookup_2021 %>% filter(str_detect(la_name, "Darlington|Durham|Somerset"))


# =============================================================================
# ASHE MEDIAN WEEKLY EARNINGS — LOCAL AUTHORITY
# =============================================================================

ASHE_raw <- read_excel(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Meso_variables/ASHE_LA.xlsx",
  col_names = FALSE
)

# Row 9 = years, Row 10 = number/conf%, Data from row 11
year_row <- as.character(ASHE_raw[9, ])
type_row <- as.character(ASHE_raw[10, ])

# Identify estimate columns (type == "number") and their years
estimate_cols <- which(type_row == "number")
estimate_years <- year_row[estimate_cols]

cat("Estimate columns:", estimate_cols, "\n")
cat("Years found:     ", estimate_years, "\n")

# =============================================================================
# BUILD CLEAN PANEL
# =============================================================================

ashe_la_long <- ASHE_raw %>%
  # Keep only data rows (row 11 onward)
  slice(11:n()) %>%
  # Select LA name column + estimate columns only
  select(1, all_of(estimate_cols)) %>%
  # Name columns
  set_names(c("la_name", paste0("earnings_", estimate_years))) %>%
  # Remove non-LA rows
  filter(
    !is.na(la_name),
    la_name != "",
    !str_detect(la_name, "^annual"),
    !str_detect(la_name, "^ONS"),
    !str_detect(la_name, "^NA")
  ) %>%
  # Pivot to long
  pivot_longer(
    cols      = starts_with("earnings_"),
    names_to  = "year_chr",
    values_to = "earnings_median"
  ) %>%
  mutate(
    year            = as.integer(str_extract(year_chr, "\\d{4}")),
    earnings_median = parse_number(as.character(earnings_median))
  ) %>%
  filter(!is.na(year), !is.na(earnings_median)) %>%
  select(la_name, year, earnings_median)

# Sanity check
ashe_la_long %>%
  group_by(year) %>%
  summarise(
    n_la            = n_distinct(la_name),
    median_earnings = median(earnings_median, na.rm = TRUE),
    p10             = quantile(earnings_median, 0.10, na.rm = TRUE),
    p90             = quantile(earnings_median, 0.90, na.rm = TRUE),
    .groups         = "drop"
  )

# Plausibility: Darlington 2013 = 446.1, 2024 = 652.4
ashe_la_long %>%
  filter(la_name == "Darlington") %>%
  arrange(year)

# =============================================================================
# ATTACH LA CODES VIA NAME LOOKUP
# =============================================================================

la_name_lookup <- la_lookup_2021 %>%
  distinct(la_code, la_name) %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

ashe_la_coded <- ashe_la_long %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  ) %>%
  left_join(
    la_name_lookup %>% select(la_code, la_name_clean),
    by = "la_name_clean"
  )

# Check unmatched
ashe_la_coded %>%
  filter(is.na(la_code)) %>%
  distinct(la_name) %>%
  arrange(la_name) %>%
  print(n = 100)

# Final panel — England only
ashe_la_panel <- ashe_la_coded %>%
  filter(!is.na(la_code), str_starts(la_code, "E")) %>%
  select(la_code, la_name, year, earnings_median)

# Coverage by year
ashe_la_panel %>%
  group_by(year) %>%
  summarise(
    n_la            = n_distinct(la_code),
    median_earnings = round(median(earnings_median, na.rm = TRUE)),
    .groups         = "drop"
  )

# =============================================================================
# MERGE INTO BES — identical structure to GDP pipeline
# =============================================================================

la_vars <- grep("^oslauaW\\d+$", names(BES_subset_panel_full_v5_England), value = TRUE)
la_vars <- la_vars[order(as.integer(str_extract(la_vars, "\\d+")))]

start_vars <- grep("^starttimeW\\d+$", names(BES_subset_panel_full_v5_England), value = TRUE)

wave_years_long <- BES_subset_panel_full_v5_England %>%
  select(all_of(start_vars)) %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols      = all_of(start_vars),
    names_to  = "wave_var",
    values_to = "starttime"
  ) %>%
  mutate(
    wave          = as.numeric(str_extract(wave_var, "\\d+")),
    year          = year(ymd_hms(starttime))
  ) %>%
  filter(!is.na(year)) %>%
  group_by(wave) %>%
  summarise(fieldwork_year = median(year), .groups = "drop") %>%
  arrange(wave)

england_la_long <- BES_subset_panel_full_v5_England %>%
  select(id, all_of(la_vars)) %>%
  mutate(id = as.character(id)) %>%
  pivot_longer(
    cols      = all_of(la_vars),
    names_to  = "wave_var",
    values_to = "la_code"
  ) %>%
  mutate(
    wave    = as.integer(str_extract(wave_var, "\\d+")),
    la_code = as.character(la_code)
  ) %>%
  filter(!is.na(la_code), la_code != "") %>%
  left_join(wave_years_long, by = "wave") %>%
  select(id, wave, la_code, fieldwork_year)

# Confirm
england_la_long %>%
  group_by(wave, fieldwork_year) %>%
  summarise(n_obs = n(), n_la = n_distinct(la_code), .groups = "drop") %>%
  print(n = 30)

# =============================================================================
# NOW MERGE ASHE
# =============================================================================

england_ashe_long <- england_la_long %>%
  left_join(
    ashe_la_panel %>% select(la_code, year, earnings_median),
    by = c("la_code" = "la_code", "fieldwork_year" = "year")
  )

# Coverage check
england_ashe_long %>%
  group_by(wave, fieldwork_year) %>%
  summarise(
    n_with_la         = n(),
    pct_with_earnings = round(mean(!is.na(earnings_median)) * 100, 1),
    .groups           = "drop"
  ) %>%
  print(n = 30)

# Pivot wide
ashe_wide <- england_ashe_long %>%
  select(id, wave, earnings_median) %>%
  pivot_wider(
    id_cols     = id,
    names_from  = wave,
    values_from = earnings_median,
    names_glue  = "earnings_medianW{wave}"
  )

# Add to England panel
BES_subset_panel_full_v5_England <- BES_subset_panel_full_v5_England %>%
  mutate(id = as.character(id)) %>%
  left_join(ashe_wide %>% mutate(id = as.character(id)), by = "id")

# Final coverage check
BES_subset_panel_full_v5_England %>%
  summarise(
    pct_earnings_W1  = mean(!is.na(earnings_medianW1[wave1   == 1])) * 100,
    pct_earnings_W15 = mean(!is.na(earnings_medianW15[wave15 == 1])) * 100,
    pct_earnings_W25 = mean(!is.na(earnings_medianW25[wave25 == 1])) * 100,
    pct_earnings_W30 = mean(!is.na(earnings_medianW30[wave30 == 1])) * 100
  )


###############################################################################

### Labour market indicators

##Claimant count 

claimant_wide <- read_csv(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Meso_variables/claimant_count_LA.csv",
  skip = 8,
  show_col_types = FALSE
)


claimant_la_panel <- claimant_wide %>%
  rename(la_name = 1) %>%
  pivot_longer(
    cols = -la_name,
    names_to = "month",
    values_to = "claimant_rate_16_64"
  ) %>%
  mutate(
    date = parse_date_time(month, orders = "B Y"),
    year = year(date),
    claimant_rate_16_64 = parse_number(claimant_rate_16_64)
  ) %>%
  filter(!is.na(year)) %>%
  group_by(la_name, year) %>%
  summarise(
    claimant_rate_16_64 = mean(claimant_rate_16_64, na.rm = TRUE),
    .groups = "drop"
  )

claimant_la_panel %>%
  group_by(year) %>%
  summarise(n_la = n_distinct(la_name), .groups = "drop")

# 1. Create LA name-code lookup from GDP panel
la_lookup <- la_lookup_2021 %>%
  distinct(la_code, la_name) %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

# 2. Clean claimant LA names
claimant_la_panel_clean <- claimant_la_panel %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

# 3. Join claimant data to lookup
claimant_la_panel_codes <- claimant_la_panel_clean %>%
  left_join(
    la_lookup %>% select(la_code, la_name_clean),
    by = "la_name_clean"
  )

# 4. Check unmatched names
claimant_unmatched <- claimant_la_panel_codes %>%
  filter(is.na(la_code)) %>%
  distinct(la_name) %>%
  arrange(la_name)

claimant_unmatched

# 5. Final claimant panel — England only
claimant_la_panel <- claimant_la_panel_codes %>%
  filter(
    !is.na(la_code),
    str_starts(la_code, "E")
  ) %>%
  select(
    la_code,
    la_name,
    year,
    claimant_rate_16_64
  )

# Claimant rate: claimants as proportion of residents aged 16–64

# 1. claimant_la_panel must already exist with:
# la_code, year, claimant_rate_16_64

# 2. Identify LA variables in England panel
la_vars <- grep("^oslauaW\\d+$", names(BES_subset_panel_full_v5_England), value = TRUE)
la_vars <- la_vars[order(as.integer(str_extract(la_vars, "\\d+")))]

# 3. Recover wave-year structure from starttime variables
start_vars <- grep("^starttimeW\\d+$", names(BES_subset_panel_full_v5_England), value = TRUE)

wave_years_long <- BES_subset_panel_full_v5_England %>%
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
    fieldwork_year = median(year),
    .groups = "drop"
  ) %>%
  arrange(wave)

# 4. Pivot LA codes to long
england_la_long <- BES_subset_panel_full_v5_England %>%
  select(id, all_of(la_vars)) %>%
  mutate(id = as.character(id)) %>%
  pivot_longer(
    cols = all_of(la_vars),
    names_to = "wave_var",
    values_to = "la_code"
  ) %>%
  mutate(
    wave = as.integer(str_extract(wave_var, "\\d+")),
    la_code = as.character(la_code)
  ) %>%
  filter(!is.na(la_code), la_code != "") %>%
  select(id, wave, la_code)

# 5. Add fieldwork year
england_la_long <- england_la_long %>%
  left_join(wave_years_long, by = "wave")

# 6. Merge claimant rate by LA code + year
england_claimant_long <- england_la_long %>%
  left_join(
    claimant_la_panel %>%
      select(la_code, year, claimant_rate_16_64),
    by = c(
      "la_code" = "la_code",
      "fieldwork_year" = "year"
    )
  )

# 7. Coverage check BEFORE going wide
england_claimant_long %>%
  group_by(wave, fieldwork_year) %>%
  summarise(
    n_with_la = n(),
    pct_with_claimant = round(mean(!is.na(claimant_rate_16_64)) * 100, 1),
    .groups = "drop"
  ) %>%
  print(n = 30)

# 8. Pivot claimant rate back to wide
claimant_wide <- england_claimant_long %>%
  select(id, wave, claimant_rate_16_64) %>%
  pivot_wider(
    id_cols = id,
    names_from = wave,
    values_from = claimant_rate_16_64,
    names_glue = "claimant_rateW{wave}"
  )

# 9. Add claimant rate to England wide panel
BES_subset_panel_full_v5_England <- BES_subset_panel_full_v5_England %>%
  mutate(id = as.character(id)) %>%
  left_join(claimant_wide, by = "id")

# 10. Final wide-format check
BES_subset_panel_full_v5_England %>%
  summarise(
    pct_claimant_W1  = mean(!is.na(claimant_rateW1[wave1 == 1])) * 100,
    pct_claimant_W15 = mean(!is.na(claimant_rateW15[wave15 == 1])) * 100,
    pct_claimant_W25 = mean(!is.na(claimant_rateW25[wave25 == 1])) * 100,
    pct_claimant_W26 = mean(!is.na(claimant_rateW26[wave26 == 1])) * 100,
    pct_claimant_W30 = mean(!is.na(claimant_rateW30[wave30 == 1])) * 100
  )

###############################################################################

# =============================================================================
# MODEL-BASED UNEMPLOYMENT RATE — LOCAL AUTHORITY
# =============================================================================

# 0. Read raw Nomis Excel file
unemp_raw <- read_excel(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Meso_variables/Unemployment_LA.xlsx",
  col_names = FALSE
)

# 1. Define structure of Nomis file
year_row   <- 7
type_row   <- 8
data_start <- 9

# 2. Keep only annual Jan YYYY-Dec YYYY columns
periods_raw <- as.character(unlist(unemp_raw[year_row, ]))
types_raw   <- as.character(unlist(unemp_raw[type_row, ]))

annual_cols <- which(
  str_detect(periods_raw, "^Jan\\s\\d{4}-Dec\\s\\d{4}$") &
    types_raw == "number"
)

# Keep LA name column + annual unemployment-rate columns
keep_cols <- c(1, annual_cols)

unemp_clean <- unemp_raw %>%
  slice(data_start:n()) %>%
  select(all_of(keep_cols))

# 3. Rename columns
annual_periods <- periods_raw[annual_cols]
annual_years <- str_extract(annual_periods, "(?<=Dec\\s)\\d{4}")

names(unemp_clean) <- c(
  "la_name",
  paste0("unemployment_rate_", annual_years)
)

# 4. Reshape to LA-year panel
unemp_la_panel <- unemp_clean %>%
  pivot_longer(
    cols = starts_with("unemployment_rate_"),
    names_to = "year",
    values_to = "unemployment_rate"
  ) %>%
  mutate(
    year = as.numeric(str_extract(year, "\\d{4}")),
    unemployment_rate = parse_number(as.character(unemployment_rate))
  ) %>%
  filter(!is.na(la_name), !is.na(year))

# 5. Create / reuse LA lookup from GDP panel
la_lookup <- la_lookup_2021 %>%
  distinct(la_code, la_name) %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

# 6. Add LA codes
unemp_la_panel_codes <- unemp_la_panel %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  ) %>%
  left_join(
    la_lookup %>% select(la_code, la_name_clean),
    by = "la_name_clean"
  )

# 7. Check unmatched names
unemp_unmatched <- unemp_la_panel_codes %>%
  filter(is.na(la_code)) %>%
  distinct(la_name) %>%
  arrange(la_name)

print(unemp_unmatched, n = 1000)

unemp_la_panel <- unemp_la_panel %>%
  filter(
    !is.na(la_name),
    !str_detect(la_name, "^[-#]"),
    !str_detect(la_name, "Unemployment estimates"),
    !str_detect(la_name, "These figures are missing")
  )

unemp_la_panel_codes <- unemp_la_panel %>%
  mutate(
    la_name_clean = la_name %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  ) %>%
  left_join(
    la_lookup %>% select(la_code, la_name_clean),
    by = "la_name_clean"
  )

# Final unemployment panel, aligned to GDP LA universe
unemp_la_panel_final <- unemp_la_panel_codes %>%
  select(
    la_code,
    la_name,
    year,
    unemployment_rate
  ) %>%
  filter(!is.na(la_code)) 


# =============================================================================
# MERGE UNEMPLOYMENT RATE INTO BES ENGLAND V4 PANEL
# =============================================================================

# 1. Identify LA variables in England panel
la_vars <- grep("^oslauaW\\d+$", names(BES_subset_panel_full_v5_England), value = TRUE)
la_vars <- la_vars[order(as.integer(str_extract(la_vars, "\\d+")))]

# 2. Recover fieldwork year per wave from starttime variables
start_vars <- grep("^starttimeW\\d+$", names(BES_subset_panel_full_v5_England), value = TRUE)

wave_years_long <- BES_subset_panel_full_v5_England %>%
  select(all_of(start_vars)) %>%
  mutate(row_id = row_number()) %>%
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
    fieldwork_year = median(year),
    .groups = "drop"
  ) %>%
  arrange(wave)

# 3. Pivot LA codes to long
england_la_long <- BES_subset_panel_full_v5_England %>%
  select(id, all_of(la_vars)) %>%
  mutate(id = as.character(id)) %>%
  pivot_longer(
    cols = all_of(la_vars),
    names_to = "wave_var",
    values_to = "la_code"
  ) %>%
  mutate(
    wave = as.integer(str_extract(wave_var, "\\d+")),
    la_code = as.character(la_code)
  ) %>%
  filter(!is.na(la_code), la_code != "") %>%
  select(id, wave, la_code)

# 4. Add fieldwork year
england_la_long <- england_la_long %>%
  left_join(wave_years_long, by = "wave")

# 5. Merge unemployment rate by LA code + year
england_unemp_long <- england_la_long %>%
  left_join(
    unemp_la_panel_final %>%
      select(la_code, year, unemployment_rate),
    by = c(
      "la_code" = "la_code",
      "fieldwork_year" = "year"
    )
  )

# 6. Coverage check before going wide
england_unemp_long %>%
  group_by(wave, fieldwork_year) %>%
  summarise(
    n_with_la = n(),
    pct_with_unemp = round(mean(!is.na(unemployment_rate)) * 100, 1),
    .groups = "drop"
  ) %>%
  print(n = 30)

# 7. Pivot unemployment back to wide
unemp_wide <- england_unemp_long %>%
  select(id, wave, unemployment_rate) %>%
  pivot_wider(
    id_cols = id,
    names_from = wave,
    values_from = unemployment_rate,
    names_glue = "unemployment_rateW{wave}"
  )

# 8. Add unemployment rate to England wide panel
BES_subset_panel_full_v5_England <- BES_subset_panel_full_v5_England %>%
  mutate(id = as.character(id)) %>%
  left_join(unemp_wide, by = "id")

# 9. Final wide-format check
BES_subset_panel_full_v5_England %>%
  summarise(
    pct_unemp_W1  = mean(!is.na(unemployment_rateW1[wave1 == 1])) * 100,
    pct_unemp_W15 = mean(!is.na(unemployment_rateW15[wave15 == 1])) * 100,
    pct_unemp_W25 = mean(!is.na(unemployment_rateW25[wave25 == 1])) * 100,
    pct_unemp_W30 = mean(!is.na(unemployment_rateW30[wave30 == 1])) * 100
  )

saveRDS(
  BES_subset_panel_full_v5_England,
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v5_England.rds"
)

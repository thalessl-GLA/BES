# claimant count
#https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/claimantcountandvacanciesdataset
#Claimant Count : K02000001 UK : People : SA : Thousands
#BCJD

# CPI
#https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindices

#CPIH ANNUAL RATE 00: ALL ITEMS 2015=100
#L55O
  
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Open file

BES_subset_panel_full_v1 <- readRDS("BES_subset_panel_full_v1.rds")

# 1) Identify all starttime wave variables
start_vars <- grep("^starttimeW\\d+$", names(BES_subset_panel_full_v1), value = TRUE)

# 2) Create the wave -> fieldwork_year lookup (median interview year per wave)
wave_year_lookup <- BES_subset_panel_full_v1 %>%
  select(all_of(start_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "wave_var",
    values_to = "starttime"
  ) %>%
  mutate(
    wave = as.integer(str_extract(wave_var, "\\d+")),
    year = year(ymd_hms(starttime))
  ) %>%
  filter(!is.na(wave), !is.na(year)) %>%
  group_by(wave) %>%
  summarise(fieldwork_year = median(year), .groups = "drop") %>%
  arrange(wave)

# wave_year_lookup is your TEMP lookup table for macro merges
wave_year_lookup

# GDP per capita level
#https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/ihxw/

# 1) Read entire sheet with no headers
raw <- read_excel("/Users/t.souza-lima.1/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Macro_variables/GDP.xls", col_names = FALSE)

# 2) Detect start row: first occurrence of "YYYY Q#"
start_row <- which(str_detect(str_squish(as.character(raw$...1)), "^\\d{4}\\s*Q[1-4]$"))[1]
if (is.na(start_row)) stop("Couldn't find the first 'YYYY Q#' row in column A (...1).")

# 3) Slice from start row and parse
gdp_qtr <- raw %>%
  slice(start_row:n()) %>%
  transmute(
    period  = str_squish(as.character(...1)),
    gdp_pc  = parse_number(as.character(...2))
  ) %>%
  filter(str_detect(period, "^\\d{4}\\s*Q[1-4]$")) %>%
  mutate(
    year    = as.integer(str_extract(period, "^\\d{4}")),
    quarter = as.integer(str_extract(period, "(?<=Q)\\d"))
  ) %>%
  arrange(year, quarter)

# 4) Keep recent years (adjust cutoff)
gdp_qtr_recent <- gdp_qtr %>% filter(year >= 2013)

# 5) Collapse quarterly GDP to annual mean
gdp_year <- gdp_qtr_recent %>%
  group_by(year) %>%
  summarise(
    gdp_pc_year = mean(gdp_pc, na.rm = TRUE),
    n_quarters = n(),
    .groups = "drop"
  ) %>%
  arrange(year)

# 6) Map GDP to waves via fieldwork_year, then pivot to wide (gdp_pcW#)
gdp_by_wave_wide <- wave_year_lookup %>%
  left_join(gdp_year, by = c("fieldwork_year" = "year")) %>%
  mutate(wave_var = paste0("gdp_pcW", wave)) %>%
  select(wave_var, gdp_pc_year) %>%
  pivot_wider(names_from = wave_var, values_from = gdp_pc_year)

# 7) Append GDP wave columns to every respondent row (no merge keys stored)
BES_subset_panel_full_v2 <- BES_subset_panel_full_v1 %>%
  bind_cols(gdp_by_wave_wide[rep(1, nrow(BES_subset_panel_full_v1)), ])

# 8) Sanity Checks

# Reference annual GDP from the original quarterly file
gdp_year_check <- gdp_qtr_recent %>%
  group_by(year) %>%
  summarise(
    gdp_pc_year = mean(gdp_pc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

# Extract wave-level GDP from BES
gdp_bes_check <- wave_year_lookup %>%
  mutate(
    gdp_pc_from_bes = sapply(
      wave,
      function(w) BES_subset_panel_full_v2[[paste0("gdp_pcW", w)]][1]
    )
  )

gdp_compare <- gdp_bes_check %>%
  left_join(gdp_year_check, by = c("fieldwork_year" = "year")) %>%
  mutate(
    diff = gdp_pc_from_bes - gdp_pc_year
  )

print(gdp_compare, n = 1000)


# Unemployment rate 
#https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms

# 1) Read entire sheet with no headers
raw_1 <- read_excel("/Users/t.souza-lima.1/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/Macro_variables/unemployment_rate.xls", col_names = FALSE)

# 2) Detect start row: first occurrence of "YYYY Q#"
start_row <- which(str_detect(str_squish(as.character(raw_1$...1)), "^\\d{4}\\s*Q[1-4]$"))[1]
if (is.na(start_row)) stop("Couldn't find the first 'YYYY Q#' row in column A (...1).")

# 3) Slice from start row and parse
unempl_rate_qtr <- raw_1 %>%
  slice(start_row:n()) %>%
  transmute(
    period  = str_squish(as.character(...1)),
    unempl_rate  = parse_number(as.character(...2))
  ) %>%
  filter(str_detect(period, "^\\d{4}\\s*Q[1-4]$")) %>%
  mutate(
    year    = as.integer(str_extract(period, "^\\d{4}")),
    quarter = as.integer(str_extract(period, "(?<=Q)\\d"))
  ) %>%
  arrange(year, quarter)

# 4) Keep recent years (adjust cutoff)
unempl_rate_recent <- unempl_rate_qtr %>% filter(year >= 2013)

# 5) Collapse quarterly unemployment to annual mean
unempl_rate_year <- unempl_rate_recent %>%
  group_by(year) %>%
  summarise(
    unempl_rate_year = mean(unempl_rate, na.rm = TRUE),
    n_quarters = n(),
    .groups = "drop"
  ) %>%
  arrange(year)

# 6) Map unemployment to waves via fieldwork_year, then pivot to wide (gdp_pcW#)
unempl_rate_by_wave_wide <- wave_year_lookup %>%
  left_join(unempl_rate_year, by = c("fieldwork_year" = "year")) %>%
  mutate(wave_var = paste0("unempl_rateW", wave)) %>%
  select(wave_var, unempl_rate_year) %>%
  pivot_wider(names_from = wave_var, values_from = unempl_rate_year)

# 7) Append unemployment wave columns to every respondent row (no merge keys stored)
BES_subset_panel_full_v2 <- BES_subset_panel_full_v2 %>%
  bind_cols(unempl_rate_by_wave_wide[rep(1, nrow(BES_subset_panel_full_v2)), ])

# 8) Sanity Checks

# Reference annual unemployment from the original quarterly file
unemp_year_check <- unempl_rate_recent %>%
  group_by(year) %>%
  summarise(
    unemp_rate_year = mean(unempl_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

# Extract wave-level unemployment from BES
unemp_bes_check <- wave_year_lookup %>%
  mutate(
    unemp_rate_from_bes = sapply(
      wave,
      function(w) BES_subset_panel_full_v2[[paste0("unempl_rateW", w)]][1]
    )
  )

unemp_compare <- unemp_bes_check %>%
  left_join(unemp_year_check, by = c("fieldwork_year" = "year")) %>%
  mutate(
    diff = unemp_rate_from_bes - unemp_rate_year
  )

print(unemp_compare, n = 1000)





  

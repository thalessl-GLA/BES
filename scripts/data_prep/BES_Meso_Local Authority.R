# =============================================================================
# STEP 1: EXTRACT LA CODES FROM BES AND ADD TO SUBSET PANEL
# =============================================================================

# Load raw BES data (assuming it's in .sav format and has the expected structure)
BES <- read_sav("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES2024_W30_Panel_v30.0.sav")

# Extract LA variable names from BES
la_vars <- grep("^oslauaW\\d+$", names(BES), value = TRUE)
cat("LA variables found in BES:", length(la_vars), "\n")

# Extract id + all LA variables from BES
bes_la <- BES %>%
  select(id, all_of(la_vars)) %>%
  mutate(id = as.character(id))

# Ensure id is character in subset panel
BES_subset_panel_full_v3_England <- BES_subset_panel_full_v3_England %>%
  mutate(id = as.character(id))

# Join
BES_subset_panel_full_v3_England <- BES_subset_panel_full_v3_England %>%
  left_join(bes_la, by = "id")

# Verify
cat("LA vars in subset panel:", 
    length(grep("^oslauaW\\d+$", names(BES_subset_panel_full_v3_England))), "\n")

# Check coverage across a few waves
BES_subset_panel_full_v3_England %>%
  summarise(across(
    all_of(la_vars),
    ~ round(mean(!is.na(.)) * 100, 1),
    .names = "pct_{.col}"
  )) %>%
  pivot_longer(everything(), names_to = "wave", values_to = "pct_non_missing") %>%
  print(n = 30)

# Spot check: what do the codes look like?
BES_subset_panel_full_v3_England %>%
  select(id, oslauaW1, oslauaW15, oslauaW25) %>%
  filter(!is.na(oslauaW1)) %>%
  head(10)

# =============================================================================
# STEP 2: ATTACH FIELDWORK YEAR PER WAVE (already built as wave_years_long)
# =============================================================================

# Confirm wave_years_long exists and looks right
wave_years_long %>% print(n = 30)

# =============================================================================
# STEP 3: PIVOT SUBSET PANEL TO LONG, MERGE GDP, PIVOT BACK TO WIDE
# This is the cleanest approach for a wide-format panel
# =============================================================================

# 3a. Pivot LA codes and wave info to long from subset panel

la_code_vars <- grep("^oslauaW\\d+$", names(BES_subset_panel_full_v3_England), value = TRUE)

subset_la_long <- BES_subset_panel_full_v3_England %>%
  select(id, all_of(la_code_vars)) %>%
  pivot_longer(
    cols      = all_of(la_code_vars),
    names_to  = "wave_var",
    values_to = "la_code"
  ) %>%
  mutate(
    wave    = as.integer(str_extract(wave_var, "\\d+")),
    la_code = as.character(la_code)
  ) %>%
  filter(!is.na(la_code), la_code != "") %>%
  left_join(wave_years_long, by = "wave")

# 3b. Merge GDP per capita
subset_gdp_long <- subset_la_long %>%
  left_join(
    gdp_la_panel %>% select(la_code, year, gdp_percap),
    by = c("la_code" = "la_code", "fieldwork_year" = "year")
  )

# Coverage check before pivoting wide
subset_gdp_long %>%
  group_by(wave, fieldwork_year) %>%
  summarise(
    n_obs          = n(),
    pct_gdp_percap = round(mean(!is.na(gdp_percap)) * 100, 1),
    .groups        = "drop"
  ) %>%
  print(n = 30)

# Keep only the correct GDP value for each id-wave row
gdp_current_wave <- subset_gdp_long %>%
  select(id, wave, la_code, fieldwork_year, gdp_percap)

# Merge directly into long England panel
BES_subset_panel_full_v3_England <- BES_subset_panel_full_v3_England %>%
  mutate(
    id = as.character(id),
    wave = as.integer(wave)
  ) %>%
  left_join(gdp_current_wave, by = c("id", "wave"))

# =============================================================================
# STEP 4: VERIFY
# =============================================================================

# Check a few wave columns exist and have sensible values
BES_subset_panel_full_v3_England %>%
  summarise(
    n_obs               = n(),
    pct_gdp_W1          = round(mean(!is.na(gdp_percapW1))  * 100, 1),
    pct_gdp_W15         = round(mean(!is.na(gdp_percapW15)) * 100, 1),
    pct_gdp_W25         = round(mean(!is.na(gdp_percapW25)) * 100, 1),
    pct_gdp_W26         = round(mean(!is.na(gdp_percapW26)) * 100, 1),  # expect ~0
    mean_gdp_W15        = round(mean(gdp_percapW15, na.rm = TRUE)),
    median_gdp_W15      = round(median(gdp_percapW15, na.rm = TRUE))
  )

# Wave 26+ should be ~0% covered (2024-2025, beyond ONS data)
# Wave 1-25 should be >80% covered depending on LA code availability in BES

# =============================================================================
# STEP 5: SAVE
# =============================================================================

saveRDS(BES_subset_panel_full_v3_England, file = here("BES_subset_panel_full_v3_England_with_gdp.rds"))
library(dplyr)
library(stringr)

readRDS("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES_subset_panel_full_v3.rds")

# 1) Build the vector of country wave columns that actually exist in the data
country_vars <- names(BES_subset_panel_full_v3)[
  str_detect(names(BES_subset_panel_full_v3), "^countryW\\d+$")
]

# (optional but recommended) sort them in true wave order: W1, W2, ..., W30
wave_num <- as.integer(str_extract(country_vars, "\\d+$"))
country_vars <- country_vars[order(wave_num)]

# sanity check
country_vars
length(country_vars)

# 2) Create "country_first" = first non-missing country observed for each id across waves
BES_subset_panel_full_v3_engl <- BES_subset_panel_full_v3 |>
  mutate(
    country_first = apply(
      pick(all_of(country_vars)),
      1,
      function(row) {
        row_num <- as.numeric(row)
        row_num <- row_num[!is.na(row_num)]
        if (length(row_num) == 0) NA_real_ else row_num[1]
      }
    )
  ) |>
  filter(country_first == 1)

# Add local authority variable to the subsample

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
BES_subset_panel_full_v4_England <- BES_subset_panel_full_v3_England %>%
  left_join(bes_la, by = "id")

# Verify number of obs
library(dplyr)
library(purrr)

la_vars <- grep("^oslauaW\\d+$", names(BES_subset_panel_full_v4_England), value = TRUE)

la_merge_check <- map_dfr(la_vars, function(v) {
  
  subset_vals <- BES_subset_panel_full_v4_England[[v]]
  original_vals <- BES[[v]][
    match(BES_subset_panel_full_v4_England$id, BES$id)
  ]
  
  tibble(
    variable = v,
    n_subset_nonmissing = sum(!is.na(subset_vals)),
    n_original_nonmissing_for_same_ids = sum(!is.na(original_vals)),
    n_exact_matches = sum(
      subset_vals == original_vals |
        (is.na(subset_vals) & is.na(original_vals)),
      na.rm = TRUE
    ),
    pct_exact_match = mean(
      subset_vals == original_vals |
        (is.na(subset_vals) & is.na(original_vals)),
      na.rm = TRUE
    ) * 100
  )
})

la_merge_check

# Verify values 
la_value_check <- map_dfr(la_vars, function(v) {
  
  subset_vals <- as.character(BES_subset_panel_full_v4_England[[v]])
  original_vals <- as.character(
    BES[[v]][match(BES_subset_panel_full_v4_England$id, BES$id)]
  )
  
  same_value <- (subset_vals == original_vals) |
    (is.na(subset_vals) & is.na(original_vals))
  
  tibble(
    variable = v,
    n_rows_checked = length(same_value),
    n_same_values = sum(same_value, na.rm = TRUE),
    n_different_values = sum(!same_value, na.rm = TRUE),
    pct_same_values = round(mean(same_value, na.rm = TRUE) * 100, 2)
  )
})

la_value_check


saveRDS(
  BES_subset_panel_full_v4_England,
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v4_England.rds"
)

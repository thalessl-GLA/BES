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

saveRDS(
  BES_subset_panel_full_v3_engl,
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v3_England.rds"
)

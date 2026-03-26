# ============================================================
# Create BES_subset_panel_full_v3 by adding identity measures
# (englishness / scottishness / welshness / europeanness)
# ============================================================

library(haven)
library(dplyr)
library(stringr)

## Open full dataset
BES <- read_sav("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES2024_W30_Panel_v30.0.sav")

## Open latest version of the subset data with meso/macro variables

BES_subset_panel_full_v2 <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v2.rds")

library(dplyr)
library(stringr)
library(haven)

# ============================================================
# 1) Define the nationalism variables to add (across waves)
# ============================================================

nat_stems <- c("englishness", "scottishness", "welshness", "europeanness")

# Pull all W# vars for those stems that actually exist
nat_vars <- unlist(lapply(nat_stems, function(s){
  grep(paste0("^", s, "W\\d+$"), names(BES), value = TRUE)
}), use.names = FALSE)

nat_vars <- unique(nat_vars)

# Country across waves (needed for scot/wales “show if country==2/3” logic)
country_vars <- grep("^countryW\\d+$", names(BES), value = TRUE)
country_vars <- unique(country_vars)

# Wave indicators (Respondent took wave X) – important for sanity checks
wave_indicators <- intersect(paste0("wave", 1:30), names(BES))

# ============================================================
# 2) Build BES_nat_block (memory-safe): v2 ids x (wave + nat + country)
# ============================================================

ids_keep <- unique(BES_subset_panel_full_v2$id)

# Only keep columns that exist
cols_keep <- intersect(c("id", wave_indicators, nat_vars, country_vars), names(BES))

# Fast row selection (no dplyr copies)
idx <- match(ids_keep, BES$id)
idx <- idx[!is.na(idx)]

BES_nat_block <- BES[idx, cols_keep, drop = FALSE]

# Zap labels only where needed (cheaper than mutate(across()))
is_lab <- vapply(BES_nat_block, inherits, logical(1), what = "haven_labelled")
BES_nat_block[is_lab] <- lapply(BES_nat_block[is_lab], haven::zap_labels)

# Ensure id is numeric (consistent merges)
BES_nat_block$id <- as.numeric(BES_nat_block$id)

# ============================================================
# 3) (Optional) Apply "show if country==2/3" restriction
#     - scottishness only meaningful when country==2
#     - welshness only meaningful when country==3
#   We do this wave-by-wave using countryW#
# ============================================================

# helper to get wave number from variable name
get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

# Scottishness restriction
scot_vars <- grep("^scottishnessW\\d+$", names(BES_nat_block), value = TRUE)
for(v in scot_vars){
  w <- get_wave_num(v)
  cvar <- paste0("countryW", w)
  if(cvar %in% names(BES_nat_block)){
    BES_nat_block[[v]] <- ifelse(BES_nat_block[[cvar]] == 2, BES_nat_block[[v]], NA)
  }
}

# Welshness restriction
wales_vars <- grep("^welshnessW\\d+$", names(BES_nat_block), value = TRUE)
for(v in wales_vars){
  w <- get_wave_num(v)
  cvar <- paste0("countryW", w)
  if(cvar %in% names(BES_nat_block)){
    BES_nat_block[[v]] <- ifelse(BES_nat_block[[cvar]] == 3, BES_nat_block[[v]], NA)
  }
}

# ============================================================
# 4) ID sanity checks (recommended)
# ============================================================

cat("Unique ids in v2:", length(unique(BES_subset_panel_full_v2$id)), "\n")
cat("Unique ids in nat_block:", length(unique(BES_nat_block$id)), "\n")

missing_in_block <- setdiff(unique(BES_subset_panel_full_v2$id), unique(BES_nat_block$id))
cat("IDs in v2 missing in nat_block:", length(missing_in_block), "\n")

# ============================================================
# 5) COUNTRY sanity checks (your step 5, corrected)
#     - Do NOT assume wave1/starttimeW1 are present in nat_block
#     - Use wave1 indicator if present
# ============================================================

if(length(country_vars) > 0){
  
  # (A) Compare against v2 if v2 already has countryW#
  country_vars_v2 <- intersect(country_vars, names(BES_subset_panel_full_v2))
  
  if(length(country_vars_v2) > 0){
    tmp <- BES_subset_panel_full_v2 %>%
      select(id, all_of(country_vars_v2)) %>%
      left_join(
        BES_nat_block %>% select(id, all_of(country_vars_v2)),
        by = "id",
        suffix = c("_v2","_bes")
      )
    
    for(cv in country_vars_v2){
      v2c  <- paste0(cv, "_v2")
      besc <- paste0(cv, "_bes")
      mismatch_n <- sum(!is.na(tmp[[v2c]]) & !is.na(tmp[[besc]]) & tmp[[v2c]] != tmp[[besc]])
      cat("Country mismatches for", cv, ":", mismatch_n, "\n")
    }
  } else {
    cat("Note: v2 has no countryW# vars to cross-check; will add via BES_nat_block.\n")
  }
  
  # (B) Distribution check for a wave where people actually took the wave (if wave indicator exists)
  # pick the first available countryW# in nat_block
  avail_country <- intersect(country_vars, names(BES_nat_block))
  if(length(avail_country) > 0){
    pick_country <- avail_country[1]              # e.g., "countryW1"
    pick_w <- get_wave_num(pick_country)          # 1
    
    # if wave indicator exists, restrict to respondents who took that wave
    wave_flag <- paste0("wave", pick_w)
    
    cat("\nCountry distribution for", pick_country, ":\n")
    if(wave_flag %in% names(BES_nat_block)){
      print(table(BES_nat_block[[pick_country]][BES_nat_block[[wave_flag]] == 1], useNA = "ifany"))
      cat("(restricted to wave==1 respondents)\n")
    } else {
      print(table(BES_nat_block[[pick_country]], useNA = "ifany"))
      cat("(no wave indicator found; showing full block)\n")
    }
  }
}

# ============================================================
# 6) Merge into v2 -> v3 (add new nationalism + country vars)
# ============================================================

# Make sure both IDs are the same type (use character)
BES_subset_panel_full_v2 <- BES_subset_panel_full_v2 %>%
  mutate(id = as.character(id))

BES_nat_block <- BES_nat_block %>%
  mutate(id = as.character(id))

# Only bring in the NEW vars you want to add (avoid duplicating columns already in v2)
vars_to_add <- setdiff(names(BES_nat_block), names(BES_subset_panel_full_v2))
vars_to_add <- setdiff(vars_to_add, "id")

BES_subset_panel_full_v3 <- BES_subset_panel_full_v2 %>%
  left_join(
    BES_nat_block %>% select(id, all_of(vars_to_add)),
    by = "id"
  )

cat("\nDone. v3 columns added:", length(vars_to_add), "\n")

# ============================================================
# 7) Checks
# ============================================================

# IDs in v3 but not in BES (should be zero)
setdiff(BES_subset_panel_full_v3$id, as.character(BES$id)) |> length()

# IDs in BES but not in v3 (expected: many)
setdiff(as.character(BES$id), BES_subset_panel_full_v3$id) |> length()

added_vars <- setdiff(
  names(BES_subset_panel_full_v3),
  names(BES_subset_panel_full_v2)
)

length(added_vars)
added_vars

check_var <- "englishnessW1"

tmp <- BES_subset_panel_full_v3 %>%
  select(id, !!check_var) %>%
  left_join(
    BES %>%
      select(id, !!check_var) %>%
      mutate(id = as.character(id)),
    by = "id",
    suffix = c("_v3", "_bes")
  )

# Count mismatches (ignore NAs)
sum(
  !is.na(tmp[[paste0(check_var, "_v3")]]) &
    !is.na(tmp[[paste0(check_var, "_bes")]]) &
    tmp[[paste0(check_var, "_v3")]] != tmp[[paste0(check_var, "_bes")]]
)

new_nat_vars <- grep(
  "^(englishness|scottishness|welshness|europeanness|country)W\\d+$",
  names(BES_subset_panel_full_v3),
  value = TRUE
)

mismatch_report <- map_df(new_nat_vars, function(v){
  tmp <- BES_subset_panel_full_v3 %>%
    select(id, !!v) %>%
    left_join(
      BES %>%
        select(id, !!v) %>%
        mutate(id = as.character(id)),
      by = "id",
      suffix = c("_v3", "_bes")
    )
  
  tibble(
    variable = v,
    mismatches = sum(
      !is.na(tmp[[paste0(v, "_v3")]]) &
        !is.na(tmp[[paste0(v, "_bes")]]) &
        tmp[[paste0(v, "_v3")]] != tmp[[paste0(v, "_bes")]]
    )
  )
})

print(mismatch_report, n = 5000)

# Saving

saveRDS(
  BES_subset_panel_full_v3,
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES_subset_panel_full_v3.rds"
)



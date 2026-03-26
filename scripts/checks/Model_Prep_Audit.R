# =========================================================
# MODEL PREP AUDIT — BES PANEL (wide-by-wave -> long)
# Uses correct variable bases:
#   DVs: immigSelfW#, immigEconW#, immigCulturalW#
#   Key IV: britishnessW#
#   Time: starttimeW#
# Also inventories demographic controls (profile/top-up) reliably.
# =========================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)

BES_subset_panel_full_v1 <- readRDS("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v1.rds")

df <- BES_subset_panel_full_v1

rm(BES_subset_panel_full_v1)
gc()

# -----------------------------
# 0) CORE BASES (correct names)
# -----------------------------
DV_MAIN   <- "immigSelf"
DV_ALT1   <- "immigEcon"
DV_ALT2   <- "immigCultural"
BRIT_BASE <- "britishness"
TIME_BASE <- "starttime"

# -----------------------------
# 1) Helpers: waves + availability
# -----------------------------
get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(df, base){
  vars <- names(df) |> keep(~ str_detect(.x, paste0("^", base, "W\\d+$")))
  sort(unique(map_int(vars, get_wave_num)))
}

availability_table <- function(df, bases){
  map_dfr(bases, \(b){
    w <- waves_available(df, b)
    tibble(
      base = b,
      n_waves = length(w),
      first_wave = ifelse(length(w) > 0, min(w), NA_integer_),
      last_wave  = ifelse(length(w) > 0, max(w), NA_integer_),
      waves = paste(w, collapse = ", ")
    )
  })
}

avail <- availability_table(df, c(DV_MAIN, DV_ALT1, DV_ALT2, BRIT_BASE, TIME_BASE))
avail

# -----------------------------
# 2) Inventory potential demographic controls (BES: p_ profile + top-up)
#    BES docs: many demographics are "profile variables" with prefix p_
#    and should not be assumed measured exactly at the wave date.
# -----------------------------

# A) Profile variables (prefix p_)
profile_vars <- names(df) |> keep(~ str_detect(.x, "^p_")) |> sort()
profile_vars[1:200]

# B) Top-up / multiwave demog-style variables (not always p_, sometimes wave-tagged)
#    This is a broad net to help you pick controls you actually have.

demog_candidates <- names(df) |>
  keep(~ str_detect(.x,
                    regex("^(p_)?(age|gender|sex|female|male|ethnic|race|relig|denom|marit|educ|qual|degree|income|home|tenure|own|rent|children|hhsize|union|occupation|class|born|country|region)",
                          ignore_case = TRUE))) |>
  sort()


demog_candidates[1:200]

# -----------------------------
# 3) Build the LONG PANEL for the main DV (immigSelf) + britishness + time
#    Intersect waves across DV, britishness, starttime.
# -----------------------------
make_long_core <- function(df, dv_base, brit_base = "britishness", time_base = "starttime"){
  
  w_dv   <- waves_available(df, dv_base)
  w_brit <- waves_available(df, brit_base)
  w_time <- waves_available(df, time_base)
  
  waves <- Reduce(intersect, list(w_dv, w_brit, w_time))
  if(length(waves) < 3) stop("Too few overlapping waves between DV, britishness, and starttime.")
  
  out <- df |>
    select(
      id,
      all_of(paste0(dv_base, "W", waves)),
      all_of(paste0(brit_base, "W", waves)),
      all_of(paste0(time_base, "W", waves))
    ) |>
    pivot_longer(
      cols = -id,
      names_to = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) |>
    mutate(
      wave = as.integer(wave),
      
      starttime_chr = as.character(.data[[time_base]]),
      
      starttime_parsed = suppressWarnings(ymd_hms(starttime_chr, quiet = TRUE)),
      starttime_parsed = dplyr::coalesce(
        starttime_parsed,
        suppressWarnings(ymd(starttime_chr, quiet = TRUE))
      ),
      
      year = lubridate::year(starttime_parsed)
    ) |>
    rename(
      dv = all_of(dv_base),
      brit = all_of(brit_base)
    ) |>
    filter(!is.na(dv), !is.na(brit), !is.na(year))
  
  out
}

panel_self <- make_long_core(df, DV_MAIN, BRIT_BASE, TIME_BASE)

# quick sanity
panel_self |> dplyr::summarise(
  n_obs = n(),
  n_ids = n_distinct(id),
  min_wave = min(wave),
  max_wave = max(wave),
  min_year = min(year, na.rm = TRUE),
  max_year = max(year, na.rm = TRUE)
)

# -----------------------------
# 4) Unbalancedness diagnostics (key for FE feasibility)
# -----------------------------
waves_per_id <- panel_self |>
  group_by(id) |>
  summarise(
    n_waves = n(),
    first_wave = min(wave),
    last_wave = max(wave),
    .groups = "drop"
  )

panel_diag <- waves_per_id |>
  summarise(
    n_ids = n(),
    mean_waves = mean(n_waves),
    p10_waves = quantile(n_waves, 0.10),
    p50_waves = quantile(n_waves, 0.50),
    p90_waves = quantile(n_waves, 0.90),
    min_waves = min(n_waves),
    max_waves = max(n_waves)
  )

panel_diag

wave_cov <- panel_self |>
  group_by(wave) |>
  summarise(
    n_obs = n(),
    n_ids = n_distinct(id),
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(wave)

wave_cov

# -----------------------------
# 5) Within-person variation checks (FE identification)
# -----------------------------
within_var <- panel_self |>
  group_by(id) |>
  summarise(
    sd_dv = sd(dv, na.rm = TRUE),
    sd_brit = sd(brit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  summarise(
    pct_dv_changes = mean(sd_dv > 0, na.rm = TRUE),
    pct_brit_changes = mean(sd_brit > 0, na.rm = TRUE)
  )

within_var

# -----------------------------
# 6) DV + Britishness distributions (bounds, central tendency)
# -----------------------------
dv_summary <- panel_self |>
  summarise(
    dv_min = min(dv, na.rm = TRUE),
    dv_max = max(dv, na.rm = TRUE),
    dv_mean = mean(dv, na.rm = TRUE),
    dv_sd = sd(dv, na.rm = TRUE)
  )

brit_summary <- panel_self |>
  summarise(
    brit_min = min(brit, na.rm = TRUE),
    brit_max = max(brit, na.rm = TRUE),
    brit_mean = mean(brit, na.rm = TRUE),
    brit_sd = sd(brit, na.rm = TRUE)
  )

dv_summary
brit_summary

# -----------------------------
# 7) OPTIONAL: build long panels for alt DVs (econ/cultural) using same function
# -----------------------------
panel_econ <- make_long_core(df, DV_ALT1, BRIT_BASE, TIME_BASE)
panel_cult <- make_long_core(df, DV_ALT2, BRIT_BASE, TIME_BASE)

alt_diag <- tibble(
  dv = c("immigSelf", "immigEcon", "immigCultural"),
  n_obs = c(nrow(panel_self), nrow(panel_econ), nrow(panel_cult)),
  n_ids = c(n_distinct(panel_self$id), n_distinct(panel_econ$id), n_distinct(panel_cult$id)),
  waves = c(
    paste(sort(unique(panel_self$wave)), collapse = ", "),
    paste(sort(unique(panel_econ$wave)), collapse = ", "),
    paste(sort(unique(panel_cult$wave)), collapse = ", ")
  )
)

alt_diag

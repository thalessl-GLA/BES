library(dplyr)
library(tidyr)
library(purrr)
library(haven)
library(stringr)
library(tibble)
library(openxlsx)
library(lubridate)


# Open raw data
BES <- read_sav("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES2024_W30_Panel_v30.0.sav")


# --- 1) Extract wave numbers from the variable names you actually have
waves_available <- immigself_vars %>%
  str_extract("(?<=W)\\d+$") %>%
  as.integer() %>%
  sort()

# --- 2) Build the complete adjacent-wave overlap table
overlap_table <- map_dfr(seq_along(waves_available)[-length(waves_available)], function(i) {
  
  w  <- waves_available[i]
  w1 <- waves_available[i + 1]
  
  v_w  <- paste0("immigSelfW", w)
  v_w1 <- paste0("immigSelfW", w1)
  
  present_w  <- presence_df[[v_w]]
  present_w1 <- presence_df[[v_w1]]
  
  n_w   <- sum(present_w,  na.rm = TRUE)
  n_w1  <- sum(present_w1, na.rm = TRUE)
  n_ov  <- sum(present_w & present_w1, na.rm = TRUE)
  
  # Extras: turnover
  n_drop <- sum(present_w & !present_w1, na.rm = TRUE)  # in w but not in w1
  n_new  <- sum(!present_w & present_w1, na.rm = TRUE)  # in w1 but not in w
  
  tibble(
    wave_t = w,
    wave_t1 = w1,
    var_t = v_w,
    var_t1 = v_w1,
    n_wave_t = n_w,
    n_wave_t1 = n_w1,
    n_overlap = n_ov,
    share_overlap_t  = ifelse(n_w  > 0, n_ov / n_w,  NA_real_),
    share_overlap_t1 = ifelse(n_w1 > 0, n_ov / n_w1, NA_real_),
    n_drop_t_to_t1 = n_drop,
    share_drop_t_to_t1 = ifelse(n_w > 0, n_drop / n_w, NA_real_),
    n_new_t1_not_t = n_new,
    share_new_t1_not_t = ifelse(n_w1 > 0, n_new / n_w1, NA_real_)
  )
})

overlap_table

#Sanity check
overlap_table %>%
  mutate(
    check_t  = (n_overlap + n_drop_t_to_t1) == n_wave_t,
    check_t1 = (n_overlap + n_new_t1_not_t) == n_wave_t1
  ) %>%
  summarise(all_ok = all(check_t & check_t1))


#Add calendar data
start_vars <- names(BES)[str_detect(names(BES), "^starttimeW\\d+$")]
end_vars   <- names(BES)[str_detect(names(BES), "^endtimeW\\d+$")]

# keep only waves where we have BOTH start and end
waves_start <- as.integer(str_extract(start_vars, "\\d+"))
waves_end   <- as.integer(str_extract(end_vars, "\\d+"))
waves_both  <- sort(intersect(waves_start, waves_end))

start_vars <- paste0("starttimeW", waves_both)
end_vars   <- paste0("endtimeW",   waves_both)

wave_calendar <- BES %>%
  select(all_of(start_vars), all_of(end_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "var",
    values_to = "datetime_chr"
  ) %>%
  mutate(
    wave = as.integer(str_extract(var, "\\d+")),
    which = if_else(str_detect(var, "^starttime"), "start", "end"),
    # parse strings like "2014-03-03 19:59:22 UTC"
    datetime = ymd_hms(datetime_chr, tz = "UTC", quiet = TRUE)
  ) %>%
  filter(!is.na(datetime)) %>%
  summarise(
    fieldwork_start = min(datetime),
    fieldwork_end   = max(datetime),
    fieldwork_median = median(datetime),
    .by = c(wave, which)
  ) %>%
  pivot_wider(
    names_from = which,
    values_from = c(fieldwork_start, fieldwork_end, fieldwork_median)
  ) %>%
  # create nice year-month labels (median is usually what you want to report)
  mutate(
    year  = year(fieldwork_median_start),  # median of start times
    month = month(fieldwork_median_start),
    year_month = paste0(year, "-", sprintf("%02d", month)),
    # also provide a readable range if you want
    fieldwork_range = paste0(
      format(fieldwork_start_start, "%Y-%m-%d"),
      " to ",
      format(fieldwork_end_end, "%Y-%m-%d")
    )
  ) %>%
  arrange(wave)

print(wave_calendar)

# Merge wave dates into your overlap table (wave_t and wave_t1)
overlap_with_dates <- overlap_table %>%
  left_join(
    wave_calendar %>% select(wave, year_month, fieldwork_range),
    by = c("wave_t" = "wave")
  ) %>%
  rename(year_month_t = year_month, fieldwork_range_t = fieldwork_range) %>%
  left_join(
    wave_calendar %>% select(wave, year_month, fieldwork_range),
    by = c("wave_t1" = "wave")
  ) %>%
  rename(year_month_t1 = year_month, fieldwork_range_t1 = fieldwork_range) %>%
  relocate(year_month_t, year_month_t1, fieldwork_range_t, fieldwork_range_t1,
           .after = wave_t1)


#Save

write.xlsx(
  overlap_with_dates,
  file = "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/immigSelf_wave_overlap.xlsx"
)


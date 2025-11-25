library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(writexl)

## Open data
BES <- read_sav("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES2024_W30_Panel_v30.0.sav")

# Summary tables

# 1. Identify wave indicator columns
wave_cols <- paste0("wave", 1:30)

# 2. Extract IDs present in each wave
ids_by_wave <- lapply(wave_cols, function(w) {
  BES$id[BES[[w]] == 1]
})
names(ids_by_wave) <- wave_cols

# 3. Build basic wave summary: N per wave + overlap
wave_summary <- tibble(
  wave = 1:30,
  n_ids = sapply(ids_by_wave, function(x) n_distinct(x)),
  overlap_prev = c(
    NA_integer_,
    sapply(2:30, function(i) {
      length(intersect(ids_by_wave[[i]], ids_by_wave[[i - 1]]))
    })
  )
) %>%
  mutate(retention_prev = overlap_prev / lag(n_ids))

# 4. Identify all starttimeW variables
start_vars <- grep("^starttimeW", names(BES), value = TRUE)

# 5. Derive median start date per wave
fieldwork_dates <- BES %>%
  select(all_of(start_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "var",
    values_to = "starttime"
  ) %>%
  filter(!is.na(starttime)) %>%
  mutate(
    wave = as.numeric(str_extract(var, "\\d+")),
    starttime = as.POSIXct(starttime, tz = "UTC"),
    month_year = format(starttime, "%b %Y")
  ) %>%
  group_by(wave) %>%
  summarise(fieldwork = first(month_year), .groups = "drop")

# 6. Merge into the main wave summary
wave_summary <- wave_summary %>%
  left_join(fieldwork_dates, by = "wave") %>%
  relocate(fieldwork, .after = wave)

wave_summary

write_xlsx(wave_summary, "wave_summary.xlsx")

#########################################################################

# ATI Variables

# See all variables that look like immigration attitudes
immig_vars <- names(BES)[str_detect(names(BES), regex("immig", ignore_case = TRUE))]

immig_vars

ati_vars <- c(
  grep("^immigEcon", names(BES), value = TRUE),
  grep("^immigCultural", names(BES), value = TRUE),
  grep("^immigSelf", names(BES), value = TRUE)
)

BES <- BES %>%
  mutate(across(all_of(ati_vars), ~ ifelse(. %in% c(9999, 8888), NA, as.numeric(.))))

ati_info <- tibble(var = ati_vars) %>%
  mutate(
    base = str_remove(var, "W\\d+$"),
    wave = as.numeric(str_remove(str_extract(var, "W\\d+$"), "W"))
  ) %>%
  mutate(available = 1) %>%
  pivot_wider(
    id_cols = base,
    names_from = wave,
    values_from = available,
    values_fill = 0
  )

ati_long <- BES %>%
  select(all_of(ati_vars)) %>%
  pivot_longer(everything(), names_to = "var", values_to = "value") %>%
  mutate(
    base = str_remove(var, "W\\d+$"),
    wave = as.numeric(str_remove(str_extract(var, "W\\d+$"), "W"))
  )

ati_summary <- ati_long %>%
  group_by(base, wave) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    .groups = "drop"
  )

ati_summary

# ATI Tables

ati_long <- BES %>%
  select(all_of(ati_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("dim", "wave"),
    names_pattern = "(immigEcon|immigCultural|immigSelf)W(\\d+)",
    values_to = "value"
  ) %>%
  mutate(
    wave = as.numeric(wave),
    dim = recode(dim,
                 "immigEcon"      = "econ",
                 "immigCultural"  = "cultural",
                 "immigSelf"      = "self")
  )

# Means
ati_means <- ati_long %>%
  group_by(wave, dim) %>%
  summarise(mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = wave,
    names_from = dim,
    values_from = mean,
    names_prefix = "mean_"
  ) %>%
  arrange(wave)

ati_means

ati_plot <- ati_means %>%
  pivot_longer(cols = starts_with("mean_"),
               names_to = "dimension",
               values_to = "mean") %>%
  mutate(dimension = recode(dimension,
                            "mean_econ" = "Economic ATI",
                            "mean_cultural" = "Cultural ATI",
                            "mean_self" = "Immigration Preferences (Self)"))

ggplot(ati_plot, aes(x = wave, y = mean, color = dimension)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Evolution of Immigration Attitudes Across BES Waves",
    x = "Wave",
    y = "Mean ATI",
    color = "Dimension"
  ) +
  theme_minimal(base_size = 14)

ati_means %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "dimension",
    values_to = "mean"
  ) %>%
  mutate(dimension = recode(dimension,
                            "mean_econ" = "Economic ATI",
                            "mean_cultural" = "Cultural ATI",
                            "mean_self" = "Immigration Preferences (Self)")) %>%
  ggplot(aes(x = wave, y = mean)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ dimension, scales = "free_y") +
  labs(
    title = "Immigration Attitudes Across BES Waves",
    x = "Wave",
    y = "Mean ATI"
  ) +
  theme_minimal(base_size = 14)

#########################################################################

# Nationalism variables

nat_vars <- names(BES)[grepl("brit|British|identity|proud|belong", names(BES), ignore.case = TRUE)]
nat_vars

# Extract all britishness variables across waves
britishness_vars <- grep("^britishnessW", names(BES), value = TRUE)

# Clean "Don't know" codes (9999, 8888 → NA)
BES <- BES %>%
  mutate(across(all_of(britishness_vars),
                ~ ifelse(. %in% c(9999, 8888), NA, as.numeric(.))))

# Convert to long format for panel models
britishness_long <- BES %>%
  mutate(id = row_number()) %>%                         # or your respondent ID variable
  select(id, all_of(britishness_vars)) %>%
  pivot_longer(
    cols = all_of(britishness_vars),
    names_to = "wave_var",
    values_to = "britishness"
  ) %>%
  mutate(
    wave = as.numeric(str_remove(str_extract(wave_var, "W\\d+$"), "W"))
  ) %>%
  select(id, wave, britishness)

britishness_long

brit_means <- britishness_long %>%
  group_by(wave) %>%
  summarise(
    mean_britishness = mean(britishness, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(wave)

brit_means


ggplot(brit_means, aes(x = wave, y = mean_britishness)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Britishness Across BES Waves",
    x = "Wave",
    y = "Mean Britishness"
  ) +
  theme_minimal(base_size = 14)


# ETHNIC NATIONALISM items
ethnic_items <- c(
  "britBornHereW11",
  "britCitizenW11",
  "britLiveHereW11",
  "britChristianW11"
)

# CIVIC NATIONALISM items
civic_items <- c(
  "britSpeakEnglishW11",
  "britCustomsW11",
  "britRespectLawW11",
  "britFeelBritishW11"
)

nat_items <- c(ethnic_items, civic_items)

# Clean DK codes
BES <- BES %>%
  mutate(across(all_of(c(ethnic_items, civic_items)),
                ~ ifelse(. %in% c(9999, 8888), NA, as.numeric(.))))

# Build trait-level nationalism indices
BES <- BES %>%
  mutate(
    ethnic_nationalism = rowMeans(across(all_of(ethnic_items)), na.rm = TRUE),
    civic_nationalism  = rowMeans(across(all_of(civic_items)),  na.rm = TRUE)
  )

# 1) Put all raw items together
raw_nat_items <- c(ethnic_items, civic_items)

nat_long <- BES %>%
  select(all_of(raw_nat_items)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

# 2) Bar plots for each raw item (counts)
ggplot(nat_long, aes(x = factor(value))) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ item, scales = "free_y") +
  labs(
    title = "Distribution of Raw Nationalism Items (Wave 11)",
    x = "Response category",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)


# Civic vs national 
BES %>%
  select(ethnic_nationalism, civic_nationalism) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
  ggplot(aes(x = type, y = value, fill = type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("firebrick", "steelblue"),
                    labels = c("Ethnic nationalism", "Civic nationalism")) +
  labs(
    title = "Civic vs Ethnic Nationalism (Wave 11)",
    x = NULL,
    y = "Score"
  ) +
  theme_minimal(base_size = 14)

#########################################################################

# ATI + NAT tables

all_vars <- c(ati_vars, britishness_vars, nat_items)

summary_list <- lapply(all_vars, function(v) {
  x <- BES[[v]]
  
  tibble(
    var        = v,
    wave       = as.numeric(str_extract(v, "\\d+")),  # gets the W## part
    mean       = mean(x, na.rm = TRUE),
    sd         = sd(x, na.rm = TRUE),
    n          = sum(!is.na(x))
  )
})

summary_stats <- bind_rows(summary_list)

write_xlsx(summary_stats, "summary_stats_BES_core_vars.xlsx")

## Correlation 

# Ati (self) vs Nat per wave

# Extract wave number for matching

ati_self_vars <- grep("^immigSelfW", names(BES), value = TRUE)

ati_self_df <- tibble(
  wave = as.numeric(str_extract(ati_self_vars, "\\d+")),
  ati_var = ati_self_vars
)

brit_df <- tibble(
  wave = as.numeric(str_extract(britishness_vars, "\\d+")),
  brit_var = britishness_vars
)

# Join to ensure matched pairs
corr_pairs <- inner_join(ati_self_df, brit_df, by = "wave")

# Compute correlation for each wave
correlation_results <- map_dfr(corr_pairs$wave, function(w) {
  
  ati_var  <- corr_pairs$ati_var[corr_pairs$wave == w]
  brit_var <- corr_pairs$brit_var[corr_pairs$wave == w]
  
  x <- BES[[ati_var]]
  y <- BES[[brit_var]]
  
  tibble(
    wave = w,
    cor  = cor(x, y, use = "pairwise.complete.obs")
  )
  
}) %>% arrange(wave)

correlation_results

ggplot(correlation_results, aes(x = wave, y = cor)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Correlation Between ATI (Self) and Britishness Across Waves",
    x = "Wave",
    y = "Pearson correlation"
  ) +
  theme_minimal(base_size = 14)

# Wave11 Nat vs ATI (self)


# ATI variable (Wave 11)
ati_self_w11 <- "immigSelfW11"

# Raw nationalism items (Wave 11)
nat_items_w11 <- nat_items   # from your earlier definition

# Aggregated indices (calculated earlier)
index_vars <- c("ethnic_nationalism", "civic_nationalism")

# Combine all nationalism-related variables
all_nat_vars <- c(nat_items_w11, index_vars)

# Compute correlations item-by-item
cor_wave11 <- map_dfr(all_nat_vars, function(v) {
  
  tibble(
    nationalism_item = v,
    correlation = cor(
      BES[[ati_self_w11]],
      BES[[v]],
      use = "pairwise.complete.obs"
    )
  )
})

cor_wave11

ggplot(cor_wave11, aes(x = reorder(nationalism_item, correlation), 
                             y = correlation)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Correlation: ATI Self (Wave 11) and Nationalism Measures (Wave 11)",
    x = "Nationalism Dimension",
    y = "Pearson Correlation"
  ) +
  theme_minimal(base_size = 14)

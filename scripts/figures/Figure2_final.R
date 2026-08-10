# ============================================================
# Figure 2. Wave-specific identity effects on immigration attitudes
# ============================================================
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(haven)
library(purrr)
library(fixest)
library(broom)
library(ggplot2)

# =============================================================================
# 0. LOAD DATA AND BUILD PANELS
# =============================================================================
df <- readRDS(
  "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES/BES_subset_panel_full_v5_England.rds"
)

DV_BASE   <- "immigSelf"
BRIT_BASE <- "britishness"
ENG_BASE  <- "englishness"
TIME_BASE <- "starttime"
AGE_BASE  <- "age"

get_wave_num <- function(x) as.integer(str_extract(x, "\\d+$"))

waves_available <- function(df, base) {
  vars <- names(df)[str_detect(names(df), paste0("^", base, "W\\d+$"))]
  sort(unique(map_int(vars, get_wave_num)))
}

make_long_with_age <- function(df, dv_base, brit_base, time_base, age_base) {
  w_dv   <- waves_available(df, dv_base)
  w_brit <- waves_available(df, brit_base)
  w_time <- waves_available(df, time_base)
  w_age  <- waves_available(df, age_base)
  waves  <- Reduce(intersect, list(w_dv, w_brit, w_time, w_age))
  if (length(waves) < 3) stop("Too few overlapping waves.")
  
  core_cols <- c(
    paste0(dv_base,   "W", waves),
    paste0(brit_base, "W", waves),
    paste0(time_base, "W", waves),
    paste0(age_base,  "W", waves)
  )
  
  df |>
    mutate(across(
      any_of(core_cols),
      ~ as.numeric(haven::zap_labels(.x))
    )) |>
    select(id, any_of(core_cols)) |>
    pivot_longer(
      cols = -id,
      names_to = c(".value", "wave"),
      names_pattern = "(.+)W(\\d+)$"
    ) |>
    mutate(
      wave = as.integer(wave),
      time = suppressWarnings(ymd_hms(as.character(.data[[time_base]]), quiet = TRUE)),
      year = year(time)
    ) |>
    rename(dv = all_of(dv_base), brit = all_of(brit_base), age = all_of(age_base)) |>
    filter(!is.na(dv), !is.na(brit), !is.na(age), !is.na(wave))
}

panel_self <- make_long_with_age(df, DV_BASE, BRIT_BASE, TIME_BASE, AGE_BASE)

panel_self_eng <- make_long_with_age(df, DV_BASE, ENG_BASE, TIME_BASE, AGE_BASE) |>
  rename(engl = brit) |>
  mutate(engl = if_else(engl %in% 1:7, engl, NA_real_)) |>
  filter(!is.na(engl))

cat("Britishness panel:", nrow(panel_self), "obs,",
    n_distinct(panel_self$id), "individuals\n")
cat("Englishness panel:", nrow(panel_self_eng), "obs,",
    n_distinct(panel_self_eng$id), "individuals\n")

# =============================================================================
# 1. ESTIMATE MODELS
# =============================================================================
m_wave_age <- feols(
  dv ~ i(wave, brit, ref = 7) + age | id,
  data    = panel_self,
  cluster = "id"
)

m_wave_eng_age <- feols(
  dv ~ i(wave, engl, ref = 7) + age | id,
  data    = panel_self_eng,
  cluster = "id"
)

# =============================================================================
# 2. TIDY COEFFICIENTS
# =============================================================================
tidy_brit <- broom::tidy(m_wave_age, conf.int = TRUE) %>%
  filter(str_detect(term, "^wave::\\d+:brit$")) %>%
  mutate(
    wave     = as.integer(str_extract(term, "(?<=wave::)\\d+")),
    identity = "Britishness"
  ) %>%
  select(identity, wave, estimate, std.error, conf.low, conf.high)

tidy_engl <- broom::tidy(m_wave_eng_age, conf.int = TRUE) %>%
  filter(str_detect(term, "^wave::\\d+:engl$")) %>%
  mutate(
    wave     = as.integer(str_extract(term, "(?<=wave::)\\d+")),
    identity = "Englishness"
  ) %>%
  select(identity, wave, estimate, std.error, conf.low, conf.high)

plot_df <- bind_rows(tidy_brit, tidy_engl) %>%
  arrange(identity, wave)

# =============================================================================
# 2b. BUILD WAVE-TO-YEAR LABELS
# Goes back to wide df so NAs from panel filtering don't lose year info
# =============================================================================
wave_year_map <- df %>%
  select(matches("^starttimeW\\d+$")) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = "wave",
    names_pattern = "starttimeW(\\d+)$",
    values_to     = "starttime_raw"
  ) %>%
  mutate(
    wave = as.integer(wave),
    year = year(starttime_raw)
  ) %>%
  filter(!is.na(year), wave %in% unique(plot_df$wave)) %>%
  group_by(wave) %>%
  summarise(mean_year = round(mean(year, na.rm = TRUE)), .groups = "drop") %>%
  group_by(mean_year) %>%
  mutate(
    n_in_year = n(),
    x_label   = if_else(n_in_year > 1,
                        paste0(mean_year, "\n(W", wave, ")"),
                        as.character(mean_year))
  ) %>%
  ungroup() %>%
  arrange(wave)

plot_df <- plot_df %>%
  left_join(wave_year_map, by = "wave") %>%
  mutate(x_label = factor(x_label, levels = unique(wave_year_map$x_label)))

# Quick sanity check — should show real years, no NaN
print(plot_df %>% select(identity, wave, x_label) %>% distinct() %>% arrange(wave))

# =============================================================================
# 3. PLOT
# =============================================================================
ggplot(plot_df, aes(x = x_label, y = estimate, group = identity,
                    shape = identity, linetype = identity)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_line(linewidth = 0.5, colour = "black") +
  geom_point(size = 2.2, colour = "black", fill = "white") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, linewidth = 0.4, colour = "black") +
  scale_shape_manual(values    = c("Britishness" = 21, "Englishness" = 24)) +
  scale_linetype_manual(values = c("Britishness" = "solid", "Englishness" = "dashed")) +
  labs(
    title    = "Figure 2. Wave-specific within-individual effects of Britishness and Englishness on immigration attitudes",
    x        = "Survey year",
    y        = "Within-individual coefficient on immigration attitudes",
    shape    = NULL,
    linetype = NULL,
    caption  = "Parenthetical wave numbers shown where multiple BES waves fall within the same calendar year."
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 10) +
  theme(
    plot.title         = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title.x       = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10)),
    axis.text          = element_text(size = 10),
    axis.text.x        = element_text(angle = 30, hjust = 1),
    legend.title       = element_blank(),
    legend.text        = element_text(size = 10),
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(size = 8, colour = "grey35", hjust = 0)
  )

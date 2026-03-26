library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ---------------------------------------------------------
# 1) Build month/year labels from actual starttime variables
# ---------------------------------------------------------

start_cols <- grep("^starttimeW[0-9]+$", names(df), value = TRUE)

wave_dates <- df %>%
  select(all_of(start_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "start_col",
    values_to = "start_raw"
  ) %>%
  mutate(
    wave = as.integer(str_extract(start_col, "[0-9]+")),
    start_parsed = as.POSIXct(start_raw, tz = "UTC")
  ) %>%
  filter(!is.na(start_parsed)) %>%
  group_by(wave) %>%
  summarise(
    median_date = as.POSIXct(median(as.numeric(start_parsed), na.rm = TRUE),
                             origin = "1970-01-01", tz = "UTC"),
    .groups = "drop"
  ) %>%
  mutate(
    wave_label = format(median_date, "%b %Y")
  )

# Keep only waves in the coefficient plot
coef_df_plot <- coef_df %>%
  left_join(wave_dates, by = "wave") %>%
  arrange(identity, wave)

# ---------------------------------------------------------
# 2) Choose readable x-axis breaks
# You can adjust these if you want fewer/more labels
# ---------------------------------------------------------

x_breaks <- coef_df_plot %>%
  distinct(wave, wave_label) %>%
  arrange(wave) %>%
  pull(wave)

x_labels <- coef_df_plot %>%
  distinct(wave, wave_label) %>%
  arrange(wave) %>%
  pull(wave_label)

# ---------------------------------------------------------
# 3) APSR-style plot
# ---------------------------------------------------------

ref_wave_2016 <- wave_dates %>%
  mutate(diff = abs(as.numeric(median_date - as.POSIXct("2016-06-23", tz = "UTC")))) %>%
  arrange(diff) %>%
  slice(1) %>%
  pull(wave)

ref_wave_2021 <- wave_dates %>%
  mutate(diff = abs(as.numeric(median_date - as.POSIXct("2021-10-01", tz = "UTC")))) %>%
  arrange(diff) %>%
  slice(1) %>%
  pull(wave)

# ---------------------------------------------------------
# 2) Fewer x-axis labels for readability
# ---------------------------------------------------------

selected_waves <- c(7, 9, 13, 16, 20, 21, 23, 26, 29, 30)

selected_labels_df <- coef_df_plot %>%
  distinct(wave, wave_label) %>%
  filter(wave %in% selected_waves) %>%
  arrange(wave)

# ---------------------------------------------------------
# 3) APSR-style plot
# ---------------------------------------------------------

p_apsr <- ggplot(
  coef_df_plot,
  aes(x = wave, y = estimate, colour = identity, shape = identity, linetype = identity)
) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "black") +
  
  geom_vline(
    xintercept = ref_wave_2016,
    linetype = "dotted",
    linewidth = 0.45,
    colour = "grey40"
  ) +
  
  geom_vline(
    xintercept = ref_wave_2021,
    linetype = "dotted",
    linewidth = 0.45,
    colour = "grey40"
  ) +
  
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.10,
    linewidth = 0.40
  ) +
  
  geom_line(
    aes(linewidth = identity, alpha = identity)
  ) +
  
  geom_point(
    aes(size = identity, alpha = identity)
  ) +
  
  scale_colour_manual(
    values = c(
      "Britishness" = "#1B3A57",
      "Englishness" = "#8B3E2F"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Britishness" = "solid",
      "Englishness" = "dashed"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Britishness" = 16,
      "Englishness" = 17
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Britishness" = 0.95,
      "Englishness" = 0.70
    ),
    guide = "none"
  ) +
  scale_size_manual(
    values = c(
      "Britishness" = 2.7,
      "Englishness" = 2.4
    ),
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c(
      "Britishness" = 1,
      "Englishness" = 0.9
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = selected_labels_df$wave,
    labels = selected_labels_df$wave_label
  ) +
  labs(
    title = "National identity and immigration attitudes over time",
    subtitle = "Wave-specific associations in the BES Internet Panel",
    x = NULL,
    y = "Association with immigration attitudes",
    colour = NULL,
    shape = NULL,
    linetype = NULL,
    caption = paste(
      "Points show wave-specific associations between national identity and immigration attitudes;",
      "vertical bars indicate 95% confidence intervals.",
      "Vertical lines mark the June 2016 EU referendum and the onset of the post-2021 cost-of-living period."
    )
  ) +
  coord_cartesian(
    ylim = c(-0.75, -0.30),
    clip = "off"
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 11, family = "Times New Roman"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10, family = "Times New Roman"),
    axis.text.y = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 12, family = "Times New Roman"),
    plot.title = element_text(size = 16, face = "bold", family = "Times New Roman"),
    plot.subtitle = element_text(size = 11, family = "Times New Roman"),
    plot.caption = element_text(size = 9, colour = "grey30", family = "Times New Roman"),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )

print(p_apsr)
# ---------------------------------------------------------
# 4) Save
# ---------------------------------------------------------

ggsave(
  filename = "apsr_identity_immigration_coefficients.png",
  plot = p_apsr,
  width = 10.5,
  height = 6.3,
  dpi = 300
)

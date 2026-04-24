# ============================================================
# Figure X. Wave-specific identity effects on immigration attitudes
# Britishness and Englishness together, with age controls
# ============================================================

library(dplyr)
library(stringr)
library(ggplot2)
library(fixest)
library(broom)

# -----------------------------
# 1. Estimate age-controlled wave-specific models
# -----------------------------

m_wave_age <- feols(
  dv ~ i(wave, brit, ref = 7) + age | id,
  data = panel_self,
  cluster = "id"
)

m_wave_eng_age <- feols(
  dv ~ i(wave, engl, ref = 7) + age | id,
  data = panel_self_eng,
  cluster = "id"
)

# -----------------------------
# 2. Tidy coefficients
# -----------------------------

tidy_brit <- broom::tidy(m_wave_age, conf.int = TRUE) %>%
  filter(str_detect(term, "^wave::\\d+:brit$")) %>%
  mutate(
    wave = as.integer(str_extract(term, "(?<=wave::)\\d+")),
    identity = "Britishness"
  ) %>%
  select(identity, wave, estimate, std.error, conf.low, conf.high)

tidy_engl <- broom::tidy(m_wave_eng_age, conf.int = TRUE) %>%
  filter(str_detect(term, "^wave::\\d+:engl$")) %>%
  mutate(
    wave = as.integer(str_extract(term, "(?<=wave::)\\d+")),
    identity = "Englishness"
  ) %>%
  select(identity, wave, estimate, std.error, conf.low, conf.high)

plot_df <- bind_rows(tidy_brit, tidy_engl) %>%
  arrange(identity, wave)

# -----------------------------
# 3. Plot
# -----------------------------

ggplot(plot_df, aes(x = wave, y = estimate, group = identity, shape = identity, linetype = identity)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_line(linewidth = 0.5, colour = "black") +
  geom_point(size = 2.2, colour = "black", fill = "white") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, linewidth = 0.4, colour = "black") +
  scale_shape_manual(values = c("Britishness" = 21, "Englishness" = 24)) +
  scale_linetype_manual(values = c("Britishness" = "solid", "Englishness" = "dashed")) +
  scale_x_continuous(breaks = sort(unique(plot_df$wave))) +
  labs(
    title = "Figure 2. Wave-specific within-individual effects of Britishness and Englishness on immigration attitudes",
    x = "BES wave",
    y = "Within-individual coefficient on immigration attitudes",
    shape = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 10) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

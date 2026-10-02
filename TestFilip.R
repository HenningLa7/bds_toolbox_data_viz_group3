# Exploratory plots: schoolweging, spreiding and test scores
#
# Refined research question:
# Among regular primary schools, how are school-level spreiding and
# schoolweging associated with average test scores within each provider?
# These school-level associations are descriptive, not causal.

library(tidyverse)
library(readODS)

project_root <- here::here()
data_dir <- file.path(project_root, "data", "raw")
figures_dir <- file.path(project_root, "figures")

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# Read all available input files. Reference-level and advice counts include
# suppressed values, so they are not used to calculate outcome percentages.
eindscores <- read_delim(
  file.path(data_dir, "eindscores_2024-2025.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""),
  show_col_types = FALSE
)

referentieniveaus <- read_delim(
  file.path(data_dir, "referentieniveaus_2024-2025.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""),
  show_col_types = FALSE
)

schooladviezen <- read_delim(
  file.path(data_dir, "schooladviezen_2024-2025.csv"),
  delim = ";",
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""),
  show_col_types = FALSE
)

schoolweging <- read_ods(
  file.path(data_dir, "schoolweging_2022-2025.ods"),
  sheet = "2024-2025"
)

# The ODS identifies locations by institution code plus a different location
# code. Average locations to institution level, weighted by pupil count,
# before joining on the institution code shared with the score data.
weging_by_school <- schoolweging |>
  mutate(INSTELLINGSCODE = str_extract(OVT, "^[^|]+")) |>
  filter(
    !is.na(INSTELLINGSCODE),
    !is.na(schoolweging),
    !is.na(spreiding),
    !is.na(aantal_leerlingen),
    aantal_leerlingen > 0
  ) |>
  group_by(INSTELLINGSCODE) |>
  summarise(
    schoolweging = weighted.mean(schoolweging, aantal_leerlingen),
    spreiding = weighted.mean(spreiding, aantal_leerlingen),
    .groups = "drop"
  )

# Provider scores use different scales. Summarise locations to the institution
# level within provider, weighting by the number of pupils tested.
scores_by_provider <- eindscores |>
  filter(SOORT_PO == "Bo") |>
  pivot_longer(
    cols = matches("^(IEP|ROUTE8|DIA|AMN|DOE|LIB)_(AANTAL|GEM)$"),
    names_to = c("provider", ".value"),
    names_pattern = "^(.+)_(AANTAL|GEM)$"
  ) |>
  mutate(
    AANTAL = as.numeric(
      if_else(
        str_detect(AANTAL, "^[0-9]+$"),
        AANTAL,
        NA_character_
      )
    )
  ) |>
  filter(AANTAL > 0, !is.na(GEM)) |>
  group_by(INSTELLINGSCODE, provider) |>
  summarise(
    mean_score = weighted.mean(GEM, AANTAL),
    .groups = "drop"
  )

plot_data <- scores_by_provider |>
  inner_join(weging_by_school, by = "INSTELLINGSCODE") |>
  filter(
    is.finite(mean_score),
    is.finite(schoolweging),
    is.finite(spreiding)
  )

# Check this joined dataset contains only schools with all plot variables.
message(
  "Matched institution-provider observations: ",
  nrow(plot_data),
  " across ",
  n_distinct(plot_data$INSTELLINGSCODE),
  " institutions."
)

theme_set(theme_minimal(base_size = 12))

plot_spread_and_weight <- ggplot(
  plot_data,
  aes(x = schoolweging, y = spreiding)
) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(
    title = "Schoolweging and spreiding",
    subtitle = "Each point represents one matched regular primary school",
    x = "Schoolweging (overall disadvantage)",
    y = "Spreiding (variation in disadvantage)"
  )

plot_score_and_spread <- ggplot(
  plot_data,
  aes(x = spreiding, y = mean_score, color = schoolweging)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(vars(provider), scales = "free_y") +
  scale_color_viridis_c() +
  labs(
    title = "Test scores and spreiding, by provider",
    subtitle = "Provider score scales differ; interpret patterns within panels",
    x = "Spreiding (variation in disadvantage)",
    y = "Average test score",
    color = "Schoolweging"
  )

plot_score_and_weight <- ggplot(
  plot_data,
  aes(x = schoolweging, y = mean_score, color = spreiding)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(vars(provider), scales = "free_y") +
  scale_color_viridis_c() +
  labs(
    title = "Test scores and schoolweging, by provider",
    subtitle = "Provider score scales differ; interpret patterns within panels",
    x = "Schoolweging (overall disadvantage)",
    y = "Average test score",
    color = "Spreiding"
  )

ggsave(
  file.path(figures_dir, "schoolweging-vs-spreiding.png"),
  plot = plot_spread_and_weight,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(figures_dir, "scores-vs-spreiding-by-provider.png"),
  plot = plot_score_and_spread,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(figures_dir, "scores-vs-schoolweging-by-provider.png"),
  plot = plot_score_and_weight,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

print(plot_spread_and_weight)
print(plot_score_and_spread)
print(plot_score_and_weight)

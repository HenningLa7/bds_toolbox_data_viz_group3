# Exploratory plots: standardized scores by schoolweging and spreiding
#
# Refined research question:
# Among regular primary schools, how is spreiding(within school disadvantage variance) associated with relative
# test performance at different levels of schoolweging (school disatvantage?
# Scores are standardized within provider and then combined, so a z-score
# indicates relative standing within a provider, not an absolute score.

library(tidyverse)
library(readODS)

project_root <- here::here()
data_dir <- file.path(project_root, "data", "raw")
figures_dir <- file.path(project_root, "figures")

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# Read all input files. The CSVs with reference-level and advice counts
# contain suppressed values, so those counts are not used as outcomes here.
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
# code. Average locations to institution level using pupil counts as weights.
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

# Suppressed counts (<5) cannot be used as exact weights, so exclude them.
# Provider is used only to standardize otherwise incomparable score scales.
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
    tested_pupils = sum(AANTAL),
    .groups = "drop"
  ) |>
  group_by(provider) |>
  mutate(
    score_z = (mean_score - mean(mean_score)) / sd(mean_score)
  ) |>
  ungroup() |>
  filter(is.finite(score_z))

# Combine any multiple provider results into one school-level value, weighted
# by tested pupils. Provider names are not used in the visualizations.
scores_by_school <- scores_by_provider |>
  group_by(INSTELLINGSCODE) |>
  summarise(
    standardized_score = weighted.mean(score_z, tested_pupils),
    .groups = "drop"
  )

plot_data <- scores_by_school |>
  inner_join(weging_by_school, by = "INSTELLINGSCODE") |>
  filter(
    is.finite(standardized_score),
    is.finite(schoolweging),
    is.finite(spreiding)
  )

tertile_cutpoints <- quantile(
  plot_data$schoolweging,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE
)

plot_data <- plot_data |>
  mutate(
    disadvantage_level = case_when(
      schoolweging <= tertile_cutpoints[[1]] ~ "Lower disadvantage",
      schoolweging <= tertile_cutpoints[[2]] ~ "Middle disadvantage",
      TRUE ~ "Higher disadvantage"
    ),
    disadvantage_level = factor(
      disadvantage_level,
      levels = c(
        "Lower disadvantage",
        "Middle disadvantage",
        "Higher disadvantage"
      )
    )
  )

message("Matched schools: ", nrow(plot_data), ".")
message(
  "Schoolweging tertile cutpoints: ",
  round(tertile_cutpoints[[1]], 1),
  " and ",
  round(tertile_cutpoints[[2]], 1),
  "."
)

theme_set(theme_minimal(base_size = 12))

# Aggregate nearby values so color represents the median standardized score.
heatmap_data <- plot_data |>
  mutate(
    weight_bin = cut_interval(schoolweging, n = 12),
    spread_bin = cut_interval(spreiding, n = 12)
  ) |>
  group_by(weight_bin, spread_bin) |>
  summarise(
    median_score_z = median(standardized_score),
    schools = n(),
    .groups = "drop"
  ) |>
  filter(schools >= 5)

near_30_data <- plot_data |>
  filter(between(schoolweging, 28, 32))

message("Schools with schoolweging from 28 to 32: ", nrow(near_30_data), ".")




plot_score_by_spread <- ggplot(
  plot_data,
  aes(x = spreiding, y = standardized_score)
) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  facet_wrap(vars(disadvantage_level)) +
  labs(
    title = "Relative scores by spreiding across disadvantage levels",
    subtitle = paste(
      "Schoolweging groups are data-derived tertiles;",
      "each point is one school"
    ),
    x = "Spreiding (variation in disadvantage)",
    y = "Standardized test score (provider-specific z-score)"
  )

plot_near_30 <- ggplot(
  near_30_data,
  aes(x = spreiding, y = standardized_score)
) +
  geom_point(alpha = 0.35, size = 1.3) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue") +
  labs(
    title = "Spreiding and relative scores near schoolweging 30",
    subtitle = "Schools with schoolweging from 28 to 32",
    x = "Spreiding (variation in disadvantage)",
    y = "Standardized test score (provider-specific z-score)"
  )

 





print(plot_score_by_spread)
print(plot_near_30)

####testing
summary(plot_data$spreiding)
range(plot_data$spreiding, na.rm = TRUE)''
sort(unique(plot_data$spreiding))
hist(plot_data$spreiding)



# Centre spreiding: 0 now represents average spreiding
plot_data$spreiding_c <- plot_data$spreiding -
  mean(plot_data$spreiding, na.rm = TRUE)

# Model without different slopes across disadvantage groups
model_no_interaction <- lm(
  standardized_score ~ spreiding_c + disadvantage_level,
  data = plot_data
)

# Model where the spreiding-performance relationship may differ by group
model_interaction <- lm(
  standardized_score ~ spreiding_c * disadvantage_level,
  data = plot_data
)
model_spreiding<-lm(
  standardized_score ~ spreiding_c,
  data = plot_data)
summary(model_spreiding)
summary(model_interaction)

# Does allowing different slopes improve the model?
anova(model_no_interaction, model_interaction)


par(mfrow = c(2, 2))
plot(model_interaction)
par(mfrow = c(1, 1))



spreiding_scores<-ggplot(
  plot_data,
  aes(x = spreiding_c, y = standardized_score)
) +
  geom_point(alpha = 0.4, colour = "grey40") +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "#0072B2",
    linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  labs(
    x = "Within-school disadvantage spread (centred)",
    y = "Standardized performance score"
  ) +
  theme_minimal()

spreiding_scores


overall_spread_scores<-ggplot(
  plot_data,
  aes(
    x = spreiding_c,
    y = standardized_score,
    colour = disadvantage_level
  )
) +
  geom_point(alpha = 0.35) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  labs(
    x = "Within-school disadvantage spread (centred)",
    y = "Standardized performance score",
    colour = "Overall school disadvantage"
  ) +
  theme_minimal()
overall_spread_scores



###
# Keep complete observations only
analysis_data <- subset(
  plot_data,
  !is.na(standardized_score) &
    !is.na(spreiding_c) &
    !is.na(disadvantage_level)
)

# Make sure the variable is a factor


# Run a separate linear regression in each tertile
models_tertile <- lapply(
  split(analysis_data, analysis_data$disadvantage_level),
  function(data_group) {
    lm(standardized_score ~ spreiding_c, data = data_group)
  }
)

# View the full regression output for each group
lapply(models_tertile, summary)

results_tertile <- do.call(
  rbind,
  lapply(names(models_tertile), function(group) {
    
    model <- models_tertile[[group]]
    coefficient <- summary(model)$coefficients["spreiding_c", ]
    ci <- confint(model)["spreiding_c", ]
    
    data.frame(
      disadvantage_level = group,
      n_schools = nobs(model),
      b_spreiding = coefficient["Estimate"],
      SE = coefficient["Std. Error"],
      t = coefficient["t value"],
      p = coefficient["Pr(>|t|)"],
      CI_lower = ci[1],
      CI_upper = ci[2],
      R_squared = summary(model)$r.squared
    )
  })
)

results_tertile
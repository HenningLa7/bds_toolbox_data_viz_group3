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
###advice performance variablenhennign schooldata
keep_single <- function(df) {
  df |>
    add_count(INSTELLINGSCODE, name = "n_locations") |>
    filter(n_locations == 1L) |>
    select(-n_locations)
}

# "00AP|C1" -> "00AP"
school_context <- schoolweging |>
  transmute(
    INSTELLINGSCODE = str_remove(OVT, "\\|.*$"),
    schoolweging,
    spreiding,
    n_students_school = aantal_leerlingen
  ) |>
  keep_single()

# "<5" -> 2.5; share of advices that are HAVO or higher
school_advice <- schooladviezen |>
  select(INSTELLINGSCODE, all_of(advice_cols)) |>
  mutate(
    across(all_of(advice_cols), ~ as.numeric(str_replace(.x, "^<5$", "2.5"))),
    n_students_advice = rowSums(pick(all_of(advice_cols))),
    HAVO_higher = 100 * rowSums(pick(all_of(havo_cols))) / n_students_advice
  ) |>
  select(INSTELLINGSCODE, HAVO_higher, n_students_advice) |>
  keep_single()

advice_cols <- c(
  "VSO", "PRO", "VMBO_B", "VMBO_B_K", "VMBO_K", "VMBO_K_GT",
  "VMBO_GT", "VMBO_GT_HAVO", "HAVO", "HAVO_VWO", "VWO"
)
havo_cols <- c("HAVO", "HAVO_VWO", "VWO")

school_advice <- schooladviezen |>
  select(INSTELLINGSCODE, all_of(advice_cols)) |>
  mutate(
    across(all_of(advice_cols), ~ as.numeric(str_replace(.x, "^<5$", "2.5"))),
    n_students_advice = rowSums(pick(all_of(advice_cols))),
    HAVO_higher = 100 * rowSums(pick(all_of(havo_cols))) / n_students_advice
  ) |>
  select(INSTELLINGSCODE, HAVO_higher, n_students_advice) |>
  keep_single()
# One row per school
school_data <- school_context |>
  inner_join(school_advice, by = "INSTELLINGSCODE", 
             relationship = "one-to-one") |>
  filter(
    !is.na(schoolweging), !is.na(spreiding),
    !is.na(HAVO_higher), n_students_advice > 0
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
###################
bin_colours <- c(
  "1" = "#440154", "2" = "#482878", "3" = "#3E4989",
  "4" = "#31688E", "5" = "#26828E", "6" = "#1F9E89",
  "7" = "#35B779", "8" = "#52C569", "9" = "#86D549",
  "10" = "#C2DF23"
)
school_data_full <- school_data

limits <- quantile(school_data$schoolweging, c(1 / 10, 9 / 10))

school_data <- school_data |>
  filter(between(schoolweging, limits[[1]], limits[[2]])) |>
  mutate(
    weging_trimmed = schoolweging,
    weging_bin = factor(ntile(weging_trimmed, 10), ordered = TRUE),
    weging_trim_c = weging_trimmed - mean(weging_trimmed),
    spreiding_c = spreiding - mean(spreiding)
  )

plot_spread_and_weight <- ggplot(
  school_data,
  aes(x = schoolweging, y = spreiding)
) +
  geom_point(alpha = 0.5, size = .5) +
  labs(
    title = "Schoolweging and spreiding",
    subtitle = "Each point represents one school",
    x = "Schoolweging",
    y = "Spreiding"
  )
plot_spread_and_weight
theme_set(theme_minimal(base_size = 12))

# Use your existing bin_colours vector

# >>> CHANGE LINE AND OUTSIDE-POINT COLOURS HERE <<<
bin_line_colour <- "grey45"
trim_line_colour <- "red"
outside_point_colour <- "grey75"

# Preserve the full dataset BEFORE trimming
school_data_full <- school_data

limits <- quantile(
  school_data_full$schoolweging,
  probs = c(0.1, 0.9),
  na.rm = TRUE
)

# Keep your original ntile() bins and variables for later analyses
school_data <- school_data_full |>
  filter(between(schoolweging, limits[[1]], limits[[2]])) |>
  mutate(
    weging_trimmed = schoolweging,
    weging_bin = factor(ntile(weging_trimmed, 10), ordered = TRUE),
    weging_trim_c = weging_trimmed - mean(weging_trimmed),
    spreiding_c = spreiding - mean(spreiding)
  )

# Place boundaries halfway between adjacent ntile() bins
bin_boundaries <- school_data |>
  group_by(weging_bin) |>
  summarise(
    bin_min = min(schoolweging),
    bin_max = max(schoolweging),
    .groups = "drop"
  ) |>
  arrange(weging_bin) |>
  mutate(cutoff = (bin_max + lead(bin_min)) / 2) |>
  filter(!is.na(cutoff))

plot_spread_and_weight2 <- ggplot(
  school_data_full,
  aes(x = schoolweging, y = spreiding)
) +
  # Full dataset in grey
  geom_point(
    colour = outside_point_colour,
    alpha = 0.5,
    size = 0.5
  ) +
  # Only schools within the trimming limits get bin colours
  geom_point(
    data = school_data,
    aes(colour = weging_bin),
    alpha = 0.5,
    size = 0.5
  ) +
  # Boundaries between ntile() bins
  geom_vline(
    data = bin_boundaries,
    aes(xintercept = cutoff),
    colour = bin_line_colour,
    linetype = "dotted",
    linewidth = 0.4
  ) +
  # 10th- and 90th-percentile limits
  geom_vline(
    xintercept = limits,
    colour = trim_line_colour,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  scale_colour_manual(
    values = bin_colours,
    name = "Schoolweging bin",
    drop = FALSE
  ) +
  labs(
    title = "Schoolweging and spreiding",
    subtitle = "Schools outside the trimming limits are shown in grey",
    x = "Schoolweging",
    y = "Spreiding"
  ) +
  theme_minimal()

plot_spread_and_weight2
##############################
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

 ######
near_35_data <- plot_data |>
  filter(between(schoolweging, 30, 40))

plot_near_35 <- ggplot(
  near_35_data,
  aes(x = spreiding, y = standardized_score)
) +
  geom_point(alpha = 0.35, size = 1.3) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue") +
  labs(
    title = "Spreiding and relative scores near schoolweging 35",
    subtitle = "Schools with schoolweging from 30 to 40",
    x = "Spreiding",
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

plot_data <- plot_data |>
  mutate(
    schoolweging_c = schoolweging - mean(schoolweging, na.rm = TRUE)
  )


model_adjusted <- lm(
  standardized_score ~ spreiding_c + schoolweging_c,
  data = plot_data
)

summary(model_adjusted)


  model_inverted <- lm(
    spreiding_c ~ standardized_score + schoolweging_c,
    data = plot_data
  )
  

summary(model_inverted)
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



model_continuous_no_interaction <- lm(
  standardized_score ~ spreiding_c + schoolweging_c,
  data = plot_data
)
summary(model_continuous_interaction)
model_continuous_interaction <- lm(
  standardized_score ~ spreiding_c * schoolweging_c,
  data = plot_data
)

anova(
  model_continuous_no_interaction,
  model_continuous_interaction
)

summary(model_continuous_no_interaction)

library(ggplot2)

# Obtain representative schoolweging values
schoolweging_values <- quantile(
  plot_data$schoolweging_c,
  probs = c(0.20, 0.50, 0.80),
  na.rm = TRUE
)

# Create values at which predictions will be made
prediction_data <- expand.grid(
  spreiding_c = seq(
    quantile(plot_data$spreiding_c, 0.02, na.rm = TRUE),
    quantile(plot_data$spreiding_c, 0.98, na.rm = TRUE),
    length.out = 100
  ),
  schoolweging_c = schoolweging_values
)

prediction_data$schoolweging_level <- factor(
  prediction_data$schoolweging_c,
  levels = schoolweging_values,
  labels = c(
    "Lower schoolweging (20th percentile)",
    "Typical schoolweging (50th percentile)",
    "Higher schoolweging (80th percentile)"
  )
)

# Predicted scores and confidence intervals
predictions <- predict(
  model_continuous_interaction,
  newdata = prediction_data,
  interval = "confidence"
)

prediction_data <- cbind(prediction_data, predictions)

# Plot conditional predicted relationships
ggplot(
  prediction_data,
  aes(
    x = spreiding_c,
    y = fit,
    colour = schoolweging_level,
    fill = schoolweging_level
  )
) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  labs(
    x = "Within-school disadvantage spread (centred)",
    y = "Predicted standardized performance",
    colour = "Schoolweging",
    fill = "Schoolweging"
  ) +
  theme_minimal()






# ------------------------------------------------------------------------------
# Third exploratory plot
#

#How is within-school socioeconomic heterogeneity 
#associated with the proportion of students receiving HAVO-or-higher recommendations 
#in schools with moderate average socioeconomic disadvantage?

bin_info <- school_data |>
  summarise(
    .by = weging_bin,
    lo = min(schoolweging), hi = max(schoolweging), n_schools = n()
  ) |>
  mutate(
    x = as.integer(weging_bin),
    txt = if_else(x <= 6, "white", "black"),
    range = paste(round(lo, 1), round(hi, 1), sep = "-")
  )

# >>> CHANGE LINE AND OUTSIDE-POINT COLOURS HERE <<<
bin_line_colour <- "grey45"
trim_line_colour <- "red"
outside_point_colour <- "grey75"

# Preserve the full dataset BEFORE trimming
school_data_full <- school_data

limits <- quantile(
  school_data_full$schoolweging,
  probs = c(0.1, 0.9),
  na.rm = TRUE
)
bin_labels <- setNames(
  paste0(
    "Bin ", bin_info$weging_bin,
    " (n=", bin_info$n_schools, ")"
  ),
  as.character(bin_info$weging_bin)
)
# Keep your original ntile() bins and variables for later analyses
school_data <- school_data_full |>
  filter(between(schoolweging, limits[[1]], limits[[2]])) |>
  mutate(
    weging_trimmed = schoolweging,
    weging_bin = factor(ntile(weging_trimmed, 10), ordered = TRUE),
    weging_trim_c = weging_trimmed - mean(weging_trimmed),
    spreiding_c = spreiding - mean(spreiding)
  )

# Place boundaries halfway between adjacent ntile() bins
bin_boundaries <- school_data |>
  group_by(weging_bin) |>
  summarise(
    bin_min = min(schoolweging),
    bin_max = max(schoolweging),
    .groups = "drop"
  ) |>
  arrange(weging_bin) |>
  mutate(cutoff = (bin_max + lead(bin_min)) / 2) |>
  filter(!is.na(cutoff))

plot_spread_and_weight2 <- ggplot(
  school_data_full,
  aes(x = schoolweging, y = spreiding)
) +
  # Full dataset in grey
  geom_point(
    colour = outside_point_colour,
    alpha = 0.5,
    size = 0.5
  ) +
  # Only schools within the trimming limits get bin colours
  geom_point(
    data = school_data,
    aes(colour = weging_bin),
    alpha = 0.5,
    size = 0.5
  ) +
  # Boundaries between ntile() bins
  geom_vline(
    data = bin_boundaries,
    aes(xintercept = cutoff),
    colour = bin_line_colour,
    linetype = "dotted",
    linewidth = 0.4
  ) +
  # 10th- and 90th-percentile limits
  geom_vline(
    xintercept = limits,
    colour = trim_line_colour,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  scale_colour_manual(
    values = bin_colours,
    labels= bin_labels,
    name = "Schoolweging bin",
    drop = FALSE
  ) +
  guides(
    colour = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      ncol = 3,              # Fewer entries per row prevent crowding
      byrow = TRUE,
      override.aes = list(
        size = 3,            # Make legend dots easier to see
        alpha = 1
      )
    )
  ) +
  labs(
    title = "Average socio-economic disadvantage (Schoolweging) vs. 
      Within-school socioeconomic heterogeneity (Spreiding)",
    subtitle = "",
    x = "Schoolweging",
    y = "Spreiding"
  ) +
  theme_minimal() +
  # Put custom settings AFTER theme_minimal()
  theme(
    legend.position = "top",
    legend.title = element_text(size = 8, face = "plain"),
    legend.text = element_text(size = 9),
    legend.key.width = grid::unit(0.4, "cm"),
    legend.key.height = grid::unit(0.4, "cm")
  )

plot_spread_and_weight2
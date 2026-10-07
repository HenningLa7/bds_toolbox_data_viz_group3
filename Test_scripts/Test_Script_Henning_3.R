# Clean workspace:
rm(list = ls())

# Packages:
source(here::here("scripts", "00-packages.R"))

# Data
source(here::here("scripts", "01-get-data.R"))

# ------------------------------------------------------------------------------
# Initial Data preparation

# - We matched the two datasets on INSTELLINGSCODE, which we took from OVT in the schoolweging dataset
# - Some institutions have several locations, and the two sources identify locations differently. 
#   We therefore kept only institutions that appear exactly once in both sources.
# --> We did not average spreiding across locations, because that would remove differences between locations.
# - We replaced "<5" with 2.5 (as suggested in the codebook)
# - HAVO_higher is the percentage of advices that are HAVO, HAVO_VWO or VWO. The excluded ADVIES_NIET_MOGELIJK (too few observations).
# - n_students_school is the pupil count behind the disadvantage measures. 
# - n_students_advice is the estimated number of advices.

# Save school form variables
advice_cols <- c(
  "VSO", "PRO", "VMBO_B", "VMBO_B_K", "VMBO_K", "VMBO_K_GT",
  "VMBO_GT", "VMBO_GT_HAVO", "HAVO", "HAVO_VWO", "VWO"
)
havo_cols <- c("HAVO", "HAVO_VWO", "VWO")

# Keep institutions that appear exactly once in a source (single location);
# counted before dropping NAs, so empty locations still count as locations
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

# One row per school
school_data <- school_context |>
  inner_join(school_advice, by = "INSTELLINGSCODE", 
             relationship = "one-to-one") |>
  filter(
    !is.na(schoolweging), !is.na(spreiding),
    !is.na(HAVO_higher), n_students_advice > 0
  )

# ------------------------------------------------------------------------------
# First Exploratory plot

# Prepare the data for the first exploratory plot:
advice_share <- schooladviezen |>
  semi_join(school_data, by = "INSTELLINGSCODE") |>
  select(all_of(advice_cols)) |>
  mutate(across(everything(), ~ as.numeric(str_replace(.x, "^<5$", "2.5")))) |>
  summarise(across(everything(), sum)) |>
  pivot_longer(everything(), names_to = "advice", values_to = "n") |>
  mutate(
    share = 100 * n / sum(n),
    havo_plus = advice %in% havo_cols,
    advice = factor(
      str_replace_all(advice, "_", "-"),
      levels = str_replace_all(advice_cols, "_", "-")
    )
  )

# Create the first exploratory plot:
ggplot(advice_share, aes(x = advice, y = share, fill = havo_plus)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.1f%%", share)),
    vjust = -0.4, fontface = "bold"
  ) +
  # cut between VMBO-GT-HAVO (8th bar) and HAVO (9th bar)
  geom_vline(xintercept = 8.5, linetype = "dashed", colour = "grey30") +
  # arrow and label above the three HAVO-or-higher bars
  annotate(
    "segment",
    x = 8.6, xend = 11.4, y = 24, yend = 24,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    colour = "#2c7fb8"
  ) +
  annotate(
    "text",
    x = 10, y = 22.8, label = "Final advice for HAVO or higher\n(our outcome variable)",
    colour = "#2c7fb8", fontface = "bold"
  ) +
  scale_fill_manual(values = c("TRUE" = "#74add1", "FALSE" = "Medium Spring Green")) +
  scale_y_continuous(
    breaks = seq(0, 20, 5), limits = c(0, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Final school advice across all Dutch primary schools",
    subtitle = "Share of pupils per secondary-school track, 2024-25",
    x = "Final school advice",
    y = "Share of pupils (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ------------------------------------------------------------------------------
# Second exploratory plot

# Scatterplot: points sized by number of students per school
scatter <- function(x, y, x_lab, y_lab) {
  ggplot(school_data, aes({{ x }}, {{ y }}, size = n_students_school)) +
    geom_point(alpha = 0.3, colour = "#2c7fb8") +
    scale_size_continuous(name = "Students per school", range = c(0.3, 3.5)) +
    guides(size = guide_legend(override.aes = list(alpha = 0.7))) +
    labs(x = x_lab, y = y_lab)
}

# Titles and labs
het <- "Socio-economic heterogeneity"
dis <- "Average socio-economic disadvantage"
adv <- "HAVO or higher advice (%)"

# Create second plot with patchwork
(scatter(spreiding, HAVO_higher, het, adv) +
    scatter(schoolweging, HAVO_higher, dis, adv) +
    scatter(schoolweging, spreiding, dis, het)) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Exploring relationships between socio-economic school context and HAVO advice",
    subtitle = "Two-variable relationships; each dot is one school"
  ) &
  theme_minimal() &
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# Subsequent data preparation

# Investigatiion of schoolweging based on the mean of spreiding:
school_data |>
  mutate(decile = ntile(schoolweging, 15)) |>
  summarise(
    .by = decile,
    min_weging = mean(schoolweging),
    mean_spreiding = mean(spreiding), n = n()
  ) |>
  arrange(decile)

# Trimming of schoolweging (cutting the lowest and highest 1/15 (about 6.7%)),
# and categorize the rest of schoolweging into 10 bins with same sample size.
limits <- quantile(school_data$schoolweging, c(1 / 15, 14 / 15))
school_data <- school_data |>
  mutate(
    weging_bin = factor(
      ntile(
        if_else(
          between(schoolweging, limits[[1]], limits[[2]]),
          schoolweging, NA_real_
        ),
        10
      ),
      ordered = TRUE
    )
  )

# ------------------------------------------------------------------------------
# Third exploratory plot

# --> Filip: Here the plot that explains the trimming and the new bins.

# ------------------------------------------------------------------------------
# Regression











# Clean workspace:
rm(list = ls())

# Packages:
source(here::here("scripts", "00-packages.R"))
install.packages("emmeans")
install.packages("marginaleffects")

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
  mutate(decile = ntile(schoolweging, 10)) |>
  summarise(
    .by = decile,
    min_weging = mean(schoolweging),
    mean_spreiding = mean(spreiding), n = n()
  ) |>
  arrange(decile)

# Trimming of schoolweging (cutting the lowest and highest 10%),
# and categorize the rest of schoolweging into 10 bins with same sample size.
limits <- quantile(school_data$schoolweging, c(1 / 10, 9 / 10))
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

# Model 1
model_1 <- lm(
  HAVO_higher ~ spreiding * weging_bin,
  data = school_data
)
summary(model_1)

# Model 2
model_2 <- lm(
  HAVO_higher ~ spreiding * weging_bin + schoolweging,
  data = school_data
)
summary(model_2)

# Check regression assumptions
old_par <- par(mfrow = c(2, 2))
plot(model_1, which = c(1, 2, 3, 5), ask = FALSE)
par(old_par)
old_par <- par(mfrow = c(2, 2))
plot(model_2, which = c(1, 2, 3, 5), ask = FALSE)
par(old_par)
# --> Similar pattern for both models, so model 1 is chosen.
# --> Non-normality at the tails can be seen. 

# Conventional 95% confidence intervals
confint(emmeans::emtrends(
  model_1, ~ weging_bin, var = "spreiding"
))

# Bin-specific slopes with HC3 robust standard errors
bin_slopes <- emmeans::emtrends(
  model_1,
  ~ weging_bin,
  var = "spreiding",
  vcov. = sandwich::vcovHC(model_1, type = "HC3")
)

# Robust 95% confidence intervals, saved for the final plot
slope_data <- as.data.frame(confint(bin_slopes))
slope_data

# ------------------------------------------------------------------------------
# Final visualization plot bottom left:

# Reusable bin colours (1 = least, 10 = most disadvantaged); edit freely
bin_colours <- c(
  "1" = "#440154", "2" = "#482878", "3" = "#3E4989",
  "4" = "#31688E", "5" = "#26828E", "6" = "#1F9E89",
  "7" = "#35B779", "8" = "#52C569", "9" = "#86D549",
  "10" = "#C2DF23"
)

# Labels for the axis
het <- "Socio-economic heterogeneity"
adv <- "HAVO or higher advice"
dis <- "Average socio-economic disadvantage"

# Bin-specific slopes: conventional vs HC3 robust 95% CIs
marginaleffects::avg_slopes(
  model_1, variables = "spreiding", by = "weging_bin"
)
slope_data <- marginaleffects::avg_slopes(
  model_1, variables = "spreiding", by = "weging_bin", vcov = "HC3"
)

# Fitted lines with HC3 robust 95% bands over each bin's observed range
plot_data <- filter(school_data, !is.na(weging_bin))
pred_data <- plot_data |>
  reframe(
    .by = weging_bin,
    spreiding = seq(min(spreiding), max(spreiding), length.out = 50)
  ) |>
  marginaleffects::predictions(model = model_1, newdata = _, vcov = "HC3")

# Create the bottom left plot:
p_left <- ggplot(plot_data, aes(x = spreiding, y = HAVO_higher)) +
  geom_point(
    aes(size = n_students_school),
    alpha = 0.25, colour = "grey35"
  ) +
  geom_ribbon(
    data = pred_data,
    aes(
      y = estimate, ymin = conf.low, ymax = conf.high,
      fill = weging_bin
    ),
    alpha = 0.25
  ) +
  geom_line(
    data = pred_data,
    aes(y = estimate, colour = weging_bin),
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ weging_bin, ncol = 5,
    labeller = labeller(weging_bin = ~ paste("Bin", .x))
  ) +
  scale_colour_manual(values = bin_colours, guide = "none") +
  scale_fill_manual(values = bin_colours, guide = "none") +
  scale_size_continuous(
    name = "Students per school", range = c(0.3, 3.5)
  ) +
  guides(size = guide_legend(override.aes = list(alpha = 0.7))) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "What the data look like",
    subtitle = "Each dot is one school; line = linear fit with robust 95% CI",
    x = het, y = adv
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

# ------------------------------------------------------------------------------
# Final visualization plot bottom right:

# Heterogeneity slope per bin with HC3 robust 95% CIs
slope_data <- marginaleffects::avg_slopes(
  model_1, variables = "spreiding", by = "weging_bin", vcov = "HC3"
)

# Create the bottom right plot:
p_right <- ggplot(slope_data, aes(x = weging_bin, y = estimate, colour = weging_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    linewidth = 0.8, size = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%+.1f%%", estimate)),
    colour = "black", fontface = "bold",
    hjust = 0, nudge_x = 0.15, size = 3.3
  ) +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.2))) +
  scale_colour_manual(values = bin_colours, guide = "none") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Slopes of socio-economic heterogeneity on advice",
    subtitle = "Slope estimated separately within each disadvantage bin; robust 95% CI",
    x = paste(dis, "(bin)"),
    y = paste0("Slope: change in ", tolower(adv), "\nper unit of ", tolower(het))
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# ------------------------------------------------------------------------------
# Final visualization plot title and subtitle at the top:

# Define title and subtitle:
final_title <- "Does socio-economic heterogeneity within schools relate to HAVO or higher advice?"
final_subtitle <- paste(
  "Research question",
  "Research question?"
)

# ------------------------------------------------------------------------------
# Final visualization legend bins

# Bin summary: range of average disadvantage and number of schools per bin
bin_info <- school_data |>
  filter(!is.na(weging_bin)) |>
  summarise(
    .by = weging_bin,
    lo = min(schoolweging), hi = max(schoolweging), n_schools = n()
  ) |>
  mutate(
    x = as.integer(weging_bin),
    txt = if_else(x <= 6, "white", "black"),
    range = paste(round(lo, 1), round(hi, 1), sep = "-")
  )

# Create the legend
p_legend <- ggplot(bin_info, aes(x = x)) +
  geom_tile(aes(y = 0, fill = weging_bin), width = 0.96, height = 1) +
  geom_text(
    aes(y = 0.25, label = paste("Bin", x), colour = txt),
    fontface = "bold", size = 3.3
  ) +
  geom_text(aes(y = 0, label = range, colour = txt), size = 3.1) +
  geom_text(
    aes(y = -0.25, label = paste(n_schools, "schools"), colour = txt),
    size = 3.1
  ) +
  annotate(
    "text", x = 5.5, y = 0.78, fontface = "bold", size = 3.5,
    label = "Schools grouped into 10 equal-sized bins of average socio-economic disadvantage"
  ) +
  annotate(
    "text", x = 0.52, y = 0.78, hjust = 0, fontface = "italic", size = 3.3,
    label = "\u2190 Less disadvantaged"
  ) +
  annotate(
    "text", x = 10.48, y = 0.78, hjust = 1, fontface = "italic", size = 3.3,
    label = "More disadvantaged \u2192"
  ) +
  annotate(
    "text", x = 5.5, y = -0.68, size = 2.8, colour = "grey40",
    label = paste(
      "(Schools with the 10% lowest and 10% highest average disadvantage",
      "are excluded, because ...)"
    )
  ) +
  scale_fill_manual(values = bin_colours) +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0.5, 10.5), ylim = c(-0.8, 0.95)) +
  theme_void() +
  theme(legend.position = "none")
p_legend

# ------------------------------------------------------------------------------
# Bottom row: scatterplot (left), slope plot (right)
bottom_row <- (p_left | p_right) + plot_layout(widths = c(3, 2))

# Create final plot:
final_plot <- p_legend / bottom_row +
  plot_layout(heights = c(1, 6.5)) +
  plot_annotation(
    title = final_title,
    subtitle = final_subtitle,
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(colour = "grey30", size = 11)
    )
  )
final_plot







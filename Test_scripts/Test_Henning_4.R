# Clean workspace:
rm(list = ls())

# Packages:
packages <- c("marginaleffects", "patchwork")
for (i in packages) {
  if (!require(i, character.only = TRUE)) install.packages(i)
  library(i, character.only = TRUE)
  print(paste("Is package", i, "loaded?", i %in% loadedNamespaces()))
}
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
  mutate(decile = ntile(schoolweging, 10)) |>
  summarise(
    .by = decile,
    min_weging = mean(schoolweging),
    mean_spreiding = mean(spreiding), n = n()
  ) |>
  arrange(decile)

# Keep all schools for the plot that explains the trimming
school_data_full <- school_data

# Trimming of schoolweging (cutting the lowest and highest 10%),
# and categorize the rest of schoolweging into 10 bins with same sample size.
# Centered the continuous varables for the subsequent interaction models.
limits <- quantile(school_data$schoolweging, c(1 / 10, 9 / 10))
school_data <- school_data |>
  filter(between(schoolweging, limits[[1]], limits[[2]])) |>
  mutate(
    weging_trimmed = schoolweging,
    weging_bin = factor(ntile(weging_trimmed, 10), ordered = TRUE),
    weging_trim_c = weging_trimmed - mean(weging_trimmed),
    spreiding_c = spreiding - mean(spreiding)
  )

# ------------------------------------------------------------------------------
# Third exploratory plot

# --> Filip: Here the plot that explains the trimming and the new bins.

# ------------------------------------------------------------------------------
# Regressions

# Model 1
model_1 <- lm(
  HAVO_higher ~ spreiding_c * weging_bin, data = school_data)
summary(model_1)

# Model 2
model_2 <- lm(HAVO_higher ~ spreiding_c * weging_trim_c, data = school_data)
summary(model_2)

# Check regression assumptions for models
old_par <- par(mfrow = c(2, 2))
plot(model_1, which = c(1, 2, 3, 5), ask = FALSE)
par(old_par)
old_par <- par(mfrow = c(2, 2))
plot(model_2, which = c(1, 2, 3, 5), ask = FALSE)
par(old_par)
# --> Similar pattern for both models, so model 1 is chosen.
# --> Non-normality at the tails can be seen. 

# ------------------------------------------------------------------------------
# Final visualization - shared settings

# Reusable bin colours (1 = least, 10 = most disadvantaged); edit freely
bin_colours <- c(
  "1"  = "#071440",  # deep navy
  "2"  = "#10286E",  # dark royal blue
  "3"  = "#1A459F",  # royal blue
  "4"  = "#2563C4",  # strong blue
  "5"  = "#3F86DB",  # sky blue 
  "6"  = "#4A9C8A",  # muted teal
  "7"  = "#5BA777",  # muted green
  "8"  = "#78B067",  # soft green
  "9"  = "#9BB95E",  # olive green
  "10" = "#BCC45C"   # muted yellow-green
)

# Labels for the axis
het <- "Socio-economic heterogeneity"
adv <- "HAVO+ advice"
dis <- "Avg. socio-economic disadvantage"

# Shared legend style, so that all legends have the same text and key size
legend_theme <- theme(
  legend.position = "bottom",
  legend.title = element_text(size = 8.5),
  legend.text = element_text(size = 8.5),
  legend.key.height = unit(0.5, "cm")
)

# ------------------------------------------------------------------------------
# Final visualization plot - bottom left

# Bin-specific slopes: conventional vs HC3 robust 95% CIs
slope_data <- marginaleffects::avg_slopes(
  model_1, variables = "spreiding_c", by = "weging_bin"
)

# Fitted lines with HC3 robust 95% bands over each bin's observed range
spreiding_mean <- mean(school_data$spreiding)
pred_data <- school_data |>
  reframe(
    .by = weging_bin,
    spreiding = seq(min(spreiding), max(spreiding), length.out = 50)
  ) |>
  mutate(spreiding_c = spreiding - spreiding_mean) |>
  marginaleffects::predictions(model = model_1, newdata = _)

# Mean advice per bin: a flat line at this level means "no relationship"
bin_means <- school_data |>
  summarise(.by = weging_bin, mean_advice = mean(HAVO_higher))

# Legend text for the dashed lines (used in both bottom plots)
zero_label <- "No relationship (zero slope)"

# Create the bottom left plot:
p_left <- ggplot(school_data, aes(x = spreiding, y = HAVO_higher)) +
  geom_point(
    aes(size = n_students_school),
    alpha = 0.15, colour = "grey35"
  ) +
  geom_hline(
    data = bin_means,
    aes(yintercept = mean_advice, linetype = zero_label),
    colour = "grey30", linewidth = 0.5
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
    name = "Students per school: Models unweighted; point size for illustration",
    range = c(0.3, 2.5)
  ) +
  scale_linetype_manual(name = NULL, values = "dashed") +
  guides(
    size = guide_legend(
      override.aes = list(alpha = 0.7),
      theme = theme(legend.title.position = "left")
    ),
    linetype = guide_legend(
      theme = theme(legend.key.width = unit(1.2, "cm"))
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "What the data look like",
    subtitle = "Each dot is one school; line = linear fit with 95% CI",
    x = het, y = adv
  ) +
  theme_minimal() +
  legend_theme +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

# ------------------------------------------------------------------------------
# Final visualization plot - bottom right

# Data for the dashed zero line, so that it gets its own legend entry
zero_line <- data.frame(
  yintercept = 0,
  explanation = "If CI includes zero = slope not significant"
)

# Create the bottom right plot:
p_right <- ggplot(
  slope_data,
  aes(x = weging_bin, y = estimate, colour = weging_bin)
) +
  geom_hline(
    data = zero_line,
    aes(yintercept = yintercept, linetype = explanation),
    colour = "grey50", inherit.aes = FALSE
  ) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    linewidth = 0.8, size = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%+.1f pp", estimate)),
    colour = "black", fontface = "bold",
    hjust = 0, nudge_x = 0.15, size = 3.3
  ) +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.2))) +
  scale_colour_manual(values = bin_colours, guide = "none") +
  scale_linetype_manual(name = NULL, values = "dashed") +
  guides(
    linetype = guide_legend(
      theme = theme(legend.key.width = unit(1.2, "cm"))
    )
  ) +
  scale_y_continuous(
    breaks = seq(-2, 6, 2),
    labels = scales::label_number(style_positive = "plus", suffix = " pp")
  ) +
  coord_cartesian(ylim = c(-2, 6)) +
  labs(
    title = "Slopes of socio-economic heterogeneity on advice",
    subtitle = paste(
      "Slope estimated separately within each disadvantage bin;",
      "95% CI"
    ),
    x = paste(dis, "(Bin)"),
    y = paste0("Change in ", adv, " per unit of ", tolower(het))
  ) +
  theme_minimal() +
  legend_theme +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ------------------------------------------------------------------------------
# Final visualization - plot title and subtitle at the top:

# Define title and subtitle:
final_title <- "Mixed schools, higher advice: the pattern holds mainly where disadvantage is low"
final_subtitle <- paste(
  "Is within-school heterogeneity in students' socioeconomic disadvantage related ",
  "to the share of students receiving a HAVO-or-higher secondary-school advice, ",
  "controlling for the school's average level of student socioeconomic disadvantage?"
)

# ------------------------------------------------------------------------------
# Final visualization legend bins 

# Height of the legend panel in cm (fixed; used in plot_layout() below)
top_cm <- 2.4
# Approximate cm per y-unit of the panel (y runs from -1 to 1)
cm_per_y <- (top_cm - 0.15) / 2

# Strip geometry (x: 0-100 across the panel; y: -1 to 1)
strip_x0 <- 41
tile_w <- 5.7
tile_h <- 0.8
tile_yc <- 0.02
y_top <- 0.82
y_bottom <- -0.74

# Bin summary: range of average disadvantage and number of schools per bin
bin_info <- school_data |>
  summarise(
    .by = weging_bin,
    lo = min(schoolweging), hi = max(schoolweging), n_schools = n()
  ) |>
  mutate(
    x = as.integer(weging_bin),
    tile_x = strip_x0 + (x - 0.5) * tile_w,
    txt = if_else(x <= 6, "white", "black"),
    range = paste(round(lo, 1), round(hi, 1), sep = "-")
  )

# One note row below the strip: tile ranges, schools per bin, trimming
strip_note <- paste0(
  "Tile numbers = range of average disadvantage. ",
  paste(unique(range(bin_info$n_schools)), collapse = "-"),
  " schools per bin. ",
  "(Lowest and highest 10% of schools excluded, because ...)"
)

# Explanation box: bold term (left column) + short sentence (right column)
box_font <- 3.2
box_items <- data.frame(
  term = c(
    "HAVO+ advice:",
    "Avg. Socio-economic disadvantage:",
    "Socio-economic heterogeneity:"
  ),
  text = c(
    "Share of pupils with a final advice of HAVO, HAVO/VWO or VWO.",
    "School average (CBS); higher = more disadvantaged. Forms the bins.",
    "Differences between pupils within a school; higher = more mixed."
  )
)
box_items$text <- stringr::str_wrap(box_items$text, width = 66)

# Spread the items evenly over the same height as the right-hand strip
line_h <- box_font * ggplot2::.pt * 1.2 * 0.95 / 72 * 2.54 / cm_per_y
box_items$n_lines <- stringr::str_count(box_items$text, "\n") + 1
text_h <- sum(box_items$n_lines) * line_h
item_gap <- max((y_top - y_bottom - text_h) / (nrow(box_items) - 1), 0.04)
box_items$y <- y_top -
  c(0, cumsum(head(box_items$n_lines * line_h + item_gap, -1)))

# Create the legend panel: one box around explanation and bin strip
p_legend <- ggplot(bin_info) +
  annotate(
    "rect",
    xmin = 0.4, xmax = 99.6, ymin = -0.94, ymax = 0.94,
    fill = "grey97", colour = "grey30", linewidth = 0.3
  ) +
  # explanation text
  geom_text(
    data = box_items,
    aes(x = 2, y = y, label = term),
    hjust = 0, vjust = 1, fontface = "bold", size = box_font,
    colour = "grey15"
  ) +
  geom_text(
    data = box_items,
    aes(x = 14, y = y, label = text),
    hjust = 0, vjust = 1, size = box_font, lineheight = 0.95,
    colour = "grey15"
  ) +
  # compressed bin strip
  geom_tile(
    aes(x = tile_x, y = tile_yc, fill = weging_bin),
    width = tile_w * 0.94, height = tile_h
  ) +
  geom_text(
    aes(x = tile_x, y = tile_yc + 0.17, label = paste("Bin", x), colour = txt),
    fontface = "bold", size = 3.2
  ) +
  geom_text(
    aes(x = tile_x, y = tile_yc - 0.17, label = range, colour = txt),
    size = 2.9
  ) +
  annotate(
    "text", x = strip_x0 + 5 * tile_w, y = 0.66, fontface = "bold",
    size = 3.3, colour = "grey15",
    label = "10 equal-sized bins of average socio-economic disadvantage"
  ) +
  annotate(
    "text", x = strip_x0, y = 0.66, hjust = 0, fontface = "italic",
    size = 3.1, colour = "grey15",
    label = "\u2190 Less disadvantaged"
  ) +
  annotate(
    "text", x = strip_x0 + 10 * tile_w, y = 0.66, hjust = 1,
    fontface = "italic", size = 3.1, colour = "grey15",
    label = "More disadvantaged \u2192"
  ) +
  # one note row: tile ranges, schools per bin, trimming
  annotate(
    "text", x = strip_x0 + 5 * tile_w, y = -0.62, size = 2.7,
    colour = "grey30", label = strip_note
  ) +
  scale_fill_manual(values = bin_colours) +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0, 100), ylim = c(-1, 1), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  )

# ------------------------------------------------------------------------------
# Final plot

# Bottom row: scatterplot (left), slope plot (right)
bottom_row <- (p_left | p_right) + plot_layout(widths = c(3, 2))

# Create final plot:
final_plot <- p_legend / bottom_row +
  plot_layout(heights = unit(c(top_cm, 1), c("cm", "null"))) +
  plot_annotation(
    title = final_title,
    subtitle = final_subtitle,
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(colour = "grey30", size = 11)
    )
  )
final_plot

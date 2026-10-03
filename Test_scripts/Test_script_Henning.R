# RQ5: Does within-school heterogeneity in student backgrounds (spreiding)
# predict the share of students receiving a HAVO-or-higher secondary-school
# advice, after controlling for the average level of disadvantage (schoolweging)?

library(tidyverse)

# ── 1. Schoolweging ───────────────────────────────────────────────────────────
# Extract INSTELLINGSCODE from OVT (format "00AP|C1") and aggregate to school
# level, weighting by number of pupils.
weging <- schoolweging |>
  mutate(INSTELLINGSCODE = str_extract(OVT, "^[^|]+")) |>
  filter(!is.na(schoolweging), !is.na(spreiding), aantal_leerlingen > 0) |>
  group_by(INSTELLINGSCODE) |>
  summarise(
    schoolweging = weighted.mean(schoolweging, aantal_leerlingen),
    spreiding    = weighted.mean(spreiding,    aantal_leerlingen),
    .groups = "drop"
  )

# ── 2. School advice → pct_havo_plus ─────────────────────────────────────────
# Replace "<5" with 2.5, convert to numeric, then compute the share of
# students advised for HAVO or higher.
parse_count <- function(x) {
  as.numeric(if_else(x == "<5", "2.5", x))
}

adviezen <- schooladviezen |>
  filter(SOORT_PO == "Bo") |>
  mutate(across(VSO:ADVIES_NIET_MOGELIJK, parse_count)) |>
  mutate(
    total     = rowSums(across(VSO:ADVIES_NIET_MOGELIJK), na.rm = TRUE),
    havo_plus = HAVO + HAVO_VWO + VWO
  ) |>
  filter(total > 0) |>
  mutate(pct_havo_plus = havo_plus / total) |>
  select(INSTELLINGSCODE, pct_havo_plus)

# ── 3. Join ───────────────────────────────────────────────────────────────────
plot_data <- adviezen |>
  inner_join(weging, by = "INSTELLINGSCODE") |>
  filter(
    is.finite(pct_havo_plus),
    is.finite(schoolweging),
    is.finite(spreiding)
  ) |>
  mutate(
    spreiding_q = cut(
      spreiding,
      breaks = quantile(spreiding, probs = 0:4 / 4, na.rm = TRUE),
      labels = c("Q1 (low spreiding)", "Q2", "Q3", "Q4 (high spreiding)"),
      include.lowest = TRUE
    )
  )

# ── 4. Plots ──────────────────────────────────────────────────────────────────
theme_set(theme_minimal(base_size = 12))

# Option 1: Binned scatter — bin schoolweging, plot mean pct_havo_plus per bin,
# color by mean spreiding within bin.
binned <- plot_data |>
  mutate(weging_bin = cut_interval(schoolweging, n = 15)) |>
  group_by(weging_bin) |>
  summarise(
    mean_havo     = mean(pct_havo_plus),
    mean_spreiding = mean(spreiding),
    .groups = "drop"
  )

p1 <- ggplot(binned, aes(x = weging_bin, y = mean_havo, color = mean_spreiding)) +
  geom_point(size = 3) +
  scale_color_viridis_c(name = "Mean spreiding") +
  labs(
    title = "Option 1: Binned scatter",
    x = "Schoolweging (binned)", y = "Share advised HAVO or higher"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Option 2: Scatter with 2D density contours — all schools as transparent
# points, colored by spreiding, with contour overlay.
p2 <- ggplot(
  plot_data,
  aes(x = schoolweging, y = pct_havo_plus, color = spreiding)
) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_density_2d(color = "grey40", linewidth = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  scale_color_viridis_c(name = "Spreiding") +
  labs(
    title = "Option 2: Scatter with density contours",
    x = "Schoolweging", y = "Share advised HAVO or higher"
  )

# Option 3: Faceted scatter by spreiding quartile — one panel per quartile,
# regression line with confidence band per panel.
p3 <- ggplot(plot_data, aes(x = schoolweging, y = pct_havo_plus)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(vars(spreiding_q)) +
  labs(
    title = "Option 3: Faceted scatter by spreiding quartile",
    x = "Schoolweging", y = "Share advised HAVO or higher"
  )

# Option 4: Slope graph — mean pct_havo_plus per schoolweging quartile,
# one line per spreiding quartile.
slope_data <- plot_data |>
  mutate(
    weging_q = cut(
      schoolweging,
      breaks = quantile(schoolweging, probs = 0:4 / 4, na.rm = TRUE),
      labels = c("Q1 (least disadvantaged)", "Q2", "Q3", "Q4 (most disadvantaged)"),
      include.lowest = TRUE
    )
  ) |>
  group_by(spreiding_q, weging_q) |>
  summarise(mean_havo = mean(pct_havo_plus), .groups = "drop")

p4 <- ggplot(
  slope_data,
  aes(x = weging_q, y = mean_havo, color = spreiding_q, group = spreiding_q)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_viridis_d(name = "Spreiding quartile") +
  labs(
    title = "Option 5: Slope graph",
    x = "Schoolweging quartile", y = "Share advised HAVO or higher"
  )

# Print all five
print(p1)
print(p2)
print(p3) # I would choose this plot for feedback!
print(p4)

# Interpretation of the plot 3 results:

# Given the mean disadvantage of a school, the variance in student backgrounds (spreiding) 
# — i.e. whether the level of disadvantage within a school is pretty homogeneous or pretty 
# heterogeneous — does not relevantly affect teachers' final secondary-school advice. 
# The four panels show near-identical negative slopes: more disadvantaged schools give fewer 
# HAVO-or-higher advices, regardless of how mixed their student population is. This means 
# there is no relevant big-fish-little-pond effect at the school level: students are advised 
# similarly for secondary education independent of the background level of their classmates.

# This could be because the doorstroomtoets standardizes the process and the final advice 
# aligns closely with the test result — leaving little room for classroom composition 
# to influence teacher judgement. Alternatively, the effect may not be detectable at 
# school level, since spreiding measures population composition rather than classroom dynamics: 
# within-school streaming could mean students never actually experience the mixed environment 
# that the hypothesis requires.

# Data
source(here::here("scripts", "01-get-data.R"))

# Head data
head(eindscores)
head(schooladviezen)
head(schoolweging)
head(referentieniveaus)

# Spreiding plots:
# 1. Clean data frame ---------------------------------------------------------

advice_tracks <- c(
  "VSO", "PRO", "VMBO_B", "VMBO_B_K", "VMBO_K", "VMBO_K_GT",
  "VMBO_GT", "VMBO_GT_HAVO", "HAVO", "HAVO_VWO", "VWO"
)
havo_plus <- c("HAVO", "HAVO_VWO", "VWO")
table(eindscores$PROVINCIE)

# Replace the suppressed "<5" by the midpoint 2.5
parse_count <- function(x) {
  x <- as.character(x)
  as.numeric(if_else(x == "<5", "2.5", x))
}

advice <- schooladviezen |>
  mutate(across(all_of(advice_tracks), parse_count)) |>
  mutate(
    n_advice = rowSums(pick(all_of(advice_tracks)), na.rm = TRUE),
    n_havo_plus = rowSums(pick(all_of(havo_plus)), na.rm = TRUE),
    havo_higher = 100 * n_havo_plus / n_advice
  ) |>
  select(
    INSTELLINGSCODE, VESTIGINGSCODE, denominatie = DENOMINATIE_VESTIGING,
    n_advice, havo_higher
  )

weging <- schoolweging |>
  separate_wider_delim(OVT, "|", names = c("INSTELLINGSCODE", "loc")) |>
  mutate(VESTIGINGSCODE = sprintf("%02d", parse_number(loc) - 1)) |>
  select(INSTELLINGSCODE, VESTIGINGSCODE, schoolweging, spreiding)

school_data <- advice |>
  inner_join(weging, by = c("INSTELLINGSCODE", "VESTIGINGSCODE")) |>
  filter(n_advice > 0) |>
  drop_na(havo_higher, schoolweging, spreiding)

# Join check: how many advice rows found no schoolweging match?http://127.0.0.1:39030/graphics/plot_zoom_png?width=1332&height=829
anti_join(advice, weging, by = c("INSTELLINGSCODE", "VESTIGINGSCODE")) |>
  nrow()

# Trim the 10% lowest and 10% highest schoolweging; create 10 equal-sized bins
limits <- quantile(school_data$schoolweging, c(0.1, 0.9))

trimmed <- school_data |>
  filter(between(schoolweging, limits[[1]], limits[[2]])) |>
  mutate(weging_bin = factor(ntile(schoolweging, 10)))

# 2. Plot 1a: spreiding vs. schoolweging --------------------------------------

ggplot(trimmed, aes(schoolweging, spreiding)) +
  geom_point(alpha = 0.3, size = 1) +
  labs(
    x = "Schoolweging (mean disadvantage)",
    y = "Spreiding (SD of disadvantage)"
  ) +
  theme_minimal()

# 3. Plot 1b: HAVO-or-higher vs. spreiding, one line per schoolweging bin ------

ggplot(trimmed, aes(spreiding, havo_higher, colour = weging_bin)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  scale_colour_viridis_d(name = "Schoolweging bin\n(1 = least disadvantaged)") +
  labs(x = "Spreiding", y = "HAVO or higher advice (%)") +
  theme_minimal()

# 4. Plot 2: residuals of havo_higher ~ schoolweging vs. spreiding -------------

model <- lm(havo_higher ~ schoolweging, data = trimmed)

mod2 <- lm(resid(model) ~ trimmed$spreiding)
summary(mod2)


trimmed <- trimmed |>
  mutate(havo_resid = resid(model))

ggplot(trimmed, aes(spreiding, havo_resid)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", colour = "firebrick") +
  labs(
    x = "Spreiding",
    y = "HAVO-or-higher advice (%), residual after controlling for schoolweging"
  ) +
  theme_minimal()

# 5. Plot 3: same residuals, one line per denomination -------------------------

denominations <- c(
  "Openbaar", "Rooms-Katholiek", "Protestants-Christelijk", "Algemeen bijzonder"
)

trimmed |>
  filter(denominatie %in% denominations) |>
  mutate(denominatie = factor(denominatie, levels = denominations)) |>
  ggplot(aes(spreiding, havo_resid, colour = denominatie)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  scale_colour_brewer(palette = "Dark2", name = "Denomination") +
  labs(
    x = "Spreiding",
    y = "HAVO-or-higher advice (%), residual after controlling for schoolweging"
  ) +
  theme_minimal()

# 5. Plot 4: Advanced plots with 10 bins -------------------------

bin_info <- trimmed |>
  group_by(weging_bin) |>
  summarise(
    w_min = min(schoolweging),
    w_max = max(schoolweging),
    mean_havo = mean(havo_higher),
    median_pupils = round(median(n_advice))
  ) |>
  mutate(
    bin_label = str_glue(
      "Bin {weging_bin}: schoolweging {round(w_min, 1)}-{round(w_max, 1)}\n",
      "median {median_pupils} pupils"
    ),
    bin_label = fct_inorder(as.character(bin_label))
  )

trimmed |>
  left_join(bin_info, by = "weging_bin") |>
  ggplot(aes(spreiding, havo_higher)) +
  geom_hline(
    aes(yintercept = mean_havo),
    linetype = "dashed", colour = "grey40"
  ) +
  geom_point(aes(size = n_advice), alpha = 0.2, colour = "grey30") +
  geom_smooth(
    method = "lm", level = 0.95,
    colour = "firebrick", fill = "firebrick", alpha = 0.2, linewidth = 0.8
  ) +
  facet_wrap(~bin_label, nrow = 2) +
  scale_size_area(name = "Pupils\nwith advice", max_size = 3) +
  labs(x = "Spreiding", y = "HAVO or higher advice (%)") +
  theme_minimal()

slopes <- trimmed |>
  group_by(weging_bin) |>
  group_modify(
    \(d, key) broom::tidy(
      lm(havo_higher ~ spreiding, data = d),
      conf.int = TRUE
    )
  ) |>
  ungroup() |>
  filter(term == "spreiding")

ggplot(slopes, aes(weging_bin, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_pointrange(colour = "firebrick") +
  labs(
    x = "Schoolweging bin (1 = least disadvantaged)",
    y = "Slope (percentage points per unit of spreiding)"
  ) +
  theme_minimal()

# _____________________________________________________________________________
# Possible patchwork plot:

# ---- Packages ---------------------------------------------------------------
library(tidyverse)
library(patchwork)

# Assumes `schooladviezen` and `schoolweging` are already loaded
# (scripts/01-get-data.R).

theme_set(theme_minimal(base_size = 12))
theme_update(
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"),
  legend.position = "bottom"
)

# ---- 1. Data preparation ----------------------------------------------------
advice_tracks <- c(
  "VSO", "PRO", "VMBO_B", "VMBO_B_K", "VMBO_K", "VMBO_K_GT",
  "VMBO_GT", "VMBO_GT_HAVO", "HAVO", "HAVO_VWO", "VWO"
)
havo_plus <- c("HAVO", "HAVO_VWO", "VWO")
join_keys <- c("INSTELLINGSCODE", "VESTIGINGSCODE")

# Hidden counts "<5" are replaced by the midpoint 2.5 (check 1 and 4 as well)
parse_count <- function(x) {
  x <- as.character(x)
  as.numeric(if_else(x == "<5", "2.5", x))
}

advice <- schooladviezen |>
  mutate(across(all_of(advice_tracks), parse_count)) |>
  mutate(
    n_advice = rowSums(pick(all_of(advice_tracks)), na.rm = TRUE),
    n_havo_plus = rowSums(pick(all_of(havo_plus)), na.rm = TRUE),
    havo_higher = 100 * n_havo_plus / n_advice
  ) |>
  select(
    all_of(join_keys), denominatie = DENOMINATIE_VESTIGING,
    n_advice, havo_higher
  )

# Assumption: OVT suffix "C1" corresponds to VESTIGINGSCODE "00", "C2" to "01"
weging <- schoolweging |>
  separate_wider_delim(OVT, "|", names = c("INSTELLINGSCODE", "loc")) |>
  mutate(VESTIGINGSCODE = sprintf("%02d", parse_number(loc) - 1)) |>
  select(all_of(join_keys), schoolweging, spreiding)

n_unmatched <- nrow(anti_join(advice, weging, by = join_keys))
message("Advice rows without a schoolweging match: ", n_unmatched)

school_data <- advice |>
  inner_join(weging, by = join_keys) |>
  filter(n_advice > 0) |>
  drop_na(havo_higher, schoolweging, spreiding)

# Trim the 10% lowest and 10% highest schoolweging, then make 10 equal bins
limits <- quantile(school_data$schoolweging, c(0.1, 0.9))

trimmed <- school_data |>
  filter(between(schoolweging, limits[[1]], limits[[2]])) |>
  mutate(weging_bin = factor(ntile(schoolweging, 10)))

# One colour per bin, shared by all panels (colourblind-safe, sequential)
bin_cols <- scales::viridis_pal(end = 0.92)(10)
names(bin_cols) <- levels(trimmed$weging_bin)

bin_info <- trimmed |>
  group_by(weging_bin) |>
  summarise(
    w_min = min(schoolweging),
    w_max = max(schoolweging),
    mean_havo = mean(havo_higher),
    median_pupils = round(median(n_advice))
  ) |>
  mutate(
    strip_label = str_glue(
      "Bin {weging_bin}\n{round(w_min, 1)}-{round(w_max, 1)}\n",
      "median {median_pupils} pupils"
    ),
    text_col = if_else(as.integer(weging_bin) <= 6, "white", "black")
  )

trimmed <- left_join(
  trimmed, select(bin_info, weging_bin, mean_havo),
  by = "weging_bin"
)

# ---- 2. Statistics ----------------------------------------------------------
# Slope of spreiding within each bin, with 95% confidence intervals
slopes <- trimmed |>
  group_by(weging_bin) |>
  group_modify(
    \(d, key) broom::tidy(
      lm(havo_higher ~ spreiding, data = d),
      conf.int = TRUE
    )
  ) |>
  ungroup() |>
  filter(term == "spreiding") |>
  mutate(clear = conf.low > 0 | conf.high < 0)

# Overall model: quadratic control for schoolweging, with and without interaction
fit_main <- lm(havo_higher ~ poly(schoolweging, 2) + spreiding, data = trimmed)
fit_int <- lm(havo_higher ~ poly(schoolweging, 2) * spreiding, data = trimmed)

p_int <- anova(fit_main, fit_int)$`Pr(>F)`[2]
p_text <- if (p_int < 0.001) "p < 0.001" else sprintf("p = %.3f", p_int)

avg_slope <- broom::tidy(fit_main, conf.int = TRUE) |>
  filter(term == "spreiding")

spreiding_q <- quantile(trimmed$spreiding, c(0.25, 0.75))

# ---- 3. Panels --------------------------------------------------------------
# 3a. Explanation strip for the 10 schoolweging bins
p_strip <- ggplot(bin_info, aes(x = as.integer(weging_bin), y = 0)) +
  geom_tile(aes(fill = weging_bin), width = 0.96, height = 0.9) +
  geom_text(
    aes(label = strip_label, colour = text_col),
    size = 3.4, lineheight = 0.95
  ) +
  annotate(
    "text", x = 0.52, y = 0.65, hjust = 0, size = 3.6, fontface = "italic",
    label = "\u2190 Less disadvantaged"
  ) +
  annotate(
    "text", x = 10.48, y = 0.65, hjust = 1, size = 3.6, fontface = "italic",
    label = "More disadvantaged \u2192"
  ) +
  annotate(
    "text", x = 5.5, y = 0.65, size = 3.8, fontface = "bold",
    label = "Schools grouped into 10 equal-sized bins of average disadvantage"
  ) +
  scale_fill_manual(values = bin_cols, guide = "none") +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0.5, 10.5), ylim = c(-0.5, 0.85)) +
  theme_void()

# 3b. Main result: slope of spreiding per bin
p_slopes <- ggplot(
  slopes,
  aes(weging_bin, estimate, ymin = conf.low, ymax = conf.high,
      colour = weging_bin, alpha = clear)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_pointrange(linewidth = 0.9, size = 0.8) +
  geom_text(
    aes(label = sprintf("%+.1f", estimate)),
    nudge_x = 0.32, size = 3.3, colour = "grey20", show.legend = FALSE
  ) +
  annotate(
    "text", x = 0.55, y = 0, label = "no association", hjust = 0,
    vjust = 1.6, size = 3.2, colour = "grey40"
  ) +
  scale_colour_manual(values = bin_cols, guide = "none") +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.35),
    labels = c("TRUE" = "CI excludes 0", "FALSE" = "CI includes 0"),
    name = NULL
  ) +
  labs(
    title = "How strong is the link?",
    subtitle = "Effect of spreiding within each bin (95% CI)",
    x = "Schoolweging bin (1 = least disadvantaged)",
    y = "Change in HAVO-or-higher advice (percentage points)\nper +1 spreiding"
  )

# 3c. Supporting evidence: the data behind each slope
p_facets <- ggplot(trimmed, aes(spreiding, havo_higher)) +
  geom_hline(
    aes(yintercept = mean_havo), linetype = "dashed", colour = "grey50"
  ) +
  geom_point(aes(size = n_advice), alpha = 0.2, colour = "grey30") +
  geom_smooth(
    aes(colour = weging_bin, fill = weging_bin),
    method = "lm", level = 0.95, alpha = 0.25, linewidth = 0.9
  ) +
  facet_wrap(
    ~weging_bin, nrow = 2,
    labeller = as_labeller(\(x) str_c("Bin ", x))
  ) +
  scale_colour_manual(values = bin_cols, guide = "none") +
  scale_fill_manual(values = bin_cols, guide = "none") +
  scale_size_area(name = "Pupils with advice", max_size = 3) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "What the data look like",
    subtitle = "Each dot is a school; line = linear fit with 95% CI",
    x = "Spreiding (SD of pupils' disadvantage within the school)",
    y = "HAVO-or-higher advice (%)"
  ) +
  theme(strip.text = element_text(face = "bold"))

# ---- 4. Combine with patchwork ---------------------------------------------
caption_text <- paste0(
  "Model (OLS, quadratic control for schoolweging): on average ",
  sprintf("%+.1f", avg_slope$estimate),
  " percentage points per +1 spreiding (95% CI ",
  sprintf("%.1f", avg_slope$conf.low), " to ",
  sprintf("%.1f", avg_slope$conf.high), "); ",
  "interaction with schoolweging: ", p_text, ".\n",
  "The middle half of schools has spreiding between ",
  sprintf("%.1f", spreiding_q[[1]]), " and ",
  sprintf("%.1f", spreiding_q[[2]]), ". ",
  "Hidden counts ('<5') set to 2.5; 10% lowest and highest schoolweging ",
  "excluded. Dashed lines in the right panels = bin mean.\n",
  "Source: DUO and Onderwijsinspectie, school year 2024-25. ",
  "School-level associations, not causal effects for individual pupils."
)

subtitle_text <- paste0(
  "Research question: at the same average disadvantage of the pupils ",
  "(schoolweging), do Dutch primary schools with more variation in pupils' ",
  "disadvantage (spreiding)\n",
  "give a larger share of HAVO-or-higher school advices? ",
  "n = ", nrow(trimmed), " schools."
)

bottom <- p_slopes + p_facets + plot_layout(widths = c(2, 3))

final_plot <- p_strip / bottom +
  plot_layout(heights = c(1, 6)) +
  plot_annotation(
    title = paste0(
      "Mixed pupil populations go with more HAVO-or-higher advice, ",
      "mainly in less disadvantaged schools"
    ),
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 12, colour = "grey25"),
      plot.caption = element_text(size = 9, hjust = 0, colour = "grey30")
    )
  )

final_plot

# In the Rmd chunk header, use e.g.: fig.width = 16, fig.height = 9,
# out.width = "100%". To save separately:
# ggsave("report/final-plot.png", final_plot, width = 16, height = 9, dpi = 300)





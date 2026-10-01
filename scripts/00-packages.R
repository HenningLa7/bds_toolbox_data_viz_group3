# 00-packages.R
#
# Installs (only if missing) and loads every package you need for this
# project: downloading and reading the DUO data (01-get-data.R), and
# building static and interactive plots in the report. Run this first -
# the .Rmd files in report/ source it at the top, so knitting a report runs
# it for you.
#
# If you add a package to your project, add it to the list below instead of
# calling install.packages() somewhere in the report - that way everyone in
# your group (and whoever grades your project) gets the same setup from one
# script.

# If your R has never installed a package before, install.packages() doesn't
# know which download server (a "CRAN mirror") to use. RStudio usually sets
# one for you, but if you're running this from a plain R console, the line
# below picks one so the installs don't fail with a confusing error.
no_cran_mirror <- is.null(getOption("repos")) ||
  identical(getOption("repos")[["CRAN"]], "@CRAN@")
if (no_cran_mirror) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

packages <- c(
  "here", # file paths that work from the project root AND from report/
  "readODS", # reads the schoolweging .ods spreadsheet
  "cowplot", # combining several ggplots into one figure (plot_grid())
  "scales", # nicer axis labels (percentages, thousands separators)
  "plotly", # interactive plots - ggplotly() turns a ggplot interactive
  "htmlwidgets", # saving an interactive plot as its own .html file
  "lintr", # code style check before submitting
  "rmarkdown", # knitting the report
  "tidyverse" # readr, dplyr, tidyr, ggplot2, stringr, ... - loaded last
  # so its functions win any name clash with the packages above
)

missing <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) install.packages(missing)

# rmarkdown only needs to be installed (RStudio uses it to knit), not loaded
for (pkg in setdiff(packages, "rmarkdown")) {
  library(pkg, character.only = TRUE)
}

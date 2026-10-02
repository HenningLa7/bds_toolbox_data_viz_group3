# get-data.R
#
# This script downloads everything we need to dig into the doorstroomtoets
# fairness debate (see doorstroomtoetsen-context.md in this same folder for
# the full story of what that debate actually is, and why the Volkskrant
# went looking at this data in the first place).
#
# We need FOUR files in total, and it's worth understanding why each one
# is here before you run this:
#
#   1. eindscores        - It has the average doorstroomtoets score per school, 
#                           per test provider (IEP, Route8, DIA, AMN, DOE, LIB).
#                         
#
#   2. referentieniveaus  - how many pupils
#                           per school actually reached the 1F/1S/2F reference
#                           levels for taal (language) and rekenen (maths).
#                           This matters because the six providers' raw scores
#                           live on six completely different number scales
#                           (an IEP score and an AMN score are not the same
#                           kind of number at all) - reference levels are the
#                           one outcome that's actually meant to mean the same
#                           thing regardless of which test a school used.
#
#   3. schooladviezen     - the secondary-school advice each school's pupils
#                           ended up with (PRO, VMBO, HAVO, VWO, ...). 
#
#   4. schoolweging       - schoolweging is a school-level score for how
#                           disadvantaged a school's pupil population is
#                           (based on parents' education level, income, etc).
#
# All four files share the same school identifiers (more on that at the very
# bottom of this script, because the schoolweging file does it slightly
# differently and that will trip you up if you don't know to expect it).


# ---- 0. packages -------------------------------------------------------
#
# readr reads the DUO files properly: they're semicolon-separated (not
# comma-separated - this is the Netherlands, where the comma is the decimal
# point), so a plain read.csv() will silently mangle every number in the
# file. readODS is here for exactly one reason: the schoolweging file from
# the Onderwijsinspectie is an .ods spreadsheet, not a .csv.
#
# If your R has never installed a package before, install.packages() doesn't
# know which download server (a "CRAN mirror") to use. RStudio usually sets
# one for you, but if you're running this from a plain R console, the line
# below picks one so the installs don't fail with a confusing error.
no_cran_mirror <- is.null(getOption("repos")) ||
  identical(getOption("repos")[["CRAN"]], "@CRAN@")
if (no_cran_mirror) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("readODS", quietly = TRUE)) install.packages("readODS")

library(readr)
library(readODS)


# ---- 1. where the downloads land ---------------------------------------
#
# Everything goes into data/raw/. The idea of a "raw" subfolder is that you
# never, ever hand-edit anything in it - it's a straight copy of what the
# source published. If a number looks wrong later, raw/ is where you go to
# double check it wasn't wrong from the start.

raw_dir <- "data/raw"
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)


# ---- 2. the four files ---------------------------------------------------
#
# Each entry below is one file: where it comes from, where it's going to
# live locally, and a one-line reminder of what's in it. Wrapping them up
# like this (instead of four separate download.file() calls) means the
# download step below is just "for each of these, go get it" - and if DUO
# ever adds a fifth file you care about, you add one entry here and nothing
# else in the script has to change.

sources <- list(
  eindscores = list(
    url  = "https://duo.nl/open_onderwijsdata/images/05.-gemiddelde-eindscores-bo-sbo-2024-2025.csv", # nolint: line_length_linter.
    dest = file.path(raw_dir, "eindscores_2024-2025.csv"),
    what = "average doorstroomtoets score per school per provider"
  ),
  referentieniveaus = list(
    url  = "https://duo.nl/open_onderwijsdata/images/10.-leerlingen-bo-referentieniveaus-2024-2025.csv", # nolint: line_length_linter.
    dest = file.path(raw_dir, "referentieniveaus_2024-2025.csv"),
    what = "number of pupils per school reaching each 1F/1S/2F reference level"
  ),
  schooladviezen = list(
    url  = "https://duo.nl/open_onderwijsdata/images/04-leerlingen-bo-sbo-schooladviezen-2024-2025.csv", # nolint: line_length_linter.
    dest = file.path(raw_dir, "schooladviezen_2024-2025.csv"),
    what = "number of pupils per school per secondary-school advice level"
  ),
  schoolweging = list(
    url = "https://www.onderwijsinspectie.nl/site/binaries/site-content/collections/documents/2026/02/09/schoolweging-2022-2023---2023-2024---2024-2025/schoolweging-2022-2023-2024.ods", # nolint: line_length_linter.
    dest = file.path(raw_dir, "schoolweging_2022-2025.ods"),
    what = paste(
      "schoolweging (disadvantage score) per school,",
      "published by the Onderwijsinspectie"
    )
  )
)


# ---- 3. actually download them -------------------------------------------
#
# We check file.exists() before downloading anything. This is just good
# manners towards DUO's servers (no reason to re-download the same 1-2MB
# file every single time you run this script), and it also means you can
# safely re-run get-data.R as often as you like - it only ever fetches what's
# actually missing.

for (name in names(sources)) {
  src <- sources[[name]]
  if (file.exists(src$dest)) {
    message("Already have '", name, "' (", src$dest, ") - skipping.")
  } else {
    message("Downloading '", name, "': ", src$what, " ...")
    download.file(src$url, destfile = src$dest, mode = "wb", quiet = TRUE)
  }
}


# ---- 4. a sanity check, so you know it actually worked -------------------
#
# Downloading a file and *having the right file* are two different things.
# Here we open each one and just print its dimensions and first couple of
# column names - enough to catch it early if, say, DUO renamed a file and
# our download.file() above silently grabbed an HTML "page not found" page
# instead of real data (this does happen - always check).

cat("\n---- what we ended up with ----\n")

eindscores <- read_delim(
  sources$eindscores$dest,
  delim = ";", quote = '"',
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""), show_col_types = FALSE
)
cat("\neindscores:", nrow(eindscores), "rows,", ncol(eindscores), "columns\n")
print(head(names(eindscores), 8))

referentieniveaus <- read_delim(
  sources$referentieniveaus$dest,
  delim = ";", quote = '"',
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""), show_col_types = FALSE
)
cat(
  "\nreferentieniveaus:",
  nrow(referentieniveaus),
  "rows,",
  ncol(referentieniveaus),
  "columns\n"
)
print(head(names(referentieniveaus), 8))

schooladviezen <- read_delim(
  sources$schooladviezen$dest,
  delim = ";", quote = '"',
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""), show_col_types = FALSE
)
cat(
  "\nschooladviezen:",
  nrow(schooladviezen),
  "rows,",
  ncol(schooladviezen),
  "columns\n"
)
print(head(names(schooladviezen), 8))

# The schoolweging file is a whole workbook, not a single table: it has one
# sheet per school year, plus a three-year-average sheet and an explanatory
# sheet. We want the "2024-2025" sheet specifically, because that's the one
# school year that actually lines up with the other three files above.
schoolweging <- read_ods(sources$schoolweging$dest, sheet = "2024-2025")
cat(
  "\nschoolweging (2024-2025 sheet):",
  nrow(schoolweging),
  "rows,",
  ncol(schoolweging),
  "columns\n"
)
print(names(schoolweging))


# ---- 5. before you try to join these together -----------------------------
#
# eindscores, referentieniveaus and schooladviezen all identify a school the
# same way: INSTELLINGSCODE + VESTIGINGSCODE (a school's "BRIN" code, plus a
# two-digit number for which building/location if a school has more than
# one). Joining those three to each other is a normal dplyr::left_join() on
# those two columns together.
#
# schoolweging does it differently. Open it up and look at its first column,
# called OVT - you'll see values like "00AP|C1", not a separate
# INSTELLINGSCODE/VESTIGINGSCODE pair. The part before the "|" is the same
# INSTELLINGSCODE you already know - split on "|" and that part joins
# straight onto eindscores/referentieniveaus/schooladviezen. Two things will
# trip you up here if you don't check for them first, so go check for them
# first:
#
#   - 310 of the 6,217 schools (INSTELLINGSCODE) in eindscores have NO row
#     at all in schoolweging - and it's not random which ones: 262 of those
#     310 are every single Sbo (special primary) school in eindscores. The
#     Inspectorate's schoolweging file is built for its regular results
#     inspection, which doesn't apply to Sbo the same way - so if you're
#     comparing schools by schoolweging, decide up front whether you're
#     restricting to Bo schools only, and say so.
#
#   - the part of OVT after the "|" (like "C1", "C2") identifies a location,
#     but NOT using the same "00"/"01" numbering as VESTIGINGSCODE. For a
#     school with only one vestiging this never matters. For the 102 schools
#     that run more than one vestiging, check before you trust it: e.g.
#     INSTELLINGSCODE "01VG" has VESTIGINGSCODE 00 = "Jenaplansch T Vlot" and
#     01 = "Peter Petersenschool" in eindscores, but OVT "01VG|C1" =
#     "Jenaplanschool 't Vlot" and "01VG|C2" = "'t Wilde Woud" in
#     schoolweging - same first vestiging, but the second one is a
#     *different school name entirely*, so "C2" is not simply "VESTIGINGSCODE
#     01 renamed". Match on INSTELLINGSCODE alone, and only worry about the
#     vestiging-level detail for the specific 102 schools where it applies.


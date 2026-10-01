# 01-get-data.R
#
# This script downloads everything you need to dig into the doorstroomtoets
# ("transfer test") fairness debate. Background on what that debate actually
# is, and why it matters, is on the course website, under Resources >
# "The data: background & codebook".
#
# We need FOUR files in total, and it's worth understanding why each one
# is here before you run this:
#
#   1. eindscores        - the average doorstroomtoets score per school, per
#                           test provider (IEP, Route8, DIA, AMN, DOE, LIB).
#
#   2. referentieniveaus  - the piece eindscores is missing: how many pupils
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
#                           ended up with (PRO, VMBO, HAVO, VWO, ...). This is
#                           the other half of the story: it's not just that
#                           test scores differ by provider, it's that the
#                           *advice* pupils get differs too.
#
#   4. schoolweging       - schoolweging is a school-level score for how
#                           disadvantaged a school's pupil population is
#                           (based on parents' education level, income, etc).
#                           This is the variable you'd use to check whether
#                           "different providers give different results" is
#                           actually just "different providers happen to be
#                           used by different kinds of schools". Comparing
#                           scores WITHOUT controlling for this is misleading
#                           - comparing scores at equal schoolweging is what
#                           makes the comparison fair.
#
# All four files share the same school identifiers (more on that at the very
# bottom of this script, because the schoolweging file does it slightly
# differently and that will trip you up if you don't know to expect it).
#
# A quick warning up front: files 1-3 are all snapshots of school year
# 2024-2025. File 4 (schoolweging) is a three-year average covering
# 2022/2023 through 2024/2025 - the Inspectorate publishes it that way on
# purpose, to smooth out year-to-year noise, but it does mean you're not
# comparing perfectly matched time windows. Keep that in mind later.


# ---- 0. packages -------------------------------------------------------
#
# readr reads the DUO files properly: they're semicolon-separated (not
# comma-separated - this is the Netherlands, where the comma is the decimal
# point), so a plain read.csv() will silently mangle every number in the
# file. readODS is here for exactly one reason: the schoolweging file from
# the Onderwijsinspectie is an .ods spreadsheet, not a .csv. here makes sure
# "data/raw" always means the same folder, whether you run this script
# directly or source() it from report/ (knitr's working directory is
# wherever the .Rmd lives, not the project root - here::here() fixes that).
#
# All three are installed and loaded by 00-packages.R - run that first (the
# report sources both scripts in order, so knitting takes care of it).

library(readr)
library(readODS)
library(here)


# ---- 1. where the downloads land ---------------------------------------
#
# Everything goes into data/raw/, at the project root. The idea of a "raw"
# subfolder is that you never, ever hand-edit anything in it - it's a
# straight copy of what the source published. If a number looks wrong later,
# raw/ is where you go to double check it wasn't wrong from the start.
# It's always reproducible by re-running this script.

raw_dir <- here("data", "raw")
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
# safely re-run 01-get-data.R as often as you like - including every time
# you knit the report - since it only ever fetches what's actually missing.

for (name in names(sources)) {
  src <- sources[[name]]
  if (file.exists(src$dest)) {
    message("Already have '", name, "' (", src$dest, ") - skipping.")
  } else {
    message("Downloading '", name, "': ", src$what, " ...")
    download.file(src$url, destfile = src$dest, mode = "wb", quiet = TRUE)
  }
}


# ---- 4. load them, so they're ready to use -------------------------------
#
# Downloading a file and *having the right file* are two different things.
# Loading each one here (instead of just downloading) means: (a) you get an
# early warning if, say, DUO renamed a file and download.file() above
# silently grabbed an HTML "page not found" page instead of real data, and
# (b) whoever sources this script (you, your groupmates, or a grader) ends
# up with eindscores/referentieniveaus/schooladviezen/schoolweging already
# loaded and ready to use, without a separate read_delim() step.

eindscores <- read_delim(
  sources$eindscores$dest,
  delim = ";", quote = '"',
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""), show_col_types = FALSE
)
message(
  "eindscores: ",
  nrow(eindscores),
  " rows, ",
  ncol(eindscores),
  " columns"
)

referentieniveaus <- read_delim(
  sources$referentieniveaus$dest,
  delim = ";", quote = '"',
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""), show_col_types = FALSE
)
message(
  "referentieniveaus: ",
  nrow(referentieniveaus),
  " rows, ",
  ncol(referentieniveaus),
  " columns"
)

schooladviezen <- read_delim(
  sources$schooladviezen$dest,
  delim = ";", quote = '"',
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("NA", ""), show_col_types = FALSE
)
message(
  "schooladviezen: ",
  nrow(schooladviezen),
  " rows, ",
  ncol(schooladviezen),
  " columns"
)

# The schoolweging file is a whole workbook, not a single table: it has one
# sheet per school year, plus a three-year-average sheet and an explanatory
# sheet. We want the "2024-2025" sheet specifically, because that's the one
# school year that actually lines up with the other three files above.
schoolweging <- read_ods(sources$schoolweging$dest, sheet = "2024-2025")
message(
  "schoolweging (2024-2025 sheet): ",
  nrow(schoolweging),
  " rows, ",
  ncol(schoolweging),
  " columns"
)


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
#   - Several hundred schools in eindscores have NO row at all in
#     schoolweging - and it's not random which ones: almost all of them are
#     Sbo (special primary) schools. The Inspectorate's schoolweging file is
#     built for its regular results inspection, which doesn't apply to Sbo
#     the same way - so if you're comparing schools by schoolweging, decide
#     up front whether you're restricting to Bo schools only, and say so.
#
#   - the part of OVT after the "|" (like "C1", "C2") identifies a location,
#     but NOT using the same "00"/"01" numbering as VESTIGINGSCODE. For a
#     school with only one vestiging this never matters. For schools that
#     run more than one vestiging, check before you trust it - match on
#     INSTELLINGSCODE alone, and only worry about the vestiging-level detail
#     for the specific schools where it applies.
#
# One more thing worth remembering before you build any charts from this:
# none of referentieniveaus or schooladviezen is broken down by *which*
# doorstroomtoets provider a school used - that information only lives in
# eindscores. So "reference levels by provider" or "advice by provider"
# isn't something you can read directly off one file; you have to bring in
# eindscores' provider columns and label each school by whichever provider
# it used (works reasonably well, since the large majority of schools use
# exactly one provider). Just be upfront in your write-up that this is a
# school-level approximation, not pupil-level data - that distinction
# matters for what conclusions your numbers can support.

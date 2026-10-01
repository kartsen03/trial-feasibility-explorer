# Pull both therapeutic areas from ClinicalTrials.gov into data/raw/.
#
# Safe to rerun: an area whose verified cache already exists is skipped. Pass --refresh
# to take a new snapshot.
#
#   Rscript scripts/01_extract.R [--refresh]

source("pipeline/extract.R")

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
for (area in names(AREAS)) fetch_area(area, refresh = refresh)

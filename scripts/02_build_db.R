# Parse the cached raw JSON, clean it, and build data/trials.sqlite.
#
# Reads only data/raw/, so it never touches the API. Run scripts/01_extract.R first.
#
#   Rscript scripts/02_build_db.R

source("pipeline/extract.R")  # AREAS and START_WINDOW, the definition of the snapshot
source("pipeline/parse.R")
source("pipeline/clean.R")
source("pipeline/load.R")

db_path <- "data/trials.sqlite"
areas <- names(AREAS)

manifests <- lapply(setNames(areas, areas), function(a) {
  path <- file.path("data/raw", a, "manifest.json")
  if (!file.exists(path)) stop("no verified cache for ", a, "; run scripts/01_extract.R", call. = FALSE)
  jsonlite::read_json(path)
})

by_area <- lapply(setNames(areas, areas), function(a) parse_area_dir(file.path("data/raw", a)))
for (a in areas) {
  message(sprintf("[%s] parsed %d studies (%d pagination repeats dropped)",
                  a, nrow(by_area[[a]]$studies), by_area[[a]]$n_repeats))
}

combined <- combine_areas(by_area)
tables <- list(
  studies = clean_studies(combined$studies),
  study_areas = combined$study_areas,
  sites = combined$sites,
  conditions = combined$conditions
)

snapshot <- c(
  source = "ClinicalTrials.gov API v2",
  start_window = paste(START_WINDOW, collapse = " to "),
  n_studies = nrow(tables$studies),
  built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  unlist(lapply(areas, function(a) setNames(
    c(manifests[[a]]$fetched_at, manifests[[a]]$n_studies, manifests[[a]]$mesh_descriptor),
    paste0(c("fetched_at_", "n_studies_", "mesh_descriptor_"), a)
  )))
)

build_database(tables, db_path, as.list(snapshot))
message(sprintf(
  "built %s: %d studies, %d area links, %d sites, %d conditions (%.1f MB)",
  db_path, nrow(tables$studies), nrow(tables$study_areas), nrow(tables$sites),
  nrow(tables$conditions), file.size(db_path) / 1e6
))

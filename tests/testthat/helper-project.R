# testthat runs from tests/testthat; load the project code relative to the repo root.
# Every file only defines functions and constants, so the order they're sourced in
# doesn't matter.
root <- normalizePath(file.path("..", ".."), winslash = "/")
for (f in list.files(file.path(root, c("pipeline", "R")), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
options(tfe.sql_dir = file.path(root, "sql"))

fixture <- function(...) file.path(root, "tests", "testthat", "fixtures", ...)

# The fixture pages run through the real pipeline into a throwaway database.
build_fixture_db <- function() {
  by_area <- list(
    oncology = parse_area_dir(fixture("raw", "oncology")),
    cardiovascular = parse_area_dir(fixture("raw", "cardiovascular"))
  )
  combined <- combine_areas(by_area)
  tables <- list(
    studies = clean_studies(combined$studies),
    study_areas = combined$study_areas,
    sites = combined$sites,
    conditions = combined$conditions
  )
  path <- tempfile(fileext = ".sqlite")
  build_database(tables, path, list(source = "test fixture"))
  path
}

select_everything <- function(con) {
  ch <- filter_choices(con)
  list(areas = ch$areas, phases = ch$phases, statuses = ch$statuses,
       sponsor_classes = ch$sponsor_classes, years = ch$years)
}

# A cleaned studies table built directly, for tests that need exact control of values.
make_studies <- function(...) {
  d <- data.frame(..., stringsAsFactors = FALSE)
  defaults <- list(
    title = "t", overall_status = "COMPLETED", why_stopped = NA_character_,
    stop_category = NA_character_, phase = "Phase 2", phase_raw = NA_character_,
    enrollment = NA_integer_, enrollment_type = "ACTUAL", start_date = "2015-01-01",
    start_date_precision = "day", start_date_type = "ACTUAL", start_year = 2015L,
    primary_completion_date = NA_character_, primary_completion_date_precision = NA_character_,
    primary_completion_date_type = NA_character_, duration_months = NA_real_,
    sponsor = "s", sponsor_class = "INDUSTRY", site_count = 1L
  )
  for (k in names(defaults)) if (!k %in% names(d)) d[[k]] <- defaults[[k]]
  d
}

build_synthetic_db <- function(studies) {
  tables <- list(
    studies = studies,
    study_areas = data.frame(nct_id = studies$nct_id, area = "oncology", stringsAsFactors = FALSE),
    sites = data.frame(nct_id = character(), facility = character(), city = character(),
                       state = character(), country = character()),
    conditions = data.frame(nct_id = character(), condition = character())
  )
  path <- tempfile(fileext = ".sqlite")
  build_database(tables, path, list(source = "synthetic"))
  path
}

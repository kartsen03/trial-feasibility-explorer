# Pull studies from the ClinicalTrials.gov API v2 and cache every page as raw JSON.
#
# The cache is the contract between extraction and everything downstream: parsing reads
# only from data/raw/, so reruns never touch the API and a snapshot can be rebuilt
# exactly. An area is cached only once its pull is verified complete (see fetch_area).

CTGOV_BASE <- "https://clinicaltrials.gov/api/v2/studies"

# Only the pieces the dashboard uses. Site *contacts* (staff names, phone numbers,
# emails) are deliberately not requested: nothing here needs them, so they are never
# stored.
CTGOV_FIELDS <- c(
  "NCTId", "BriefTitle", "OverallStatus", "WhyStopped", "StudyType", "Phase",
  "EnrollmentCount", "EnrollmentType",
  "StartDate", "StartDateType",
  "PrimaryCompletionDate", "PrimaryCompletionDateType",
  "LeadSponsorName", "LeadSponsorClass",
  "Condition", "ConditionMeshTerm", "ConditionAncestorTerm",
  "LocationFacility", "LocationCity", "LocationState", "LocationCountry"
)

# Therapeutic areas are defined by NLM's MeSH hierarchy rather than by keywords: a study
# belongs to an area when any of its conditions maps to the area's descriptor or to a
# descendant of it.
AREAS <- c(
  oncology       = "Neoplasms",               # MeSH tree C04
  cardiovascular = "Cardiovascular Diseases"  # MeSH tree C14
)

# 2010 onward: FDAAA 801 made registration mandatory for most interventional trials
# from late 2007, so earlier years over-represent sponsors who registered voluntarily.
# Ending at 2025 keeps every year on the axis a complete calendar year.
START_WINDOW <- c("2010-01-01", "2025-12-31")

area_filter <- function(area, window = START_WINDOW) {
  term <- AREAS[[area]]
  # A study whose own MeSH term *is* the top descriptor does not list it as an
  # ancestor, so matching ancestors alone silently drops it. Match either.
  sprintf(
    paste(
      '(AREA[ConditionMeshTerm]"%1$s" OR AREA[ConditionAncestorTerm]"%1$s")',
      "AND AREA[StudyType]INTERVENTIONAL",
      "AND AREA[StartDate]RANGE[%2$s,%3$s]"
    ),
    term, window[[1]], window[[2]]
  )
}

ctgov_request <- function(filter, page_size = 1000, page_token = NULL, count_total = FALSE) {
  httr2::request(CTGOV_BASE) |>
    httr2::req_url_query(
      filter.advanced = filter,
      fields = paste(CTGOV_FIELDS, collapse = ","),
      pageSize = page_size,
      format = "json",
      countTotal = if (count_total) "true",
      pageToken = page_token
    ) |>
    httr2::req_user_agent("trial-feasibility-explorer (github.com/kartsen03/trial-feasibility-explorer)") |>
    httr2::req_throttle(capacity = 30, fill_time_s = 60) |>
    httr2::req_retry(
      max_tries = 6,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    httr2::req_timeout(180)
}

# Fetch every page for one area into raw_dir/<area>/page_NNNN.json.
#
# The pull is written to a staging directory and only moved into place once the number
# of studies received equals the totalCount the API reported on page 1. A partial pull
# is never cached, because a cache that looks complete but isn't would quietly
# under-count everything downstream.
fetch_area <- function(area, raw_dir = "data/raw", page_size = 1000, refresh = FALSE) {
  stopifnot(area %in% names(AREAS))
  area_dir <- file.path(raw_dir, area)
  manifest_path <- file.path(area_dir, "manifest.json")

  if (!refresh && file.exists(manifest_path)) {
    m <- jsonlite::read_json(manifest_path)
    message(sprintf("[%s] using cache: %d studies, %d pages, fetched %s",
                    area, m$n_studies, m$n_pages, m$fetched_at))
    return(invisible(m))
  }

  staging <- paste0(area_dir, ".partial")
  unlink(staging, recursive = TRUE)
  dir.create(staging, recursive = TRUE)

  filter <- area_filter(area)
  token <- NULL
  page <- 0L
  received <- 0L
  expected <- NA_integer_
  started <- Sys.time()

  repeat {
    page <- page + 1L
    resp <- httr2::req_perform(ctgov_request(filter, page_size, token, count_total = page == 1L))
    raw <- httr2::resp_body_raw(resp)
    body <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)

    if (page == 1L) expected <- as.integer(body$totalCount)
    writeBin(raw, file.path(staging, sprintf("page_%04d.json", page)))
    received <- received + length(body$studies)
    message(sprintf("[%s] page %d: %d / %d studies", area, page, received, expected))

    token <- body$nextPageToken
    if (is.null(token)) break
  }

  if (received != expected) {
    stop(sprintf(
      "[%s] incomplete pull: received %d studies but the API reported %d. The registry may have changed mid-pull; rerun. Nothing was cached.",
      area, received, expected
    ), call. = FALSE)
  }

  manifest <- list(
    area = area,
    mesh_descriptor = AREAS[[area]],
    filter = filter,
    fields = CTGOV_FIELDS,
    n_studies = received,
    n_pages = page,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    elapsed_s = round(as.numeric(difftime(Sys.time(), started, units = "secs")))
  )
  jsonlite::write_json(manifest, file.path(staging, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)

  unlink(area_dir, recursive = TRUE)
  if (!file.rename(staging, area_dir)) stop("could not move ", staging, " to ", area_dir, call. = FALSE)
  invisible(manifest)
}

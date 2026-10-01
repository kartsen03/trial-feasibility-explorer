# Turn cached API pages into tidy tables.
#
# Every function here is pure: parsed JSON in, data frames out. Nothing touches the
# network or the database, which is what lets the messy-data rules be unit-tested
# against small hand-written fixtures.

# Walk a nested list, returning NULL as soon as a key is missing. The API omits absent
# fields entirely rather than sending null (whyStopped only exists on stopped trials,
# for example), so a missing key is the normal case, not an error.
dig <- function(x, ...) {
  for (key in c(...)) {
    if (!is.list(x) || is.null(x[[key]])) return(NULL)
    x <- x[[key]]
  }
  x
}

as_chr <- function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]])
as_int <- function(x) if (length(x) == 0) NA_integer_ else as.integer(x[[1]])

study_id <- function(study) as_chr(dig(study, "protocolSection", "identificationModule", "nctId"))

# One page of the API response -> list(studies, sites, conditions).
#
# `studies` keeps raw values (the phase array, date strings exactly as registered);
# interpreting them is clean.R's job, so parsing and cleaning are tested separately.
# `site_count` is the number of listed locations, so it always equals that study's
# row count in `sites`.
parse_page <- function(page) {
  ps <- lapply(page$studies, `[[`, "protocolSection")
  field <- function(...) vapply(ps, function(p) as_chr(dig(p, ...)), character(1))
  nct_id <- field("identificationModule", "nctId")

  studies <- data.frame(
    nct_id                       = nct_id,
    title                        = field("identificationModule", "briefTitle"),
    overall_status               = field("statusModule", "overallStatus"),
    why_stopped                  = field("statusModule", "whyStopped"),
    study_type                   = field("designModule", "studyType"),
    # Multi-phase trials arrive as an array, e.g. ["PHASE1", "PHASE2"].
    phase_raw                    = vapply(ps, function(p) {
      ph <- unlist(dig(p, "designModule", "phases"))
      if (length(ph) == 0) NA_character_ else paste(ph, collapse = "|")
    }, character(1)),
    enrollment                   = vapply(ps, function(p) as_int(dig(p, "designModule", "enrollmentInfo", "count")), integer(1)),
    enrollment_type              = field("designModule", "enrollmentInfo", "type"),
    start_date_raw               = field("statusModule", "startDateStruct", "date"),
    start_date_type              = field("statusModule", "startDateStruct", "type"),
    primary_completion_date_raw  = field("statusModule", "primaryCompletionDateStruct", "date"),
    primary_completion_date_type = field("statusModule", "primaryCompletionDateStruct", "type"),
    sponsor                      = field("sponsorCollaboratorsModule", "leadSponsor", "name"),
    sponsor_class                = field("sponsorCollaboratorsModule", "leadSponsor", "class"),
    site_count                   = vapply(ps, function(p) length(dig(p, "contactsLocationsModule", "locations")), integer(1)),
    stringsAsFactors = FALSE
  )

  locs <- lapply(ps, function(p) dig(p, "contactsLocationsModule", "locations"))
  flat_locs <- unlist(locs, recursive = FALSE)
  loc_field <- function(key) vapply(flat_locs, function(l) as_chr(l[[key]]), character(1))
  sites <- data.frame(
    nct_id   = rep(nct_id, lengths(locs)),
    facility = loc_field("facility"),
    city     = loc_field("city"),
    state    = loc_field("state"),
    country  = loc_field("country"),
    stringsAsFactors = FALSE
  )

  conds <- lapply(ps, function(p) as.character(unlist(dig(p, "conditionsModule", "conditions"))))
  conditions <- data.frame(
    nct_id    = rep(nct_id, lengths(conds)),
    condition = unlist(conds, use.names = FALSE) %||% character(),
    stringsAsFactors = FALSE
  )

  list(studies = studies, sites = sites, conditions = conditions)
}

# Every cached page for one area, bound into one set of tables.
#
# Paginating a live registry can return the same study on two pages if it is updated
# mid-pull. Repeats are dropped *before* parsing (first page wins), so a repeated study
# can't double its sites or conditions. The number dropped is returned for reporting.
parse_area_dir <- function(area_dir) {
  pages <- sort(list.files(area_dir, pattern = "^page_\\d+\\.json$", full.names = TRUE))
  if (length(pages) == 0) stop("no cached pages in ", area_dir, call. = FALSE)

  seen <- character()
  n_repeats <- 0L
  parsed <- vector("list", length(pages))
  for (i in seq_along(pages)) {
    page <- jsonlite::read_json(pages[[i]], simplifyVector = FALSE)
    ids <- vapply(page$studies, study_id, character(1))
    keep <- !(ids %in% seen) & !duplicated(ids)
    n_repeats <- n_repeats + sum(!keep)
    page$studies <- page$studies[keep]
    seen <- c(seen, ids[keep])
    parsed[[i]] <- parse_page(page)
  }

  bind <- function(name) do.call(rbind, lapply(parsed, `[[`, name))
  list(studies = bind("studies"), sites = bind("sites"), conditions = bind("conditions"),
       n_repeats = n_repeats)
}

# Combine per-area extracts into one set of tables.
#
# A trial can belong to both areas (cardio-oncology, for example), so area membership is
# returned as its own many-to-many table and every other table holds each trial once.
# For a trial pulled under both areas, the first area's copy of its rows is kept; the
# two pulls run minutes apart, so the copies are near-certainly identical.
combine_areas <- function(by_area) {
  study_areas <- do.call(rbind, lapply(names(by_area), function(a) {
    data.frame(nct_id = by_area[[a]]$studies$nct_id, area = a, stringsAsFactors = FALSE)
  }))

  owner <- study_areas[!duplicated(study_areas$nct_id), ]
  pick <- function(name) {
    do.call(rbind, lapply(names(by_area), function(a) {
      tbl <- by_area[[a]][[name]]
      tbl[tbl$nct_id %in% owner$nct_id[owner$area == a], , drop = FALSE]
    }))
  }

  out <- list(
    studies = pick("studies"),
    study_areas = study_areas,
    sites = pick("sites"),
    conditions = pick("conditions")
  )
  rownames(out$studies) <- rownames(out$sites) <- rownames(out$conditions) <- NULL
  out
}

# Write the cleaned tables into SQLite.

# Execute a file of semicolon-separated statements. Line comments are stripped first;
# the schema contains no string literals, so splitting on ";" is safe here.
run_sql_file <- function(con, path) {
  lines <- sub("--.*$", "", readLines(path, warn = FALSE))
  statements <- trimws(strsplit(paste(lines, collapse = "\n"), ";", fixed = TRUE)[[1]])
  for (statement in statements[nzchar(statements)]) DBI::dbExecute(con, statement)
  invisible(TRUE)
}

# Build the database from scratch. Foreign keys are enforced during the load, so a site
# or condition that points at a missing trial fails the build instead of being stored.
build_database <- function(tables, db_path, snapshot,
                           sql_dir = getOption("tfe.sql_dir", "sql")) {
  schema <- file.path(sql_dir, "schema.sql")
  derive <- file.path(sql_dir, "derive.sql")
  if (file.exists(db_path)) file.remove(db_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  run_sql_file(con, schema)
  DBI::dbWithTransaction(con, {
    for (name in c("studies", "study_areas", "sites", "conditions")) {
      DBI::dbAppendTable(con, name, tables[[name]])
    }
    DBI::dbAppendTable(con, "snapshot", data.frame(
      key = names(snapshot), value = vapply(snapshot, as.character, character(1)),
      stringsAsFactors = FALSE
    ))
    run_sql_file(con, derive)
  })
  DBI::dbExecute(con, "ANALYZE")
  DBI::dbExecute(con, "VACUUM")
  invisible(db_path)
}

# Invariants checked after every build. expected_by_area is the study count the API
# reported for each area. Matching it here catches what the extractor's count check
# can't: a study repeated across pages while another was skipped leaves the received
# count right but the set of studies wrong. Any failure stops the build.
validate_database <- function(db_path, expected_by_area) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  value <- function(sql, params = NULL) DBI::dbGetQuery(con, sql, params = params)[[1]]

  problems <- character()
  check <- function(ok, message) if (!isTRUE(ok)) problems <<- c(problems, message)

  for (area in names(expected_by_area)) {
    linked <- value("SELECT COUNT(*) FROM study_areas WHERE area = ?", list(area))
    check(linked == expected_by_area[[area]],
          sprintf("%s: %d studies linked, but the API reported %d", area, linked, expected_by_area[[area]]))
  }
  check(value("SELECT COUNT(*) FROM study_areas WHERE area NOT IN (SELECT value FROM json_each(?))",
              list(as.character(jsonlite::toJSON(names(expected_by_area))))) == 0,
        "study_areas contains an area that wasn't extracted")
  check(value("SELECT SUM(site_count) FROM studies") == value("SELECT COUNT(*) FROM sites"),
        "site_count totals don't match the rows in sites")
  check(value("SELECT COUNT(*) FROM studies AS s WHERE NOT EXISTS
                 (SELECT 1 FROM study_areas AS a WHERE a.nct_id = s.nct_id)") == 0,
        "some studies belong to no area")
  check(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")) == 0, "foreign key violations")
  check(value("PRAGMA integrity_check") == "ok", "SQLite integrity check failed")

  if (length(problems) > 0) {
    stop(paste(c("database failed validation:", problems), collapse = "\n  - "), call. = FALSE)
  }
  invisible(TRUE)
}

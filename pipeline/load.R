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

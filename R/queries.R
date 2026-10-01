# Run the dashboard's SQL. Shiny sources R/ automatically when the app starts; the
# pipeline scripts and tests source this file directly.
#
# Every query in sql/ reads from the temp tables built by apply_filters(), so the
# filter is defined, and parameterised, in one place. Temp tables are private to a
# connection, and the app opens one connection per session, so concurrent users can't
# see each other's filters.

sql_dir <- function() getOption("tfe.sql_dir", "sql")

read_sql <- function(name) {
  paste(readLines(file.path(sql_dir(), paste0(name, ".sql")), warn = FALSE), collapse = "\n")
}

db_path <- function() Sys.getenv("TFE_DB_PATH", "data/trials.sqlite")

open_db <- function(path = db_path()) {
  if (!file.exists(path)) {
    stop("database not found at ", path, ". Build it with scripts/01_extract.R and ",
         "scripts/02_build_db.R, or download a snapshot with scripts/00_download_snapshot.R",
         call. = FALSE)
  }
  DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
}

json_array <- function(x) as.character(jsonlite::toJSON(as.character(x)))

# filters: list(areas, phases, statuses, sponsor_classes, years = c(min, max)).
apply_filters <- function(con, filters) {
  every_area <- DBI::dbGetQuery(con, "SELECT DISTINCT area FROM study_areas")$area
  params <- list(
    all_areas = as.integer(all(every_area %in% filters$areas)),
    areas = json_array(filters$areas),
    phases = json_array(filters$phases),
    statuses = json_array(filters$statuses),
    sponsor_classes = json_array(filters$sponsor_classes),
    year_min = as.integer(filters$years[[1]]),
    year_max = as.integer(filters$years[[2]])
  )
  DBI::dbExecute(con, "DROP TABLE IF EXISTS temp.f_areas")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS temp.f")
  DBI::dbExecute(con, paste("CREATE TEMP TABLE f AS", read_sql("filter")), params = params)
  # CREATE TABLE AS copies no keys, and most queries join on nct_id.
  DBI::dbExecute(con, "CREATE UNIQUE INDEX temp.ix_f_nct_id ON f (nct_id)")
  DBI::dbExecute(con, paste("CREATE TEMP TABLE f_areas AS", read_sql("filter_areas")),
                 params = params["areas"])
  invisible(con)
}

run_query <- function(con, name) DBI::dbGetQuery(con, read_sql(name))

# Choices offered by the filters, read from the data rather than hard-coded.
filter_choices <- function(con) {
  distinct <- function(sql) DBI::dbGetQuery(con, sql)[[1]]
  list(
    areas = distinct("SELECT DISTINCT area FROM study_areas ORDER BY area"),
    phases = distinct("SELECT DISTINCT phase FROM studies"),
    statuses = distinct("SELECT overall_status FROM studies GROUP BY overall_status ORDER BY COUNT(*) DESC"),
    sponsor_classes = distinct("SELECT sponsor_class FROM studies GROUP BY sponsor_class ORDER BY COUNT(*) DESC"),
    years = range(distinct("SELECT start_year FROM studies WHERE start_year IS NOT NULL"))
  )
}

snapshot_info <- function(con) {
  s <- DBI::dbGetQuery(con, "SELECT key, value FROM snapshot")
  stats::setNames(as.list(s$value), s$key)
}

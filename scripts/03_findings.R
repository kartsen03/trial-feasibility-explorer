# Compute the README's snapshot summary and key findings from data/trials.sqlite, and
# write them between the generated-block markers in README.md.
#
# Every number comes from the dashboard's own queries, run with the filters listed next
# to it, so each finding can be reproduced in the app. Each qualitative claim ("most
# common", "more often") is checked before its sentence is written: if a refreshed
# snapshot ever makes one false, this script stops rather than publish it.
#
#   Rscript scripts/03_findings.R

source("R/labels.R")
source("R/queries.R")
source("pipeline/clean.R")

tag <- readLines("data/SNAPSHOT", warn = FALSE)[[1]]
release_url <- sprintf("https://github.com/kartsen03/trial-feasibility-explorer/releases/tag/%s", tag)

con <- open_db()
choices <- filter_choices(con)
everything <- list(areas = choices$areas, phases = choices$phases, statuses = choices$statuses,
                   sponsor_classes = choices$sponsor_classes, years = choices$years)
run <- function(query, ...) {
  apply_filters(con, modifyList(everything, list(...)))
  run_query(con, query)
}
claim <- function(ok, what) if (!isTRUE(ok)) stop("finding no longer holds: ", what, call. = FALSE)

snapshot <- snapshot_info(con)
links <- DBI::dbGetQuery(con, "
  SELECT SUM(area = 'oncology') AS oncology, SUM(area = 'cardiovascular') AS cardiovascular,
         COUNT(*) - COUNT(DISTINCT nct_id) AS both_areas
  FROM study_areas")

# 1. Why trials stop early.
reasons <- run("stop_reasons")
accrual <- reasons[reasons$category == "Low accrual", ]
claim(reasons$category[[1]] == "Low accrual", "low accrual is the most common stop reason")
for (area in c("oncology", "cardiovascular")) {
  claim(run("stop_reasons", areas = area)$category[[1]] == "Low accrual",
        paste("low accrual leads in", area))
}

# How reliably the categoriser identifies low accrual, on the hand-labelled sample.
validation <- read.csv("tests/testthat/fixtures/stop_reasons_validation.csv", stringsAsFactors = FALSE)
predicted <- categorize_stop_reason(validation$why_stopped)
precision <- mean(validation$label[predicted == "Low accrual"] == "Low accrual")
recall <- mean(predicted[validation$label == "Low accrual"] == "Low accrual")

# 2. Oncology against cardiovascular, among trials old enough to have finished.
mature <- c(2010L, 2019L)
oncology <- run("kpis", areas = "oncology", years = mature)
cardio <- run("kpis", areas = "cardiovascular", years = mature)
claim(oncology$stopped_early_rate > cardio$stopped_early_rate, "oncology trials stop early more often")

# 3. Phase 3: industry against academic and hospital sponsors.
industry <- run("kpis", phases = "Phase 3", sponsor_classes = "INDUSTRY")
academic <- run("kpis", phases = "Phase 3", sponsor_classes = "OTHER")
claim(industry$median_sites > academic$median_sites, "industry Phase 3 trials use more sites")
claim(industry$median_duration_months < academic$median_duration_months,
      "industry Phase 3 trials finish sooner")

DBI::dbDisconnect(con)

months <- function(x) sprintf("%.1f months", x)
start_years <- sub("^(\\d{4})-\\d{2}-\\d{2} to (\\d{4})-\\d{2}-\\d{2}$", "\\1 to \\2", snapshot$start_window)

snapshot_block <- sprintf(
  "**Snapshot [`%s`](%s):** %s interventional trials starting %s, pulled from ClinicalTrials.gov on %s: %s oncology and %s cardiovascular, %s of them in both.",
  tag, release_url, fmt_int(as.numeric(snapshot$n_studies)), start_years,
  substr(snapshot$fetched_at_oncology, 1, 10), fmt_int(links$oncology), fmt_int(links$cardiovascular),
  fmt_int(links$both_areas)
)

findings_block <- c(
  sprintf(paste(
    "1. **Low accrual is the most common reason trials stop early.** It accounts for %s of the",
    "%s trials that were terminated or withdrawn (%s trials), and it leads in both therapeutic",
    "areas. On the hand-labelled validation sample the categoriser identifies low accrual with",
    "%s precision and %s recall. *Filters: none.*"),
    fmt_pct(accrual$share), fmt_int(sum(reasons$n_trials)), fmt_int(accrual$n_trials),
    fmt_pct(precision, 0), fmt_pct(recall, 0)),
  sprintf(paste(
    "2. **Oncology trials stop early more often than cardiovascular trials:** %s against %s of",
    "trials that started 2010 to 2019 and reached a final status (%s and %s trials). Leaving out",
    "later start years removes a known bias: a recent trial can only be finished if it stopped",
    "early. *Filters: one area at a time; start year 2010 to 2019.*"),
    fmt_pct(oncology$stopped_early_rate), fmt_pct(cardio$stopped_early_rate),
    fmt_int(oncology$n_final), fmt_int(cardio$n_final)),
  sprintf(paste(
    "3. **Industry-sponsored Phase 3 trials run on many more sites and finish sooner.** Their",
    "median is %s sites against %s for academic and hospital sponsors, and their completed",
    "trials took a median of %s from start to primary completion against %s (%s and %s Phase 3",
    "trials). *Filters: Phase 3; lead sponsor Industry, then Academic / other.*"),
    fmt_int(industry$median_sites), fmt_int(academic$median_sites),
    months(industry$median_duration_months), months(academic$median_duration_months),
    fmt_int(industry$n_trials), fmt_int(academic$n_trials))
)

replace_block <- function(text, name, content) {
  start <- grep(sprintf("<!-- %s:start", name), text, fixed = TRUE)
  end <- grep(sprintf("<!-- %s:end -->", name), text, fixed = TRUE)
  if (length(start) != 1 || length(end) != 1 || end < start) {
    stop("README.md needs exactly one ", name, " block", call. = FALSE)
  }
  c(text[seq_len(start)], content, text[end:length(text)])
}

readme <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
readme <- replace_block(readme, "snapshot", snapshot_block)
readme <- replace_block(readme, "findings", c("", findings_block, ""))
writeLines(readme, "README.md", useBytes = TRUE)
message("README.md updated from ", tag)

cat("\n== Resume numbers ==\n")
cat(sprintf("Total studies in the dataset: %s\n", fmt_int(as.numeric(snapshot$n_studies))))
cat(sprintf(
  "Finding: low accrual was the most common reason trials stopped early, behind %s of the %s terminated or withdrawn trials.\n",
  fmt_pct(accrual$share), fmt_int(sum(reasons$n_trials))
))

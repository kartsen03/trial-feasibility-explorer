# Display vocabulary. The pipeline writes these values and the app reads them, so tests
# check that the two agree (see test-contract.R).

PHASE_LEVELS <- c(
  "Early Phase 1", "Phase 1", "Phase 1/2", "Phase 2", "Phase 2/3", "Phase 3", "Phase 4",
  "Not applicable", "Not reported", "Other"
)

# Must match the CASE labels in sql/sites_per_trial.sql.
SITE_BUCKETS <- c("1", "2-10", "11-50", "51-100", ">100")

AREA_LABELS <- c(oncology = "Oncology", cardiovascular = "Cardiovascular")

STATUS_LABELS <- c(
  COMPLETED = "Completed",
  TERMINATED = "Terminated",
  WITHDRAWN = "Withdrawn",
  SUSPENDED = "Suspended",
  RECRUITING = "Recruiting",
  ACTIVE_NOT_RECRUITING = "Active, not recruiting",
  ENROLLING_BY_INVITATION = "Enrolling by invitation",
  NOT_YET_RECRUITING = "Not yet recruiting",
  UNKNOWN = "Unknown (not verified in 2+ years)"
)

# ClinicalTrials.gov's lead-sponsor classes. OTHER is mostly universities, hospitals
# and foundations.
SPONSOR_CLASS_LABELS <- c(
  INDUSTRY = "Industry",
  OTHER = "Academic / other",
  NIH = "NIH",
  FED = "Other U.S. federal",
  OTHER_GOV = "Other government",
  NETWORK = "Network",
  INDIV = "Individual",
  AMBIG = "Ambiguous",
  UNKNOWN = "Unknown"
)

# Catch-all categories are listed last in the stop-reason chart regardless of size.
STOP_CATEGORY_LAST <- c("Other", "Not reported")

label_of <- function(x, labels) {
  out <- unname(labels[x])
  ifelse(is.na(out), x, out)
}

# Named choice vectors for the filter inputs, in the order given (label = value).
labelled_choices <- function(values, labels) stats::setNames(values, label_of(values, labels))

fmt_int <- function(x) ifelse(is.na(x), "—", formatC(round(x), format = "d", big.mark = ","))
fmt_pct <- function(x, digits = 1) ifelse(is.na(x), "—", sprintf("%.*f%%", digits, 100 * x))
fmt_months <- function(x) ifelse(is.na(x), "—", sprintf("%.1f mo", x))

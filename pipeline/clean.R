# Interpret the raw registry values. Each rule here is a judgment call about messy data,
# so each is written as a small vectorised function with its own tests.

# ---- Phase -------------------------------------------------------------------------

# The registry stores phase as an array. Combined designs ("PHASE1|PHASE2") become their
# own category rather than being counted under both phases, so every trial is counted
# exactly once in any by-phase breakdown. "NA" is the registry's code for trials where
# phase does not apply (devices, behavioural interventions); an absent value is a
# different thing and is labelled "Not reported". Combinations the registry should not
# produce are labelled "Other" rather than coerced into a real phase.
normalize_phase <- function(phase_raw) {
  single <- c(
    EARLY_PHASE1 = "Early Phase 1", PHASE1 = "Phase 1", PHASE2 = "Phase 2",
    PHASE3 = "Phase 3", PHASE4 = "Phase 4", "NA" = "Not applicable"
  )
  vapply(phase_raw, function(p) {
    if (is.na(p) || p == "") return("Not reported")
    parts <- sort(unique(strsplit(p, "|", fixed = TRUE)[[1]]))
    if (length(parts) == 1 && parts %in% names(single)) return(single[[parts]])
    if (identical(parts, c("PHASE1", "PHASE2"))) return("Phase 1/2")
    if (identical(parts, c("PHASE2", "PHASE3"))) return("Phase 2/3")
    "Other"
  }, character(1), USE.NAMES = FALSE)
}

# ---- Dates -------------------------------------------------------------------------

# Registry dates come at day ("2012-08-21") or month ("2012-08") precision. Month-
# precision dates are imputed to the 15th, which bounds the error at about +/-15 days
# instead of up to a month, and the precision is kept so the imputation is auditable.
# Anything else, including impossible calendar dates, becomes NA.
parse_ct_date <- function(x) {
  precision <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), "day",
               ifelse(grepl("^\\d{4}-\\d{2}$", x), "month", NA_character_))
  iso <- ifelse(precision == "month", paste0(x, "-15"), x)
  date <- as.Date(iso, format = "%Y-%m-%d")
  precision[is.na(date)] <- NA_character_
  list(date = date, precision = precision)
}

# Start to primary completion, in months, only when both dates are ACTUAL. An ESTIMATED
# date is a plan, not an observation, so including it would mix forecasts into the
# benchmark. A completion date before the start date is a data-entry error and also
# yields NA.
duration_months <- function(start, start_type, end, end_type) {
  ok <- !is.na(start) & !is.na(end) &
    start_type %in% "ACTUAL" & end_type %in% "ACTUAL" & end >= start
  out <- rep(NA_real_, length(start))
  out[ok] <- as.numeric(end[ok] - start[ok]) / (365.25 / 12)
  out
}

# ---- Why stopped -------------------------------------------------------------------

# whyStopped is free text ("Low Accrual", "poor recrutment", "Sponsor decision", ...).
# Reasons are assigned by the first rule that matches, so the order is part of the
# definition: specific external causes first, then outcome-driven reasons, then
# operational ones. "Sponsor decision due to slow enrollment" is therefore low accrual,
# the underlying cause, rather than a business decision, and "PI no longer available"
# is an investigator issue because that rule is checked before drug supply. Text that
# matches nothing is "Other" and is reported, not hidden.
STOP_RULES <- c(
  "COVID-19"                        = "covid|coronavirus|sars-?cov|pandemic",
  "Safety"                          = "safety|toxic|adverse (event|effect|reaction)|side effect|\\bdeaths?\\b|\\bdied\\b|fatal|\\bdsmb\\b|\\bdmc\\b",
  # "futility in recruitment" is an accrual problem, not an efficacy finding.
  "Efficacy / futility"             = "futil(?!ity (in|of|for) (the )?(recruit|accru|enrol))|efficacy|lack of (benefit|effect|response|activity)|insufficient (clinical )?(benefit|activity|response)|no (clinical |perceivable |significant |added )?(benefit|effect|value)|ineffective|interim|did not meet|endpoint|outcome measures",
  # recrut/reclut: the spelling used by Romance-language registrants ("recrutement",
  # "reclutamiento"), and a common English misspelling.
  "Low accrual"                     = "accru|enrol|recruit|recrut|reclut|retention|competing|slow|\\bcandidates\\b|insufficient (number of )?(patients|subjects|participants)|insufficient (patient |study )?(population|sample)|lack of (eligible )?(patients|subjects|participants)|(few|no) (eligible )?(patients|subjects|participants)|inclu(de|sion|ding) (of )?(new )?(patients|participants|subjects)|target (population|sample|number) (was )?not (reached|achieved)",
  "Funding"                         = "fund|financ|budget|\\bgrant\\b|money|resourc|\\bcosts?\\b|(loss|lack|withdrawal) of support",
  "Investigator / site"             = "investigator|\\bp\\.?i\\.?\\b|personnel|departure|left the (institution|university|hospital|company)|relocat|retire|staff|(site|clinic|unit|center|centre|department)s? (was |were |has been )?clos",
  "Drug supply"                     = "supply|unavailab|(drug|product|device|agent|medication|vaccine|compound|substance)s? (is |was |are |were )?(no longer|not) availab|availability of (the )?(study )?(drug|product|device)|manufactur|shortage|expir|not enough (product|drug)|production|substance discontinued",
  # Includes a competing product reaching the market: the trial's question changed.
  "Standard of care / new evidence" = "standard of care|landscape|treatment (standard|guideline|guide)|practice of medicine|(clinical|medical) practice|patterns of care|new (data|evidence|information|findings)|(other|similar|previous|recent) (studies|study|trials?)|published (data|results)|became (fda |commercially )?(approved|available)|is now (approved|available|ce[- ]marked)|ce[- ]marked",
  "Business / sponsor decision"     = "business|sponsor|strateg|company|commercial|portfolio|priorit|decision|decided|merger|acquisition|(stop|halt|discontinu|suspend|terminat|ceas|end)\\w* (the |its |all )?(clinical )?development|development (program|plan|strateg|was|has been|of (the|this) (product|drug|compound|asset))",
  "Regulatory / administrative"     = "regulator|\\bfda\\b|\\birb\\b|\\bethic|authorit|policy|approv|administrat|compliance|contract",
  "Feasibility / logistics"         = "feasib|logistic|technical|equipment|implementation|operational|redesign|overhaul|(study|trial) design|protocol (amend|change|revis|modif)|device (modification|malfunction|issue|problem)",
  "Never started"                   = "never (started|began|begun|opened|initiated|activated|moved forward)|not (started|initiated|opened|activated)|before (active|activation|enrollment|start)"
)

# Sponsors routinely add a disclaimer when a trial stops for business reasons ("No
# safety concerns contributed to this decision", "not due to safety or efficacy").
# Matched naively, that disclaimer files the trial under Safety, so negated mentions are
# removed before matching. Each pattern requires the trailing noun or a causal phrase:
# "no safety concerns" is a disclaimer, while "no efficacy" is a genuine reason. Paired
# terms ("safety or efficacy") are removed together so the second one doesn't survive.
NEGATED_TERM <- "(safety|efficacy|toxicity|tolerability)"
NEGATED_TERMS <- sprintf("%1$s( (or|and|nor) (any )?%1$s)?", NEGATED_TERM)
NEGATED_MENTIONS <- paste(
  sprintf("\\bno (new |known |significant |major |specific )?%s (concerns?|issues?|signals?|reasons?|findings?|problems?|risks?)", NEGATED_TERMS),
  sprintf("\\bnot (due to|related to|because of|based on|a result of|the result of|a consequence of|driven by|for|associated with|prompted by|in response to|linked to) (any |a |the )?(new )?%s", NEGATED_TERMS),
  sprintf("\\bunrelated to (any |the )?%s", NEGATED_TERMS),
  "\\bnon-?safety",
  sprintf("\\bnot (a |any )?%s[- ]related", NEGATED_TERM),
  sprintf("\\bwithout (any )?%s (concerns?|issues?)", NEGATED_TERMS),
  sep = "|"
)

categorize_stop_reason <- function(text) {
  out <- rep(NA_character_, length(text))
  has_text <- !is.na(text) & nzchar(trimws(text))
  lowered <- gsub(NEGATED_MENTIONS, " ", tolower(text), perl = TRUE)
  for (category in names(STOP_RULES)) {
    hit <- has_text & is.na(out) & grepl(STOP_RULES[[category]], lowered, perl = TRUE)
    out[hit] <- category
  }
  out[has_text & is.na(out)] <- "Other"
  out
}

# ---- Assemble ----------------------------------------------------------------------

# Raw parsed studies -> the analysis table written to SQLite.
clean_studies <- function(studies) {
  start <- parse_ct_date(studies$start_date_raw)
  pcd <- parse_ct_date(studies$primary_completion_date_raw)

  stopped <- studies$overall_status %in% c("TERMINATED", "WITHDRAWN", "SUSPENDED")
  stop_category <- categorize_stop_reason(studies$why_stopped)
  stop_category[stopped & is.na(stop_category)] <- "Not reported"
  stop_category[!stopped] <- NA_character_

  data.frame(
    nct_id                            = studies$nct_id,
    title                             = studies$title,
    overall_status                    = studies$overall_status,
    why_stopped                       = studies$why_stopped,
    stop_category                     = stop_category,
    phase                             = normalize_phase(studies$phase_raw),
    phase_raw                         = studies$phase_raw,
    enrollment                        = studies$enrollment,
    enrollment_type                   = studies$enrollment_type,
    start_date                        = format(start$date),
    start_date_precision              = start$precision,
    start_date_type                   = studies$start_date_type,
    start_year                        = as.integer(format(start$date, "%Y")),
    primary_completion_date           = format(pcd$date),
    primary_completion_date_precision = pcd$precision,
    primary_completion_date_type      = studies$primary_completion_date_type,
    duration_months                   = duration_months(start$date, studies$start_date_type,
                                                        pcd$date, studies$primary_completion_date_type),
    sponsor                           = studies$sponsor,
    sponsor_class                     = ifelse(is.na(studies$sponsor_class), "UNKNOWN", studies$sponsor_class),
    site_count                        = studies$site_count,
    stringsAsFactors = FALSE
  )
}

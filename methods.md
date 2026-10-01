### What's in the data

Interventional studies registered on **ClinicalTrials.gov** with a start date from
2010 through 2025, in two therapeutic areas. Registration became mandatory for most
interventional trials in late 2007 (FDAAA 801), so earlier years over-represent sponsors
who registered voluntarily. 2025 is the last complete year.

**Therapeutic area** follows NLM's MeSH hierarchy, not keyword matching: a trial is
*oncology* if any of its conditions maps to *Neoplasms* (MeSH C04) or a descendant, and
*cardiovascular* for *Cardiovascular Diseases* (C14). Both the condition's own term and
its ancestors are matched; matching ancestors alone silently drops trials whose term *is*
the top descriptor. A trial can be in both areas and counts once in every headline number.

### How each number is defined

| Measure | Population | Why |
|---|---|---|
| Enrollment | Trials whose enrollment is **actual**, and above zero | An *estimated* count is the sponsor's target, not what was achieved. Withdrawn trials enrolled no one by definition. |
| Sites per trial | Trials listing at least one location | An empty location list usually means sites were never registered, not that there were none. |
| Duration | Start to **primary completion**, both dates **actual** | Estimated dates are plans. Completed and terminated trials are shown separately, because a terminated trial's completion date is when it stopped. |
| Stopped early | Terminated or withdrawn, out of trials with a **final status** | Ongoing trials haven't had the chance to stop. *Unknown*-status trials (not verified in two years) have no reliable outcome. Phases with fewer than 20 finished trials aren't charted. |

Medians and quartiles are computed in SQL with window functions; quartiles use the
nearest-rank method.

### Messy data, and how it's handled

- **Partial dates.** Many registry dates are month-precision (`2012-08`). They're
  imputed to the 15th, which bounds the error at about two weeks, and the precision is
  stored so the imputation can be audited.
- **Multi-phase trials.** `PHASE1 + PHASE2` becomes *Phase 1/2*, its own category, so
  every trial is counted once in a by-phase chart.
- **Free-text stop reasons.** `whyStopped` is free text, so reasons are grouped by
  documented rules. Sponsor disclaimers such as *"no safety concerns contributed to this
  decision"* are removed before matching; without that, a naive keyword match files 61%
  more trials under *Safety*. 100 randomly sampled reasons were labelled by hand before
  the rules were scored against them: the rules agreed on **84**, with 95% precision and
  recall on low accrual, the largest category. Scoring exposed one bug (a negated
  "safety *or efficacy*" left "efficacy" behind); fixing it brought agreement to 85, and
  the rules were not tuned on that sample beyond the fix.

### Caveats

- Termination rates for recent start years skew high: a trial that started in 2023 can
  only have a final status yet if it stopped early.
- Sponsors update records irregularly, so a trial's status can lag reality.
- This is a feasibility *orientation* tool built on public registry data, not a
  substitute for a sponsor's own site and patient-population data.

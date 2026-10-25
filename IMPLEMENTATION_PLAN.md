# OSAS Dashboard 2026 Update Implementation Plan

## Purpose and success criteria

Update the dashboard so users can compare Oregon state, district, and school results; find high-performing schools or districts and strong grades for a selected student population; understand data availability; and move between views without interpreting dense or misleading charts.

The update is successful when:

- State, district, and school organizations remain available together in the comparison selector and are visibly identified by type.
- Filters live in a collapsible sidebar, leaving the main pane for charts, tables, notes, and downloads.
- Time-series views use spring testing years, break lines across missing years, and mark spring 2020–2021 with a labeled COVID-era data-gap band.
- Trend lines never connect different organizations; stable typed IDs prevent name changes or reused names from merging entities.
- Duplicate sources do not create duplicate points or ranks, and field-level source conflicts remain visible without suppressing agreed metrics.
- Rankings can combine any selected organization types and are explicitly scoped by organization types, subject, grade, year, student group, and tested-student range. Results show both an overall rank and a within-type rank.
- Users can identify top organizations in a chart and table, then inspect those organizations across grades relative to same-grade Oregon results.
- A four-organization trend updates locally in under 2 seconds, an eight-organization trend in under 4 seconds, and steady app data memory stays below 200 MB.

## Product decisions and boundaries

- Keep all three organization levels in one persistent comparison picker. Order choices as State, District, and School without repeating the type in each label; add district context only to otherwise duplicate school names; support live search; and default to Oregon. A selected school may add its current parent district and Oregon as benchmarks with one action.
- Limit Trends and Cohorts to eight total organizations, including automatically added benchmarks. Statewide exploration belongs in Rankings and Coverage.
- Use a 320-pixel collapsible sidebar with global Student group and single-select Subject controls, one persistent comparison/pin selector shared by all views, and view-specific controls. Trends and Cohorts consume the shared organization selection; Rankings treats it as pin/benchmark state. The sidebar starts open on desktop and must remain usable as an overlay/collapsed control surface on narrow screens.
- Organize the main pane into four views: **Trends**, **Rankings**, **Cohorts**, and **Coverage & Data**. Source notes, methodology, and credits move into an About/Data Notes panel in the final view.
- Rankings use `percent_proficient` only in this release. Achievement-level percentages remain available in Trends with friendly labels.
- Ranking organization type is a multi-select containing State, District, and School, with all three selected by default. The mixed display assigns an overall rank across the selected types and a separate within-type rank. Clearly identify every result's type and explain that state, district, and school units overlap and the combined rank is a descriptive ordering rather than a comparison of independent peers.
- Keep student groups in an alphabetical searchable picker for this release. Do not introduce a manually maintained category taxonomy yet.
- Treat cohort charts as synthetic grade-cohort trajectories, not longitudinal student tracking, and state that limitation beside the chart.
- Do not add maps, external geographic data, or URL bookmarking in this release. Do not deploy or publish until user acceptance testing is complete.

## Implementation

### 1. Build a slim, canonical dashboard artifact

- Add `scripts/build_dashboard_data.R` as a reproducible offline build step. It will read the full dated ODE RDS, select only dashboard-used columns, normalize display labels, derive `spring_year`, canonicalize duplicate records, and write a versioned slim artifact plus a validation summary. The app will load only the slim artifact; the full source RDS remains in the source workflow but is excluded from the deployment bundle.
- Define `organization_key` as `state:oregon`, `district:<district_id>`, or `school:<school_id>`. Use it for selection values, grouping, colors, deduplication, trends, and joins. Display the most recent name plus type/district context, retain historical names in tooltips, and treat changed school IDs as distinct entities unless a curated crosswalk is added later.
- Define the logical observation key as `spring_year + subject + grade + student_group + organization_key`. Preserve district context as metadata and validate that each school key has one parent district for a given spring year.
- Resolve duplicate sources by field:
  - Collapse a field when all duplicate sources agree.
  - If proficiency or an achievement-level value conflicts, omit that observation only from views using the conflicting metric and record the conflict.
  - If participant counts conflict but proficiency agrees, retain proficiency, store the participant range and conflict flag, and use the minimum count for tested-student eligibility.
  - If participation rates conflict but proficiency agrees, retain proficiency, show the rate range and conflict flag, and do not discard the ranking record.
  - Preserve unique source file/URL pairs so every displayed source opens its own link.
- Store source filename/date, build timestamp, row counts, duplicate counts, and field-level conflict counts in the artifact metadata and validation output. Surface these counts in Coverage & Data.
- Produce shared prepared tables for trend series, distinct-organization coverage, ranking inputs, state comparisons, and organization lookup. Validate dynamic metric names against an explicit allow-list.
- Remove unused packages, Sankey styling, debug statements, and unused global objects. Move dashboard CSS into `www/dashboard.css`.

### 2. Rebuild navigation and sidebar controls

- Expand the existing `shinydashboard` sidebar and give `sidebarMenu` an input ID for the four views.
- Place alphabetical searchable Student group and single-select Subject controls at the top, followed by the one persistent Organization comparison/pin picker ordered by organization type. Organization choices retain all types without adding redundant type text to their labels; selection values use `organization_key` rather than names. Coverage ignores the global Subject selection and explicitly displays all subjects.
- Add view-specific controls:
  - Trends: metric, grades, endpoint-label toggle, and benchmark action. Controls update automatically after a short debounce.
  - Rankings: spring year, organization types (multi-select; State, District, and School selected by default), grade, Top N, and inclusive tested-student range.
  - Cohorts: graduating classes, metric, grades, and benchmark action. Controls update automatically after a short debounce.
  - Coverage & Data: organization type and download filters.
- Use debounced automatic updates for Trends and Cohorts so picker changes feel immediate without triggering redundant renders. Multi-select menus place selected options first so they are easy to find and remove. Reject a ninth total organization with a clear message; automatically added benchmarks count toward eight.
- Keep the compact multi-grade Trend layout as the desktop default and provide a vertically draggable, keyboard-operable resize handle for users who want a larger Assessment outcomes plot. Switch to two facet columns and a taller minimum plot on narrow screens.
- Preserve filter and pin state while switching views. Provide Reset filters, explicit empty/error messages, keyboard-reachable controls, and responsive cards. Verify the existing Bootstrap 3 sidebar behavior at narrow widths and add only the CSS/JS needed to make its collapsed/overlay state usable.

### 3. Correct and redesign Trends and Cohorts

- **Trends:** Render one selected subject section with grade small multiples and stable organization colors. Group every trace by `organization_key` within its facet. Use actual spring-year ticks from 2015–2026 and insert explicit missing 2020/2021 values so no line bridges 2019 to 2022.
- Draw a translucent time band from 2019.5 through 2021.5 labeled **“No records in dashboard source data — spring 2020–2021.”** Keep that wording unless an official ODE source is separately verified for a broader claim.
- Render Oregon and each selected school’s unique parent district as optional neutral benchmark lines. If multiple selected schools have different parent districts, add each distinct parent once, subject to the eight-organization limit.
- **Cohorts:** Correct trace grouping by `organization_key + class_of`. Because its x-axis is grade, do not draw a calendar-year COVID band. Derive each point's testing year as `class_of - (12 - grade_num)`; within each subject's valid tested-grade sequence, insert `NA` only where that derived spring year is 2020 or 2021. Do not treat normally untested grades as COVID gaps. Show an adjacent note explaining the missing source years.
- Integrate participation into tooltips and add a compact participation trend toggle in Trends rather than rendering a separate full chart on initial load.

### 4. Build mixed-type Rankings and grade heatmap

- Render a sorted horizontal dot/lollipop chart plus a linked compact table. The eligible set has one of the selected organization types, the selected spring year, subject, grade, and student group; nonmissing proficiency and participant count; and participant count within the inclusive bounds. Use the minimum participant count when duplicate sources conflict.
- Rank unrounded proficiency values with competition ranking (`min_rank(desc(percent_proficient))`), rounding only for display. Calculate an **Overall Rank** across every eligible result from the selected types and a **Type Rank** within State, District, or School. Show “Overall Rank X of N selected organizations” and “Type Rank Y of M [type] organizations.”
- Calculate tie-aware overall and within-type proficiency percentiles from ascending average rank: when `N > 1`, `100 * (average_rank_ascending - 1) / (N - 1)`; when `N == 1`, return 100. Round only displayed percentiles.
- Show overall rank/percentile, type rank/percentile, proficiency, same-grade Oregon difference, tested-student value/range, participation value/range, district, organization type, conflict flags, and source links. If the matching Oregon result is missing, show “Comparison unavailable,” never zero.
- Encode organization type redundantly with a visible Type column and chart point shape while retaining color for organization identity. Place a concise note above the chart explaining that state, district, and school results represent overlapping populations.
- In the separate pinned-comparison section, an eligible selected organization outside Top N retains and displays both actual ranks; an organization outside participant bounds displays “Not rank-eligible”; and an organization whose type is not selected remains visible as an unranked benchmark labeled “Type excluded.”
- **Grade heatmap:** Fix rows to the Top N organizations from the combined selected-type ranking for the selected grade plus pinned organizations. Columns are available tested grades for the same subject, spring year, and student group. Exclude `All Grades` from heatmap columns, retain a visible type marker on each row, and display percentage-point difference from the matching Oregon grade, with raw proficiency, N, and conflicts in the tooltip.
- Keep `All Grades` available as an explicitly labeled aggregate in the ranking grade control, but never treat it as a grade-specific heatmap column.

### 5. Add Coverage & Data, then polish performance and charts

- Replace the current raw-record summary with a year-by-subject coverage heatmap and distinct State/District/School counts for the selected student group. Add filtered CSV download, paired source links, last-update/build metadata, suppression and missing-data notes, and the field-level conflict report.
- Use friendly metric names, percent-formatted axes, descriptive subtitles, and explicit spring-year labels. Tooltips include organization/type, current and historical name when different, district, spring year, subject, grade, student group, metric, tested N/range, participation/range, conflicts, and a bounded source preview. Clicking an observation in Trends, Rankings, ranking heatmaps, or Cohorts persists the complete server-validated tooltip plus a list of linked source files below the plot; another observation updates the panel and clicking the selected observation again clears it.
- Enclose every chart in a visible bordered card with its title inside the card header. Give each faceted small-multiple panel its own border and an attached bordered title strip so grades, cohorts, and organization types cannot be visually mismatched.
- Use a colorblind-safe eight-color palette with a stable `organization_key` mapping for the session. Render benchmarks in neutral line styles and reinforce identity with point symbols or line dashes rather than color alone.
- Label only final available points, dynamically size plots by grade-panel count, and keep legends compact. Replace the hard-capped student-count histogram with a tested-student distribution tied to the inclusive range control; use a log axis when the displayed span exceeds one order of magnitude.
- First meet performance targets through slim canonical data, deduplication, one subject, eight total organizations, active-tab rendering, shared reactives, and bounded caching. Retain `ggplotly()` if targets are met; replace individual charts with direct Plotly traces only when measurements show a target or widget-payload failure.

## Verification and acceptance tests

### Automated checks

- Extract pure preparation, organization-identity, duplicate-resolution, ranking, percentile, benchmark, COVID-gap, and trend-series helpers. Test them directly with `testthat`.
- Cover stable typed IDs, historical names, reused names, parent-district validation, exact duplicates, field-specific conflicts, paired sources, distinct coverage counts, metric allow-listing, inclusive bounds, minimum-N conflict handling, unrounded competition ties, midrank percentiles, missing Oregon comparisons, and empty data.
- Add regression tests proving two selected organizations yield independent traces within each facet; spring 2019 does not connect to 2022; the COVID band bounds are 2019.5–2021.5; cohort gaps occur at the correct grades; all three ranking types are selected by default; overall ranks span the selected types; and type ranks are calculated within type.
- Use `shiny::testServer()` for reactive dependencies, automatic Trend and Cohort updates, the eight-total-organization limit, benchmark addition, persistent pins, ranking organization-type selection, pinned eligibility status, and download contents. Do not use it as proof of browser layout or Plotly rendering.
- Parse all R files, rebuild the slim artifact from the dated source, start the app in a clean R session, and verify runtime dependencies—not development-only packages such as `testthat`—in the deployment manifest.

### Manual acceptance scenarios

- Compare Oregon, one district, and two schools across several grades; colors stay stable, lines never cross-connect, and benchmark styling is distinct.
- Search the unified picker and add a state, district, and school. Add parent-district/Oregon benchmarks, confirm they count toward eight, and receive a clear message when a ninth organization would be added.
- Confirm spring 2020 and 2021 show as the labeled source-data gap and no trend line bridges 2019 to 2022. Confirm cohort charts break at affected grades and use a note instead of a time band.
- Narrow the organization-type selector to Schools and find the top 10 for a chosen spring year, subject, grade, and student group; change inclusive tested-student bounds and confirm chart, table, ranks, percentiles, and heatmap agree. Repeat with Districts only, then restore all three types and confirm the combined view.
- Confirm State, District, and School are selected by default; the mixed result shows correct overall and type ranks; changing the type selection recomputes both; ties share a competition rank; ranking uses unrounded values; and missing Oregon matches show “Comparison unavailable.”
- Select historically duplicated 2019 and conflicting 2023 combinations; confirm agreed proficiency appears once, conflicts/ranges are visible, size eligibility uses the minimum N, and every source link works.
- Confirm coverage counts distinct organizations, downloads match active filters, empty combinations explain why no data appears, and long names remain understandable.
- Perform keyboard/focus, color contrast, narrow-screen sidebar, mobile-width, label-density, and Plotly-hover checks in a real browser.
- Measure initial load, app data memory, four- and eight-organization trend latency, ranking-filter latency, trace count, and serialized widget payload. Required local targets are under 200 MB steady app data, under 2 seconds for four organizations, and under 4 seconds for eight; document any environment-dependent variance.

## Delivery sequence

1. Work on branch `updates-26`; keep this document synchronized with approved scope changes.
2. **Milestone 1 — trustworthy data:** implement the offline canonical artifact, typed identities, duplicate resolution, metadata, and pure helper tests.
3. **Milestone 2 — usable core:** implement sidebar/navigation, corrected Trends/Cohorts, and configurable mixed-type Rankings; pass core automated and manual scenarios before continuing.
4. **Milestone 3 — supporting analysis:** implement the grade heatmap, Coverage & Data, downloads, accessibility polish, and measured performance work.
5. Complete an internal edit/review pass, then ask a skeptic agent to review the implemented behavior against this plan. Incorporate justified revisions before returning the branch for user testing.
6. Hand off with a change log, validation report, known limitations, measured performance, test results, and local run instructions. Do not merge or deploy without a separate user request.

## Skeptic review record

An independent skeptic review was completed on 2026-10-24. The plan was revised to use typed ODE IDs, resolve duplicate conflicts by field, define ranking and percentile math, bound the heatmap population, distinguish trend and cohort COVID-gap treatments, count automatic benchmarks toward the render limit, defer taxonomy/bookmarking, generate a truly slim offline artifact, set measurable performance targets, and stage delivery into tested milestones. The skeptic recommended separate peer-type rankings; the user subsequently chose mixed-type rankings. The plan therefore defaults all organization types on and reports both combined and within-type ranks with an explicit overlapping-populations note.

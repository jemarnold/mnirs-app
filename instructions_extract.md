Extract intervals from the processed data using `mnirs::extract_intervals()` for further analysis.

Signal processing should be applied first on the *Process Data* page.

Display the full session in the top plot with *Start* and *End* events indicated with vertical dashed green and red lines. Intervals are displayed separately in facet plots below.

#### Interval Boundaries:
Specify *Start* and/or *End* boundaries by any combination of methods. Multiple values can be comma-separated. 

- **By Time**: values of `time_channel` (e.g. seconds).
- **By Label**: match text strings in the `event_channel` (or `event_labels` from manually placed event markers). Labels are pattern-matched by default; select *Fixed* for literal matching.
- **By Lap**: integer lap numbers from the `event_channel`.
- **By Sample**: row numbers of the data table.

#### Span:
Extend boundaries around events by a timespan, where negative values extend before the event, and positive values extend after the event. e.g. `span = c(-30, 60)` will extend the bounds *30-sec before* and *60-sec after* the specified interva.

#### Group Intervals:
Either keep each interval separate (*"Distinct"*), or *"Ensemble"*-average all intervals into one.

#### Zero Interval Time:
Rebase `time_channel` values to start at zero.

#### Free y-axis Scales:
Zoom-in each interval facet to it's own y-axis range.

#### Download Intervals:
Export extracted intervals to an Excel file.

#### Download Plots:
Save two PNG files: the full session plot with interval boundaries, and the facetted extracted intervals plot.

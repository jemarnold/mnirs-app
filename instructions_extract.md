Extract intervals from the processed data for further analysis. Interval boundaries can be defined by any combination of methods, which are resolved to time values and displayed on the full plot (start = green dashed, end = red dashed). Extracted intervals are displayed in a facetted plot below.

#### Interval Boundaries:
Specify *Start* and/or *End* boundaries by any combination of methods. Multiple values can be comma-separated.

- **By Time**: values of `time_channel` (i.e., seconds).
- **By Label**: match text labels in the `event_channel` (or `event_labels` from manually placed event markers). Labels are pattern-matched by default; select *Fixed* for literal matching.
- **By Lap**: integer lap numbers from the `event_channel`.
- **By Sample**: row numbers of the data table.

#### Span:
Extend boundaries by a signed timespan: negative values extend before *Start*, positive values extend after *End*. When only one boundary side is specified (e.g., *Start* only), a positive-width span is required to define the interval window.

#### Group Intervals:
Either keep each interval separate (*"Distinct"*), or combine intervals into an ensemble average (*"Ensemble"*).

#### Zero Interval Time:
Reset each interval time to start at zero.

#### Free y-axis Scales:
Display each interval facet with an independent y-axis range.

#### Download Intervals:
Export extracted intervals as an Excel file, one sheet per interval.

#### Download Plots:
Save two PNG files: the full session plot with interval boundaries, and the facetted extracted intervals plot.

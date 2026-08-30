mNIRS files can be imported and processed using standardised methods,
and displayed in a plot and data table. The plot is interactive and can
be zoomed in and out (changing settings will reset plot zoom).
Processed data can be downloaded for further analysis.

#### Upload File:
Upload an `.xls(x)`, `.csv`, or `.txt` file containing mNIRS
data exported from many common wearable devices. Files exported from
common NIRS devices should be automatically recognised, with all
detected `nirs_channels` returned.

#### mNIRS Channel Names:
Specify the column name(s) containing mNIRS data. Multiple channels
can be specified using comma-separated `new_name = file_column_name`
pairs.

Example: `smo2_left = SmO2, smo2_right = SmO2(2)`

#### Time/Sample Channel Name:
Specify the column containing time or sample values.

Example: `time = Timestamp (seconds)`

#### Lap/Event Channel Name (optional):
Optionally specify column with lap/event markers.

#### Zero Start Time:
Reset `time_channel` to start at zero.

#### Sample Rate:
Specify the exported data sample rate in Hz. This will be automatically
estimated from the time channel and can be manually overridden.

#### Resample Rate:
Data can be resampled to a higher or lower sample rate. Also used to
correct `time_channel` values for data with irregular or duplicated
samples.

#### Trim Head/Tail Timespan:
Remove samples from the beginning or end of the recording, specified in
units of `time_channel` (i.e., seconds).

#### Replace Invalid Values:
Replace specific fixed values (e.g., `c(0, 100)`) from `nirs_channels`.

#### Replace Outliers:
Remove local outliers using a moving window Hampel filter approach.

#### Replace Missing Values:
Linearly interpolate across missing values in `nirs_channel`.

#### Digital Filter Method:
Apply smoothing filters to improve signal-to-noise ratio. Methods
include a cubic *"smoothing-spline"*, a low-pass *"Butterworth"*
filter, or a simple *"moving average"*. Additional parameters for
each filter method can be specified.

#### Shift Data:
Move `nirs_channels` values up or down to a new specified reference
value, based on the *"first"*, *"minimum"*, or *"maximum"* data points.
Multiple channels can be shifted together (*"Ensemble"*) or
independently (*"Distinct"*).

#### Rescale Data:
Normalise `nirs_channels` to a new specified dynamic range. Multiple
channels can be shifted together (*"Ensemble"*) or independently
(*"Distinct"*).

#### Correct Blood Volume:
Normalise `oxy[haem]` (O2Hb) and `deoxy[haem]` (HHb) channels for
blood-volume changes (method developed from *Beever & Tripp et al,
2020*). Specify at least two of the oxy, deoxy, and total (THb)
channels; the third is derived. After correction, `total[haem]` is
definitionally zero.

#### Place Event Markers:
Manually add event markers at specified time points. Will add an
`event_channel` to the data table if not already specified. If the
existing `event_channel` holds character labels, markers are written
there; if it holds integer lap numbers, it is left unchanged and
markers are written to a new `event_labels` column placed after it
(*By Label* on the Extract page then matches `event_labels`).

#### Keep All Columns:
Either keep all columns in the file data table (the default), or keep
only the channels specified.

#### Download Data:
Export processed data as an Excel file for further analysis.

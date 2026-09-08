mNIRS files can be imported and processed using recommended methods, and displayed in a plot and data table. 

The plot is interactive and zoomable (changing settings will reset plot zoom). Processed data and plot can be downloaded for further analysis.

#### Upload File:
Upload an `.xls(x)`, `.csv`, `.txt`, or `.ftn(2)` file containing mNIRS data exported from many common wearable devices. Files exported from known NIRS devices should be automatically recognised and return relevant default channels.

#### mNIRS Channel Names:
Specify the column name(s) containing mNIRS data. Multiple NIRS channels can be specified and renamed using comma-separated `new_name = file_column_name` pairs.

Example: `smo2_left = SmO2, smo2_right = SmO2(2)`

#### Time Channel Name:
Specify the column containing time or sample values.

Example: `time = Timestamp (seconds)`

#### Lap/Event Channel Name (optional):
Specify a column containing event labels or lap integers.

#### Zero Start Time:
Rebase `time_channel` values to start at zero.

#### Display time as "mm:ss":
Plots will diplay x-axis `time_channel` values as time format. The data table will still contain numeric values.

#### Sample Rate:
Specify the exported sample rate in samples per second (Hz). This will be automatically estimated from the time channel and can be manually overridden.

#### Resample Rate:
Data can be up- or down-sampled, or resampling to the same `sample_rate` will regularise the time grid for data with irregular samples.

#### Trim Head/Tail Timespan:
Remove samples from the beginning or end of the recording, specified in units of `time_channel` (e.g. seconds).

#### Replace Invalid Values:
Replace specific fixed values (e.g., `c(0, 100)`) from `nirs_channels`.

#### Replace Outliers:
Remove local outliers using a moving window Hampel filter approach.

#### Replace Missing Values:
Linearly interpolate across missing values.

#### Digital Filter Method:
Apply smoothing filters to improve signal-to-noise ratio. Methods include a cubic *"smoothing-spline"*, a low-pass *"Butterworth"* filter, or a simple *"moving average"*. Additional parameters for each filter method can be specified.

#### Show Raw Tracings:
Un-filtered signals will be diplayed under the smoothed data, to compare and empirically select filter parameters.

#### Shift Data:
Move `nirs_channels` values up or down to a new specified reference value, based on the *"first"*, *"minimum"*, or *"maximum"* data points. Multiple channels can be shifted together (*"Ensemble"*) or independently (*"Distinct"*).

#### Rescale Data:
Normalise `nirs_channels` amplitude to a new specified dynamic range. Multiple channels can be shifted together (*"Ensemble"*) or independently (*"Distinct"*).

#### Correct Blood Volume:
Normalise `oxy[haem]` (O2Hb) and `deoxy[haem]` (HHb) channels for changes in `total[haem]` (THb); i.e. blood-volume changes. Specify at least two of the oxy, deoxy, and total channels. After correction, `total[haem]` is definitionally zero and the oxy and deoxy channels are mirror images.

#### Place Event Markers:
Manually add event markers at specified time points. Will add an `event_channel` to the data table if not already specified. If an existing `event_channel` holds character labels, markers are written there. If it holds integer lap numbers, it is left unchanged and markers are written to a new `event_labels` column placed after it (*By Label* on the Extract page then matches `event_labels`).

#### Keep All Columns:
Either keep all columns in the data table, or keep only the channels specified.

#### Download Data:
Export processed data to an Excel file for further analysis.

#### Download Plots:
Save the currently rendered plot as a PNG file.


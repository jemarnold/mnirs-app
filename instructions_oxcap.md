This page handles recursive kinetics analysis specifically designed for evaluating muscle oxygen uptake (mV̇O<sub>2</sub>) recovery rate, a proxy for muscle oxidative capacity (OxCap) assessment from a repeated occlusion protocol.

#### Prerequisites:

1. **Process Data**: import the repeated-occlusion file, process as required, and *Correct for changes in Blood Volume*.

2. **Extract Intervals**: extract each occlusion interval from event labels or time values, etc. `span` can be used to adjust the boundaries, e.g. to exclude mechanical signal disruption caused by the occlusion inflation. 

3. **Analyse Kinetics**: fit the **Peak Slope** method on the desired NIRS channel with an appropriate window, e.g. a 3-sec slope `span` for a 5-sec occlusion interval. The slope during occlusions represents the local mV̇O<sub>2</sub> rates.

4. **OxCap Analysis**: A *Monoexponential*-family model will be fit across the slope and time values automatically. Multiple repeated occlusion trials can be *grouped* by interval number and analysed separately.

The rate constant *k* of the recovery curve primarily quantifies oxidative capacity. See the [OxCap analysis article](https://jemarnold.github.io/mnirs/articles/oxcap-analysis.html) for the full method.


#### Recovery Model:

- **Monoexponential**: Three or four-parameter monoexponential model fit, optionally including a time delay (*TD*) term. Time constant *tau* and rate constant *k* `= 1/tau` represent the rate of mV̇O<sub>2</sub> recovery, a proxy for muscle oxidative capacity.

- **Exponential Drift**: Two-phase kinetics with a fast monoexponential *primary* phase and a slow linear drift *secondary* phase. *<Unvalidated experimental method>*.

- **Biexponential**: Two concurrent exponential phases: a fast *primary* component driving the initial excursion, and a slow *secondary* component (`B2`, `tau2`) recovering toward a stable plateau. Falls back to *Exponential Drift*, then *Monoexponential*, when the data do not support both phases. *<Unvalidated experimental method>*.

#### Fit Time Delay (TD):
Attempts to fit a time delay `TD` between the first occlusion and the recovery onset. If unchecked, or if the fit fails (with a warning), falls back to a reduced model without `TD`.

#### Drift Onset Fraction:
For *Exponential Drift*. Fraction of the primary response amplitude (between `0.5` and `1`, exclusive) at which the linear secondary drift begins (default `0.95`; i.e. `TD + 3 * tau`). The drift onset is held constant, not estimated.

#### Group Intervals:
Fit trials separately by listing the sample numbers (interval number) per trial, comma-separated with optional names, e.g. `trial1 = 1:10, trial2 = 11:20`. By default (blank/`NULL`) fits all samples as a single curve.

#### Zero Trial Start Times:
Rebase each trial to start from time zero (default on).

#### Free y-axis Scales:
Zoom-in each interval facet to its own y-axis range.

#### Show Result Labels:
Annotate the plot with *tau* and *k* coefficients per trial. *k* is reported in min⁻¹ (`k × 60`); the coefficients table gives both *k* (reciprocal units of `time_channel`, i.e. sec⁻¹) and `k_min`.

#### Download Fitted Data:
Export observed and fitted data to an Excel file.

#### Download Coefficients:
Export an Excel file with model coefficients, diagnostics, warnings, and channel argument tables.

#### Download Plot:
Save the recovery fit plot as a PNG file.

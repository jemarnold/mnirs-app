Fit kinetics models to extracted intervals using `mnirs::analyse_kinetics()`. 

Intervals should first be defined on the *Extract Intervals* page.

Fitted models are displayed in a facetted plot, with coefficients, model diagnostics, and any warnings in tables below.

#### NIRS Channels:
Select one or more NIRS channels to analyse.

#### Kinetics Method:
All kinetics return coefficients for:
- `interval`: the ordered interval number from *Extract Intervals*.
- `nirs_channel`: the channel analysed.
- `start_time`: the `time_channel` value at the response start; i.e. *Start* time from *Extract Intervals*, or `0` if time rebased to zero.
- All time coefficients are in units of `time_channel` (e.g. seconds), and durations are relative to `start_time`.

Most kinetics also return:
- `A`: The mean baseline value before the response start, or the starting asymptote estimate.
- `B`: The ending extrema value (peak or trough) of the response, or the ending asymptote estimate.

### Response Time: 
Time taken to reach a specified *Response Fraction* of the response amplitude (default `0.5` = half-response time).
Coefficients: 
- `response_time`: time to reach the specified fraction of the response from `start_time`.
- `response_value`: `nirs_channel` value at the `response_time`.

### Peak Slope: 
Steepest (positive or negative) slope from a rolling linear regression. Specify the rolling window by one of either *Width* (number of samples) or *Span* (timespan). Window alignment can be *centred*, *left*, or *right*.
Coefficients:
- `slope`: the linear slope `dx/dt` in units of `nirs_channel` over `time_channel`.
- `intercept`: the `nirs_channel` value at `time_channel = start_time` (not necessarily at zero).
- `peak_slope_time`: the duration to where the peak `slope` occurs from `start_time`.

### Monoexponential: 
Three or four-parameter monoexponential model fit, optionally including a time delay (*TD*) term.
Coefficients:
- `TD`: time delay to the start of the exponential response from `start_time`, if specified.
- `tau`: the time constant, a property of an exponential function acceleration/deceleration rate.
- `k`: the rate constant; the reciprocal of `tau` (`k = 1 / tau`), in reciprocal time units (e.g. `sec^-1`).
- `MRT`: the mean response time; the time sum `TD + tau`.

### Exponential Drift: 
Two-phase kinetics with a fast monoexponential *primary* phase and a slow linear drift *secondary* phase. The secondary phase begins at `0.95` of the primary response amplitude by default, i.e. at `TD + 3 * tau`.
Coefficients as for *Monoexponential*, plus:
- `slope_B`: the linear drift rate `dx/dt` at the ending asymptote `B`.
- `texc`: the excursion time, where the drift rate overtakes the decaying primary rate, from `start_time`. 

### Biexponential: 
Two concurrent exponential phases: a fast *primary* component driving the initial excursion, and a slow *secondary* component (coefficients: `B2`, `tau2`) recovering the response toward a stable plateau.
Coefficients as for *Monoexponential*, plus:
- `B2`: the ending asymptote estimate of the slow phase.
- `tau2`: the time constant of the slow phase (typically `tau2 >> tau`).
- `texc`: the excursion time, where the secondary phase overtakes the decaying primary phase, from `start_time`.

### Sigmoidal: 
Four-parameter sigmoid fit with *Symmetric* (generalised logistic), *Gompertz*, or *Gompertz-Left* shapes. *Gompertz* (early acceleration, inflection near `A`) suits fast-onset, slow-tail responses; *Gompertz-Left* (late acceleration, inflection near `B`) suits slow-onset, fast-tail responses.
Coefficients:
- `xmid`: the time to the inflection point; the steepest point of the response, from `start_time`. `xmid` occurs at 50% (in the *middle*) of the response amplitude for the *Symmetric* form, 36.8% (`1/e`) for *Gompertz*, and 63.2% (`1 - 1/e`) for *Gompertz-Left*.
- `slope`: the response rate `dx/dt` at the inflection.

### Sigmoidal Drift: 
Two-phase kinetics with a fast sigmoidal *primary* phase and a slow linear drift *secondary* phase. The secondary phase begins at `0.95` of the primary response amplitude by default.
Coefficients as for *Sigmoidal*, plus:
- `slope_B`: the linear drift rate `dx/dt` at the ending asymptote `B`.
- `texc`: the excursion time, where the drift rate overtakes the decaying primary rate, from `start_time`. 


#### Fit Time Delay (TD):
For *Monoexponential*, *Exponential Drift*, and *Biexponential*. Attempts to fit a time delay `TD` between `start_time` and the response onset (default on). If unchecked, or if the fit fails (with a warning), falls back to a reduced model without `TD`.

#### Sigmoid Shape:
For *Sigmoidal* and *Sigmoidal Drift*. *Symmetric* (default; inflection at 50% amplitude), *Gompertz* (early inflection; 36.8%), or *Gompertz-Left* (late inflection; 63.2%).

#### Drift Onset Fraction:
For *Exponential Drift* and *Sigmoidal Drift*. Fraction of the primary response amplitude (between `0.5` and `1`, exclusive) at which the linear secondary drift begins (default `0.95`; i.e. `TD + 3 * tau` for *Exponential Drift*). The drift onset is held constant, not estimated.

#### Start Time:
Time at which the kinetics response begins. By default (blank/`NULL`) takes the event *Start* times defined in *Extract Intervals*.

#### End Window Timespan:
Set the fitting window limit, where the first extrema (peak/trough) is returned with no greater extrema within the specified timespan. The fitting window is limited to that timespan. By default (blank/`NULL`) fits the full window.

#### Response Direction:
Response direction; *"Auto"* detects automatically for each interval and `nirs_channel` responses. Or *"Positive"* / *"Negative"* can be specified for all responses.

#### Free y-axis Scales:
Zoom-in each interval facet to its own y-axis range.

#### Show Result Labels:
Annotate the plots with kinetics coefficients.

#### Download Fitted Data:
Export observed and fitted data to an Excel file.

#### Download Coefficients:
Export an Excel file with model coefficients, diagnostics, warnings, and channel argument tables.

#### Download Plot:
Save the facetted kinetics plot as a PNG file.

Fit kinetics models to extracted intervals using `mnirs::analyse_kinetics()`. Intervals must first be defined on the *Extract Intervals* page. Fitted models are displayed in a facetted plot, with coefficients, model diagnostics, and any warnings in tables below.

#### mNIRS Channels:
Select one or more mNIRS channels to analyse. The fit updates when the selection box loses focus.

#### Kinetics Method:
- **Response Time**: time taken to reach a specified *Response Fraction* of the response amplitude (default `0.5` = half-time).
- **Peak Slope**: steepest slope from a rolling linear regression. Specify the rolling window as either *Width* (number of samples) or *Span* (timespan), but not both. Window alignment can be *centred*, *left*, or *right*.
- **Monoexponential**: nonlinear monoexponential model fit, optionally including a time delay (*TD*) term.
- **Exponential Drift**: monoexponential model plus a linear drift term, beginning where the primary response reaches the *Drift Onset Fraction* of its amplitude (default `0.95`). Falls back to *Monoexponential* when the data do not support a drift.
- **Sigmoidal**: sigmoid model fit with *symmetric*, *Gompertz*, or *Gompertz-left* shapes.
- **Sigmoidal Drift**: sigmoid model plus a linear drift term, beginning where the sigmoid reaches the *Drift Onset Fraction* of its amplitude (default `0.95`). Falls back to *Sigmoidal* when the data do not support a drift.

#### Kinetics Window:
- **Start Time**: time at which the kinetics response begins (blank = interval start).
- **End Window Timespan**: limit the fitting window duration from the start time (blank = full interval).
- **Response Direction**: expected direction of the response; *"Auto"* detects from the data, or specify *"Positive"* or *"Negative"*.

#### Free y-axis Scales:
Display each interval facet with an independent y-axis range.

#### Show Result Labels:
Annotate the plot with fitted parameter estimates.

#### Download Fitted Data:
Export observed and fitted data as an Excel file, one sheet per interval.

#### Download Coefficients:
Export an Excel workbook with model coefficients, diagnostics, warnings, and channel arguments.

#### Download Plot:
Save the facetted kinetics plot as a PNG file.

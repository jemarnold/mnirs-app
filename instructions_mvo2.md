Model muscle oxidative capacity (OxCap) from a repeated arterial occlusion protocol. `mnirs::analyse_kinetics()` is called recursively: an exponential recovery model is fit through the *Peak Slope* results from the *Analyse Kinetics* page, where each occlusion slope is a proxy for muscle oxygen uptake (mVO<sub>2</sub>). The rate constant *k* of the recovery curve quantifies oxidative capacity. See the [OxCap analysis article](https://jemarnold.github.io/mnirs/articles/oxcap-analysis.html) for the full method.

#### Prerequisites:
1. **Process Data**: import the repeated-occlusion file and apply *Correct Blood Volume* (required for valid slope analysis).
2. **Extract Intervals**: extract each occlusion, e.g. start label `Occlusion` with a *Start Span* of `1` and *End Span* of `5` sec.
3. **Analyse Kinetics**: fit the **Peak Slope** method on the deoxy[haem] channel, e.g. a `3` sec *Window Span* with *Positive* response direction.

#### Recovery Model:
- **Monoexponential**: nonlinear monoexponential fit through the occlusion slopes over time. Coefficients *A* (peak mVO<sub>2</sub>) and *B* (resting asymptote), with time constant *tau* and rate constant *k* `= 1/tau`.
- **Exponential Drift**: monoexponential fit plus a linear drift term beginning at *Drift Onset* (a multiple of *tau*, default `3`), e.g. to separate a slow hyperaemic drift from the primary recovery. Experimental method.

#### Fit Time Delay (TD):
Include a time-delay (*TD*) parameter for a delayed exponential onset. Leave unchecked (default) when recovery is expected to begin immediately after the exercise stimulus.

#### Group Intervals:
Fit trials separately by listing the slope sample (row) numbers per trial, comma-separated with optional names, e.g. `trial1 = 1:16, trial2 = 17:32`. Blank fits all samples as a single curve.

#### Zero Trial Start Times:
Rebase each trial to start from time zero (default on).

#### Free y-axis Scales:
Display each trial facet with an independent y-axis range.

#### Show Result Labels:
Annotate the plot with *tau* and *k* per trial. *k* is reported in min⁻¹ (`k × 60`), as common in the OxCap literature; the coefficients table gives both *k* (sec⁻¹) and `k_min`.

#### Download Fitted Data:
Export observed slopes and fitted recovery data as an Excel file, one sheet per trial.

#### Download Coefficients:
Export an Excel workbook with model coefficients, diagnostics, warnings, and channel arguments.

#### Download Plot:
Save the recovery fit plot as a PNG file.

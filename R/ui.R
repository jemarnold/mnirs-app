## UI builders for app.R page_navbar()

## textInput / numericInput that update on blur (the app default)
blur_text <- function(id, label = NULL, ...) {
    return(textInput(id, label, updateOn = "blur", ...))
}

blur_numeric <- function(id, label = NULL, value = NA, ...) {
    return(numericInput(id, label, value = value, updateOn = "blur", ...))
}

## non-filling card with a header, the standard output container
output_card <- function(title, ...) {
    return(card(fill = FALSE, card_header(title), ...))
}

## card rendering one page's markdown instructions
instructions_card <- function(title, file) {
    return(card(
        fill = FALSE,
        card_header(title),
        markdown(paste(readLines(file), collapse = "\n"))
    ))
}

## sidebar help button opening the page's instructions modal
help_button <- function(id) {
    return(actionButton(
        id,
        tagList(icon("circle-question"), "Instructions"),
        class = "btn-sm"
    ))
}

## app theme ===================================================
app_theme <- function() {
    return(
        bs_theme(
            bootswatch = "cosmo",
            "navbar-bg" = "#2780e3",
            base_font = font_google("Merriweather Sans"),
            code_font = font_google("Space Mono")
        ) |>
            bs_add_rules(
                "
            .btn-file {
                background-color: var(--bs-primary);
                border-color: var(--bs-primary);
                color: var(--bs-white, #fff);
            }
            .btn-file:hover {
                background-color: color-mix(in srgb, var(--bs-primary), black 15%);
                border-color: color-mix(in srgb, var(--bs-primary), black 20%);
                color: var(--bs-white, #fff);
            }
            .navbar .nav-item .nav-link.active {
                color: #fff;
            }
            .navbar .nav-item a {
                color: #bed4fc;
            }
            .navbar .nav-item a:hover {
                color: #fff;
            }
        "
            )
    )
}

## Process Data Tab ===========================================
process_tab <- function() {
    return(nav_panel(
        "Process Data",
        layout_sidebar(
            sidebar = sidebar(
                open = TRUE,
                help_button("help_process"),
                fileInput(
                    "upload_file",
                    label = NULL,
                    buttonLabel = "Upload File",
                    # fmt: skip
                    accept = c(
                        '.xlsx', '.xls', '.csv', '.CSV', '.txt', '.tsv', 
                        '.ftn', '.ftn2'
                    )
                ),

                ## input channels
                tags$b("mNIRS Channel Names"),
                helpText("Multiples comma-separated"),
                blur_text("nirs_channels"),
                blur_text("time_channel", "Time Channel Name"),
                blur_text(
                    "event_channel",
                    "Lap/Event Channel Name\n(optional)"
                ),

                hr(),
                ## reset start time to zero
                checkboxInput("zero_time_logical", "Zero Start Time"),

                ## display x as mm:ss
                checkboxInput(
                    "time_labels",
                    'Display Time as "mm:ss"',
                    value = TRUE
                ),

                blur_numeric(
                    "sample_rate",
                    "Sample Rate\n(estimated automatically)",
                    min = 0
                ),
                blur_numeric("resample_rate", "Resample Rate", min = 0),

                hr(),
                ## remove head/tail timespan
                blur_numeric(
                    "head_trim",
                    "Trim Head Timespan",
                    min = 0,
                    step = 1
                ),
                blur_numeric(
                    "tail_trim",
                    "Trim Tail Timespan",
                    min = 0,
                    step = 1
                ),

                hr(),
                ## replace invalid values (column wise)
                blur_text(
                    "invalid_values",
                    "Replace Invalid Values",
                    placeholder = "0, 100, ..."
                ),

                ## replace outliers (column wise)
                checkboxInput("replace_outliers", "Replace Outliers"),
                conditionalPanel(
                    condition = "input.replace_outliers",
                    blur_numeric(
                        "outlier_span",
                        "Outlier Detection Span",
                        value = 15,
                        min = 1,
                        step = 1
                    )
                ),

                ## replace missing values (column wise)
                checkboxInput("replace_missing", "Replace Missing Values"),

                hr(),
                ## filter/smooth data (column wise)
                selectInput(
                    "filter_method",
                    label = "Digital Filter Method",
                    choices = c(
                        "None" = "none",
                        "Smooth-Spline" = "smooth_spline",
                        "Moving-Average" = "moving_average",
                        "Butterworth" = "butterworth"
                    )
                ),
                conditionalPanel(
                    condition = "input.filter_method != 'none'",
                    checkboxInput("show_raw", "Show Raw Tracings", FALSE)
                ),
                conditionalPanel(
                    condition = "input.filter_method == 'butterworth'",
                    selectInput(
                        "butter_type",
                        "Butterworth Filter Type",
                        choices = c("Low-Pass" = "low", "High-Pass" = "high")
                    ),
                    blur_numeric(
                        "order",
                        "Filter Order",
                        value = 2,
                        min = 1,
                        max = 10,
                        step = 1
                    ),
                    blur_numeric(
                        "fc",
                        "Cutoff Frequency (Hz)",
                        value = 0.1,
                        min = 0,
                        step = 0.05
                    )
                ),
                conditionalPanel(
                    condition = "input.filter_method == 'moving_average'",
                    numericInput(
                        "filter_span",
                        "Moving-Average Span",
                        value = 10,
                        min = 1,
                        step = 1
                    )
                ),

                hr(),
                ## shift data (dataframe)
                checkboxInput("shift_logical", "Shift Data"),
                conditionalPanel(
                    condition = "input.shift_logical",
                    blur_numeric("shift_to", "Shift To", value = 0),
                    selectInput(
                        "shift_position",
                        label = "Shift Position",
                        choices = c(
                            "Minimum" = "min",
                            "Maximum" = "max",
                            "First" = "first"
                        )
                    ),
                    blur_numeric("shift_span", "Shift Timespan", value = 1),
                    selectInput(
                        "shift_which_cols",
                        label = "Shift Channels",
                        choices = c(
                            "Ensemble" = "ensemble",
                            "Distinct" = "distinct"
                        )
                    )
                ),

                ## rescale (dataframe)
                checkboxInput("rescale_logical", "Rescale Data"),
                conditionalPanel(
                    condition = "input.rescale_logical",
                    blur_numeric(
                        "rescale_min",
                        "Rescale Range Minimum",
                        value = 0
                    ),
                    blur_numeric(
                        "rescale_max",
                        "Rescale Range Maximum",
                        value = 100
                    ),
                    selectInput(
                        "rescale_which_cols",
                        label = "Rescale Channels",
                        choices = c(
                            "Ensemble" = "ensemble",
                            "Distinct" = "distinct"
                        )
                    )
                ),

                ## blood-volume correction (dataframe)
                checkboxInput(
                    "bv_correct_logical",
                    "Correct Blood Volume"
                ),
                uiOutput("bv_ui"),

                hr(),
                ## place manual event lines in data
                tags$b("Place Event Markers"),
                helpText("Time values, multiples comma-separated"),
                blur_text("manual_events", placeholder = "60, 120, ..."),

                checkboxInput(
                    "keep_all",
                    "Keep all Columns in Export",
                    value = TRUE
                ),

                downloadButton(
                    "download_data",
                    "Download Data",
                    class = "btn-primary"
                ),
                ## actionButton: PNG generated client-side by plotly.js
                actionButton(
                    "download_plot",
                    "Download Plot",
                    class = "btn-primary"
                )
            ),

            output_card(
                "mNIRS Plot",
                plotly::plotlyOutput("plot", height = "600px"),
                card_header("Data Table"),
                DTOutput("nirs_table", fill = FALSE)
            )
        )
    ))
}

## Extract Intervals Tab ======================================
extract_tab <- function() {
    return(nav_panel(
        "Extract Intervals",
        layout_sidebar(
            sidebar = sidebar(
                open = TRUE,
                help_button("help_extract"),

                ## interval boundaries: any combination of methods accepted,
                ## grouped by method with start & end paired together
                tags$b("Interval Boundaries (accept multiple)"),

                tags$b("By Time"),
                blur_text("start_time", "Start", placeholder = "60, 120, ..."),
                blur_text("end_time", "End"),
                hr(),

                tags$b("By Label"),
                blur_text("start_label", "Start", placeholder = "start, ..."),
                blur_text("end_label", "End"),
                checkboxInput("label_fixed", "Fixed (literal) string matching"),
                hr(),

                tags$b("By Lap"),
                blur_text("start_lap", "Start", placeholder = "1, 2, 3, ..."),
                blur_text("end_lap", "End"),
                hr(),

                tags$b("By Sample"),
                blur_text("start_sample", "Start"),
                blur_text("end_sample", "End"),
                hr(),

                ## global signed span offsets applied to all interval
                ## boundaries: +ve = after, -ve = before. single value
                ## windows around a lone boundary; blank reads as 0
                tags$b("Span"),
                helpText(
                    "Extend bounds before start (-ve) and after end (+ve)"
                ),
                blur_numeric("span_start", "Start Span"),
                blur_numeric("span_end", "End Span"),

                hr(),
                radioButtons(
                    "group_intervals",
                    label = "Group Intervals",
                    choices = c(
                        "Distinct" = "distinct",
                        "Ensemble" = "ensemble"
                    )
                ),

                checkboxInput("extract_zero_time", "Zero Interval Time"),
                checkboxInput("interval_free_y", "Free y-axis scales"),

                downloadButton(
                    "download_intervals",
                    "Download Intervals",
                    class = "btn-primary"
                ),
                ## one visible button triggers both hidden download links
                actionButton(
                    "download_plots",
                    "Download Plots",
                    class = "btn-primary"
                ),
                shinyjs::hidden(downloadButton("download_session_plot", "")),
                shinyjs::hidden(downloadButton("download_facet_plot", ""))
            ),

            output_card(
                "Full Plot with Interval Boundaries",
                plotOutput("boundary_plot", height = "auto")
            ),

            output_card(
                "Extracted Intervals",
                plotOutput("interval_plot", height = "auto")
            )
        )
    ))
}

## Analyse Kinetics Tab ===========================================
kinetics_tab <- function() {
    return(nav_panel(
        "Analyse Kinetics",
        layout_sidebar(
            sidebar = sidebar(
                open = TRUE,
                help_button("help_kinetics"),

                tags$b("mNIRS Channels"),
                helpText("Select from multiple"),
                selectizeInput(
                    "kin_nirs_channels",
                    label = NULL,
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                        ## push selection to server only when box loses focus
                        onBlur = I(
                            'function() {
                            Shiny.setInputValue(
                                "kin_nirs_channels_blur",
                                this.getValue(),
                                {priority: "event"}
                            );
                        }'
                        )
                    )
                ),

                hr(),
                selectInput(
                    "kin_method",
                    label = "Kinetics Method",
                    choices = c(
                        "Response Time" = "response_time",
                        "Peak Slope" = "peak_slope",
                        "Monoexponential" = "monoexponential",
                        "Exponential Drift" = "exponential_drift",
                        "Biexponential" = "biexponential",
                        "Sigmoidal" = "sigmoidal",
                        "Sigmoidal Drift" = "sigmoidal_drift"
                    )
                ),

                tags$b("Method Options"),
                conditionalPanel(
                    condition = "input.kin_method == 'response_time'",
                    blur_numeric(
                        "kin_fraction",
                        "Response Fraction",
                        value = 0.5,
                        min = 0,
                        max = 1,
                        step = 0.05
                    )
                ),
                conditionalPanel(
                    condition = "input.kin_method == 'peak_slope'",
                    helpText(
                        "Rolling window as Width (samples) OR Span (time)"
                    ),
                    blur_numeric(
                        "kin_width",
                        "Window Width",
                        min = 1,
                        step = 1
                    ),
                    blur_numeric("kin_span", "Window Span", min = 0),
                    selectInput(
                        "kin_align",
                        label = "Window Alignment",
                        choices = c(
                            "Centre" = "centre",
                            "Left" = "left",
                            "Right" = "right"
                        )
                    )
                ),
                conditionalPanel(
                    condition = paste(
                        "input.kin_method == 'monoexponential' ||",
                        "input.kin_method == 'exponential_drift' ||",
                        "input.kin_method == 'biexponential'"
                    ),
                    checkboxInput(
                        "kin_use_TD",
                        "Fit Time Delay (TD)",
                        value = TRUE
                    )
                ),
                conditionalPanel(
                    condition = paste(
                        "input.kin_method == 'sigmoidal' ||",
                        "input.kin_method == 'sigmoidal_drift'"
                    ),
                    selectInput(
                        "kin_shape",
                        label = "Sigmoid Shape",
                        choices = c(
                            "Symmetric" = "symmetric",
                            "Gompertz" = "gompertz",
                            "Gompertz-Left" = "gompertz_left"
                        )
                    )
                ),
                conditionalPanel(
                    condition = paste(
                        "input.kin_method == 'exponential_drift' ||",
                        "input.kin_method == 'sigmoidal_drift'"
                    ),
                    blur_numeric(
                        "kin_drift_fraction",
                        "Drift Onset Fraction",
                        value = 0.95,
                        min = 0.5,
                        max = 1,
                        step = 0.01
                    )
                ),

                hr(),
                tags$b("Kinetics Window"),
                helpText("Blank = interval start"),
                blur_numeric("kin_start_time", "Start Time"),
                blur_numeric("kin_end_window", "End Window Timespan", min = 0),
                helpText("Blank = full interval"),
                selectInput(
                    "kin_direction",
                    label = "Response Direction",
                    choices = c(
                        "Auto" = "auto",
                        "Positive" = "positive",
                        "Negative" = "negative"
                    )
                ),

                hr(),
                checkboxInput("kin_free_y", "Free y-axis scales"),
                checkboxInput("kin_labels", "Show Result Labels", value = TRUE),

                downloadButton(
                    "kin_download_data",
                    "Download Fitted Data",
                    class = "btn-primary"
                ),
                downloadButton(
                    "kin_download_coefs",
                    "Download Coefficients",
                    class = "btn-primary"
                ),
                downloadButton(
                    "kin_download_plot",
                    "Download Plot",
                    class = "btn-primary"
                )
            ),

            output_card(
                "Kinetics Fits",
                plotOutput("kin_plot", height = "auto")
            ),

            output_card(
                "Coefficients",
                DTOutput("kin_coefficients", fill = FALSE),
                card_header("Model Diagnostics"),
                DTOutput("kin_diagnostics", fill = FALSE),
                ## warnings header + table render only when warnings exist
                uiOutput("kin_warnings_ui")
            )
        )
    ))
}

## oxcap Recovery Kinetics Tab ======================================
oxcap_title <- function() {
    # return(tagList("mVO", tags$sub("2"), " Recovery Kinetics"))
    return(tagList("OxCap Analysis"))
}

oxcap_tab <- function() {
    return(nav_panel(
        oxcap_title(),
        layout_sidebar(
            sidebar = sidebar(
                open = TRUE,
                help_button("help_oxcap"),
                helpText(paste(
                    "Fits an exponential recovery model through Peak Slope",
                    "results from the Analyse Kinetics page"
                )),

                hr(),
                radioButtons(
                    "oxcap_method",
                    label = "Recovery Model",
                    choices = c(
                        "Monoexponential" = "monoexponential",
                        "Exponential Drift" = "exponential_drift",
                        "Biexponential" = "biexponential"
                    )
                ),
                checkboxInput("oxcap_use_TD", "Fit Time Delay (TD)"),
                conditionalPanel(
                    condition = "input.oxcap_method == 'exponential_drift'",
                    blur_numeric(
                        "oxcap_drift_frac",
                        "Drift Onset Fraction",
                        value = 0.95,
                        min = 0.5,
                        max = 1,
                        step = 0.01
                    )
                ),

                hr(),
                tags$b("Group Intervals"),
                helpText(
                    "Slope sample numbers per trial; blank = ensemble fit"
                ),
                blur_text(
                    "oxcap_groups",
                    placeholder = "trial1 = 1:10, trial2 = 11:20"
                ),
                checkboxInput(
                    "oxcap_zero_time",
                    "Zero Trial Start Times",
                    value = TRUE
                ),

                hr(),
                checkboxInput("oxcap_free_y", "Free y-axis scales"),
                checkboxInput(
                    "oxcap_labels",
                    "Show Result Labels",
                    value = TRUE
                ),

                downloadButton(
                    "oxcap_download_data",
                    "Download Fitted Data",
                    class = "btn-primary"
                ),
                downloadButton(
                    "oxcap_download_coefs",
                    "Download Coefficients",
                    class = "btn-primary"
                ),
                downloadButton(
                    "oxcap_download_plot",
                    "Download Plot",
                    class = "btn-primary"
                )
            ),

            output_card(
                tagList(oxcap_title(), " Fit"),
                plotOutput("oxcap_plot", height = "auto")
            ),

            output_card(
                "Coefficients",
                DTOutput("oxcap_coefficients", fill = FALSE),
                card_header("Model Diagnostics"),
                DTOutput("oxcap_diagnostics", fill = FALSE),
                ## warnings header + table render only when warnings exist
                uiOutput("oxcap_warnings_ui")
            )
        )
    ))
}

## Instructions Tab ===========================================
instructions_tab <- function() {
    return(nav_panel(
        "Instructions",
        div(
            ## allows full card to scroll preserving navbar at top
            style = "height: calc(100vh - 80px); overflow-y: auto;",
            ## Citation ===============================================
            card(
                min_height = "180px",
                fill = FALSE,
                card_header("Citation"),
                markdown(
                    '
        This app provides basic functionality from the open-source R 
        package *{mnirs}*, available on [CRAN](https://cran.r-project.org/web/packages/mnirs/index.html).

        See *{mnirs}* package documentation:
        https://jemarnold.github.io/mnirs/index.html

        Author: Jem Arnold'
                )
            ),

            instructions_card("Process Data", "instructions_process.md"),
            instructions_card("Extract Intervals", "instructions_extract.md"),
            instructions_card("Analyse Kinetics", "instructions_kinetics.md"),
            instructions_card(oxcap_title(), "instructions_oxcap.md")
        )
    ))
}

## Socials ==========================================
socials_nav <- function() {
    return(list(
        nav_spacer(),
        nav_item(input_dark_mode(id = "color_mode", mode = "dark")),
        nav_item(
            tags$span(
                style = "display: flex; align-items: center; gap: 15px; padding-right: 15px;",
                tags$span("Jem Arnold"),
                local({
                    ## icon name -> profile link
                    links <- c(
                        github = "https://github.com/jemarnold/mnirs",
                        # bluesky = "https://bsky.app/profile/jemarnold.bsky.social",
                        linkedin = "https://www.linkedin.com/in/jem--arnold/",
                        # twitter = "https://twitter.com/jem_arnold",
                        researchgate = "https://www.researchgate.net/profile/Jem-Arnold"
                    )
                    unname(Map(\(.icon, .href) {
                        tags$a(href = .href, target = "_blank", icon(.icon))
                    }, names(links), links ))
                })
            )
        )
    ))
}

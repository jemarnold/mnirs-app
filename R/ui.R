## UI builders for app.R page_navbar()

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
                fileInput(
                    "upload_file",
                    label = NULL,
                    buttonLabel = "Upload File",
                    accept = c('.xlsx', '.xls', '.csv', '.CSV', '.txt', '.tsv')
                ),

                ## input channels
                textInput(
                    "nirs_channels",
                    label = "mNIRS Channel Names\n(accepts multiple)",
                    updateOn = "blur"
                ),
                textInput(
                    "time_channel",
                    label = "Time/Sample Channel Name",
                    updateOn = "blur"
                ),
                textInput(
                    "event_channel",
                    label = "Lap/Event Channel Name\n(optional)",
                    updateOn = "blur"
                ),

                hr(),
                ## reset start time to zero
                checkboxInput("zero_time_logical", "Zero Start Time"),

                ## display x as h:mm:ss
                checkboxInput(
                    "time_labels",
                    'Display Time as "h:mm:ss"',
                    value = TRUE
                ),

                numericInput(
                    "sample_rate",
                    label = "Sample Rate\n(estimated automatically)",
                    value = NA,
                    min = 0,
                    updateOn = "blur"
                ),
                numericInput(
                    "resample_rate",
                    label = "Resample Rate",
                    value = NA,
                    min = 0,
                    updateOn = "blur"
                ),

                hr(),
                ## remove head/tail timespan
                numericInput(
                    "head_trim",
                    label = "Trim Head Timespan",
                    value = NA,
                    min = 0,
                    step = 1,
                    updateOn = "blur"
                ),
                numericInput(
                    "tail_trim",
                    label = "Trim Tail Timespan",
                    value = NA,
                    min = 0,
                    step = 1,
                    updateOn = "blur"
                ),

                hr(),
                ## replace invalid values (column wise)
                textInput(
                    "invalid_values",
                    label = "Replace Invalid Values",
                    placeholder = "0, 100, ...",
                    updateOn = "blur"
                ),

                ## replace outliers (column wise)
                checkboxInput("replace_outliers", "Replace Outliers"),
                conditionalPanel(
                    condition = "input.replace_outliers",
                    numericInput(
                        "outlier_span",
                        label = "Outlier Detection Span",
                        value = 15,
                        min = 1,
                        step = 1,
                        updateOn = "blur"
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
                        "None",
                        "Smooth-Spline",
                        "Butterworth",
                        "Moving-Average"
                    )
                ),
                conditionalPanel(
                    condition = "input.filter_method != 'None'",
                    checkboxInput("show_raw", "Show Raw Tracings", FALSE)
                ),
                conditionalPanel(
                    condition = "input.filter_method == 'Butterworth'",
                    selectInput(
                        "butter_type",
                        "Butterworth Filter Type",
                        #, "Stop-Band", "Pass-Band"
                        choices = c("Low-Pass", "High-Pass")
                    ),
                    numericInput(
                        "order",
                        label = "Filter Order",
                        value = 2,
                        min = 1,
                        max = 10,
                        step = 1,
                        updateOn = "blur"
                    ),
                    numericInput(
                        "fc",
                        label = "Cutoff Frequency (Hz)",
                        value = 0.1,
                        min = 0,
                        step = 0.05,
                        updateOn = "blur"
                    )
                ),
                conditionalPanel(
                    condition = "input.filter_method == 'Moving-Average'",
                    numericInput(
                        "filter_span",
                        "Moving-Average Span",
                        value = 10,
                        min = 1,
                        step = 1
                    )
                ),

                hr(),
                ## blood-volume correction (dataframe)
                checkboxInput(
                    "bv_correct_logical",
                    "Correct Blood Volume"
                ),
                uiOutput("bv_ui"),

                ## shift data (dataframe)
                checkboxInput("shift_logical", "Shift Data"),
                conditionalPanel(
                    condition = "input.shift_logical",
                    numericInput(
                        "shift_to",
                        label = "Shift To",
                        value = 0,
                        updateOn = "blur"
                    ),
                    selectInput(
                        "shift_position",
                        label = "Shift Position",
                        choices = c("Minimum", "Maximum", "First")
                    ),
                    numericInput(
                        "shift_span",
                        label = "Shift Timespan",
                        value = 1,
                        updateOn = "blur"
                    ),
                    selectInput(
                        "shift_which_cols",
                        label = "Shift Channels",
                        choices = c("Ensemble", "Distinct")
                    )
                ),

                ## rescale (dataframe)
                checkboxInput("rescale_logical", "Rescale Data"),
                conditionalPanel(
                    condition = "input.rescale_logical",
                    numericInput(
                        "rescale_min",
                        "Rescale Range Minimum",
                        value = 0,
                        updateOn = "blur"
                    ),
                    numericInput(
                        "rescale_max",
                        label = "Rescale Range Maximum",
                        value = 100,
                        updateOn = "blur"
                    ),
                    selectInput(
                        "rescale_which_cols",
                        label = "Rescale Channels",
                        choices = c("Ensemble", "Distinct")
                    )
                ),

                hr(),
                ## place manual event lines in data
                textInput(
                    "manual_events",
                    label = "Place Event Markers",
                    placeholder = "60, 120, ...",
                    # value = c("370, 1080"),
                    updateOn = "blur"
                ),

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
            ),

            card(
                fill = FALSE,
                card_header("Signals Display"),
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

                ## interval boundaries: any combination of methods accepted,
                ## grouped by method with start & end paired together
                tags$b("Interval Boundaries (accept multiple)"),

                tags$b("By Time"),
                textInput(
                    "start_time",
                    label = "Start",
                    placeholder = "60, 120, ...",
                    updateOn = "blur"
                ),
                textInput(
                    "end_time",
                    label = "End",
                    updateOn = "blur"
                ),

                hr(),
                tags$b("By Label"),
                textInput(
                    "start_label",
                    label = "Start",
                    updateOn = "blur"
                ),
                textInput(
                    "end_label",
                    label = "End",
                    updateOn = "blur"
                ),
                checkboxInput(
                    "label_fixed",
                    "Fixed (literal) Label Matching"
                ),

                hr(),
                tags$b("By Lap"),
                textInput(
                    "start_lap",
                    label = "Start",
                    updateOn = "blur"
                ),
                textInput(
                    "end_lap",
                    label = "End",
                    updateOn = "blur"
                ),

                hr(),
                tags$b("By Sample"),
                textInput(
                    "start_sample",
                    label = "Start",
                    updateOn = "blur"
                ),
                textInput(
                    "end_sample",
                    label = "End",
                    updateOn = "blur"
                ),

                hr(),
                ## global signed span offsets applied to all interval
                ## boundaries: +ve = after, -ve = before. single value
                ## windows around a lone boundary; blank reads as 0
                tags$b("Span"),
                helpText("Extend bounds before start (-ve) and after end (+ve)"),
                numericInput(
                    "span_start",
                    label = "Start Span",
                    value = NA,
                    updateOn = "blur"
                ),
                numericInput(
                    "span_end",
                    label = "End Span",
                    value = NA,
                    updateOn = "blur"
                ),

                radioButtons(
                    "group_intervals",
                    label = "Group Intervals",
                    choices = c("Distinct", "Ensemble")
                ),

                checkboxInput("extract_zero_time", "Zero Interval Time"),
                checkboxInput("interval_free_y", "Free y-axis scales"),

                downloadButton(
                    "download_intervals",
                    "Download Intervals",
                    class = "btn-primary"
                ),
            ),

            card(
                fill = FALSE,
                card_header("Full Plot with Interval Boundaries"),
                plotOutput("boundary_plot", height = "300px")
            ),

            card(
                fill = FALSE,
                card_header("Extracted Intervals"),
                plotOutput("interval_plot", height = "600px")
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

                selectInput(
                    "kin_method",
                    label = "Kinetics Method",
                    choices = c(
                        "Response Time",
                        "Peak Slope",
                        "Monoexponential",
                        "Biexponential",
                        "Sigmoidal"
                    )
                ),

                hr(),
                tags$b("Kinetics Window"),
                numericInput(
                    "kin_start_time",
                    label = "Start Time",
                    value = NA,
                    updateOn = "blur"
                ),
                helpText("Blank = interval start"),
                numericInput(
                    "kin_end_window",
                    label = "End Window Timespan",
                    value = NA,
                    min = 0,
                    updateOn = "blur"
                ),
                helpText("Blank = full interval"),
                selectInput(
                    "kin_direction",
                    label = "Response Direction",
                    choices = c("Auto", "Positive", "Negative")
                ),

                hr(),
                tags$b("Method Options"),
                conditionalPanel(
                    condition = "input.kin_method == 'Response Time'",
                    numericInput(
                        "kin_fraction",
                        label = "Response Fraction",
                        value = 0.5,
                        min = 0,
                        max = 1,
                        step = 0.05,
                        updateOn = "blur"
                    )
                ),
                conditionalPanel(
                    condition = "input.kin_method == 'Peak Slope'",
                    helpText(
                        "Rolling window as Width (samples) OR Span (time)"
                    ),
                    numericInput(
                        "kin_width",
                        label = "Window Width",
                        value = NA,
                        min = 1,
                        step = 1,
                        updateOn = "blur"
                    ),
                    numericInput(
                        "kin_span",
                        label = "Window Span",
                        value = NA,
                        min = 0,
                        updateOn = "blur"
                    ),
                    selectInput(
                        "kin_align",
                        label = "Window Alignment",
                        choices = c("Centre", "Left", "Right")
                    )
                ),
                conditionalPanel(
                    condition = paste(
                        "input.kin_method == 'Monoexponential' ||",
                        "input.kin_method == 'Biexponential'"
                    ),
                    checkboxInput("kin_use_TD", "Fit Time Delay", value = TRUE)
                ),
                conditionalPanel(
                    condition = "input.kin_method == 'Sigmoidal'",
                    selectInput(
                        "kin_shape",
                        label = "Sigmoid Shape",
                        choices = c("Symmetric", "Gompertz", "Gompertz-Left")
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
            ),

            card(
                fill = FALSE,
                card_header("Kinetics Fit"),
                plotOutput("kin_plot", height = "600px")
            ),

            card(
                fill = FALSE,
                card_header("Coefficients"),
                tableOutput("kin_coefficients"),
                card_header("Model Diagnostics"),
                tableOutput("kin_diagnostics"),
                ## warnings header + table render only when warnings exist
                uiOutput("kin_warnings_ui")
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
        This is a basic implementation of functionality provided in the
        open-source R package *{mnirs}*.

        Available from: https://github.com/jemarnold/mnirs

        For more information see the *{mnirs}* package documentation:
        https://jemarnold.github.io/mnirs/index.html

        Author: Jem Arnold'
                )
            ),

            card(
                fill = FALSE,
                card_header("Instructions"),
                markdown(
                    paste(readLines("instructions.md"), collapse = "\n")
                )
            )
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
                tags$a(
                    href = "https://github.com/jemarnold/mnirs",
                    target = "_blank",
                    icon("github"),
                ),
                tags$a(
                    href = "https://bsky.app/profile/jemarnold.bsky.social",
                    target = "_blank",
                    icon("bluesky"),
                ),
                tags$a(
                    href = "https://www.linkedin.com/in/jem--arnold/",
                    target = "_blank",
                    icon("linkedin"),
                ),
                tags$a(
                    href = "https://twitter.com/jem_arnold",
                    target = "_blank",
                    icon("twitter"),
                ),
                tags$a(
                    href = "https://www.researchgate.net/profile/Jem-Arnold",
                    target = "_blank",
                    icon("researchgate"),
                )
            )
        )
    ))
}

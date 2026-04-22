## setup =====================================
suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(DT)
    library(tidyr)
    library(scales)
    library(ggplot2)
    library(signal)
    library(mnirs)
    library(thematic)
    library(plotly)
})

# pak::pak("jemarnold/mnirs@dev")

thematic::thematic_shiny(font = "auto")

options(
    mnirs.verbose = FALSE,
    shiny.maxRequestSize = 50 * 1024^2
)

## Parse comma-separated name=value pairs
split_named_vec <- function(x) {
    if (!nchar(x)) {
        return(NULL)
    }

    noquotes <- gsub('["\'`]', '', x)
    channels_vec <- trimws(strsplit(noquotes, ",")[[1L]])
    parts <- strsplit(channels_vec, "\\s*=\\s*")
    names <- vapply(parts, `[`, character(1L), 1L)
    vals <- vapply(parts, \(.x) {
        if (length(.x) > 1L) paste(.x[-1L], collapse = "=") else .x[1L]
    }, character(1L))

    return(setNames(vals, names))
}

## Parse comma-separated numeric values
string_to_numeric <- function(x) {
    if (!nchar(x)) {
        return(NULL)
    }
    return(as.numeric(strsplit(x, split = "\\s*,\\s*")[[1L]]))
}

## Clean CLI error messages
clean_cli_message <- function(e) {
    msg <- cli::ansi_strip(conditionMessage(e))
    msg <- gsub("`|\\{\\.[^}]+\\}", "", msg)
    msg <- gsub("\\s+", " ", msg)
    return(trimws(msg))
}

## Convert blank/empty inputs to NULL
blank_to_null <- function(x) {
    if (is.null(x) || length(x) == 0L || all(is.na(x))) {
        return(NULL)
    }
    if (is.character(x) && all(nchar(x) == 0L)) {
        return(NULL)
    }
    return(x)
}

## Conditional data transformation
apply_if <- function(data, condition, fn, ...) {
    if (condition) return(fn(data, ...)) else return(data)
}

## Group channels for ensemble/distinct operations
group_channels <- function(channels, mode) {
    switch(
        mode,
        "Ensemble" = list(channels),
        "Distinct" = as.list(channels)
    )
}

## Trim rows from the head of a time series
trim_head <- function(data, time_channel, trim) {
    data[data[[time_channel]] > trim, ]
}

## Trim rows from the tail of a time series
trim_tail <- function(data, time_channel, trim) {
    cutoff <- max(data[[time_channel]], na.rm = TRUE) - trim
    data[data[[time_channel]] < cutoff, ]
}

## Safe wrapper for filter_mnirs with error handling
try_filter <- function(data, nirs_channels, time_channel, ...) {
    tryCatch(
        mnirs::filter_mnirs(
            data,
            nirs_channels = nirs_channels,
            time_channel = time_channel,
            ...
        ),
        error = \(e) {
            validate(need(FALSE, clean_cli_message(e)))
        }
    )
}

## Build interactive plotly plot reproducing theme_mnirs() elements
plot_mnirs_plotly <- function(
    data,
    time_labels = FALSE,
    ink = "#373a3c",
    paper = "#fff",
    manual_events = NULL,
    base_size = 20,
    raw_data = NULL
) {
    tc <- attr(data, "time_channel")
    chans <- attr(data, "nirs_channels")
    cols <- mnirs::palette_mnirs(length(chans))
    x_vals <- data[[tc]]

    ## fractional h:mm:ss for hover (axis ticks use integer format_hmmss)
    format_hmmss_frac <- function(x) {
        x <- as.numeric(x)
        sign <- ifelse(x < 0, "-", "")
        hrs <- as.integer(abs(x) %/% 3600)
        mins <- as.integer((abs(x) %% 3600) %/% 60)
        secs <- abs(x) %% 60
        out <- ifelse(
            hrs > 0,
            sprintf("%s%d:%02d:%06.3f", sign, hrs, mins, secs),
            sprintf("%s%02d:%06.3f", sign, mins, secs)
        )
        out[is.na(x)] <- NA_character_
        return(out)
    }

    hover_x <- if (time_labels) format_hmmss_frac(x_vals) else x_vals

    p <- plotly::plot_ly()

    ## optional raw (unfiltered) traces drawn first at low alpha
    if (!is.null(raw_data)) {
        for (i in seq_along(chans)) {
            ch <- chans[[i]]
            p <- plotly::add_trace(
                p,
                x = x_vals,
                y = raw_data[[ch]],
                type = "scatter",
                mode = "lines",
                name = paste0(ch, " (raw)"),
                showlegend = FALSE,
                hoverinfo = "skip",
                line = list(color = cols[[i]], width = 1),
                opacity = 0.6
            )
        }
    }

    ## one line trace per NIRS channel
    for (i in seq_along(chans)) {
        ch <- chans[[i]]
        p <- plotly::add_trace(
            p,
            x = x_vals,
            y = data[[ch]],
            type = "scatter",
            mode = "lines",
            name = ch,
            showlegend = TRUE,
            line = list(color = cols[[i]], width = 1.5),
            text = hover_x,
            hovertemplate = paste0(
                "<b>",
                ch,
                "</b><br>",
                tc,
                ": %{text}<br>",
                "value: %{y:.2f}<extra></extra>"
            )
        )
    }

    ## dashed vertical lines for manual events
    shapes <- if (length(manual_events)) {
        lapply(manual_events, \(xv) {
            list(
                type = "line",
                x0 = xv,
                x1 = xv,
                xref = "x",
                y0 = 0,
                y1 = 1,
                yref = "paper",
                line = list(color = ink, dash = "dash", width = 1)
            )
        })
    } else {
        list()
    }

    ## x-axis with optional h:mm:ss tick text
    xaxis <- list(
        title = if (time_labels) paste(tc, "(h:mm:ss)") else tc,
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = ink,
        color = ink
    )
    if (time_labels) {
        tv <- mnirs::breaks_timespan(n = 5)(x_vals)
        xaxis$tickvals <- tv
        xaxis$ticktext <- mnirs::format_hmmss(tv)
    }

    plotly::layout(
        p,
        paper_bgcolor = paper,
        plot_bgcolor = paper,
        font = list(size = base_size * 0.7, color = ink),
        xaxis = xaxis,
        yaxis = list(
            title = "mNIRS",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = TRUE,
            linecolor = ink,
            color = ink
        ),
        shapes = shapes,
        showlegend = TRUE,
        legend = list(
            orientation = "h",
            x = 1,
            xanchor = "right",
            y = 0.95,
            yanchor = "bottom"
        ),
        hovermode = "closest",
        margin = list(t = 40, r = 20, b = 50, l = 60)
    ) |>
        plotly::config(
            displaylogo = FALSE,
            modeBarButtonsToRemove = c(
                "lasso2d",
                "select2d",
                "autoScale2d",
                "hoverCompareCartesian",
                "toggleSpikelines"
            )
        )
}

## UI ===========================================================
ui <- page_navbar(
    title = tagList(
        img(
            src = "mnirs-hex.svg",
            height = "50px",
            style = "margin-right: 8px; vertical-align: middle;"
        ),
        "{mnirs} Data Processing"
    ),
    theme = bs_theme(
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
        ),

    ## Process Data Tab ===========================================
    nav_panel(
        "Process Data",
        layout_sidebar(
            sidebar = sidebar(
                open = TRUE,
                fileInput(
                    "upload_file",
                    label = NULL,
                    buttonLabel = "Upload File",
                    accept = c('.xlsx', '.xls', '.csv', '.CSV')
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
                    # placeholder = "new_name = file_column_name",
                    updateOn = "blur"
                ),

                numericInput(
                    "sample_rate",
                    label = "Sample Rate\n(estimated automatically)",
                    value = NA,
                    min = 0
                ),
                numericInput(
                    "resample_rate",
                    label = "Re-sample Rate",
                    value = NA,
                    min = 0
                ),

                ## remove head/tail timespan
                numericInput(
                    "head_trim",
                    label = "Trim Head Timespan",
                    value = NA,
                    min = 0,
                    step = 1
                ),
                numericInput(
                    "tail_trim",
                    label = "Trim Tail Timespan",
                    value = NA,
                    min = 0,
                    step = 1
                ),

                ## replace invalid values (column wise)
                textInput(
                    "invalid_values",
                    label = "Replace Invalid Values",
                    placeholder = "0, 100, ...",
                    updateOn = "blur"
                ),

                ## replace outliers (column wise)
                checkboxInput("replace_outliers", "Replace Outliers"),

                ## replace missing values (column wise)
                checkboxInput("replace_missing", "Replace Missing Values"),

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
                uiOutput("filter_method_ui"),

                ## shift data (dataframe)
                checkboxInput("shift_logical", "Shift Data"),
                uiOutput("shift_ui"),

                ## rescale (dataframe)
                checkboxInput("rescale_logical", "Rescale Data"),
                uiOutput("rescale_ui"),

                ## reset start time to zero
                checkboxInput("zero_time_logical", "Zero Start Time"),

                ## display x as h:mm:ss
                checkboxInput(
                    "time_labels",
                    'Display Time as "h:mm:ss"',
                    value = TRUE
                ),

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
                    "Keep all Columns in File Export",
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
    ),

    ## Instructions Tab ===========================================
    nav_panel(
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
                    'This is a basic implementation of functionality provided in the
        open-source R package ***{mnirs}***.

        Available from: https://github.com/jemarnold/mnirs <br>
        Author: Jem Arnold
        `<currently under development>`'
                )
            ),

            card(
                fill = FALSE,
                card_header("Instructions"),
                markdown(
                    '
        mNIRS files can be imported and processed using standardised methods,
        and displayed in a plot and data table. Processed data can be
        downloaded for further analysis.

        #### Upload File:
        Upload an `.xls(x)` or `.csv` file containing mNIRS data exported from
        many common wearable devices. Files exported from common NIRS devices 
        should be automatically recognised, with the first detected 
        `nirs_channel` returned.

        #### mNIRS Channel Names:
        Specify the column name(s) containing mNIRS data. Multiple channels
        can be specified using comma-separated `new_name = file_column_name`
        pairs.

        Example: `nirs_channels = c(smo2_left = SmO2, smo2_right = SmO2(2))`

        #### Time/Sample Channel Name:
        Specify the column containing time or sample values. This channel will
        be estimated from the file, if not specified.

        Example: `time_channel = c(time = Timestamp (seconds))`

        #### Lap/Event Channel Name (optional):
        Optionally specify column with lap/event markers.

        #### Sample Rate:
        Specify the exported data sample rate in Hz. This will be automatically
        estimated from the time channel and can be manually overridden.

        #### Re-sample Rate:
        Data can be re-sampled to a desired higher or lower sample rate. Also
        used to correct `time_channel` values for data with irregular or
        duplicated samples.

        #### Trim Head/Tail Timespan:
        Remove samples from the beginning or end of the recording, specified in
        units of `time_channel` (i.e., seconds).

        #### Replace Invalid Values:
        Replace specific fixed values (e.g., `c(0, 100)`) from `nirs_channels`.

        #### Replace Outliers:
        Remove local outliers using a Hampel filter moving window approach.

        #### Replace Missing Values:
        Linearly interpolate across missing `nirs_channel` values.

        #### Digital Filter Method:
        Apply smoothing filters to improve signal-to-noise ratio. Methods
        include a cubic *"smoothing-spline"*, a low-pass *"Butterworth"*
        filter, or a simple *"moving average"*. Additional parameters for
        each filter method can be specified.

        #### Shift Data:
        Move `nirs_channels` values up or down to a new specified reference
        value, based on the *"first"*, *"minimum"*, or *"maximum"* data points.
        Channels can be shifted together (*"Ensemble"*) or independently
        (*"Distinct"*).

        #### Rescale Data:
        Normalise `nirs_channels` to a new specified range. Channels can be
        shifted together (*"Ensemble"*) or independently (*"Distinct"*).

        #### Zero Start Time:
        Reset `time_channel` to start at zero.

        #### Place Event Markers:
        Manually add event markers at specified time points. Will add an
        `event_channel` to the data table if not already specified, otherwise
        will add events to an existing `event_channel`.

        #### Keep All Columns:
        Either keep all columns in the file data table (the default), or keep 
        only the channels specified.

        #### Download Data:
        Export processed data as an Excel file for further analysis.'
                )
            )
        )
    ),

    ## Socials ==========================================
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
)

## server ===========================================
server <- function(input, output, session) {
    ## payload written only by do_read(). raw_data() wrapper surfaces
    ## stored errors via validate() so render contexts (plot, table)
    ## display them in place of old data.
    raw_data_val <- reactiveVal(NULL)
    raw_data_err <- reactiveVal(NULL)

    raw_data <- reactive({
        if (!is.null(raw_data_err())) {
            validate(need(FALSE, raw_data_err()))
        }
        req(raw_data_val())
    })

    ## suppresses the edit-observer during programmatic input sync
    ## after upload.
    suppress_edit <- reactiveVal(FALSE)

    ## reactive metadata ===================================
    metadata <- reactive({
        req(raw_data())

        return(list(
            nirs_channels = attr(raw_data(), "nirs_channels"),
            time_channel = attr(raw_data(), "time_channel"),
            event_channel = attr(raw_data(), "event_channel"),
            sample_rate = attr(raw_data(), "sample_rate")
        ))
    })

    ## run read_mnirs with current inputs; caller controls triggers.
    do_read <- function() {
        isolate({
            req(input$upload_file)

            out <- tryCatch(
                read_mnirs(
                    file_path = input$upload_file$datapath,
                    nirs_channels = split_named_vec(input$nirs_channels),
                    time_channel = split_named_vec(input$time_channel),
                    event_channel = split_named_vec(input$event_channel),
                    sample_rate = blank_to_null(input$sample_rate),
                    add_timestamp = FALSE,
                    keep_all = input$keep_all
                ),
                error = \(e) {
                    raw_data_val(NULL)
                    raw_data_err(clean_cli_message(e))
                    return(NULL)
                }
            )
            print("do_read run")
            req(out)
            raw_data_err(NULL)
            raw_data_val(out)
        })
    }

    ## on upload: read, then sync blank inputs to detected metadata.
    ## suppress_edit armed so edit-observer ignores programmatic updates.
    observeEvent(input$upload_file, {
        do_read()
        md <- isolate(metadata())
        suppress_edit(TRUE)

        if (!nchar(isolate(input$nirs_channels) %||% "")) {
            updateTextInput(
                session, "nirs_channels",
                value = paste(md$nirs_channels, collapse = ", ")
            )
        }
        if (!nchar(isolate(input$time_channel) %||% "")) {
            updateTextInput(
                session, "time_channel",
                value = md$time_channel %||% ""
            )
        }
        if (is.na(isolate(input$sample_rate) %||% NA)) {
            updateNumericInput(
                session, "sample_rate",
                value = md$sample_rate
            )
        }
    })

    ## on user edits to channel/rate inputs: re-run read_mnirs.
    ## bindEvent fires on genuine input changes AND on programmatic
    ## updates, so we gate with suppress_edit to skip the latter.
    observeEvent(
        list(
            input$nirs_channels,
            input$time_channel,
            input$event_channel,
            input$sample_rate,
            input$keep_all
        ),
        {
            req(input$upload_file)
            if (isolate(suppress_edit())) {
                suppress_edit(FALSE)
                return()
            }
            do_read()
        },
        ignoreInit = TRUE
    )

    ## dynamic UI: filter_mnirs ======================================
    output$filter_method_ui <- renderUI({
        req(input$filter_method)

        ## different UI based on selection
        switch(
            input$filter_method,
            "Butterworth" = tagList(
                selectInput(
                    "butter_type",
                    "Butterworth Filter Type",
                    ## TODO #, "High-Pass", "Stop-Band", "Pass-Band")
                    choices = c("Low-Pass")
                ),
                numericInput(
                    "order",
                    label = "Filter Order",
                    value = 2,
                    min = 1,
                    max = 10,
                    step = 1
                ),
                numericInput(
                    "fc",
                    label = "Cutoff Frequency (Hz)",
                    value = 0.1,
                    min = 0,
                    # max = metadata()$sample_rate * 0.5,
                    step = 0.05
                )
            ),
            "Moving-Average" = tagList(
                numericInput(
                    "filter_span",
                    "Moving-Average Span",
                    value = 10,
                    min = 1,
                    step = 1
                ),
            ),
            NULL
        )
    })

    butter_type <- reactive({
        req(input$filter_method)

        ## pass through default condition
        butter_type <- input$butter_type %||% "Low-Pass"

        switch(
            butter_type,
            "Low-Pass" = "low",
            "High-Pass" = "high",
            "Stop-Band" = "stop",
            "Pass-Band" = "pass"
        )
    })

    ## dynamic UI: shift_mnirs ======================================
    output$shift_ui <- renderUI({
        req(input$shift_logical)

        if (input$shift_logical) {
            tagList(
                numericInput(
                    "shift_to",
                    label = "Shift To",
                    value = 0
                ),
                selectInput(
                    "shift_position",
                    label = "Shift Position",
                    choices = c("Minimum", "Maximum", "First")
                ),
                numericInput(
                    "shift_span",
                    label = "Shift Timespan",
                    value = 1
                ),
                selectInput(
                    "shift_which_cols",
                    label = "Shift Channels",
                    choices = c("Ensemble", "Distinct")
                ),
            )
        }
    })

    ## dynamic UI: rescale_mnirs ======================================
    output$rescale_ui <- renderUI({
        req(input$rescale_logical)

        if (input$rescale_logical) {
            tagList(
                numericInput(
                    "rescale_min",
                    "Rescale Range Minimum",
                    value = 0
                ),
                numericInput(
                    "rescale_max",
                    label = "Rescale Range Maximum",
                    value = 100
                ),
                selectInput(
                    "rescale_which_cols",
                    label = "Rescale Channels",
                    choices = c("Ensemble", "Distinct")
                ),
            )
        }
    })

    ## reactive trimmed_data ====================================================
    trimmed_data <- reactive({
        req(raw_data())

        time_channel <- metadata()$time_channel
        head_val <- blank_to_null(input$head_trim)
        tail_val <- blank_to_null(input$tail_trim)

        raw_data() |>
            apply_if(!is.null(head_val), trim_head, time_channel, head_val) |>
            apply_if(!is.null(tail_val), trim_tail, time_channel, tail_val)
    })

    ## reactive resampled and zeroed time =================================
    resampled_data <- reactive({
        req(trimmed_data())

        resample_rate <- blank_to_null(input$resample_rate)
        time_channel <- metadata()$time_channel

        out <- trimmed_data() |>
            apply_if(
                !is.null(resample_rate),
                mnirs::resample_mnirs,
                resample_rate = resample_rate,
                method = "linear" ## need the interpolation
            )

        ## zero time after resample
        if (input$zero_time_logical) {
            time_vec <- out[[time_channel]]
            out[[time_channel]] <- time_vec - time_vec[1L]
        }

        return(out)
    }) |>
        bindCache(
            trimmed_data(),
            input$resample_rate,
            input$zero_time_logical
        )

    ## reactive replaced data ======================================
    replaced_data <- reactive({
        req(resampled_data())

        invalid_values <- string_to_numeric(input$invalid_values)
        if (input$replace_outliers) {
            outlier_cutoff <- 3
            outlier_span <- 15 ## TODO custom outlier span?
        } else {
            outlier_cutoff <- NULL
            outlier_span <- NULL
        }
        ## TODO not sure why I need this intermediate step, but I seem to do
        interp_method <- if (input$replace_missing) "linear" else NULL

        resampled_data() |>
            apply_if(
                !is.null(c(invalid_values, outlier_span, interp_method)),
                mnirs::replace_mnirs,
                invalid_values = invalid_values,
                outlier_cutoff = outlier_cutoff,
                span = outlier_span,
                method = interp_method %||% "none"
            )
    }) |>
        bindCache(
            resampled_data(),
            input$invalid_values,
            input$replace_outliers,
            input$replace_missing
        )

    ## reactive filtered data ======================================
    filtered_data <- reactive({
        req(replaced_data())

        if (input$filter_method == "None") {
            return(replaced_data())
        }

        method <- tolower(sub("-", "_", input$filter_method))

        try_filter(
            replaced_data(),
            nirs_channels = metadata()$nirs_channels,
            time_channel = metadata()$time_channel,
            method = method,
            type = butter_type(),
            order = input$order %||% 2L,
            fc = input$fc %||% 0.1,
            span = input$filter_span %||% 10,
            partial = TRUE,
            na.rm = TRUE
        )
    }) |>
        bindCache(
            replaced_data(),
            butter_type(),
            input$filter_method,
            input$order,
            input$fc,
            input$filter_span
        )

    ## reactive shifted_data ======================================
    shifted_data <- reactive({
        req(filtered_data())

        if (!input$shift_logical) {
            return(filtered_data())
        }

        req(input$shift_which_cols, input$shift_position)

        channels <- group_channels(
            metadata()$nirs_channels,
            input$shift_which_cols
        )

        mnirs::shift_mnirs(
            filtered_data(),
            nirs_channels = channels,
            to = blank_to_null(input$shift_to),
            span = blank_to_null(input$shift_span),
            position = tolower(sub("imum", "", input$shift_position))
        )
    }) |>
        bindCache(
            filtered_data(),
            input$shift_logical,
            input$shift_to,
            input$shift_which_cols,
            input$shift_position,
            input$shift_span
        )

    ## reactive rescaled_data ======================================
    rescaled_data <- reactive({
        req(shifted_data())

        if (!input$rescale_logical) {
            return(shifted_data())
        }

        req(input$rescale_which_cols)

        channels <- group_channels(
            metadata()$nirs_channels,
            input$rescale_which_cols
        )

        mnirs::rescale_mnirs(
            shifted_data(),
            nirs_channels = channels,
            range = c(
                blank_to_null(input$rescale_min),
                blank_to_null(input$rescale_max)
            )
        )
    }) |>
        bindCache(
            shifted_data(),
            input$rescale_logical,
            input$rescale_min,
            input$rescale_max,
            input$rescale_which_cols
        )

    ## reactive events data ==============================================
    nirs_data <- reactive({
        req(rescaled_data())

        time_channel <- metadata()$time_channel
        event_channel <- metadata()$event_channel
        manual_events <- string_to_numeric(input$manual_events)
        nirs_data <- rescaled_data()
        time_vec <- nirs_data[[time_channel]]

        ## add manual event markers using nearest-match
        if (!is.null(manual_events)) {
            match_idx <- vapply(manual_events, \(.event) {
                which.min(abs(time_vec - .event))
            }, integer(1L))
            time_vals <- time_vec[match_idx]

            if (is.null(event_channel)) {
                nirs_data$event <- NA_character_
                nirs_data$event[match_idx] <- paste0("event_", time_vals)
            } else if (is.numeric(nirs_data[[event_channel]])) {
                nirs_data[[event_channel]][match_idx] <- time_vals
            } else {
                nirs_data[[event_channel]][match_idx] <- paste0(
                    "event_", time_vals
                )
            }
        }

        return(nirs_data)
    })

    ## Output: Data table ==========================================
    output$nirs_table <- renderDT({
        req(nirs_data())

        time_channel <- metadata()$time_channel

        ## format numeric columns for display
        display_data <- nirs_data()
        display_data[] <- lapply(names(display_data), \(.name) {
            col <- display_data[[.name]]
            if (.name == time_channel) {
                mnirs:::signif_trailing(col, 3L)
            } else if (rlang::is_integerish(col)) {
                as.integer(col)
            } else if (is.numeric(col)) {
                mnirs:::signif_trailing(col, 4L, "signif")
            } else {
                col
            }
        })

        datatable(
            display_data,
            rownames = FALSE,
            options = list(
                dom = 'frtip',
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = FALSE
            )
        )
    })

    ## Output: Plot ==========================================
    output$plot <- plotly::renderPlotly({
        req(nirs_data())

        mode <- input$color_mode
        if (identical(mode, "dark")) {
            ink <- "#fff"
            paper <- "#212529"
        } else {
            ink <- "#373a3c"
            paper <- "#fff"
        }

        manual_events <- string_to_numeric(input$manual_events)

        show_raw <- isTRUE(input$show_raw) && input$filter_method != "None"
        raw_data <- if (show_raw) replaced_data() else NULL

        plot_mnirs_plotly(
            nirs_data(),
            time_labels = input$time_labels,
            ink = ink,
            paper = paper,
            manual_events = manual_events,
            raw_data = raw_data
        )
    }) |>
        bindEvent(
            nirs_data(),
            input$manual_events,
            input$time_labels,
            input$color_mode,
            input$show_raw
        )

    ## Download handler =============================================
    output$download_data <- downloadHandler(
        filename = \() paste0("mnirs_processed_", Sys.Date(), ".xlsx"),
        content = \(file) writexl::write_xlsx(nirs_data(), path = file)
    )
}

shinyApp(ui = ui, server = server)

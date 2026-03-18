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
})

# devtools::install_github("jemarnold/mnirs", force = TRUE)

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
        if (length(.x) > 1L) {
            paste(.x[-1L], collapse = "=")
        } else {
            .x[1L]
        }
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
    switch(mode,
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

## UI ===========================================================
ui <- page_navbar(
    title = "{mnirs} Data Processing",
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
                    placeholder = "new_name = file_column_name",
                    updateOn = "blur"
                ),
                textInput(
                    "time_channel",
                    label = "Time/Sample Channel Name",
                    placeholder = "new_name = file_column_name",
                    updateOn = "blur"
                ),
                textInput(
                    "event_channel",
                    label = "Lap/Event Channel Name\n(optional)",
                    placeholder = "new_name = file_column_name",
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
                plotOutput("plot", height = "600px"),

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
    nirs_channels <- reactive(input$nirs_channels)
    time_channel <- reactive(input$time_channel)
    event_channel <- reactive(input$event_channel)

    ## reactive raw data ====================================
    raw_data <- reactive({
        req(input$upload_file, nchar(nirs_channels()) > 0L)

        upload_file <- input$upload_file$datapath

        raw_data <- tryCatch(
            read_mnirs(
                file_path = upload_file,
                nirs_channels = split_named_vec(nirs_channels()),
                time_channel = split_named_vec(time_channel()),
                event_channel = split_named_vec(event_channel()),
                sample_rate = blank_to_null(input$sample_rate),
                add_timestamp = FALSE,
                keep_all = input$keep_all
            ),
            error = \(e) {
                ## remove CLI formatting from error message
                validate(need(FALSE, clean_cli_message(e)))
            }
        )

        return(raw_data)
    })

    ## auto-update: channels & sample_rate ==============================
    detect_and_fill_channels <- function() {
        detected <- tryCatch(
            read_mnirs(
                file_path = input$upload_file$datapath,
                nirs_channels = NULL,
                time_channel = NULL,
                add_timestamp = FALSE,
                keep_all = TRUE
            ),
            error = \(e) NULL
        )

        if (!is.null(detected)) {
            nirs_channels <- attr(detected, "nirs_channels")
            time_channel <- if (
                attr(detected, "nirs_device") == "Artinis"
            ) {
                "sample = 1"
            } else {
                attr(detected, "time_channel")
            }

            updateTextInput(
                session,
                "nirs_channels",
                value = paste(nirs_channels, collapse = ", ")
            )
            updateTextInput(
                session,
                "time_channel",
                value = time_channel
            )
        }
    }

    observeEvent(input$upload_file, {
            ## reset to blank on new uploaded file
            updateNumericInput(
                session,
                inputId = "sample_rate",
                value = NA
            )

            ## auto-detect nirs channels for recognised devices
            if (!nchar(input$nirs_channels)) {
                detect_and_fill_channels()
            }
        },
        ignoreNULL = FALSE
    )

    ## re-detect channels if user clears nirs_channels field
    observeEvent(input$nirs_channels, {
        req(input$upload_file, !nchar(input$nirs_channels))
        detect_and_fill_channels()
    })

    observeEvent(raw_data(), {
        req(raw_data())

        ## update with new value
        updateNumericInput(
            session,
            inputId = "sample_rate",
            value = metadata()$sample_rate
        )
    })

    ## reactive metadata ===================================
    metadata <- reactive({
        req(raw_data())

        raw_data <- raw_data()
        
        return(list(
            nirs_channels = attr(raw_data, "nirs_channels"),
            time_channel = attr(raw_data, "time_channel"),
            event_channel = attr(raw_data, "event_channel"),
            sample_rate = attr(raw_data, "sample_rate")
        ))
    })

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

        trimmed_data() |>
            apply_if(
                !is.null(resample_rate),
                mnirs::resample_mnirs,
                resample_rate = resample_rate,
                method = "linear" ## need the interpolation
            )
    }) |>
        bindCache(
            trimmed_data(),
            input$resample_rate
        )

    ## reactive replaced data ======================================
    replaced_data <- reactive({
        req(resampled_data())

        invalid_values <- string_to_numeric(input$invalid_values)
        if (input$replace_outliers) {
            outlier_cutoff <- 3
            outlier_span <- 7 ## TODO custom outlier span?
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

    ## reactive nirs_data ====================================================
    nirs_data <- reactive({
        req(rescaled_data())

        time_channel <- metadata()$time_channel
        event_channel <- metadata()$event_channel
        manual_events <- string_to_numeric(input$manual_events)
        nirs_data <- rescaled_data()
        time_vec <- nirs_data[[time_channel]]

        ## re-calc zero time after processing stages
        if (input$zero_time_logical) {
            nirs_data[[time_channel]] <- time_vec - time_vec[1L]
            time_vec <- nirs_data[[time_channel]]
        }

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
            if (is.numeric(col)) {
                mnirs:::signif_trailing(col, digits = 3L, format = "signif")
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
    output$plot <- renderPlot({
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

        plot <- mnirs:::plot.mnirs(
            nirs_data(),
            time_labels = input$time_labels
        ) +
            theme_mnirs(base_size = 20, ink = ink, paper = paper)

        if (!is.null(manual_events)) {
            plot <- plot +
                ggplot2::geom_vline(
                    xintercept = manual_events,
                    linetype = "dashed",
                    colour = ink
                )
        }

        plot
    }) |>
        bindEvent(nirs_data(), input$manual_events, input$time_labels, input$color_mode)

    ## Download handler =============================================
    output$download_data <- downloadHandler(
        filename = \() paste0("mnirs_processed_", Sys.Date(), ".xlsx"),
        content = \(file) writexl::write_xlsx(nirs_data(), path = file)
    )
}

shinyApp(ui = ui, server = server)

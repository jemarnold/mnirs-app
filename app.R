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

## helper functions sourced automatically from R/ directory:
## R/utils.R, R/plotly_mnirs.R

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
                uiOutput("outlier_ui"),

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
                uiOutput("filter_method_ui"),

                hr(),
                ## blood-volume correction (dataframe)
                checkboxInput(
                    "bv_correct_logical",
                    "Correct Blood Volume"
                ),
                uiOutput("bv_ui"),

                ## shift data (dataframe)
                checkboxInput("shift_logical", "Shift Data"),
                uiOutput("shift_ui"),

                ## rescale (dataframe)
                checkboxInput("rescale_logical", "Rescale Data"),
                uiOutput("rescale_ui"),

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
    ),

    ## Extract Intervals Tab ======================================
    nav_panel(
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
                ## global span applied to all intervals. blank reads as 0
                numericInput(
                    "span_before",
                    label = "Span Before Start",
                    value = NA,
                    updateOn = "blur"
                ),
                numericInput(
                    "span_after",
                    label = "Span After End",
                    value = NA,
                    updateOn = "blur"
                ),

                radioButtons(
                    "group_intervals",
                    label = "Group Intervals",
                    choices = c("Distinct", "Ensemble")
                ),

                checkboxInput("extract_zero_time", "Zero Interval Time"),

                downloadButton(
                    "download_intervals",
                    "Download Data",
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
            sample_rate = attr(raw_data(), "sample_rate"),
            nirs_device = attr(raw_data(), "nirs_device")
        ))
    })

    ## scalar bindCache keys ======================================
    ## each pipeline stage keys on the upload identity plus all
    ## upstream inputs, so caches never hash full data frames
    raw_key <- reactive({
        req(raw_data())
        list(
            ## datapath is unique per upload, so re-uploading an
            ## edited file with the same name invalidates caches
            input$upload_file$datapath,
            input$nirs_channels,
            input$time_channel,
            input$event_channel,
            input$sample_rate
        )
    })

    resample_key <- reactive(c(
        raw_key(),
        list(
            input$head_trim,
            input$tail_trim,
            input$resample_rate,
            input$zero_time_logical
        )
    ))

    replace_key <- reactive(c(
        resample_key(),
        list(
            input$invalid_values,
            input$replace_outliers,
            input$outlier_span,
            input$replace_missing
        )
    ))

    filter_key <- reactive(c(
        replace_key(),
        list(
            input$filter_method,
            butter_type(),
            input$order,
            input$fc,
            input$filter_span
        )
    ))

    correct_key <- reactive(c(
        filter_key(),
        list(
            input$bv_correct_logical,
            input$bv_oxy,
            input$bv_deoxy,
            input$bv_total
        )
    ))

    shift_key <- reactive(c(
        correct_key(),
        list(
            input$shift_logical,
            input$shift_to,
            input$shift_which_cols,
            input$shift_position,
            input$shift_span
        )
    ))

    rescale_key <- reactive(c(
        shift_key(),
        list(
            input$rescale_logical,
            input$rescale_min,
            input$rescale_max,
            input$rescale_which_cols
        )
    ))

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
                    keep_all = TRUE
                ),
                error = \(e) {
                    raw_data_val(NULL)
                    raw_data_err(clean_cli_message(e))
                    return(NULL)
                }
            )

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
                session,
                "nirs_channels",
                value = paste(md$nirs_channels, collapse = ", ")
            )
        }
        if (!nchar(isolate(input$time_channel) %||% "")) {
            updateTextInput(
                session,
                "time_channel",
                value = if (md$nirs_device == "Artinis") {
                    "sample = 1"
                } else {
                    md$time_channel %||% ""
                }
            )
        }
        if (is.na(isolate(input$sample_rate) %||% NA)) {
            updateNumericInput(
                session,
                "sample_rate",
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
            input$sample_rate
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
                    choices = c(
                        "Low-Pass",
                        "High-Pass" #, "Stop-Band", "Pass-Band"
                    )
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
                    # max = metadata()$sample_rate * 0.5,
                    step = 0.05,
                    updateOn = "blur"
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

    ## dynamic UI: correct_blood_volume ==============================
    output$bv_ui <- renderUI({
        req(input$bv_correct_logical)

        ## channel choices from detected nirs_channels; "" = unspecified
        choices <- c("(none)" = "", metadata()$nirs_channels)

        tagList(
            selectInput("bv_oxy", "Oxy Channel (O2Hb)", choices = choices),
            selectInput("bv_deoxy", "Deoxy Channel (HHb)", choices = choices),
            selectInput("bv_total", "Total Channel (THb)", choices = choices),
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

    ## dynamic UI: outlier span ======================================
    output$outlier_ui <- renderUI({
        req(input$replace_outliers)

        numericInput(
            "outlier_span",
            label = "Outlier Detection Span",
            value = 15,
            min = 1,
            step = 1,
            updateOn = "blur"
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
                ),
            )
        }
    })

    ## untick show_raw when a value-altering transform is ticked
    observeEvent(
        list(
            input$shift_logical,
            input$rescale_logical,
            input$bv_correct_logical
        ),
        {
            if (
                isTRUE(input$shift_logical) ||
                    isTRUE(input$rescale_logical) ||
                    isTRUE(input$bv_correct_logical)
            ) {
                updateCheckboxInput(session, "show_raw", value = FALSE)
            }
        },
        ignoreInit = TRUE
    )

    ## reactive trimmed_data ===========================================
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
                method = "linear" ## need the interpolation for display
            )

        ## zero time after resample
        if (input$zero_time_logical) {
            time_vec <- out[[time_channel]]
            out[[time_channel]] <- time_vec - time_vec[1L]
        }

        return(out)
    }) |>
        bindCache(resample_key())

    ## reactive replaced data ======================================
    replaced_data <- reactive({
        req(resampled_data())

        invalid_values <- string_to_numeric(input$invalid_values)
        if (input$replace_outliers) {
            outlier_cutoff <- 3
            outlier_span <- input$outlier_span %||% 15
        } else {
            outlier_cutoff <- NULL
            outlier_span <- NULL
        }
        ## not sure why I need this intermediate step, but I seem to do
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
        bindCache(replace_key())

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
        bindCache(filter_key())

    ## reactive corrected_data (blood volume) ==========================
    corrected_data <- reactive({
        req(filtered_data())

        if (!isTRUE(input$bv_correct_logical)) {
            return(filtered_data())
        }

        channels <- list(
            oxy_channel = blank_to_null(input$bv_oxy),
            deoxy_channel = blank_to_null(input$bv_deoxy),
            total_channel = blank_to_null(input$bv_total)
        )

        ## need >= 2 of oxy/deoxy/total; third is derived
        validate(need(
            sum(lengths(channels)) >= 2L,
            "Select at least 2 of oxy / deoxy / total channels."
        ))

        out <- tryCatch(
            do.call(
                mnirs::correct_blood_volume,
                c(list(filtered_data()), channels)
            ),
            error = \(e) validate(need(FALSE, clean_cli_message(e)))
        )

        ## correct_blood_volume() replaces nirs_channels with only the
        ## channels passed to it; restore the rest so they survive to
        ## the plot and export
        attr(out, "nirs_channels") <- union(
            attr(out, "nirs_channels"),
            attr(filtered_data(), "nirs_channels")
        )

        return(out)
    }) |>
        bindCache(correct_key())

    ## reactive shifted_data ======================================
    shifted_data <- reactive({
        req(corrected_data())

        if (!input$shift_logical) {
            return(corrected_data())
        }

        req(input$shift_which_cols, input$shift_position)

        mnirs::shift_mnirs(
            corrected_data(),
            nirs_channels = metadata()$nirs_channels,
            group_channels = tolower(input$shift_which_cols),
            to = blank_to_null(input$shift_to),
            span = blank_to_null(input$shift_span),
            position = tolower(sub("imum", "", input$shift_position))
        )
    }) |>
        bindCache(shift_key())

    ## reactive rescaled_data ======================================
    rescaled_data <- reactive({
        req(shifted_data())

        if (!input$rescale_logical) {
            return(shifted_data())
        }

        req(input$rescale_which_cols)

        mnirs::rescale_mnirs(
            shifted_data(),
            nirs_channels = metadata()$nirs_channels,
            group_channels = tolower(input$rescale_which_cols),
            range = c(
                blank_to_null(input$rescale_min),
                blank_to_null(input$rescale_max)
            )
        )
    }) |>
        bindCache(rescale_key())

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
            match_idx <- vapply(
                manual_events,
                \(.event) {
                    which.min(abs(time_vec - .event))
                },
                integer(1L)
            )
            ## keep the matched sample time; round only to display
            ## precision so labels carry no floating-point tail
            digits <- time_digits(time_vec)
            time_vals <- round(time_vec[match_idx], digits)
            event_labels <- paste0(
                "event_",
                vapply(
                    time_vals,
                    mnirs:::signif_trailing,
                    character(1L),
                    digits
                )
            )

            if (is.null(event_channel)) {
                nirs_data$event <- NA_character_
                nirs_data$event[match_idx] <- event_labels
            } else if (is.numeric(nirs_data[[event_channel]])) {
                nirs_data[[event_channel]][match_idx] <- time_vals
            } else {
                nirs_data[[event_channel]][match_idx] <- event_labels
            }
        }

        return(nirs_data)
    })

    ## reactive export data ==============================================
    ## data is read with keep_all = TRUE so extra columns survive the
    ## pipeline; this drops unrecognised columns from the table and
    ## download when the user unticks keep_all
    export_data <- reactive({
        data <- nirs_data()

        if (isTRUE(input$keep_all)) {
            return(data)
        }

        keep <- c(
            metadata()$time_channel,
            metadata()$nirs_channels,
            metadata()$event_channel,
            "event"
        )
        return(data[intersect(names(data), keep)])
    })

    ## Output: Data table ==========================================
    output$nirs_table <- renderDT({
        data <- export_data()
        req(data)

        time_channel <- metadata()$time_channel

        ## format numerics client-side; keeps columns numeric so
        ## table sorting works and avoids re-formatting in R on
        ## every invalidation
        num_cols <- names(data)[vapply(data, is.numeric, logical(1L))]
        int_cols <- num_cols[vapply(
            data[num_cols],
            rlang::is_integerish,
            logical(1L)
        )]
        sig_cols <- setdiff(num_cols, c(int_cols, time_channel))

        dt <- datatable(
            data,
            rownames = FALSE,
            options = list(
                dom = 'frtip',
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = FALSE
            )
        )
        ## time shown as decimal places, not sig figs: past ~1000 s
        ## sig figs collapse adjacent samples to the same value
        dt <- if (time_channel %in% setdiff(num_cols, int_cols)) {
            formatRound(
                dt,
                time_channel,
                digits = time_digits(data[[time_channel]])
            )
        } else {
            dt
        }
        if (length(sig_cols)) formatSignif(dt, sig_cols, digits = 4) else dt
    })

    ## Output: Plot ==========================================
    ## plot uses rescaled_data(): manual event markers only touch the
    ## event channel (not plotted), so the table/download pipeline
    ## invalidating doesn't force a plot rebuild. cosmetic inputs
    ## (colour mode, event markers, show-raw) update in place via
    ## plotlyProxy observers below; they are isolated here so full
    ## rebuilds still respect their current state.
    output$plot <- plotly::renderPlotly({
        req(rescaled_data())

        dark <- identical(isolate(input$color_mode), "dark")
        ink <- if (dark) "#fff" else "#373a3c"
        paper <- if (dark) "#212529" else "#fff"

        manual_events <- string_to_numeric(isolate(input$manual_events))
        raw_data <- if (input$filter_method != "None") {
            replaced_data()
        } else {
            NULL
        }

        plotly_mnirs(
            rescaled_data(),
            time_labels = input$time_labels,
            ink = ink,
            paper = paper,
            manual_events = manual_events,
            raw_data = raw_data,
            show_raw = isTRUE(isolate(input$show_raw))
        )
    }) |>
        bindEvent(
            rescaled_data(),
            input$time_labels
        )

    ## toggle raw trace visibility in place; raw traces occupy the
    ## first n plot slots when a filter is active
    observeEvent(
        input$show_raw,
        {
            req(rescaled_data(), input$filter_method != "None")

            n <- length(metadata()$nirs_channels)
            plotly::plotlyProxy("plot", session) |>
                plotly::plotlyProxyInvoke(
                    "restyle",
                    list(visible = isTRUE(input$show_raw)),
                    as.list(seq_len(n) - 1L)
                )
        },
        ignoreInit = TRUE
    )

    ## redraw event marker shapes without a full plot rebuild
    observeEvent(
        input$manual_events,
        {
            req(rescaled_data())

            ink <- if (identical(input$color_mode, "dark")) {
                "#fff"
            } else {
                "#373a3c"
            }
            shapes <- event_shapes(
                string_to_numeric(input$manual_events),
                ink = ink,
                time_labels = input$time_labels
            )
            plotly::plotlyProxy("plot", session) |>
                plotly::plotlyProxyInvoke("relayout", list(shapes = shapes))
        },
        ignoreInit = TRUE
    )

    ## dark/light mode: recolour layout and event shapes in place
    observeEvent(
        input$color_mode,
        {
            req(rescaled_data())

            dark <- identical(input$color_mode, "dark")
            ink <- if (dark) "#fff" else "#373a3c"
            paper <- if (dark) "#212529" else "#fff"
            shapes <- event_shapes(
                string_to_numeric(input$manual_events),
                ink = ink,
                time_labels = input$time_labels
            )
            plotly::plotlyProxy("plot", session) |>
                plotly::plotlyProxyInvoke(
                    "relayout",
                    list(
                        paper_bgcolor = paper,
                        plot_bgcolor = paper,
                        "font.color" = ink,
                        "xaxis.color" = ink,
                        "xaxis.linecolor" = ink,
                        "yaxis.color" = ink,
                        "yaxis.linecolor" = ink,
                        shapes = shapes
                    )
                )
        },
        ignoreInit = TRUE
    )

    ## Download handler =============================================
    output$download_data <- downloadHandler(
        filename = \() paste0("mnirs_processed_", Sys.Date(), ".xlsx"),
        content = \(file) writexl::write_xlsx(export_data(), path = file)
    )

    ## Extract Intervals tab ========================================
    ## uses nirs_data() so manual event markers are targets for
    ## by_label/by_lap. mixed by_* methods are resolved to times
    ## app-side because extract_intervals() accepts one type per call
    ## resolve start/end boundary specs to times once; shared by the
    ## boundary plot and extract_intervals(). blank inputs give NULLs
    boundary_times <- reactive({
        req(nirs_data())

        tryCatch(
            {
                starts <- resolve_boundary_times(
                    nirs_data(),
                    list(
                        parse_boundary("time", input$start_time),
                        parse_boundary(
                            "label",
                            input$start_label,
                            fixed = isTRUE(input$label_fixed)
                        ),
                        parse_boundary("lap", input$start_lap),
                        parse_boundary("sample", input$start_sample)
                    ),
                    boundary = "start"
                )
                ends <- resolve_boundary_times(
                    nirs_data(),
                    list(
                        parse_boundary("time", input$end_time),
                        parse_boundary(
                            "label",
                            input$end_label,
                            fixed = isTRUE(input$label_fixed)
                        ),
                        parse_boundary("lap", input$end_lap),
                        parse_boundary("sample", input$end_sample)
                    ),
                    boundary = "end"
                )
                list(starts = starts, ends = ends)
            },
            error = \(e) validate(need(FALSE, clean_cli_message(e)))
        )
    })

    interval_list <- reactive({
        req(nirs_data())
        ## req outside tryCatch so blank inputs stay silent
        req(any(nzchar(trimws(c(
            input$start_time,
            input$start_label,
            input$start_lap,
            input$start_sample,
            input$end_time,
            input$end_label,
            input$end_lap,
            input$end_sample
        )))))

        bounds <- boundary_times()

        tryCatch(
            mnirs::extract_intervals(
                nirs_data(),
                group_intervals = tolower(
                    input$group_intervals %||% "Distinct"
                ),
                start = if (!is.null(bounds$starts)) mnirs::by_time(bounds$starts),
                end = if (!is.null(bounds$ends)) mnirs::by_time(bounds$ends),
                ## blank span inputs read as no offset
                span = c(
                    blank_to_null(input$span_before) %||% 0,
                    blank_to_null(input$span_after) %||% 0
                ),
                zero_time = isTRUE(input$extract_zero_time)
            ),
            error = \(e) validate(need(FALSE, clean_cli_message(e)))
        )
    })

    ## Output: boundary plot ========================================
    ## full-data plot with resolved interval boundaries. plot.mnirs
    ## keeps a numeric x scale even with time_labels, so vline
    ## xintercepts are plain seconds in both modes
    output$boundary_plot <- renderPlot({
        req(nirs_data())
        bounds <- boundary_times()

        p <- plot(nirs_data(), time_labels = isTRUE(input$time_labels)) + 
            theme_mnirs(base_size = 18)
        if (!is.null(bounds$starts)) {
            p <- p + geom_vline(
                xintercept = bounds$starts,
                colour = "green4", linetype = "dashed", alpha = 0.7
            )
        }
        if (!is.null(bounds$ends)) {
            p <- p + geom_vline(
                xintercept = bounds$ends,
                colour = "red3", linetype = "dashed", alpha = 0.7
            )
        }
        p
    })

    ## Output: interval plot ========================================
    ## static ggplot facetted by interval; thematic_shiny() themes it
    output$interval_plot <- renderPlot({
        plot(interval_list(), time_labels = isTRUE(input$time_labels)) + 
            theme_mnirs(base_size = 18, border = "full")
    })

    ## Download handler =============================================
    output$download_intervals <- downloadHandler(
        filename = \() paste0("mnirs_intervals_", Sys.Date(), ".xlsx"),
        content = \(file) writexl::write_xlsx(interval_list(), path = file)
    )
}

shinyApp(ui = ui, server = server)

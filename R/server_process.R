## Process Data tab server logic. Returns shared reactives used by
## the Extract Intervals and Analyse Kinetics tabs
process_server <- function(input, output, session) {
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

    ## scalar bindCache keys: only the resample and filter stages are
    ## expensive enough to cache; keys hold scalar inputs so caches
    ## never hash full data frames. datapath is unique per upload, so
    ## re-uploading an edited file with the same name invalidates
    resample_key <- reactive({
        req(raw_data())
        list(
            input$upload_file$datapath,
            input$nirs_channels,
            input$time_channel,
            input$event_channel,
            input$sample_rate,
            input$head_trim,
            input$tail_trim,
            input$resample_rate,
            input$zero_time_logical
        )
    })

    ## includes the replace-stage inputs because filtered_data()
    ## consumes the uncached replaced_data()
    filter_key <- reactive(c(
        resample_key(),
        list(
            input$invalid_values,
            input$replace_outliers,
            input$outlier_span,
            input$replace_missing,
            input$filter_method,
            input$butter_type,
            input$order,
            input$fc,
            input$filter_span
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

        ## detected defaults fill only inputs the user left blank;
        ## updateTextInput() also updates numericInput (same message)
        defaults <- list(
            nirs_channels = paste(md$nirs_channels, collapse = ", "),
            time_channel = if (identical(md$nirs_device, "Artinis")) {
                "sample = 1"
            } else {
                md$time_channel %||% ""
            },
            event_channel = event_channel_string(
                input$upload_file$datapath,
                md
            ),
            sample_rate = md$sample_rate
        )
        Map(
            \(.id, .value) {
                current <- isolate(input[[.id]])
                blank <- is.null(current) || isTRUE(is.na(current)) ||
                    !isTRUE(nzchar(current))
                if (blank) updateTextInput(session, .id, value = .value)
            },
            names(defaults),
            defaults
        )

        ## default shift timespan to one sample interval
        if (!is.null(md$sample_rate)) {
            updateNumericInput(
                session,
                "shift_span",
                value = signif(1 / md$sample_rate, 3)
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

    ## untick show_raw when a value-altering transform is ticked
    observeEvent(
        list(
            input$shift_logical,
            input$rescale_logical,
            input$bv_correct_logical
        ),
        {
            ticked <- isTRUE(input$shift_logical) ||
                isTRUE(input$rescale_logical) ||
                isTRUE(input$bv_correct_logical)
            if (ticked) {
                updateCheckboxInput(session, "show_raw", value = FALSE)
            }
        },
        ignoreInit = TRUE
    )

    ## reactive trimmed_data ===========================================
    trimmed_data <- reactive({
        req(raw_data())

        out <- raw_data()
        time_channel <- metadata()$time_channel
        head_val <- blank_to_null(input$head_trim)
        tail_val <- blank_to_null(input$tail_trim)

        if (!is.null(head_val)) {
            out <- out[out[[time_channel]] > head_val, ]
        }
        if (!is.null(tail_val)) {
            cutoff <- max(out[[time_channel]], na.rm = TRUE) - tail_val
            out <- out[out[[time_channel]] < cutoff, ]
        }
        return(out)
    })

    ## reactive resampled and zeroed time =================================
    resampled_data <- reactive({
        req(trimmed_data())

        out <- trimmed_data()
        resample_rate <- blank_to_null(input$resample_rate)
        time_channel <- metadata()$time_channel

        if (!is.null(resample_rate)) {
            out <- mnirs::resample_mnirs(
                out,
                resample_rate = resample_rate,
                method = "linear" ## need the interpolation for display
            )
        }
        ## zero time after resample
        if (input$zero_time_logical) {
            out[[time_channel]] <- out[[time_channel]] - out[[time_channel]][1L]
        }
        return(out)
    }) |>
        bindCache(resample_key())

    ## reactive replaced data ======================================
    replaced_data <- reactive({
        req(resampled_data())

        invalid_values <- string_to_numeric(input$invalid_values)
        replace_outliers <- isTRUE(input$replace_outliers)
        replace_missing <- isTRUE(input$replace_missing)

        if (is.null(invalid_values) && !replace_outliers && !replace_missing) {
            return(resampled_data())
        }
        mnirs::replace_mnirs(
            resampled_data(),
            invalid_values = invalid_values,
            outlier_cutoff = if (replace_outliers) 3,
            span = if (replace_outliers) input$outlier_span %||% 15,
            method = if (replace_missing) "linear" else "none"
        )
    })

    ## reactive filtered data ======================================
    filtered_data <- reactive({
        req(replaced_data())

        if (input$filter_method == "none") {
            return(replaced_data())
        }

        try_validate(mnirs::filter_mnirs(
            replaced_data(),
            nirs_channels = metadata()$nirs_channels,
            time_channel = metadata()$time_channel,
            method = input$filter_method,
            type = input$butter_type %||% "low",
            order = input$order %||% 2L,
            fc = input$fc %||% 0.1,
            span = input$filter_span %||% 10,
            partial = TRUE,
            na.rm = TRUE
        ))
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

        out <- try_validate(do.call(
            mnirs::correct_blood_volume,
            c(list(filtered_data()), channels)
        ))

        ## correct_blood_volume() replaces nirs_channels with only the
        ## channels passed to it; restore the rest so they survive to
        ## the plot and export
        attr(out, "nirs_channels") <- union(
            attr(out, "nirs_channels"),
            attr(filtered_data(), "nirs_channels")
        )

        return(out)
    })

    ## reactive shifted_data ======================================
    shifted_data <- reactive({
        req(corrected_data())

        if (!isTRUE(input$shift_logical)) {
            return(corrected_data())
        }

        req(input$shift_which_cols, input$shift_position)

        mnirs::shift_mnirs(
            corrected_data(),
            nirs_channels = metadata()$nirs_channels,
            group_channels = input$shift_which_cols,
            to = blank_to_null(input$shift_to),
            span = blank_to_null(input$shift_span),
            position = input$shift_position
        )
    })

    ## reactive rescaled_data ======================================
    rescaled_data <- reactive({
        req(shifted_data())

        if (!isTRUE(input$rescale_logical)) {
            return(shifted_data())
        }

        req(input$rescale_which_cols)

        mnirs::rescale_mnirs(
            shifted_data(),
            nirs_channels = metadata()$nirs_channels,
            group_channels = input$rescale_which_cols,
            range = c(
                blank_to_null(input$rescale_min),
                blank_to_null(input$rescale_max)
            )
        )
    })

    ## reactive events data ==============================================
    nirs_data <- reactive({
        req(rescaled_data())
        events <- string_to_numeric(input$manual_events)
        time_vec <- rescaled_data()[[metadata()$time_channel]]
        rng <- range(time_vec, na.rm = TRUE)

        ## out-of-range events would silently snap to first/last sample
        validate(need(
            all(!is.na(events) & events >= rng[1L] & events <= rng[2L]),
            sprintf(
                "Event markers must be numeric and within the time channel range (%s to %s).",
                rng[1L],
                rng[2L]
            )
        ))
        add_events(rescaled_data(), events)
    })

    ## reactive base data ==============================================
    ## data is read with keep_all = TRUE so extra columns survive the
    ## pipeline; this drops unrecognised columns from the table and
    ## download when the user unticks keep_all
    base_data <- reactive({
        data <- nirs_data()

        if (isTRUE(input$keep_all)) {
            return(data)
        }

        keep <- c(
            metadata()$time_channel,
            metadata()$nirs_channels,
            metadata()$event_channel,
            "event",
            "event_labels"
        )
        return(data[intersect(names(data), keep)])
    })

    ## Output: Data table ==========================================
    output$nirs_table <- renderDT({
        data <- base_data()
        req(data)

        signif_datatable(
            data,
            time_channel = metadata()$time_channel,
            options = list(
                dom = 'frtip',
                pageLength = 20,
                scrollX = TRUE,
                searchHighlight = FALSE,
                ## jump-to-page input appended beside pagination buttons
                initComplete = JS("
                    function() {
                        var api = this.api();
                        $('<input type=\"number\" min=\"1\" title=\"Go to page\"' +
                            ' class=\"form-control form-control-sm' +
                            ' d-inline-block w-auto ms-2\" placeholder=\"Page\">')
                            .on('change', function() {
                                var info = api.page.info();
                                var p = Math.min(
                                    Math.max(parseInt(this.value, 10), 1),
                                    info.pages
                                );
                                if (p) api.page(p - 1).draw('page');
                            })
                            .appendTo($(api.table().container())
                                .find('.dataTables_paginate'));
                    }
                ")
            )
        )
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

        cols <- mode_colours(isolate(input$color_mode))

        manual_events <- string_to_numeric(isolate(input$manual_events))
        raw_data <- if (input$filter_method != "none") {
            replaced_data()
        } else {
            NULL
        }

        plotly_mnirs(
            rescaled_data(),
            time_labels = input$time_labels,
            ink = cols$ink,
            paper = cols$paper,
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
            req(rescaled_data(), input$filter_method != "none")

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

    ## event markers and dark/light mode share one relayout: both
    ## rebuild the shapes with the current ink colour, without a full
    ## plot rebuild
    observeEvent(
        list(input$manual_events, input$color_mode),
        {
            req(rescaled_data())

            cols <- mode_colours(input$color_mode)
            shapes <- event_shapes(
                string_to_numeric(input$manual_events),
                ink = cols$ink,
                time_labels = input$time_labels
            )
            plotly::plotlyProxy("plot", session) |>
                plotly::plotlyProxyInvoke(
                    "relayout",
                    list(
                        paper_bgcolor = cols$paper,
                        plot_bgcolor = cols$paper,
                        "font.color" = cols$ink,
                        "xaxis.color" = cols$ink,
                        "xaxis.linecolor" = cols$ink,
                        "yaxis.color" = cols$ink,
                        "yaxis.linecolor" = cols$ink,
                        shapes = shapes
                    )
                )
        },
        ignoreInit = TRUE
    )

    ## Download handler =============================================
    download_xlsx(output, "download_data", "mnirs_processed", base_data)

    ## client-side PNG via plotly.js keeps current zoom, colour mode and
    ## raw traces; scale 3 ≈ 300 dpi at on-screen size
    observeEvent(input$download_plot, {
        shinyjs::runjs(sprintf(
            'Plotly.downloadImage(document.getElementById("plot"), {format: "png", scale: 3, filename: "mnirs_plot_%s"})',
            Sys.Date()
        ))
    })
    toggle_download("download_plot", rescaled_data)

    return(list(base_data = base_data))
}

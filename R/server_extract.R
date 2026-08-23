## Extract Intervals tab server logic. Takes export_data() from
## process_server() so manual event markers are targets for
## by_label/by_lap. mixed by_* methods are resolved to sorted times
## app-side so the boundary plot shares them with extract_intervals()
extract_server <- function(input, output, session, export_data) {
    ## all boundary text input ids, e.g. "start_time" ... "end_sample"
    boundary_ids <- outer(
        c("start", "end"),
        c("time", "label", "lap", "sample"),
        paste,
        sep = "_"
    )

    ## resolve one side's boundary specs to sorted times
    resolve_side <- function(side) {
        resolve_boundary_times(
            export_data(),
            list(
                parse_boundary("time", input[[paste0(side, "_time")]]),
                parse_boundary(
                    "label",
                    input[[paste0(side, "_label")]],
                    fixed = isTRUE(input$label_fixed)
                ),
                parse_boundary("lap", input[[paste0(side, "_lap")]]),
                parse_boundary("sample", input[[paste0(side, "_sample")]])
            ),
            boundary = side
        )
    }

    ## resolve start/end boundary specs to times once; shared by the
    ## boundary plot and extract_intervals(). blank inputs give NULLs
    boundary_times <- reactive({
        req(export_data())

        tryCatch(
            list(starts = resolve_side("start"), ends = resolve_side("end")),
            error = \(e) validate(need(FALSE, clean_cli_message(e)))
        )
    })

    interval_list <- reactive({
        req(export_data())
        ## req outside tryCatch so blank inputs stay silent
        vals <- unlist(lapply(boundary_ids, \(.id) input[[.id]]))
        req(any(nzchar(trimws(vals))))

        bounds <- boundary_times()
        ## signed span offsets; single blank input recycles scalar by
        ## sign (mnirs convention: 1 -> c(0, 1), -5 -> c(-5, 0));
        ## both blank -> c(0, 0)
        span_vals <- c(
            blank_to_null(input$span_start),
            blank_to_null(input$span_end)
        )
        span <- if (length(span_vals) == 2L) {
            span_vals
        } else {
            sort(c(0, span_vals %||% 0))
        }

        ## single boundary side needs positive-width span window
        validate(need(
            !xor(is.null(bounds$starts), is.null(bounds$ends)) ||
                diff(span) > 0,
            "Cannot process interval range: specify `start` and `end`, or a range with `span`."
        ))

        tryCatch(
            mnirs::extract_intervals(
                export_data(),
                group_intervals = tolower(
                    input$group_intervals %||% "Distinct"
                ),
                start = if (!is.null(bounds$starts)) {
                    mnirs::by_time(bounds$starts)
                },
                end = if (!is.null(bounds$ends)) mnirs::by_time(bounds$ends),
                span = span,
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
        req(export_data())
        bounds <- boundary_times()

        vlines <- list(
            list(x = bounds$starts, col = "green4"),
            list(x = bounds$ends, col = "red3")
        )
        Reduce(\(p, v) {
            if (is.null(v$x)) {
                return(p)
            }
            p +
                geom_vline(
                    xintercept = v$x,
                    colour = v$col,
                    linetype = "dashed",
                    alpha = 0.7
                )
        },
        vlines,
        init = plot(export_data(), time_labels = isTRUE(input$time_labels)) +
            theme_mnirs(base_size = 20)
        )
    })

    ## Output: interval plot ========================================
    ## static ggplot facetted by interval; thematic_shiny() themes it
    output$interval_plot <- renderPlot({
        plot(
            interval_list(),
            time_labels = isTRUE(input$time_labels),
            scales = if (isTRUE(input$interval_free_y)) "free" else "free_x"
        ) +
            theme_mnirs(base_size = 20, border = "full")
    })

    ## Download handler =============================================
    output$download_intervals <- downloadHandler(
        filename = \() paste0("mnirs_intervals_", Sys.Date(), ".xlsx"),
        content = \(file) writexl::write_xlsx(interval_list(), path = file)
    )

    ## expose intervals to the kinetics tab
    return(list(interval_list = interval_list))
}

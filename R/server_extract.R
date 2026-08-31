## Extract Intervals tab server logic. Boundaries and intervals resolve
## from base_data() (manual event markers are targets for by_label);
## this page never modifies the data. mixed by_* methods are resolved
## to sorted times app-side so the boundary plot shares them with
## extract_intervals()
extract_server <- function(input, output, session, base_data) {
    boundary_methods <- c("time", "label", "lap", "sample")

    ## resolve one side's boundary specs to sorted times; `fixed` is
    ## ignored by the non-label parse_boundary branches
    resolve_side <- function(side) {
        specs <- Map(
            \(.m) parse_boundary(
                .m,
                input[[paste0(side, "_", .m)]],
                fixed = isTRUE(input$label_fixed)
            ),
            boundary_methods
        )
        return(resolve_boundary_times(base_data(), specs, boundary = side))
    }

    ## resolve start/end boundary specs to times once; shared by the
    ## boundary plot and extract_intervals(). blank inputs give NULLs
    boundary_times <- reactive({
        req(base_data())

        try_validate(
            list(starts = resolve_side("start"), ends = resolve_side("end"))
        )
    })

    interval_list <- reactive({
        req(base_data())
        ## req outside try_validate so blank inputs stay silent
        ids <- paste0(rep(c("start_", "end_"), each = 4L), boundary_methods)
        vals <- unlist(lapply(ids, \(.id) input[[.id]]))
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

        try_validate(mnirs::extract_intervals(
            base_data(),
            group_intervals = input$group_intervals %||% "distinct",
            start = if (!is.null(bounds$starts)) {
                mnirs::by_time(bounds$starts)
            },
            end = if (!is.null(bounds$ends)) mnirs::by_time(bounds$ends),
            span = span,
            zero_time = isTRUE(input$extract_zero_time)
        ))
    })

    ## Output: boundary plot ========================================
    ## full-data plot with resolved interval boundaries. plot.mnirs
    ## keeps a numeric x scale even with time_labels, so vline
    ## xintercepts are plain seconds in both modes
    boundary_gg <- function(base_size = plot_base_size) {
        req(base_data())
        bounds <- boundary_times()

        ## empty xintercept draws nothing, so blank sides are safe
        return(
            plot(base_data(), time_labels = isTRUE(input$time_labels)) +
                geom_vline(
                    xintercept = bounds$starts %||% numeric(),
                    colour = "green4",
                    linetype = "dashed",
                    alpha = 0.7
                ) +
                geom_vline(
                    xintercept = bounds$ends %||% numeric(),
                    colour = "red3",
                    linetype = "dashed",
                    alpha = 0.7
                ) +
                theme_mnirs(base_size = base_size)
        )
    }
    output$boundary_plot <- render_plot_mm(boundary_gg, \() 80, "boundary_plot")

    ## Output: interval plot ========================================
    ## static ggplot facetted by interval; thematic_shiny() themes it.
    ## dynamic facet grid capped at 5 columns; height grows with facet
    ## rows so panels don't squash
    interval_dims <- reactive(facet_dims(interval_list()))

    interval_gg <- function(base_size = plot_base_size) {
        plot(
            interval_list(),
            time_labels = isTRUE(input$time_labels),
            scales = if (isTRUE(input$interval_free_y)) "free" else "free_x",
            ncol = interval_dims()$ncol
        ) +
            theme_mnirs(base_size = base_size, border = "full")
    }
    output$interval_plot <- render_plot_mm(
        interval_gg,
        \() interval_dims()$height_mm,
        "interval_plot"
    )

    ## Download handlers ============================================
    download_xlsx(
        output,
        "download_intervals",
        "mnirs_intervals",
        interval_list
    )
    download_png(
        output,
        "download_session_plot",
        "intervals_session",
        boundary_gg,
        \() 80
    )
    download_png(
        output,
        "download_facet_plot",
        "intervals_facet",
        interval_gg,
        \() interval_dims()$height_mm
    )
    ## hidden download links: keep outputs alive so hrefs are assigned
    outputOptions(output, "download_session_plot", suspendWhenHidden = FALSE)
    outputOptions(output, "download_facet_plot", suspendWhenHidden = FALSE)

    ## one visible button triggers both hidden download links; second
    ## click delayed so the browser registers both downloads
    observeEvent(input$download_plots, {
        shinyjs::click("download_session_plot")
        shinyjs::delay(500, shinyjs::click("download_facet_plot"))
    })
    toggle_download("download_plots", interval_list)

    ## expose intervals to the kinetics tab
    return(list(interval_list = interval_list))
}

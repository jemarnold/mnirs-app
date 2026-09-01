## Analyse Kinetics tab server logic. fits mnirs::analyse_kinetics()
## on intervals from extract_server()
kinetics_server <- function(
    input,
    output,
    session,
    interval_list,
    base_data
) {
    ## channels driving the fit; updated on data load and on select-box
    ## blur so each click inside the multi-select does not refit
    kin_channels <- reactiveVal()

    ## sync channel choices to processed data; first channel selected
    observeEvent(base_data(), {
        chs <- attr(base_data(), "nirs_channels")
        updateSelectizeInput(
            session,
            "kin_nirs_channels",
            choices = chs,
            selected = chs[1]
        )
        kin_channels(chs[1])
    })

    ## empty JS array arrives as list(); unlist() -> NULL blocks req()
    observeEvent(input$kin_nirs_channels_blur, {
        kin_channels(unlist(input$kin_nirs_channels_blur))
    })

    kinetics_results <- reactive({
        ## req outside try_validate so missing intervals stay silent
        req(interval_list())
        method <- req(input$kin_method)

        ## shared args; blank numerics -> NULL (metadata defaults),
        ## blank end_window -> Inf (full interval)
        args <- list(
            data = interval_list(),
            nirs_channels = req(kin_channels()),
            method = method,
            start_time = blank_to_null(input$kin_start_time),
            direction = input$kin_direction %||% "auto",
            end_window = blank_to_null(input$kin_end_window) %||% Inf,
            partial = FALSE,
            na.rm = TRUE
        )

        ## method-specific args; NULL elements drop from the list so
        ## only the populated width/span reaches analyse_kinetics()
        extra <- switch(
            method,
            response_time = list(
                fraction = blank_to_null(input$kin_fraction) %||% 0.5
            ),
            peak_slope = {
                width <- blank_to_null(input$kin_width)
                span <- blank_to_null(input$kin_span)
                validate(need(
                    xor(is.null(width), is.null(span)),
                    "Peak Slope requires exactly one of Window Width or Window Span."
                ))
                list(
                    width = width,
                    span = span,
                    align = input$kin_align %||% "centre"
                )
            },
            monoexponential = list(use_TD = isTRUE(input$kin_use_TD)),
            exponential_drift = list(
                use_TD = isTRUE(input$kin_use_TD),
                tau_mult = blank_to_null(input$kin_tau_mult) %||% 3
            ),
            sigmoidal = list(shape = input$kin_shape %||% "symmetric")
        )

        try_validate(do.call(mnirs::analyse_kinetics, c(args, extra)))
    })

    ## Output: kinetics plot ========================================
    ## static ggplot facetted by interval; thematic_shiny() themes it.
    ## dynamic facet grid capped at 5 columns; height grows with facet
    ## rows so panels don't squash
    kin_dims <- reactive(facet_dims(kinetics_results()$data))

    kin_gg <- function(base_size = plot_base_size) {
        plot(
            kinetics_results(),
            time_labels = isTRUE(input$time_labels),
            labels = isTRUE(input$kin_labels),
            scales = if (isTRUE(input$kin_free_y)) "free" else "free_x",
            ncol = kin_dims()$ncol,
            ## geom_text size is absolute mm, so it must track base_size to
            ## keep the same label:text ratio on the wide screen device and
            ## the canonical-width export
            label_size = 3 * base_size / plot_base_size
        ) +
            theme_mnirs(base_size = base_size, border = "full")
    }
    output$kin_plot <- render_plot_mm(
        kin_gg,
        \() kin_dims()$height_mm,
        "kin_plot"
    )

    ## Output: results tables =======================================
    ## small static tables: no search/paging controls
    kin_opts <- list(dom = 't', scrollX = TRUE)
    output$kin_coefficients <- renderDT(
        signif_datatable(kinetics_results()$coefficients, options = kin_opts)
    )
    output$kin_diagnostics <- renderDT(
        signif_datatable(kinetics_results()$diagnostics, options = kin_opts)
    )

    ## warnings header + table only when warnings exist
    output$kin_warnings_ui <- renderUI({
        req(nrow(kinetics_results()$warnings) > 0L)
        tagList(
            card_header("Warnings"),
            DTOutput("kin_warnings", fill = FALSE)
        )
    })
    output$kin_warnings <- renderDT(
        signif_datatable(kinetics_results()$warnings, options = kin_opts)
    )

    ## Download handlers ============================================
    download_xlsx(
        output,
        "kin_download_data",
        "mnirs_kinetics_data",
        \() kinetics_results()$data,
        enable_fn = kinetics_results
    )
    download_xlsx(
        output,
        "kin_download_coefs",
        "mnirs_kinetics_results",
        \() {
            results <- kinetics_results()
            list(
                "coefficients" = results$coefficients,
                "diagnostics" = results$diagnostics,
                "warnings" = results$warnings,
                "channel arguments" = results$channel_args
            )
        },
        enable_fn = kinetics_results
    )
    download_png(
        output,
        "kin_download_plot",
        "kinetics_facet",
        kin_gg,
        \() kin_dims()$height_mm,
        enable_fn = kinetics_results
    )

    ## expose fitted results for the mVO2 Recovery Kinetics page
    return(list(kinetics_results = kinetics_results))
}

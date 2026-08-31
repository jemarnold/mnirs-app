## Analyse Kinetics tab server logic. fits mnirs::analyse_kinetics()
## on intervals from extract_server(); select display labels recoded
## to canonical arg values app-side
kinetics_server <- function(
    input,
    output,
    session,
    interval_list,
    base_data
) {
    ## display label -> analyse_kinetics() method value
    methods <- c(
        "Response Time" = "response_time",
        "Peak Slope" = "peak_slope",
        "Monoexponential" = "monoexponential",
        "Biexponential" = "biexponential",
        "Sigmoidal" = "sigmoidal"
    )

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
        ## req outside tryCatch so missing intervals stay silent
        req(interval_list())
        method <- methods[[req(input$kin_method)]]

        ## shared args; blank numerics -> NULL (metadata defaults),
        ## blank end_window -> Inf (full interval)
        args <- list(
            data = interval_list(),
            nirs_channels = req(kin_channels()),
            method = method,
            start_time = blank_to_null(input$kin_start_time),
            direction = tolower(input$kin_direction %||% "Auto"),
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
                    align = tolower(input$kin_align %||% "Centre")
                )
            },
            monoexponential = list(use_TD = isTRUE(input$kin_use_TD)),
            biexponential = list(use_TD = isTRUE(input$kin_use_TD)),
            sigmoidal = list(
                shape = switch(
                    input$kin_shape %||% "Symmetric",
                    "Symmetric" = "symmetric",
                    "Gompertz" = "gompertz",
                    "Gompertz-Left" = "gompertz_left"
                )
            )
        )

        tryCatch(
            do.call(mnirs::analyse_kinetics, c(args, extra)),
            error = \(e) validate(need(FALSE, clean_cli_message(e)))
        )
    })

    ## Output: kinetics plot ========================================
    ## static ggplot facetted by interval; thematic_shiny() themes it.
    ## dynamic facet grid capped at 5 columns; height grows with facet
    ## rows so panels don't squash
    kin_n <- reactive({
        x <- kinetics_results()$data
        if (is.data.frame(x)) 1L else length(x)
    })

    kin_height_mm <- \() facet_height_mm(facet_rows(kin_n()))

    kin_gg <- function(base_size = plot_base_size) {
        plot(
            kinetics_results(),
            time_labels = isTRUE(input$time_labels),
            labels = isTRUE(input$kin_labels),
            scales = if (isTRUE(input$kin_free_y)) "free" else "free_x",
            ncol = facet_ncol(kin_n()),
            ## geom_text size is absolute mm, so it must track base_size to
            ## keep the same label:text ratio on the wide screen device and
            ## the canonical-width export
            label_size = 3 * base_size / plot_base_size
        ) +
            theme_mnirs(base_size = base_size, border = "full")
    }
    output$kin_plot <- render_plot_mm(kin_gg, kin_height_mm, "kin_plot")

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
    output$kin_download_data <- downloadHandler(
        filename = \() paste0("mnirs_kinetics_data_", Sys.Date(), ".xlsx"),
        content = \(file) {
            writexl::write_xlsx(
                kinetics_results()$data,
                path = file
            )
        }
    )
    output$kin_download_coefs <- downloadHandler(
        filename = \() paste0("mnirs_kinetics_results_", Sys.Date(), ".xlsx"),
        content = \(file) {
            results <- kinetics_results()
            writexl::write_xlsx(
                list(
                    "coefficients" = results$coefficients,
                    "diagnostics" = results$diagnostics,
                    "warnings" = results$warnings,
                    "channel arguments" = results$channel_args
                ),
                path = file
            )
        }
    )
    output$kin_download_plot <- downloadHandler(
        filename = \() paste0("kinetics_facet_", Sys.Date(), ".png"),
        content = \(file) save_plot_png(file, kin_gg, kin_height_mm())
    )
    toggle_download("kin_download_data", kinetics_results)
    toggle_download("kin_download_coefs", kinetics_results)
    toggle_download("kin_download_plot", kinetics_results)
}

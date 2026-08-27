## Analyse Kinetics tab server logic. fits mnirs::analyse_kinetics()
## on intervals from extract_server(); select display labels recoded
## to canonical arg values app-side
kinetics_server <- function(input, output, session, interval_list) {
    ## display label -> analyse_kinetics() method value
    methods <- c(
        "Response Time" = "response_time",
        "Peak Slope" = "peak_slope",
        "Monoexponential" = "monoexponential",
        "Biexponential" = "biexponential",
        "Sigmoidal" = "sigmoidal"
    )

    kinetics_results <- reactive({
        ## req outside tryCatch so missing intervals stay silent
        req(interval_list())
        method <- methods[[req(input$kin_method)]]

        ## shared args; blank numerics -> NULL (metadata defaults),
        ## blank end_window -> Inf (full interval)
        args <- list(
            data = interval_list(),
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
    ## static ggplot facetted by interval; thematic_shiny() themes it
    output$kin_plot <- renderPlot({
        plot(
            kinetics_results(),
            labels = isTRUE(input$kin_labels),
            scales = if (isTRUE(input$kin_free_y)) "free" else "free_x"
        ) +
            theme_mnirs(base_size = 20, border = "full")
    })

    ## Output: results tables =======================================
    output$kin_coefficients <- renderTable(
        kinetics_results()$coefficients,
        digits = 3
    )
    output$kin_diagnostics <- renderTable(
        kinetics_results()$diagnostics,
        digits = 3
    )

    ## warnings header + table only when warnings exist
    output$kin_warnings_ui <- renderUI({
        req(nrow(kinetics_results()$warnings) > 0L)
        tagList(card_header("Warnings"), tableOutput("kin_warnings"))
    })
    output$kin_warnings <- renderTable(kinetics_results()$warnings)

    ## Download handlers ============================================
    output$kin_download_data <- downloadHandler(
        filename = \() paste0("mnirs_kinetics_data_", Sys.Date(), ".xlsx"),
        content = \(file) writexl::write_xlsx(
            kinetics_results()$data,
            path = file
        )
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
    toggle_download("kin_download_data", kinetics_results)
    toggle_download("kin_download_coefs", kinetics_results)
}

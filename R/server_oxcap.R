## oxcap Recovery Kinetics tab server logic. recursively fits an
## exponential recovery model through peak-slope results from
## kinetics_server(), per the OxCap repeated-occlusion method
oxcap_server <- function(input, output, session, kinetics_results) {
    recovery_results <- reactive({
        ## req outside try_validate so missing upstream results stay silent
        slopes <- req(kinetics_results())
        validate(need(
            all(c("slope", "peak_slope_time") %in% names(slopes$coefficients)),
            "Run the Peak Slope method on the Analyse Kinetics page first."
        ))
        method <- req(input$oxcap_method)
        oxcap_groups <- parse_group_intervals(input$oxcap_groups) %||%
            "ensemble"

        try_validate({
            ## blank groups fit all slope samples as one "ensemble" curve
            args <- c(
                list(
                    data = slopes,
                    nirs_channels = "slope",
                    time_channel = "peak_slope_time",
                    method = method,
                    use_TD = isTRUE(input$oxcap_use_TD),
                    zero_time = isTRUE(input$oxcap_zero_time),
                    group_intervals = oxcap_groups
                ),
                if (identical(method, "exponential_drift")) {
                    list(tau_mult = blank_to_null(input$oxcap_tau_mult) %||% 3)
                }
            )
            do.call(mnirs::analyse_kinetics, args)
        })
    })

    ## Output: recovery fit plot ====================================
    oxcap_dims <- reactive(facet_dims(recovery_results()$data))

    ## package label rows give colour and corner per channel; their text is
    ## replaced by two OxCap lines (tau, k) stacked 1.6 text heights apart
    coef_labels <- reactive({
        results <- recovery_results()
        coefs <- results$coefficients
        ann <- mnirs:::kinetics_annotations(results)
        lab <- ann[nzchar(ann$label), ]
        lab <- lab[!duplicated(lab[c("interval", "nirs_channels")]), ]
        i <- match(
            paste(lab$interval, lab$nirs_channels),
            paste(coefs$interval, coefs$nirs_channels)
        )
        lab <- lab[rep(seq_len(nrow(lab)), each = 2L), ]
        lab$label <- c(rbind(
            sprintf("tau = %s sec", signif(coefs$tau[i], 3)),
            sprintf("k = %s min⁻¹", signif(coefs$k[i] * 60, 3))
        ))
        idx <- stats::ave(seq_along(lab$label), lab$interval, FUN = seq_along) -
            1L
        lab$vjust <- ifelse(lab$yval < 0, -1.6 * idx, 1 + 1.6 * idx)
        return(lab)
    })

    ## k also in min⁻¹, the OxCap reporting convention, inserted after k
    oxcap_coefs <- reactive({
        coefs <- recovery_results()$coefficients
        coefs$k_min <- coefs$k * 60
        return(coefs[append(
            setdiff(names(coefs), "k_min"),
            "k_min",
            after = match("k", names(coefs))
        )])
    })

    oxcap_gg <- function(base_size = plot_base_size) {
        p <- plot(
            recovery_results(),
            time_labels = isTRUE(input$time_labels),
            points = TRUE,
            labels = FALSE,
            scales = if (isTRUE(input$oxcap_free_y)) "free" else "free_x",
            ncol = oxcap_dims()$ncol,
            ## show the two-phase fit components for exponential drift
            components = identical(input$oxcap_method, "exponential_drift")
        ) +
            theme_mnirs(base_size = base_size, border = "full") +
            ## fitted y is peak slope, not a raw mNIRS signal
            labs(
                x = if (isTRUE(input$time_labels)) "time (mm:ss)" else "time",
                y = "mNIRS Slope (/sec)"
            )
        if (isTRUE(input$oxcap_labels)) {
            ## geom_text size is absolute mm, so it must track base_size
            ## (see kin_gg in server_kinetics.R)
            p <- p +
                geom_text(
                    data = coef_labels(),
                    aes(
                        x = xval,
                        y = yval,
                        label = label,
                        colour = nirs_channels,
                        vjust = vjust
                    ),
                    hjust = 1.05,
                    size = 3.5 * base_size / plot_base_size,
                    show.legend = FALSE,
                    inherit.aes = FALSE
                )
        }
        return(p)
    }
    output$oxcap_plot <- render_plot_mm(
        oxcap_gg,
        \() oxcap_dims()$height_mm,
        "oxcap_plot"
    )

    ## Output: results tables =======================================
    oxcap_opts <- list(dom = 't', scrollX = TRUE)
    output$oxcap_coefficients <- renderDT(
        signif_datatable(trim_coefs(oxcap_coefs()), options = oxcap_opts)
    )
    output$oxcap_diagnostics <- renderDT(
        signif_datatable(recovery_results()$diagnostics, options = oxcap_opts)
    )

    ## warnings header + table only when warnings exist
    output$oxcap_warnings_ui <- renderUI({
        req(nrow(recovery_results()$warnings) > 0L)
        tagList(
            card_header("Warnings"),
            DTOutput("oxcap_warnings", fill = FALSE)
        )
    })
    output$oxcap_warnings <- renderDT(
        signif_datatable(recovery_results()$warnings, options = oxcap_opts)
    )

    ## Download handlers ============================================
    download_xlsx(
        output,
        "oxcap_download_data",
        "mnirs_oxcap_recovery_data",
        \() recovery_results()$data,
        tab = "OxCap Analysis",
        enable_fn = recovery_results
    )
    download_xlsx(
        output,
        "oxcap_download_coefs",
        "mnirs_oxcap_recovery_results",
        \() {
            results <- recovery_results()
            list(
                "coefficients" = oxcap_coefs(),
                "diagnostics" = results$diagnostics,
                "warnings" = results$warnings,
                "channel arguments" = results$channel_args
            )
        },
        tab = "OxCap Analysis",
        enable_fn = recovery_results
    )
    download_png(
        output,
        "oxcap_download_plot",
        "oxcap_recovery_fit",
        oxcap_gg,
        \() oxcap_dims()$height_mm,
        enable_fn = recovery_results,
        tab = "OxCap Analysis"
    )
}

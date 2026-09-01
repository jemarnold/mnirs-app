## mVO2 Recovery Kinetics tab server logic. recursively fits an
## exponential recovery model through peak-slope results from
## kinetics_server(), per the OxCap repeated-occlusion method
mvo2_server <- function(input, output, session, kinetics_results) {
    recovery_results <- reactive({
        ## req outside try_validate so missing upstream results stay silent
        slopes <- req(kinetics_results())
        validate(need(
            all(c("slope", "peak_slope_time") %in% names(slopes$coefficients)),
            "Run the Peak Slope method on the Analyse Kinetics page first."
        ))
        method <- req(input$mvo2_method)
        mvo2_groups <- parse_group_intervals(input$mvo2_groups) %||%
            "ensemble"

        try_validate({
            ## blank groups fit all slope samples as one "ensemble" curve
            args <- c(
                list(
                    data = slopes,
                    nirs_channels = "slope",
                    time_channel = "peak_slope_time",
                    method = method,
                    use_TD = isTRUE(input$mvo2_use_TD),
                    zero_time = isTRUE(input$mvo2_zero_time),
                    group_intervals = mvo2_groups
                ),
                if (identical(method, "exponential_drift")) {
                    list(tau_mult = blank_to_null(input$mvo2_tau_mult) %||% 3)
                }
            )
            do.call(mnirs::analyse_kinetics, args)
        })
    })

    ## Output: recovery fit plot ====================================
    mvo2_dims <- reactive(facet_dims(recovery_results()$data))

    ## package annotation rows (colour/corner/stagger) with OxCap tau + k text
    coef_labels <- reactive({
        results <- recovery_results()
        coefs <- results$coefficients
        ann <- mnirs:::kinetics_annotations(results)[seq_len(nrow(coefs)), ]
        ann$label <- sprintf(
            "tau = %s sec\nk = %s min⁻¹",
            signif(coefs$tau, 3),
            signif(coefs$k * 60, 3)
        )
        ## re-stagger: uniform two-line labels replace method default text
        rank <- stats::ave(ann$vjust, ann$interval, FUN = seq_along)
        gap <- (2.2 * (rank - 1) + 0.2) / 2
        ann$vjust <- ifelse(ann$yval_corner < 0, -gap, 1 + gap)
        return(ann)
    })

    ## k also in min⁻¹, the OxCap reporting convention, inserted after k
    mvo2_coefs <- reactive({
        coefs <- recovery_results()$coefficients
        coefs$k_min <- coefs$k * 60
        return(coefs[append(
            setdiff(names(coefs), "k_min"),
            "k_min",
            after = match("k", names(coefs))
        )])
    })

    mvo2_gg <- function(base_size = plot_base_size) {
        p <- plot(
            recovery_results(),
            time_labels = isTRUE(input$time_labels),
            points = TRUE,
            labels = FALSE,
            scales = if (isTRUE(input$mvo2_free_y)) "free" else "free_x",
            ncol = mvo2_dims()$ncol,
            ## show the two-phase fit components for exponential drift
            components = identical(input$mvo2_method, "exponential_drift")
        ) +
            theme_mnirs(base_size = base_size, border = "full") +
            ## fitted y is peak slope, not a raw mNIRS signal
            labs(
                x = if (isTRUE(input$time_labels)) "time (mm:ss)" else "time",
                y = "mNIRS Slope (/sec)"
            )
        if (isTRUE(input$mvo2_labels)) {
            ## geom_text size is absolute mm, so it must track base_size
            ## (see kin_gg in server_kinetics.R)
            p <- p +
                geom_text(
                    data = coef_labels(),
                    aes(
                        y = yval_corner,
                        label = label,
                        colour = nirs_channels,
                        vjust = vjust
                    ),
                    x = Inf,
                    hjust = 1.05,
                    size = 3.5 * base_size / plot_base_size,
                    show.legend = FALSE,
                    inherit.aes = FALSE
                )
        }
        return(p)
    }
    output$mvo2_plot <- render_plot_mm(
        mvo2_gg,
        \() mvo2_dims()$height_mm,
        "mvo2_plot"
    )

    ## Output: results tables =======================================
    mvo2_opts <- list(dom = 't', scrollX = TRUE)
    output$mvo2_coefficients <- renderDT(
        signif_datatable(trim_coefs(mvo2_coefs()), options = mvo2_opts)
    )
    output$mvo2_diagnostics <- renderDT(
        signif_datatable(recovery_results()$diagnostics, options = mvo2_opts)
    )

    ## warnings header + table only when warnings exist
    output$mvo2_warnings_ui <- renderUI({
        req(nrow(recovery_results()$warnings) > 0L)
        tagList(
            card_header("Warnings"),
            DTOutput("mvo2_warnings", fill = FALSE)
        )
    })
    output$mvo2_warnings <- renderDT(
        signif_datatable(recovery_results()$warnings, options = mvo2_opts)
    )

    ## Download handlers ============================================
    download_xlsx(
        output,
        "mvo2_download_data",
        "mnirs_mvo2_recovery_data",
        \() recovery_results()$data,
        enable_fn = recovery_results
    )
    download_xlsx(
        output,
        "mvo2_download_coefs",
        "mnirs_mvo2_recovery_results",
        \() {
            results <- recovery_results()
            list(
                "coefficients" = mvo2_coefs(),
                "diagnostics" = results$diagnostics,
                "warnings" = results$warnings,
                "channel arguments" = results$channel_args
            )
        },
        enable_fn = recovery_results
    )
    download_png(
        output,
        "mvo2_download_plot",
        "mvo2_recovery_fit",
        mvo2_gg,
        \() mvo2_dims()$height_mm,
        enable_fn = recovery_results
    )
}

## Dashed vertical line shapes for manual event markers; shared by
## the initial plot build and plotlyProxy relayout updates
event_shapes <- function(manual_events, ink, time_labels = FALSE) {
    if (!length(manual_events)) {
        return(list())
    }

    ## match the POSIXct x-axis when time is displayed as h:mm:ss
    events <- if (time_labels) {
        lapply(manual_events, \(.e) .POSIXct(as.numeric(.e), tz = "UTC"))
    } else {
        manual_events
    }

    return(lapply(events, \(.event) {
        list(
            type = "line",
            x0 = .event,
            x1 = .event,
            xref = "x",
            y0 = 0,
            y1 = 1,
            yref = "paper",
            line = list(color = ink, dash = "dash", width = 1)
        )
    }))
}

## Build interactive plotly plot reproducing theme_mnirs() elements
plotly_mnirs <- function(
    data,
    time_labels = FALSE,
    ink = "#373a3c",
    paper = "#fff",
    manual_events = NULL,
    base_size = 20,
    raw_data = NULL,
    show_raw = FALSE
) {
    time_ch <- attr(data, "time_channel")
    nirs_ch <- attr(data, "nirs_channels")
    colours <- mnirs::palette_mnirs(length(nirs_ch))

    ## When labelling axis as h:mm:ss, render x as POSIXct so plotly
    ## auto-recomputes ticks on zoom
    time_vec <- if (time_labels) {
        .POSIXct(as.numeric(data[[time_ch]]), tz = "UTC")
    } else {
        data[[time_ch]]
    }

    time_label <- if (time_labels) {
        mnirs::format_hmmss(time_vec)
    } else {
        mnirs:::signif_trailing(time_vec, 3L)
    }

    plot <- plotly::plot_ly()

    ## optional raw (unfiltered) traces drawn first at low alpha;
    ## always added when available so plotlyProxy can toggle
    ## visibility without a full rebuild
    if (!is.null(raw_data)) {
        plot <- Reduce(\(p, i) {
            plotly::add_trace(
                p,
                x = time_vec,
                y = raw_data[[nirs_ch[[i]]]],
                type = "scattergl",
                mode = "lines",
                name = paste0(nirs_ch[[i]], " (raw)"),
                showlegend = FALSE,
                hoverinfo = "skip",
                visible = show_raw,
                line = list(color = colours[[i]], width = 1),
                opacity = 0.6
            )
        }, seq_along(nirs_ch), init = plot)
    }

    ## one line trace per NIRS channel
    plot <- Reduce(\(p, i) {
        plotly::add_trace(
            p,
            x = time_vec,
            y = data[[nirs_ch[[i]]]],
            type = "scattergl",
            mode = "lines",
            name = nirs_ch[[i]],
            showlegend = TRUE,
            line = list(color = colours[[i]], width = 1.5),
            text = time_label,
            hovertemplate = paste0(
                time_ch, ": %{text}<br>",
                "<b>", nirs_ch[[i]], ":</b> %{y:.2f}<extra></extra>"
            )
        )
    }, seq_along(nirs_ch), init = plot)

    shapes <- event_shapes(manual_events, ink, time_labels)

    ## x-axis with optional h:mm:ss tick text
    xaxis <- list(
        title = if (time_labels) paste(time_ch, "(h:mm:ss)") else time_ch,
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = ink,
        color = ink
    )
    if (time_labels) {
        xaxis$type <- "date"
        xaxis$tickformat <- "%-H:%M:%S"
    }

    plotly::layout(
        plot,
        paper_bgcolor = paper,
        plot_bgcolor = paper,
        font = list(size = base_size * 0.7, color = ink),
        xaxis = xaxis,
        yaxis = list(
            title = "mNIRS",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = TRUE,
            linecolor = ink,
            color = ink
        ),
        shapes = shapes,
        showlegend = TRUE,
        legend = list(
            orientation = "h",
            x = 1,
            xanchor = "right",
            y = 0.95,
            yanchor = "bottom"
        ),
        hovermode = "closest",
        margin = list(t = 40, r = 20, b = 50, l = 60)
    ) |>
        plotly::config(
            displaylogo = FALSE,
            modeBarButtonsToRemove = c(
                "lasso2d",
                "select2d",
                "autoScale2d",
                "hoverCompareCartesian",
                "toggleSpikelines"
            )
        )
}

## seconds -> "YYYY-mm-dd HH:MM:SS[.f]" strings for the plotly date
## axis. strings are timezone-naive; numeric ms would be read as
## browser-local time. sprintf() over the split fields is ~10x faster
## than format.POSIXct(); the date part is formatted once per unique day
iso_time <- function(t, decimals) {
    d <- t %/% 86400
    s <- t %% 86400
    days <- unique(d)
    fmt <- sprintf(
        "%%s %%02d:%%02d:%%0%d.%df",
        2L + decimals + (decimals > 0L),
        decimals
    )
    out <- sprintf(
        fmt,
        format(.Date(days))[match(d, days)],
        s %/% 3600,
        (s %% 3600) %/% 60,
        s %% 60
    )
    out[is.na(t)] <- NA
    return(out)
}

## Dashed vertical line shapes for manual event markers; shared by
## the initial plot build and plotlyProxy relayout updates
event_shapes <- function(manual_events, ink, time_labels = FALSE) {
    if (!length(manual_events)) {
        return(list())
    }

    ## match the date x-axis when time is displayed as h:mm:ss.
    ## pre-format: plotlyProxy sends shapes through Shiny's JSON
    ## encoder, which truncates POSIXct to whole seconds
    events <- if (time_labels) {
        iso_time(as.numeric(manual_events), 3L)
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
    ## alphabetical legend order, independent of the nirs_channels
    ## attribute order set upstream
    nirs_ch <- sort(attr(data, "nirs_channels"))
    colours <- mnirs::palette_mnirs(length(nirs_ch))

    ## decimal places, not sig figs, which collapse adjacent samples to
    ## the same hover time past ~1000 s; capped at 2 to avoid
    ## floating-point noise from non-terminating sample intervals
    ## (e.g. 3 Hz)
    decimals <- min(mnirs:::count_decimals(data[[time_ch]]), 2L)

    ## When labelling axis as h:mm:ss, x is date-time strings on a date
    ## axis so plotly auto-recomputes ticks on zoom
    time_vec <- if (time_labels) {
        iso_time(as.numeric(data[[time_ch]]), decimals)
    } else {
        data[[time_ch]]
    }

    ## hover time formatted client-side from x
    time_fmt <- if (time_labels) {
        paste0("%{x|%-H:%M:%S", if (decimals > 0L) ".%L", "}")
    } else {
        sprintf("%%{x:.%df}", decimals)
    }

    ## one scattergl line trace per NIRS channel; raw (unfiltered)
    ## traces drawn first at low alpha with hover/legend suppressed, so
    ## plotlyProxy can toggle their visibility without a full rebuild
    add_channel_traces <- function(plot, src, raw) {
        return(Reduce(\(p, i) {
            plotly::add_trace(
                p,
                x = time_vec,
                y = src[[nirs_ch[[i]]]],
                type = "scattergl",
                mode = "lines",
                name = if (raw) paste0(nirs_ch[[i]], " (raw)") else nirs_ch[[i]],
                showlegend = !raw,
                hoverinfo = if (raw) "skip" else NULL,
                visible = if (raw) show_raw else TRUE,
                opacity = if (raw) 0.6 else 1,
                line = list(color = colours[[i]], width = if (raw) 1 else 1.5),
                hovertemplate = if (raw) NULL else paste0(
                    time_ch, ": ", time_fmt, "<br>",
                    "<b>", nirs_ch[[i]], ":</b> %{y:.2f}<extra></extra>"
                )
            )
        }, seq_along(nirs_ch), init = plot))
    }

    plot <- plotly::plot_ly()
    if (!is.null(raw_data)) {
        plot <- add_channel_traces(plot, raw_data, raw = TRUE)
    }
    plot <- add_channel_traces(plot, data, raw = FALSE)

    shapes <- event_shapes(manual_events, ink, time_labels)

    ## shared axis styling; x-axis gets optional h:mm:ss tick text.
    ## plotly_build() treats character x as categorical and fills
    ## categoryarray with every unique time string unless already set;
    ## the placeholder is ignored by plotly.js on a date axis
    axis_style <- list(
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = ink,
        color = ink
    )
    xaxis <- c(
        list(title = if (time_labels) paste(time_ch, "(h:mm:ss)") else time_ch),
        axis_style,
        if (time_labels) {
            list(type = "date", tickformat = "%-H:%M:%S", categoryarray = I(""))
        }
    )

    plot <- plotly::layout(
        plot,
        paper_bgcolor = paper,
        plot_bgcolor = paper,
        font = list(size = base_size * 0.7, color = ink),
        xaxis = xaxis,
        yaxis = c(list(title = "mNIRS"), axis_style),
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

    ## plotly_build() recycles scalar hover attributes to one copy per
    ## sample; collapse them so each trace carries a single string
    plot <- plotly::plotly_build(plot)
    plot$x$data <- lapply(plot$x$data, \(.tr) {
        .tr$hovertemplate <- .tr$hovertemplate[1L]
        .tr$hoverinfo <- .tr$hoverinfo[1L]
        return(.tr)
    })
    return(plot)
}

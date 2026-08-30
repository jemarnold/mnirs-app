## Parse comma-separated name=value pairs
split_named_vec <- function(x) {
    if (!nchar(x)) {
        return(NULL)
    }

    noquotes <- gsub('["\'`]', '', x)
    channels_vec <- trimws(strsplit(noquotes, ",")[[1L]])
    parts <- strsplit(channels_vec, "\\s*=\\s*")
    names <- vapply(parts, `[`, character(1L), 1L)
    vals <- vapply(
        parts,
        \(.x) {
            if (length(.x) > 1L) paste(.x[-1L], collapse = "=") else .x[1L]
        },
        character(1L)
    )

    return(setNames(vals, names))
}

## enable a download button only while its source reactive evaluates
## without error; req()/validate() failures grey it out so the
## downloadHandler is never hit with an uncatchable condition (HTTP 500).
## toggled after flush because the download output's own render re-enables
## the button client-side when its value arrives
toggle_download <- function(id, source, session = getDefaultReactiveDomain()) {
    observe({
        ok <- !inherits(tryCatch(source(), error = identity), "error")
        session$onFlushed(\() shinyjs::toggleState(id, condition = ok))
    })
}

## save ggplot as 220 mm wide PNG at 300 dpi; height and text size scaled
## from the on-screen pixel dims so export matches the rendered plotOutput.
## explicit white bg: thematic_shiny() doesn't apply inside downloadHandler
save_plot_png <- function(file, plot_fn, w_px, h_px, width_mm = 220) {
    width_in <- width_mm / 25.4
    ggsave(
        file,
        plot_fn(base_size = 18 * width_in * 96 / w_px),
        width = width_in,
        height = width_in * h_px / w_px,
        units = "in",
        dpi = 300,
        bg = "white"
    )
}

## Parse comma-separated numeric values
string_to_numeric <- function(x) {
    if (!nchar(x)) {
        return(NULL)
    }
    return(as.numeric(strsplit(x, split = "\\s*,\\s*")[[1L]]))
}

## Decimal places for displaying a time vector; capped to avoid
## floating-point noise from non-terminating sample intervals (e.g. 3 Hz)
time_digits <- function(x, max_digits = 2L) {
    return(min(mnirs:::count_decimals(x), max_digits))
}

## DT with numerics formatted client-side: integerish columns as-is,
## time column at fixed decimals, remaining numerics to sig figs.
## keeps columns numeric so sorting works; no R-side re-formatting
signif_datatable <- function(data, time_channel = NULL, digits = 4L, ...) {
    num_cols <- names(data)[vapply(data, is.numeric, logical(1L))]
    int_cols <- num_cols[vapply(
        data[num_cols],
        rlang::is_integerish,
        logical(1L)
    )]
    sig_cols <- setdiff(num_cols, c(int_cols, time_channel))

    dt <- datatable(data, rownames = FALSE, ...)
    ## time shown as decimal places, not sig figs: past ~1000 s
    ## sig figs collapse adjacent samples to the same value
    if (isTRUE(time_channel %in% setdiff(num_cols, int_cols))) {
        dt <- formatRound(
            dt,
            time_channel,
            digits = time_digits(data[[time_channel]])
        )
    }
    if (length(sig_cols)) {
        dt <- formatSignif(dt, sig_cols, digits = digits)
    }
    return(dt)
}

## nearest-sample event markers; labels `event_<time>` use the input
## time as entered, not the snapped sample time. overwrites existing
## labels. integer lap channels must stay integer for
## extract_intervals(), so labels go to a character `event_labels`
## column placed after the event channel
add_events <- function(data, times) {
    if (!length(times)) {
        return(data)
    }
    time_channel <- attr(data, "time_channel")
    event_channel <- blank_to_null(unname(attr(data, "event_channel"))) %||% "event"
    time_vec <- data[[time_channel]]

    idx <- vapply(times, \(.t) which.min(abs(time_vec - .t)), integer(1L))
    labels <- paste0(
        "event_",
        vapply(times, format, character(1L), scientific = FALSE)
    )

    if (!event_channel %in% names(data)) {
        data[[event_channel]] <- NA_character_
        attr(data, "event_channel") <- event_channel
    }
    label_channel <- if (is.character(data[[event_channel]])) {
        event_channel
    } else {
        "event_labels"
    }
    if (!label_channel %in% names(data)) {
        data[[label_channel]] <- NA_character_
        ## in-place column reorder keeps mnirs attributes (`data[ord]` drops them)
        ord <- append(
            setdiff(names(data), label_channel),
            label_channel,
            after = match(event_channel, names(data))
        )
        data[] <- data[ord]
        names(data) <- ord
    }
    data[[label_channel]][idx] <- labels
    return(data)
}

## Clean CLI error messages
clean_cli_message <- function(e) {
    msg <- cli::ansi_strip(conditionMessage(e))
    msg <- gsub("`|\\{\\.[^}]+\\}", "", msg)
    msg <- gsub("\\s+", " ", msg)
    return(trimws(msg))
}

## Convert blank/empty inputs to NULL
blank_to_null <- function(x) {
    if (is.null(x) || length(x) == 0L || all(is.na(x))) {
        return(NULL)
    }
    if (is.character(x) && all(nchar(x) == 0L)) {
        return(NULL)
    }
    return(x)
}

## event_channel input string from detected metadata. artinis attr
## holds renamed "event", not legend id, so re-read needs "event = <id>"
event_channel_string <- function(path, md) {
    if (is.null(md$event_channel)) {
        return("")
    }
    if (!identical(md$nirs_device, "Artinis")) {
        return(paste(md$event_channel, collapse = ", "))
    }
    raw <- mnirs:::read_file(path)
    legend <- mnirs:::parse_oxysoft_legend(
        raw,
        mnirs:::detect_mnirs_device(raw)$header_row
    )
    return(paste(
        names(legend$event),
        legend$event,
        sep = " = ",
        collapse = ", "
    ))
}

## plot ink/paper colours for current bslib colour mode
mode_colours <- function(mode) {
    if (identical(mode, "dark")) {
        return(list(ink = "#fff", paper = "#212529"))
    }
    return(list(ink = "#373a3c", paper = "#fff"))
}

## Conditional data transformation
apply_if <- function(data, condition, fn, ...) {
    if (condition) return(fn(data, ...)) else return(data)
}

## Trim rows from the head of a time series
trim_head <- function(data, time_channel, trim) {
    data[data[[time_channel]] > trim, ]
}

## Trim rows from the tail of a time series
trim_tail <- function(data, time_channel, trim) {
    cutoff <- max(data[[time_channel]], na.rm = TRUE) - trim
    data[data[[time_channel]] < cutoff, ]
}

## single-method extract_intervals boundary spec from comma-separated text
parse_boundary <- function(method, x, fixed = FALSE) {
    x <- blank_to_null(trimws(x %||% ""))
    if (is.null(x)) {
        return(NULL)
    }

    out <- switch(
        method,
        time = mnirs::by_time(string_to_numeric(x)),
        sample = mnirs::by_sample(string_to_numeric(x)),
        lap = mnirs::by_lap(string_to_numeric(x)),
        label = {
            tokens <- trimws(strsplit(x, ",")[[1L]])
            ## numeric labels regex-match every sample in matching laps/events
            if (any(!is.na(suppressWarnings(as.numeric(tokens))))) {
                stop(
                    "Numeric values are not valid Labels. ",
                    "Use By Lap, By Time, or By Sample instead."
                )
            }
            mnirs::by_label(tokens, fixed = fixed)
        }
    )
    return(out)
}

## Resolve mixed-method boundary specs to sorted times via a single
## span-0 extraction (extract_intervals accepts a list of by_* specs)
resolve_boundary_times <- function(data, specs, boundary = c("start", "end")) {
    boundary <- match.arg(boundary)
    specs <- Filter(Negate(is.null), specs)
    if (length(specs) == 0L) {
        return(NULL)
    }

    resolve_one <- function(specs, event_channel = NULL) {
        if (!length(specs)) {
            return(numeric(0L))
        }
        args <- list(
            data,
            span = 0,
            group_intervals = "distinct",
            verbose = FALSE,
            event_channel = event_channel
        )
        args[[boundary]] <- specs
        df_list <- do.call(mnirs::extract_intervals, args)
        vapply(df_list, \(.df) attr(.df, "interval_times")[[1L]], numeric(1L))
    }

    ## label specs match manual labels in `event_labels` when the event
    ## channel holds integer lap numbers; lap/time/sample specs stay on it
    ev <- blank_to_null(unname(attr(data, "event_channel")))
    is_label <- vapply(specs, `[[`, character(1L), "type") == "label"
    redirect <- any(is_label) && !is.null(ev) && !is.character(data[[ev]])
    times <- if (redirect) {
        c(resolve_one(specs[!is_label]), resolve_one(specs[is_label], "event_labels"))
    } else {
        resolve_one(specs)
    }
    ## over-matching patterns (e.g. ".")
    if (length(times) > 100) {
        stop(
            "Boundary spec matched ",
            length(times),
            " points. ",
            "Refine the Label pattern."
        )
    }
    return(sort(times))
}

## Safe wrapper for filter_mnirs with error handling
try_filter <- function(data, nirs_channels, time_channel, ...) {
    tryCatch(
        mnirs::filter_mnirs(
            data,
            nirs_channels = nirs_channels,
            time_channel = time_channel,
            ...
        ),
        error = \(e) {
            validate(need(FALSE, clean_cli_message(e)))
        }
    )
}

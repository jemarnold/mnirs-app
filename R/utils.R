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

    args <- list(data, span = 0, group_intervals = "distinct", verbose = FALSE)
    args[[boundary]] <- specs
    df_list <- do.call(mnirs::extract_intervals, args)
    times <- vapply(
        df_list,
        \(.df) attr(.df, "interval_times")[[1L]],
        numeric(1L)
    )
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

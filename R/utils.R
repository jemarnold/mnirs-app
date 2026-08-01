## Parse comma-separated name=value pairs
split_named_vec <- function(x) {
    if (!nchar(x)) {
        return(NULL)
    }

    noquotes <- gsub('["\'`]', '', x)
    channels_vec <- trimws(strsplit(noquotes, ",")[[1L]])
    parts <- strsplit(channels_vec, "\\s*=\\s*")
    names <- vapply(parts, `[`, character(1L), 1L)
    vals <- vapply(parts, \(.x) {
        if (length(.x) > 1L) paste(.x[-1L], collapse = "=") else .x[1L]
    }, character(1L))

    return(setNames(vals, names))
}

## Parse comma-separated numeric values
string_to_numeric <- function(x) {
    if (!nchar(x)) {
        return(NULL)
    }
    return(as.numeric(strsplit(x, split = "\\s*,\\s*")[[1L]]))
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

#' @import dplyr
#' @import tidyr
#' @export
filter <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::filter, .funname = "filter", ...)
}

#' @export
filter_all <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::filter_all, .funname = "filter_all", ...)
}

#' @export
filter_if <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::filter_if, .funname = "filter_if", ...)
}

#' @export
filter_at <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::filter_at, .funname = "filter_at", ...)
}

#' @export
distinct <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::distinct, .funname = "distinct", ...)
}

#' @export
distinct_all <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::distinct_all, .funname = "distinct_all", ...)
}

#' @export
distinct_if <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::distinct_if, .funname = "distinct_if", ...)
}

#' @export
distinct_at <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::distinct_at, .funname = "distinct_at", ...)
}

#' @export
top_n <- function(x, ...) {
    log_filter(x, .fun = dplyr::top_n, .funname = "top_n", ...)
}

#' @export
top_frac <- function(x, ...) {
    log_filter(x, .fun = dplyr::top_frac, .funname = "top_frac", ...)
}

#' @export
sample_n <- function(tbl, ...) {
    log_filter(tbl, .fun = dplyr::sample_n, .funname = "sample_n", ...)
}

#' @export
sample_frac <- function(tbl, ...) {
    log_filter(tbl, .fun = dplyr::sample_frac, .funname = "sample_frac", ...)
}

#' @export
slice <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice, .funname = "slice", ...)
}

#' @export
slice_head <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_head, .funname = "slice_head", ...)
}

#' @export
slice_tail <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_tail, .funname = "slice_tail", ...)
}

#' @export
slice_min <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_min, .funname = "slice_min", ...)
}

#' @export
slice_max <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_max, .funname = "slice_max", ...)
}

#' @export
slice_sample <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_sample, .funname = "slice_sample", ...)
}

#' @export
drop_na <- function(data, ...) {
    log_filter(data, .fun = tidyr::drop_na, .funname = "drop_na", ...)
}

log_filter <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    # set up some repetitive strings
    fun_name <- code_wrap(.funname)
    data_change_summary <- get_shape_summary(fun_name, .data, newdata)

    # add group or rowwise status
    original <- function_prefix(fun_name, newdata)
    group_cols <- dplyr::group_vars(newdata)

    callout_words <- lapply(group_cols, function(x) list(word = x, change = "internal-change"))

    n <- nrow(.data) - nrow(newdata)
    if (n == 0) {
        display(glue::glue(data_change_summary, "{original} did not remove any rows.", .sep = " "),
                callout_words = callout_words
        )
    } else if (n == nrow(.data)) {
        display(glue::glue(data_change_summary, "{original} removed all rows (100%).", .sep = " "),
                callout_words = callout_words
        )
    } else {
        total <- nrow(.data)
        display(glue::glue(
                data_change_summary,
                "{original}",
                "removed {plural(n, 'row')}",
                "({percent(n, {total})}), with {plural(nrow(newdata), 'row')} remaining.",
                .sep = " "),
                callout_words = callout_words
        )
    }
    newdata
}

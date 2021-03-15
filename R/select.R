#' @export
select <- function(.data, ...) {
    log_select(.data, .fun = dplyr::select, .funname = "select", ...)
}

#' @export
select_all <- function(.tbl, ...) {
    log_select(.tbl, .fun = dplyr::select_all, .funname = "select_all", ...)
}

#' @export
select_if <- function(.tbl, ...) {
    log_select(.tbl, .fun = dplyr::select_if, .funname = "select_if", ...)
}

#' @export
select_at <- function(.tbl, ...) {
    log_select(.tbl, .fun = dplyr::select_at, .funname = "select_at", ...)
}

#' @export
relocate <- function(.data, ...) {
    log_select(.data, .fun = dplyr::relocate, .funname = "relocate", ...)
}

log_select <- function(.data, .fun, .funname, ...) {
    cols <- names(.data)
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    newcols <- names(newdata)
    dropped_vars <- setdiff(cols, newcols)
    renamed_vars <- setdiff(newcols, cols)
    # this captures all of the arguments as unevaluated expressions which is used
    # to infer parameter info
    args <- rlang::enquos(...)
    # filter out the formal parameters
    args <- get_column_vars(args, .fun)

    # set up some repetitive strings
    fun_name <- code_wrap(.funname)
    data_change_summary <- get_shape_summary(fun_name, .data, newdata)

    # add group or rowwise status
    original <- function_prefix(fun_name, newdata)
    group_cols <- dplyr::group_vars(newdata)
    callout_words <- lapply(group_cols, function(x) list(word = x, change = "internal-change"))

    if (ncol(newdata) == 0) {
        display(glue::glue("{data_change_summary}", "{original} dropped all variables.", .sep = " "),
                callout_words = callout_words)
    } else if (length(renamed_vars) > 0 & length(renamed_vars) == length(dropped_vars)) {
        # renamed only
        # get the "old" to "new" strings
        all_renamed_pairs <- get_column_change_pairs(args, renamed_vars)
        display(glue::glue("{data_change_summary}", "{original} renamed {plural(length(renamed_vars), 'variable')}",
                           "({format_list(all_renamed_pairs, .code_wrap = FALSE)})", .sep = " "),
                callout_words = append(
                    lapply(renamed_vars, function(x) list(word = x, change = "visible-change")),
                    callout_words
                )
        )
    } else if (length(dropped_vars) > 0 & length(renamed_vars) > 0) {
        # dropped & renamed
        # get the "old" to "new" strings
        all_renamed_pairs <- get_column_change_pairs(args, renamed_vars)
        n_dropped <- length(dropped_vars) - length(renamed_vars)
        display(glue::glue("{data_change_summary}",
                           "{original} renamed {plural(length(renamed_vars), 'variable')}",
                           "({format_list(all_renamed_pairs, .code_wrap = FALSE)})",
                           "and dropped {plural(n_dropped, 'variable')}.", .sep = " "),
                callout_words = append(
                    lapply(renamed_vars, function(x) list(word = x, change = "visible-change")),
                    callout_words
                )
            )
    } else if (length(dropped_vars) > 0) {
        # dropped only
        display(glue::glue("{data_change_summary}", "{original} dropped {plural(length(dropped_vars), 'variable')}",
                           "({format_list(dropped_vars)}).", .sep = " "),
                callout_words = callout_words
        )
    } else {
        # no dropped, no removed
        if (all(names(newdata) == cols)) {
            display(glue::glue("{data_change_summary}", "{original} resulted in no changes.", .sep = " "),
                    callout_words = callout_words
            )
        } else {
            display(glue::glue("{data_change_summary}", "{original} reordered columns",
                               "({format_list(names(newdata))}).", .sep = " "),
                    callout_words = callout_words
            )
        }
    }

    newdata
}

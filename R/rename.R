#' @export
rename <- function(.data, ...) {
    log_rename(.data, .fun = dplyr::rename, .funname = "rename", ...)
}

#' @export
rename_all <- function(.tbl, ...) {
    log_rename(.tbl, .fun = dplyr::rename_all, .funname = "rename_all", ...)
}

#' @export
rename_if <- function(.tbl, ...) {
    log_rename(.tbl, .fun = dplyr::rename_if, .funname = "rename_if", ...)
}

#' @export
rename_at <- function(.tbl, ...) {
    log_rename(.tbl, .fun = dplyr::rename_at, .funname = "rename_at", ...)
}

#' @export
rename_with <- function(.data, ...) {
    log_rename(.data, .fun = dplyr::rename_with, .funname = "rename_with", ...)
}

log_rename <- function(.data, .fun, .funname, ...) {
    cols <- names(.data)
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }
    # this captures all of the arguments as unevaluated expressions which is used
    # to infer parameter info
    args <- rlang::enquos(...)
    # filter out the formal parameters
    args <- get_column_vars(args, .fun)
    # set up some repetitive strings
    fun_name <- code_wrap(.funname)

    renamed_vars <- setdiff(names(newdata), cols)
    n <- length(renamed_vars)
    if (n > 0) {
        # get the "old" to "new" strings
        all_renamed_pairs <- get_column_change_pairs(args, renamed_vars)
        display(glue::glue(
            "{fun_name} does not change the data shape.",
            "{fun_name} renamed {plural(n, 'variable')}",
            "({format_list(all_renamed_pairs, .code_wrap = FALSE)})", .sep = " "),
            callout_words = lapply(renamed_vars, function(x) list(word = x, change = "visible-change"))
        )
    }
    newdata
}

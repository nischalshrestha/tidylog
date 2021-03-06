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

    dropped_vars <- setdiff(cols, names(newdata))
    renamed_vars <- setdiff(names(newdata), cols)

    # TODO:
    # - data change [row x column]
    if (ncol(newdata) == 0) {
        display(glue::glue("<code class='code'>{.funname}</code> dropped all variables."))
    } else if (length(renamed_vars) > 0 & length(renamed_vars) == length(dropped_vars)) {
        # renamed only
        # TODO: want to know the old variable name so we can say renamed "old" to "new"
        display(glue::glue("<code class='code'>{.funname}</code> renamed {plural(length(renamed_vars), 'variable')}",
                           " ({format_list(renamed_vars)})."))
    } else if (length(dropped_vars) > 0 & length(renamed_vars) > 0) {
        # dropped & renamed
        n_dropped <- length(dropped_vars) - length(renamed_vars)
        display(glue::glue("<code class='code'>{.funname}</code> ",
                           "renamed {plural(length(renamed_vars), 'variable')}",
                           " ({format_list(renamed_vars)})",
                           " and dropped {plural(n_dropped, 'variable')}."))
    } else if (length(dropped_vars) > 0) {
        # dropped only
        display(glue::glue("<code class='code'>{.funname}</code> dropped {plural(length(dropped_vars), 'variable')}",
                           " ({format_list(dropped_vars)})."))
    } else {
        # no dropped, no removed
        if (all(names(newdata) == cols)) {
            display(glue::glue("<code class='code'>{.funname}</code> resulted in no changes."))
        } else {
            display(glue::glue("<code class='code'>{.funname}</code> reordered columns ",
                               " ({format_list(names(newdata))})."))
        }
    }

    newdata
}

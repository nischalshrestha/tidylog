#' @export
pivot_longer <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::pivot_longer, .funname = "pivot_longer", ...)
}

#' @export
pivot_wider <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::pivot_wider, .funname = "pivot_wider", ...)
}

#' @export
gather <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::gather, .funname = "gather", ...)
}


#' @export
spread <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::spread, .funname = "spread", ...)
}

log_longer_wider <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)

    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    newcols <- setdiff(names(newdata), names(.data))
    oldcols <- setdiff(names(.data), names(newdata))

    # set up some repetitive strings
    verb <- ""
    if (.funname %in% c("spread", "pivot_wider")) {
        verb <- "widened"
    } else if (.funname %in% c("gather", "pivot_longer")) {
        verb <- "lengthened"
    }
    # set up some repetitive strings
    fun_name <- code_wrap(.funname)
    data_change_summary <- get_shape_summary(fun_name, .data, newdata)

    display(glue::glue(
        data_change_summary,
        "{fun_name} {verb} the data by reorganizing ({format_list(oldcols)})",
        "into ({format_list(code_wrap(newcols, .code_class = 'visible-change'), .code_wrap = F)}).",
        .sep = " "
    ))

    newdata
}

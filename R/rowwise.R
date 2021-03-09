#' @export
rowwise <- function(.data, ...) {
    log_rowwise(.data, .fun = dplyr::rowwise, .funname = "rowwise", ...)
}

log_rowwise <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    # set up some repetitive strings
    fun_name <- code_wrap(.funname)
    data_change_summary <- glue::glue("{fun_name} has no visible effect on the data.")

    display(glue::glue(
        "{data_change_summary}",
        "{fun_name} implictly grouped the data by row.",
        "<hr>",
        "<div>",
        "<i class='far fa-lightbulb'></i> Like {code_wrap('group_by()')}, {code_wrap('rowwise()')}",
        "doesn't really do anything itself; it just changes how the other verbs work. {code_wrap('rowwise')}",
        "allows you to compute on the dataframe a row-at-a-time, instead of on entire columns.",
        "</div>",
        .sep = " "))

    newdata
}


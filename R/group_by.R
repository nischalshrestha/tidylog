#' @export
group_by <- function(.data, ...) {
    log_group_by(.data, .fun = dplyr::group_by, .funname = "group_by", ...)
}

#' @export
group_by_all <- function(.tbl, ...) {
    log_group_by(.tbl, .fun = dplyr::group_by_all, .funname = "group_by_all", ...)
}

#' @export
group_by_if <- function(.tbl, ...) {
    log_group_by(.tbl, .fun = dplyr::group_by_if, .funname = "group_by_if", ...)
}

#' @export
group_by_at <- function(.tbl, ...) {
    log_group_by(.tbl, .fun = dplyr::group_by_at, .funname = "group_by_at", ...)
}

#' @export
ungroup <- function(x, ...) {
    log_group_by(x, .fun = dplyr::ungroup, .funname = "ungroup", ...)
}

log_group_by <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    # set up some repetitive strings
    fun_name <- code_wrap(.funname)
    data_change_summary <- glue::glue("{fun_name} has no visible effect on the data.")

    # tailor message depending on group status
    group_vars <- get_groups(newdata)
    is_ungroup <- identical(.funname, "ungroup")
    if (is_ungroup) {
        data_change_summary <- glue::glue(
            data_change_summary,
            "{fun_name} instead ungrouped the previous data frame.",
            .sep = " "
        )
    }

    if (is.null(group_vars) && !is_ungroup) {
        display(glue::glue("{data_change_summary}", "{fun_name} did not group any variables", .sep = " "))
    } else if (!is_ungroup) {
        display(glue::glue(
            "{data_change_summary}",
            "{fun_name} has internally grouped {plural(length(group_vars), 'variable')}",
            "({format_list(group_vars, .code_class ='internal-change')})",
            "<hr>",
            "<div>",
            "<i class='far fa-lightbulb'></i> {code_wrap('group_by()')} doesn't really do anything itself; it just changes how the other verbs work.",
            "It now allows us to execute functions like {code_wrap('summarise')} to perform statistics by groups ({format_list(group_vars, .code_class ='internal-change')}).",
            "</div>",
            .sep = " "),
            callout_words = lapply(group_vars, function(x) list(word = x, change = "internal-change"))
        )
    } else {
        display(data_change_summary)
    }

    newdata
}

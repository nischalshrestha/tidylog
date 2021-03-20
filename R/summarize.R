#' @export
summarize <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarize, .funname = "summarize", ...)
}

#' @export
summarize_all <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarize_all, .funname = "summarize_all", ...)
}

#' @export
summarize_at <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarize_at, .funname = "summarize_at", ...)
}

#' @export
summarize_if <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarize_if, .funname = "summarize_if", ...)
}

#' @export
summarise <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarise, .funname = "summarise", ...)
}

#' @export
summarise_all <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarise_all, .funname = "summarise_all", ...)
}

#' @export
summarise_at <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarise_at, .funname = "summarise_at", ...)
}

#' @export
summarise_if <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarise_if, .funname = "summarise_if", ...)
}

#' @export
tally <- function(x, ...) {
    log_summarize(x, .fun = dplyr::tally, .funname = "tally", ...)
}

#' @export
count <- function(x, ...) {
    log_summarize(x, .fun = dplyr::count, .funname = "count", ...)
}

#' @export
uncount <- function(data, ...) {
    log_summarize(data, .fun = tidyr::uncount, .funname = "uncount", ...)
}

log_summarize <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    cols <- names(.data)
    newcols <- names(newdata)
    renamed_vars <- setdiff(newcols, cols)
    # this captures all of the arguments as unevaluated expressions which is used
    # to infer parameter info
    args <- rlang::enquos(...)
    # filter out the formal parameters
    new_vars <- names(args)
    new_vars <- new_vars[!new_vars %in% names(formals(dplyr::summarise))]
    args <- args[new_vars]
    new_vars_values <- as.character(lapply(args, function(x) rlang::quo_squash(x)))
    var_to_values_pairs <- paste0(code_wrap(new_vars, .code_class = "visible-change"), " via ", code_wrap(new_vars_values))

    # set up some repetitive strings
    fun_name <- code_wrap(.funname)
    data_change_summary <- get_shape_summary(fun_name, .data, newdata)

    # add group or rowwise status
    group_vars <- get_groups(newdata)
    group_length <- length(group_vars)
    original <- function_prefix(fun_name, .data, apply_class = FALSE)
    # summarise the new variables + aggregate expression used for each.
    new_vars_summary <- ""
    callout_words <- list()
    if (!is.na(new_vars) && nzchar(new_vars)) {
        new_vars_summary <- glue::glue(
            "{original} created {plural(length(new_vars), 'variable')}",
            "({format_list(var_to_values_pairs, .code_wrap = F)}).",
            .sep = " "
        )
        callout_words <- lapply(new_vars, function(x) list(word = x, change = "visible-change"))
    }

    if (group_length > 0) {
        pre <- "is"
        if (group_length > 1) {
           pre <- "are"
        }
        # when we do have remaining group variables, let's call those out
        callout_words <- append(callout_words, lapply(group_vars, function(x) list(word = x, change = "internal-change")))
        display(glue::glue(
            "{data_change_summary}",
            "{new_vars_summary}",
            "There {pre} {plural(group_length, 'group variable')} remaining",
            "({format_list(group_vars, .code_class = 'internal-change')}).",
            "<hr>",
            # example of an extra note which could be customized via hook function
            "<div><i class='far fa-lightbulb'></i> Keep in mind, the data is internally grouped according to {format_list(group_vars, .code_class='internal-change')}.</div>",
            .sep = " "),
            callout_words = callout_words
        )
    } else {
        display(glue::glue(
            "{data_change_summary}",
            "{new_vars_summary}",
            "The data is now ungrouped.", .sep = " "),
            callout_words = callout_words
        )
    }

    newdata
}

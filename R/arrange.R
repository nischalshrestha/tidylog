#' @export
arrange <- function(.data, ...) {
    log_arrange(.data, .fun = dplyr::arrange, .funname = "arrange", ...)
}

log_arrange <- function(.data, .fun, .funname, ...) {
    cols <- names(.data)
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    # this captures all of the arguments as unevaluated expressions which is used
    # to infer parameter info
    args <- rlang::enquos(...)
    # filter out the formal parameters
    # for arrange, if an argument had a name, it's a formal
    # so we only filter for the args without a name
    args <- args[names(args) == ""]

    # squash all of the args
    args_squashed <- lapply(args, function(x) {
        return(rlang::quo_squash(x))
    })
    # get the column variables arrange is sorting by
    col_variables <- unlist(lapply(args_squashed, function(x) {
        if (rlang::is_call(x)) {
            return(rlang::call_args(x))

        }
        return(x)
    }))

    # get the column by [sorting] order text for each argument
    col_sort_order_summary <- unlist(lapply(args_squashed, function(x) {
        if (rlang::is_call(x)) {
            variable <- rlang::call_args(x)
            sorting <- rlang::call_name(x)
            if (identical(sorting, "desc")) {
                return(paste(code_wrap(variable, .code_class = "visible-change"), "by descending order"))
            }
            return(paste(code_wrap(variable, .code_class = "visible-change"), "by some other sorting"))
        }
        return(paste(code_wrap(x, .code_class = "visible-change"), "by ascending order"))
    }))

    # set up some repetitive strings
    fun_name <- code_wrap(.funname)
    data_change_summary <- glue::glue("{fun_name} does not change the data shape.")
    callout_words <- lapply(col_variables, function(x) list(word = as.character(x), change = "visible-change"))
    # GOTCHA: not sure of exact reason but we're working on a list `col_variables`, so we have to nullify the names
    # so that if we did a shiny:::toJSON (e.g. when communicating with JS) we won't get an output like
    # {"1":{"word":"color","change":"visible-change"}}
    # the names after the lapply is apparently "" :(
    names(callout_words) <- NULL
    display(glue::glue(data_change_summary,
        "{fun_name} sorted the data by {plural(length(col_variables), 'variable')}",
        "({format_list(col_sort_order_summary, .code_wrap = F)}).", .sep = " "
        ),
        # Note: for arrange, we have to turn the column variables back to string when constructing callout words
        callout_words = callout_words
    )

    newdata
}


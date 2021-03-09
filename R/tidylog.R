# helper function to wrap an element `x` with <span class='number [.code_class]'>x</span>
span_wrap <- function(x, .span_class = "") {
    span_class <- glue::glue("number", "{.span_class}", .sep = " ")
    if (!nzchar(.span_class)) {
        span_class <- "number"
    }
    return(paste0(glue::glue("<span class='{span_class}'>"), x, "</span>"))
}

plural <- function(n_items, noun, mid = "", .span_class = "") {
    if (n_items == 1) {
        return(paste0("one ", mid, noun))
    } else {
        return(paste0(
            span_wrap(format(n_items, big.mark = ",", scientific = FALSE), .span_class),
            " ", mid, noun, "s"
        ))
    }
}

shorten <- function(str) {
    if (nchar(str) > 25) {
        paste0(substr(str, 1, 23), "..")
    } else {
        str
    }
}

percent <- function(n, total, .span_class = "") {
    p <- round(n / total * 100)
    rtn <- ""
    if (n == total) {
        rtn <- "100%"
    } else if (p == 100) {
        rtn <- ">99%"
    } else if (n == 0) {
        rtn <- "0%"
    } else if (p == 0) {
        rtn <- "<1%"
    } else {
        rtn <- paste0(p, "%")
    }
    return(span_wrap(rtn, .span_class))
}

# helper function to wrap an elements `items` with <code class='code [.code_class]'>items</code>
code_wrap <- function(items, .code_class = "") {
    code_class <- glue::glue("code", "{.code_class}", .sep = " ")
    if (!nzchar(.code_class)) {
        code_class <- "code"
    }
    return(paste0(glue::glue("<code class='{code_class}'>"), items, "</code>"))
}

#' @import clisymbols
format_list <- function(items, .code_wrap = TRUE, .code_class = "") {
    if (.code_wrap) {
        items <- code_wrap(items, .code_class)
    }
    if (length(items) <= 5) {
        return(paste0(items, collapse = ", "))
    } else {
        return(paste0(c(items[1:5], clisymbols::symbol$ellipsis), collapse = ", "))
    }
}

get_type <- function(v) {
    if (is.ordered(v)) {
        "ordered factor"
    } else if (is.factor(v)) {
        "factor"
    } else if (inherits(v, "Date")) {
        "Date"
    } else if (inherits(v, "units")) {
        "units"
    } else {
        typeof(v)
    }
}

get_groups <- function(.data) {
    if (!is.null(attr(.data, "groups"))) {
        # support for dplyr >= 0.8
        groups <- attr(.data, "groups")
        return(utils::head(names(groups), -1))
    } else {
        # support for dplyr < 0.8
        return(attr(.data, "vars"))
    }
}

#' @import rlang
display <- function(text) {
    functions <- getOption("tidylog.display")
    if (is.null(functions)) {
        rlang::inform(text)
    } else if (is.list(functions)) {
        for (f in functions) {
            if (is.function(f)) {
                f(text)
            } else {
                warning("tidylog.display needs to be set to either NULL or a list of functions")
            }
        }
    } else {
        warning("tidylog.display needs to be set to either NULL or a list of functions")
    }
}

should_display <- function() {
    is.null(getOption("tidylog.display")) | length(getOption("tidylog.display")) > 0
}

# helper function that returns a string describing how a function changed the data
# empty string if no change, otherwise the summary statement of dimensions changed
get_shape_summary <- function(fun_name, .data, newdata) {
    data_shape_change <- ""
    # find differences in shape
    row_diff <- nrow(.data) - nrow(newdata)
    col_diff <- ncol(.data) - ncol(newdata)
    # if there were any shape differences, state how the shape changed
    if (row_diff != 0 || col_diff != 0) {
        # for now, we'll just highlight the whole [row x col] for new data
        # TODO but, we could more intelligently highlight either row or col
        data_shape_change <- glue::glue(
            "{fun_name} changed the dataframe shape from",
            "<span class = 'number'>[{nrow(.data)} x {ncol(.data)}]</span> to",
            "<span class = 'visible-change number'>[{nrow(newdata)} x {ncol(newdata)}]</span>.",
            .sep = " "
        )
    }
    return(data_shape_change)
}

# helper function used in verbs like `select`/`rename` to return a comma separated character vector
# of "old" to "new" column name pairs
get_column_change_pairs <- function(args, renamed_vars) {
    renamed_from <- lapply(args[renamed_vars], function(x) rlang::as_name(x))
    formatted_names <- paste0("<code class = 'code visible-change'>", names(renamed_from), "</code>")
    formatted_vals <- paste0("<code class = 'code'>", renamed_from, "</code>")
    return(paste0(formatted_vals, " to ", formatted_names))
}

#' outputs some information about the data frame/tbl
#'
#' @param .data a tbl/data frame
#' @return same as .data
#' @examples
#' tidylog(mtcars)
#' #> tidylog: data.frame with 32 rows and 11 columns
#' @export
tidylog <- function(.data) {
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(.data)
    }

    if ("grouped_df" %in% class(.data)) {
        type <- glue::glue("grouped tibble")
    } else if ("tbl" %in% class(.data)) {
        type <- "tibble"
    } else if ("data.table" %in% class(.data)) {
        type <- "data.table"
    } else {
        type <- "data.frame"
    }

    display(glue::glue("tidylog: {type} with {plural(nrow(.data), 'row')} and ",
        "{plural(ncol(.data), 'column')}"))
    .data
}

#' A functional version of `tidylog`, where the display string is returned instead of displayed
#'
#' @param .data a tbl/data frame
#'
#' @return character
#' @export
#'
#' @examples
#' get_data_summary(mtcars)
get_data_summary <- function(.data) {
    if (!"data.frame" %in% class(.data)) {
        return(.data)
    }

    if ("grouped_df" %in% class(.data)) {
        type <- glue::glue("grouped tibble")
    } else if ("tbl" %in% class(.data)) {
        type <- "tibble"
    } else if ("data.table" %in% class(.data)) {
        type <- "data.table"
    } else {
        type <- "data.frame"
    }

    return(glue::glue("{type} with {plural(nrow(.data), 'row')} and {plural(ncol(.data), 'column')}"))
}

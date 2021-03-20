#' @export
mutate <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate, .funname = "mutate", ...)
}

#' @export
mutate_all <- function(.tbl, ...) {
    log_mutate(.tbl, .fun = dplyr::mutate_all, .funname = "mutate_all", ...)
}

#' @export
mutate_if <- function(.tbl, ...) {
    log_mutate(.tbl, .fun = dplyr::mutate_if, .funname = "mutate_if", ...)
}

#' @export
mutate_at <- function(.tbl, ...) {
    log_mutate(.tbl, .fun = dplyr::mutate_at, .funname = "mutate_at", ...)
}

#' @export
transmute <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute, .funname = "transmute", ...)
}

#' @export
transmute_all <- function(.tbl, ...) {
    log_mutate(.tbl, .fun = dplyr::transmute_all, .funname = "transmute_all", ...)
}

#' @export
transmute_if <- function(.tbl, ...) {
    log_mutate(.tbl, .fun = dplyr::transmute_if, .funname = "transmute_if", ...)
}

#' @export
transmute_at <- function(.tbl, ...) {
    log_mutate(.tbl, .fun = dplyr::transmute_at, .funname = "transmute_at", ...)
}

#' @export
add_tally <- function(x, ...) {
    log_mutate(x, .fun = dplyr::add_tally, .funname = "add_tally", ...)
}

#' @export
add_count <- function(x, ...) {
    log_mutate(x, .fun = dplyr::add_count, .funname = "add_count", ...)
}

#' @export
replace_na <- function(data, ...) {
    log_mutate(data, .fun = tidyr::replace_na, .funname = "replace_na", ...)
}

#' @export
fill <- function(data, ...) {
    log_mutate(data, .fun = tidyr::fill, .funname = "fill", ...)
}

log_mutate <- function(.data, .fun, .funname, ...) {
    cols <- names(.data)
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

    # mention dropped variables too
    dropped_vars <- setdiff(names(.data), names(newdata))
    n <- length(dropped_vars)
    dropped_summary <- NULL
    if (ncol(newdata) == 0) {
        display(glue::glue(original, "dropped all variables"))
        return(newdata)
    } else if (length(dropped_vars) > 0) {
        dropped_summary <- glue::glue("dropped {plural(n, 'variable')} ({format_list(dropped_vars)})")
    }
    prefix <- paste(data_change_summary, original)
    summaries <- c(dropped_summary)

    # a list of callouts for code text
    callout_words <- list()

    # construct summaries for each variable in the newdata
    # TODO I bet this could be simplified if we did things via rlang::enquos instead?
    has_changed <- FALSE
    for (var in names(newdata)) {
        # new var
        if (!var %in% cols) {
            has_changed <- TRUE
            n <- length(unique(newdata[[var]]))
            p_na <- percent(sum(is.na(newdata[[var]])), length(newdata[[var]]))
            summaries <- c(summaries, glue::glue("added new variable {code_wrap(var, .code_class = 'visible-change')} ({get_type(newdata[[var]])}) ",
                                        "with {plural(n, 'value', 'unique ')} and {p_na} {span_wrap('NA')}s"))
        } else {
            # existing var
            # use identical to account for missing values - this is fast
            if (identical(newdata[[var]], .data[[var]])) {
                next
            }
            has_changed <- TRUE
            old <- .data[[var]]
            new <- newdata[[var]]
            typeold <- get_type(old)
            typenew <- get_type(new)

            if (typeold %in% c("factor", "ordered factor") &
                typenew %in% c("factor", "ordered factor")) {
                # when factor, compare based on character values
                # this will include both changes in the factor levels and recodes
                old <- as.character(old)
                new <- as.character(new)
            }

            if (typeold == typenew) {
                # same type
                if (typeold == "list") {
                    different <- sapply(seq_len(length(new)),
                                        function(i) !identical(new[[i]], old[[i]]))
                } else {
                    different <- new != old
                    different[is.na(new) & !is.na(old)] <- TRUE
                    different[!is.na(new) & is.na(old)] <- TRUE
                    different[is.na(new) & is.na(old)] <- FALSE
                }
                n <- sum(different)
                p <- percent(n, length(different))
                old_na <- sum(is.na(old))
                new_na <- sum(is.na(new)) - sum(is.na(old))
                old_na_text <- plural(abs(old_na), span_wrap("NA"))
                na_text <- plural(abs(new_na), span_wrap("NA"), mid = ifelse(new_na >= 0, "new ", "fewer "))
                summaries <- c(summaries, glue::glue("changed {plural(n, 'value')} ",
                                                     "({p}) of '{code_wrap(var, .code_class = 'visible-change')}' (previously {old_na_text}, now {na_text})"))
            } else {
                # different type
                new_na <- sum(is.na(new)) - sum(is.na(old))
                if (new_na == length(new)) {
                    summaries <- c(summaries, glue::glue("converted {code_wrap(var, .code_class = 'visible-change')} from {typeold}",
                                                                     " to {typenew} (now 100% {span_wrap('NA')})"))
                } else {
                    old_na <- sum(is.na(old))
                    old_na_text <- plural(abs(old_na), span_wrap("NA"))
                    na_text <- glue::glue("{abs(new_na)} ",
                                          ifelse(new_na >= 0, "new", "fewer"), " {span_wrap('NA')}")
                    summaries <- c(summaries, glue::glue("converted {code_wrap(var, .code_class = 'visible-change')} from {typeold}",
                                                                     " to {typenew} (previously {old_na_text}, now {na_text})"))
                }
            }
        }
        # add to the callout word lists
        callout_words <- append(callout_words, list(list(word = var, change = "visible-change")))
    }
    callout_words <- append(
        callout_words,
        lapply(group_cols, function(x) list(word = x, change = "internal-change"))
    )

    # ; separate all the summaries
    summaries <- paste0(summaries, collapse = "; ")

    display(glue::glue("{prefix} {summaries}."), callout_words = callout_words)
    if (!has_changed) {
        display(glue::glue("{prefix} resulted in no changes."))
    }

    newdata
}

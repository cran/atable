#' Create Tables for Reporting of Clinical Trials
#'
#' Applies descriptive statistics and hypothesis tests to data, and arranges the results for printing.
#'
#' @param x An object. If \code{x} is a data.frame, it must have unique and syntactically valid colnames,
#' see \code{\link[atable]{is_syntactically_valid_name}}.
#'
#' @param data Passed to \code{atable(x = data, ...)}.
#'
#' @param target_cols A character vector containing some column names of \code{x}.
#'
#' Descriptive statistics and hypothesis test are applied to these columns depending on their class.
#' The descriptive statistics are defined by \code{\link{statistics}};
#' their representation and format by \code{\link{format_statistics}}.
#'
#' Hypothesis test are defined by \code{\link{two_sample_htest}} or \code{\link{multi_sample_htest}}
#' (depending on the number of levels of \code{group_col});
#' their representation and format by \code{\link{format_tests}}.
#' Note that atable always adds one name to \code{target_cols} to count the number of obsservations.
#' This name is stored in \code{atable_options('colname_for_observations')}.
#'
#' @param group_col A character of length 1 containing a column of \code{x} or \code{NULL}.
#' This column defines the groups that are compared by the hypothesis tests.
#' \code{\link[base:factor]{as.factor}} is applied to this column before further processing.
#' Default is \code{NULL}, meaning that no hypothesis tests are applied.
#'
#' @param split_cols A character vector containing some of \code{colnames(x)} or \code{NULL}.
#' \code{x} is splitted by these columns before descriptive statistics and hypothesis test are applied.
#' \code{\link[base:factor]{as.factor}} is applied to this column before further processing.
#' Default is \code{NULL}, meaning that no splitting is done.
#'
#' @param format_to A character vector of length 1. Specifies the format of the output of \code{atable}.
#'  Possible values are \code{'Latex'}, \code{'Word'}, \code{'Raw'}, \code{'HTML'}.
#'  Default is defined \code{\link{atable_options}}.
#'
#' @param drop_levels A logical. If \code{TRUE} then \code{\link[base]{droplevels}} is called on \code{group_col}
#'  and \code{split_cols} beforefurther processsing. Default is \code{TRUE}.
#'
#' @param add_levels_for_NA If \code{TRUE} then \code{\link[base:factor]{addNA}} is called on \code{group_col} and
#' \code{split_cols} before further processsing. Default is \code{FALSE}.
#'
#' @param ... Passed from and to other methods.

#' @param formula A formula of the form \code{target_cols ~ group_col | split_cols}.
#' The \code{|} separates the \code{group_col} from the \code{split_cols}.
#' Read the \code{|} as 'given' as in a conditional probability \code{P(target_cols | split_cols)}.
#' \code{target_cols} and \code{split_cols} may contain multiple names separated by \code{+}.
#' \code{group_col} must be a single name if given.
#' \code{group_col} and \code{split_cols} may be omitted and can be replaced by \code{1} in this case.
#' The \code{|} may also be omitted if no \code{split_cols} are given.
#'
#'
#' @return
#' Results depend on \code{format_to}:
#' \itemize{
#' \item{\code{'Raw'}: }{A list with two elemtents called \code{'statistics_result'} and \code{'tests_result'}, that
#' contain all results of the descriptve statistics and the hypothesis tests.
#' This format useful, when extracting a specific result unformated
#'  (when \code{format_to} is not \code{'Raw'} all numbers are also returned, but as rounded
#'  characters for pretty printing and squeezed into a data.frame).
#' \itemize{
#'  \item{\code{'statistics_result'}: } { contains a data.frame with colnames \code{c(split_cols, group_col, target_cols}.
#'  \code{split_cols} and \code{group_col} retain their original values (now as factor).
#'  \code{target_cols} contain lists with the results of function \code{\link{statistics}}.
#'  As the result of function \code{statistics} is also a list, \code{target_cols} contain lists of lists.}
#'
#'    \item {\code{'tests_result'}: } {has the same structure as \code{'statistics_result'}, but contains the results
#'  of \code{\link{two_sample_htest}} and \code{\link{multi_sample_htest}}.
#'  Note that \code{tests_result} only exists if \code{split_cols} is not \code{NULL}.}}
#' }
#' \item{\code{'Word'}: }{A data.frame.
#' Column \code{atable_options('colname_for_group')} contains
#' all combinations of the levels of \code{split_cols} and
#' the names of the results of function \code{\link{format_statistics}}.
#'
#' Further columns are the levels of \code{group_col} the names of the results of \code{format_tests}.
#'
#' The levels of \code{split_cols} and the statistics are arranged vertically.
#' The hypothesis test are arranged horizontally.
#'
#' }
#' \item{\code{'HTML'}: }{Same as for \code{format_to = 'Word'} but a different character indents
#' the first column.}
#' \item{\code{'Latex'}: }{Same as for \code{format_to = 'Word'} but a different character indents
#' the first column and with \code{\link{translate_to_LaTeX}} applied afterwards. }
#' }
#' @examples
#' # See vignette for more examples:
#' # utils::vignette("atable_usage", package = "atable")
#'
#' # Analyse datasets::ToothGrowth:
#' # Length of tooth for each dose level and delivery method:
#' atable::atable(datasets::ToothGrowth,
#'   target_cols = 'len',
#'   group_col = 'supp',
#'   split_cols = 'dose',
#'   format_to = 'Word')
#' # Print in .docx with e.g. flextable::regulartable and officer::body_add_table
#'
#' # Analyse datasets::ChickWeight:
#' # Weight of chickens for each time point and diet:
#' atable(weight ~ Diet | Time, datasets::ChickWeight, format_to = 'Latex')
#' # Print as .pdf with e.g. Hmisc::latex
#'
#' # Analyse atable::test_data:
#' atable(Numeric + Logical + Factor + Ordered ~ Group | Split1 + Split2,
#'   atable::test_data, format_to = 'HTML')
#'# Print as .html with e.g. knitr::kable and options(knitr.kable.NA = '')
#'
#' # For print on Console use format_to = 'Word'.


#' @export
atable <- function(x, ...) {
    UseMethod("atable")
}


#' @export
#' @describeIn atable applies descriptive statistics and hypothesis tests, arranges the results for printing.
atable.data.frame <- function(x, target_cols, group_col = NULL, split_cols = NULL,
    format_to = atable_options("format_to"), drop_levels = TRUE, add_levels_for_NA = FALSE,
    ...) {

    DD = x
    stopifnot(is_syntactically_valid_name(colnames(DD)),
              is.character(target_cols),
              is.character(format_to),
              length(target_cols) > 0,
              target_cols %in% colnames(DD),
              is.null(group_col) || (is.character(group_col) && group_col %in% colnames(DD)),
              is.null(split_cols) || (is.character(split_cols) && all(split_cols %in% colnames(DD))),
              anyDuplicated(c(target_cols, group_col, split_cols)) == 0)



    DD <- DD[c(target_cols, split_cols, group_col)]  # only these columns are relevant

    # I cast group_col and split_cols to factor that allows more flexibility for the
    # input than only factor: grouping and splitting by character, numeric and
    # logical is possible
    DD[c(group_col, split_cols)] <- lapply(DD[c(group_col, split_cols)], as.factor)

    DD[c(group_col, split_cols)] <- if (isTRUE(drop_levels)) {
        droplevels(DD[c(group_col, split_cols)])
    } else {
        DD[c(group_col, split_cols)]
    }

    # add one level to factors without a level factors without a level can happen if
    # a factor contains only NA and droplevels is applied
    DD[c(group_col, split_cols)] <- lapply(DD[c(group_col, split_cols)], function(x) if (nlevels(x) == 0) {
        addNA(x)
    } else {
        x
    })



    # Explicitly include missing values of DD[c(group_col, split_cols)] in the
    # analysis (if any NA in DD[c(group_col, split_cols)]).
    DD <- if (isTRUE(add_levels_for_NA)) {
        DD[c(group_col, split_cols)] <- lapply(DD[c(group_col, split_cols)], function(x) if (any(is.na(x))) {
            addNA(x)
        } else {
            x
        })
        DD
    } else {
        DD[stats::complete.cases(DD[c(group_col, split_cols)]), , drop = FALSE]
    }



    # I want to calculate he number of observations in every group.  I do this by
    # adding a column called atable_options('colname_for_observations') of class
    # 'count_me' to DD.  The package atable defines the function
    # 'statistics.count_me'. It just returns the length of the vector.
    DD <- add_observation_column(DD)



    # Check if group_col or split_cols are NULL and call the appropriate atable-functions
    result <- if (is.null(group_col)) {
        if (is.null(split_cols)) {
            atable_unsplitted_ungrouped(DD = DD, target_cols = target_cols, format_to = format_to)
        } else {
            atable_splitted_ungrouped(DD = DD, target_cols = target_cols,
                                      split_cols = split_cols, format_to = format_to)
        }
    } else {
        if (is.null(split_cols)) {
            atable_unsplitted_grouped(DD = DD, target_cols = target_cols, group_col = group_col,
                                    split_cols = split_cols, format_to = format_to)
        } else {
            atable_splitted_grouped(DD = DD, target_cols = target_cols, group_col = group_col,
                                    split_cols = split_cols, format_to = format_to)
        }
    }

    # localization
    result <- if ("Group" %in% colnames(result)) {
        doBy::renameCol(result, "Group", atable_options("colname_for_group"))
    } else {
        result
    }
    result <- if ("value" %in% colnames(result)) {
        doBy::renameCol(result, "value", atable_options("colname_for_value"))
    } else {
        result
    }

    return(result)
}


#' @export
#' @describeIn atable parses the formula and passes its parts to \code{atable}.
atable.formula <- function(formula, data, ...) {




    ff <- as.character(formula)

    LH <- ff[2]
    RH <- ff[3]

    pipe_split <- strsplit(RH, " \\| ")[[1]]  # as.character(f) always has blanks around those '|', which are not part of a name


    b <- !(all.vars(formula) %in% colnames(data))
    if (any(b)) {
        stop("Not all names of formula in colnames of data: ", paste(all.vars(formula)[b],
            collapse = ", "))
    }

    var_search <- function(x, data) {
        # Intersect of colnames(data) and x by grepl
        colnames_in_formula <- sapply(colnames(data), grepl, x = x, fixed = TRUE)  # order of names now as in data
        if (any(colnames_in_formula)) {
            colnames_in_formula <- names(which(colnames_in_formula))
            colnames_in_formula <- intersect(all.vars(formula), colnames_in_formula)  # order of names as given in the formula
            return(colnames_in_formula)
        } else {
            return(NULL)
        }
    }

    target_cols <- var_search(LH, data)
    group_col <- var_search(pipe_split[1], data)

    split_cols <- if (length(pipe_split) == 1) {
        NULL
    } else {
        if (length(pipe_split) > 2) {
            warning("formula should only have up to one '|' on the right side, not more. Taking only the first.")
        }
        var_search(pipe_split[2], data)
    }

    atable(x = data, target_cols = target_cols, group_col = group_col, split_cols = split_cols,
        ...)

}
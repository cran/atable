


# helper functions. Not exported. Not documented.  I do not expect that they will
# be changed by the user.

#' @export
`[.count_me` <- function(x, i, j, ...) {
    # see function add_observation_column
    y <- unclass(x)[i, ...]
    class(y) <- c("count_me", class(y))
    return(y)
}


test_caller <- function(DD, group_col, value_col, ...) {
    # Call two_sample_htest or multi_sample_htest depending on the number of levels
    # of DD$group_col Also extracts DD$value_col and passes it to
    # two_sample_htest/multi_sample_htest.  Then the methods of two_sample_htest is
    # chosen


    stopifnot(is.character(group_col) & length(group_col) == 1,
              is.character(value_col) & length(value_col) == 1,
              group_col %in% colnames(DD) & value_col %in% colnames(DD),
              is.factor(DD[[group_col]]))


    # Dispatch is on the first argument for S3-functions.  I use value to decide
    # which test to use.  So value is the first argument.

    # what happens if only one level?  multi_sample_htest is called.  the tests
    # applied there are enclosed by try().  So anything can be passed to them and the
    # error is ignored, because I explicitely return a htest-list with p=NAN etc...
    value <- DD[[value_col]]
    group <- DD[[group_col]]

    group_levels <- levels(DD[[group_col]])

    result <- if (length(group_levels) == 2) {
        two_sample_htest(value, group, ...)
    } else {
        multi_sample_htest(value, group, ...)
    }

    return(result)
}

apply_tests_helper <- function(DD, target_cols, group_col, ...) {
    b <- is.data.frame(DD) && is.character(target_cols) && is.character(group_col) &&
        length(group_col) == 1 && target_cols %in% colnames(DD) && group_col %in%
        colnames(DD) && is.factor(DD[[group_col]]) && nlevels(DD[[group_col]]) >
        0 && length(intersect(target_cols, group_col)) == 0




    test_out <- mapply(test_caller, value_col = target_cols, MoreArgs = list(DD = DD,
        group_col = group_col, ...), SIMPLIFY = FALSE)

    b <- lapply(test_out, check_tests)



    test_out <- lapply(test_out, list)  # now the results can be stored in a column of a data.frame

    # create a empty data.frame with colnames to store the results of the
    # hypothesis-test-functions
    df <- data.frame(matrix(vector(), 1, length(target_cols), dimnames = list(c(),
        target_cols)), stringsAsFactors = FALSE, check.names = FALSE, fix.empty.names = FALSE)


    df[target_cols] <- test_out
    return(df)

}


apply_tests <- function(DD, target_cols, group_col, split_cols, ...) {

    splitted <- base::split(DD, DD[split_cols], drop = FALSE)


    Raw <- plyr::ldply(splitted, apply_tests_helper, target_cols, group_col)

    # i want to keep the group_col and split_cols as they are. split mangles their
    # values in one column .id, separated by .
    keys <- base::expand.grid(lapply(DD[split_cols], levels))


    Raw$.id <- NULL
    Raw <- cbind(keys, Raw)


    return(Raw)
}

format_tests_caller <- function(x, cols) {
    out <- lapply(x[cols], Vectorize(format_tests, vectorize.args = "x", SIMPLIFY = FALSE))


    # yyy

    b <- lapply(out, Vectorize(check_format_tests))


    # name clash: colnames(out[[1]][[1]]) mit colnames(x) split_cols and result of
    # format_tests have same colnames
    x <- setdiff(colnames(x), cols)  # this is split_cols
    y <- colnames(out[[1]][[1]])  # this is colnames of one formated test. All must have the same colnames.

    b <- intersect(x, y)


    if (length(b) > 0) {
        stop("Name clash. Please change: ", paste(b, collapse = ", "), ". Search in split_cols and function format_tests().")
    }
    # the colnames vanish. I still need them. So i add them in the data.frame
    out2 <- mapply(add_name_to_tests, out[cols], name = cols, SIMPLIFY = FALSE, USE.NAMES = FALSE)


    # rbind all data.frames
    out3 <- plyr::ldply(out2, data.frame, stringsAsFactors = FALSE, .id = NULL, check.names = FALSE)




    # preserve the order given by the user. the column 'variable' was created by
    # add_name
    out3[[atable_options("colname_for_variable")]] <- factor(out3[[atable_options("colname_for_variable")]],
        levels = cols)

    return(out3)
}







rbind_helper <- function(x, cols) {
    # rbind the list of data.frames to a single data.frame
    plyr::ldply(x[cols], data.frame, stringsAsFactors = FALSE, check.names = FALSE,
        fix.empty.names = FALSE, .id = NULL)
}

add_name_to_tests <- function(x, name) {
    # add a column to a data.frame x with value name as character
    UseMethod("add_name_to_tests")
}

#' @export
add_name_to_tests.list <- function(x, name) {
    return(lapply(x, add_name_to_tests, name))
}

#' @export
add_name_to_tests.data.frame <- function(x, name, colname_for_variable = atable_options("colname_for_variable")) {
    # the colnames of x are generated by format_tests(), which is user defined.
    # there may be name clashes with atable_options('colname_for_variable')
    b <- colname_for_variable %in% colnames(x)
    if (b) {
        stop("Name clash. ", colname_for_variable, " already in ", paste(colnames(x),
            collapse = ", "), ". Please change atable_options('colname_for_variable')")
    }

    x[[colname_for_variable]] <- name
    return(x)
}


add_name_to_statistics <- function(x, name) {
    # add a column to a data.frame x with value name as character
    UseMethod("add_name_to_statistics")
}

#' @export
add_name_to_statistics.list <- function(x, name) {
    return(lapply(x, add_name_to_statistics, name))
}

#' @export
add_name_to_statistics.data.frame <- function(x, name, colname_for_variable = atable_options("colname_for_variable")) {

    # there may be name clashes with atable_options('colname_for_variable')
    b <- colname_for_variable %in% colnames(x)
    if (b) {
        stop("Name clash. ", colname_for_variable, " already in ", paste(colnames(x),
            collapse = ", "), ". Please change atable_options('colname_for_variable') to something different")
    }


    x[[colname_for_variable]] <- name
    x <- x[c(colname_for_variable, "tag", "value")]  # order of the columns

    return(x)
}



apply_statistics_helper <- function(x, cols, ...) {

    out <- lapply(x[cols], statistics)

    b <- lapply(out, check_statistics)


    out <- lapply(out, list)  # now the results can be stored in a column of a data.frame

    # create a empty data.frame with colnames to store the results of statistics()
    df <- data.frame(matrix(vector(), 1, length(cols), dimnames = list(c(), cols)),
        stringsAsFactors = FALSE, check.names = FALSE, fix.empty.names = FALSE)


    df[cols] <- out
    return(df)
}


apply_statistics <- function(DD, target_cols, split_cols) {

    splitted <- split(DD, DD[split_cols], drop = FALSE)

    Raw <- plyr::ldply(splitted, apply_statistics_helper, cols = target_cols)


    # i want to keep the group_col and split_cols as they are. split mangles their
    # values in one column called '.id', separated by |
    keys <- expand.grid(lapply(DD[split_cols], levels))

    Raw$.id <- NULL
    Raw <- cbind(keys, Raw)


    return(Raw)
}







format_statistics_caller <- function(x, cols) {
    out <- lapply(x[cols], Vectorize(format_statistics, vectorize.args = "x", SIMPLIFY = FALSE))

    b <- lapply(out, Vectorize(check_format_statistics))

    # the colnames vanish. I still need them. So i add them in the data.frame
    out2 <- mapply(add_name_to_statistics, out[cols], name = cols, SIMPLIFY = FALSE,
        USE.NAMES = FALSE)


    # statistics_result has hard-coded colnames 'tag' and 'value'. These may clash
    # with split_cols, group_col
    split_col_group_col_names <- setdiff(colnames(x), cols)
    b <- "tag" %in% split_col_group_col_names
    if (b) {
        stop("Name clash. 'tag' already in split_cols or group_col: ", paste(split_col_group_col_names,
            collapse = ", "), ". Please change colnames(DD).")
    }


    b <- "value" %in% split_col_group_col_names
    if (b) {
        stop("Name clash. 'value' already in split_cols or group_col: ", paste(split_col_group_col_names,
            collapse = ", "), ". Please change colnames(DD).")
    }


    # rbind all data.frames
    out3 <- plyr::ldply(out2, data.frame, stringsAsFactors = FALSE, .id = NULL)

    # preserve the order given by the user. the column 'variable' was created by
    # add_name
    out3[[atable_options("colname_for_variable")]] <- factor(out3[[atable_options("colname_for_variable")]],
        levels = cols)

    return(out3)
}

arrange_statistics <- function(formated_statistics_result, split_cols, format_to) {
    formated_statistics_result <- replace_NA(formated_statistics_result)

    return(arrange_helper(formated_statistics_result, split_cols, format_to))
}


arrange_statistics_and_tests <- function(formated_statistics_result, formated_tests_result,
    group_col, split_cols, format_to) {
    formated_statistics_result <- replace_NA(formated_statistics_result)


    ff <- stats::as.formula(paste0(paste0(c(split_cols, atable_options("colname_for_variable"),
        "tag"), collapse = " + "), " ~ ", group_col))

    # dcast can create duplicated colnames.  I do not want duplicated colnames.  So I
    # check that and then do the dcast
    names <- c(split_cols, atable_options("colname_for_variable"), "tag", levels(formated_statistics_result[[group_col]]))
    b <- duplicated(names)
    if (any(b)) {
        stop("Name clash. Please change: ", paste(names[b], collapse = ", "), ". Search in split_cols, atable_options('colname_for_variable'), 'tag' or levels of DD[[group_col]].")
    }

    tabc <- reshape2::dcast(formated_statistics_result, ff)
    # when group_col has level '' (character with nchar=0) this level will be
    # silently renamed by make.names (I think, this is because reshape2::dcast calls
    # as.data.frame) New level will be 'Var.4' or 'Var.5' etc depending on the levels
    # position



    # My code demands that the two data.frames tabc and formated_tests_result must
    # have only these columns in common: split_cols,
    # atable_options('colname_for_variable') The columns of formated_tests_result are
    # defined by function format_tests(), which is user-defined Also the columns of
    # tabc are levels(DD[[group_col]]), which is also user-defined I check the common
    # columns and then do the merge.

    x <- intersect(colnames(tabc), colnames(formated_tests_result))
    y <- c(split_cols, atable_options("colname_for_variable"))
    b <- isTRUE(all.equal(sort(x), sort(y), check.attributes = FALSE))

    if (!b) {
        stop("Name clash. Please change: ", paste(c(setdiff(x, y), setdiff(y, x)),
            collapse = ", "), ". Search in format_tests(), split_cols, atable_options('colname_for_variable'), 'tag' or levels of DD[[group_col]]")
    }

    tabm <- merge(tabc, formated_tests_result, all.x = TRUE)



    # merge does reorder the tag column (cause it is not in 'by').
    ff <- stats::as.formula(paste0("~", paste0(c(split_cols, atable_options("colname_for_variable"),
        "tag"), collapse = " + ")))
    tabm <- doBy::orderBy(ff, tabm)



    # I need the name of the last columns of formated_tests_result (the results of
    # format_tests() and format_tests_caller()) This function may be generated by the
    # user.  Thus formated_tests_result may have arbitrary number of columns and
    # colnames

    cols <- setdiff(colnames(formated_tests_result), c(split_cols, atable_options("colname_for_variable")))


    tabr <- plyr::ddply(tabm, c(split_cols, atable_options("colname_for_variable")),
        function(x) {
            x[cols] <- lapply(x[cols], replace_consecutive, by = NA)
            return(x)
        })


    return(arrange_helper(tabr, split_cols, format_to))
}

arrange_helper <- function(tab, split_cols, format_to) {
    switch(format_to, Word = {
        tab <- indent_data_frame(tab, keys = c(split_cols, atable_options("colname_for_variable"),
            "tag"), indent_character = "    ")
        return(tab)
    }, Latex = {
        # the order is important: translate_to_LaTeX may change the colnames of tab. So
        # it is applied second.
        tab <- indent_data_frame(tab, keys = c(split_cols, atable_options("colname_for_variable"),
            "tag"), indent_character = "\\quad")
        tab <- translate_to_LaTeX(tab)
        return(tab)
    }, HTML = {
        tab <- indent_data_frame(tab, keys = c(split_cols, atable_options("colname_for_variable"),
            "tag"), indent_character = " &emsp; ")
        return(tab)
    }, {
        stop("format_to ", format_to, " unknown.")
    })
}




atable_splitted_grouped <- function(DD, target_cols, group_col, split_cols, format_to) {
    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)


    statistics_result <- apply_statistics(DD, Observation_and_target_cols, c(group_col,
        split_cols))

    tests_result <- apply_tests(DD, target_cols, group_col, split_cols)



    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result, tests_result = tests_result))
    }

    formated_statistics_result <- plyr::ddply(statistics_result, c(group_col, split_cols),
        format_statistics_caller, cols = Observation_and_target_cols)

    formated_tests_result <- plyr::ddply(tests_result, split_cols, format_tests_caller,
        cols = target_cols)



    atable_result <- arrange_statistics_and_tests(formated_statistics_result, formated_tests_result,
        group_col, split_cols, format_to)
    return(atable_result)
}

atable_unsplitted_grouped <- function(DD, target_cols, group_col, split_cols, format_to) {

    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)

    statistics_result <- apply_statistics(DD, Observation_and_target_cols, c(group_col,
        split_cols))

    # cannot do ddply as in atable_splitted_ungrouped because split_col=NULL
    tests_result <- apply_tests_helper(DD, target_cols, group_col)


    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result, tests_result = tests_result))
    }

    formated_statistics_result <- plyr::ddply(statistics_result, c(group_col, split_cols),
        format_statistics_caller, cols = Observation_and_target_cols)

    formated_tests_result <- format_tests_caller(tests_result, cols = target_cols)


    atable_result <- arrange_statistics_and_tests(formated_statistics_result, formated_tests_result,
        group_col, split_cols, format_to)
    return(atable_result)
}

atable_splitted_ungrouped <- function(DD, target_cols, split_cols, format_to) {
    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)

    statistics_result <- apply_statistics(DD, Observation_and_target_cols, split_cols)

    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result))
    }


    formated_statistics_result <- plyr::ddply(statistics_result, split_cols, format_statistics_caller,
        cols = Observation_and_target_cols)


    atable_result <- arrange_statistics(formated_statistics_result, split_cols, format_to)


    return(atable_result)
}

atable_unsplitted_ungrouped <- function(DD, target_cols, format_to) {

    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)

    statistics_result <- apply_statistics_helper(DD, Observation_and_target_cols)


    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result))
    }


    formated_statistics_result <- format_statistics_caller(statistics_result, Observation_and_target_cols)

    split_cols <- NULL  # i do not want to write another helper function.
    # Just use arrange_statistics, which uses split_cols.
    atable_result <- arrange_statistics(formated_statistics_result, split_cols, format_to)
    return(atable_result)
}



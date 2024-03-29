


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


    stopifnot(is.character(group_col), length(group_col) == 1, is.character(value_col),
        length(value_col) == 1, group_col %in% colnames(DD), value_col %in% colnames(DD),
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


    Raw <- plyr::ldply(splitted, apply_tests_helper, target_cols, group_col, ...)

    # i want to keep the group_col and split_cols as they are. split mangles their
    # values in one column .id, separated by .
    keys <- base::expand.grid(lapply(DD[split_cols], levels))


    Raw$.id <- NULL
    Raw <- cbind(keys, Raw)


    return(Raw)
}

format_tests_caller <- function(x, cols, ...) {
    out <- lapply(x[cols], Vectorize(format_tests, vectorize.args = "x", SIMPLIFY = FALSE),
        ...)




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




apply_statistics_helper <- function(x, cols, ...) {

    out <- lapply(x[cols], statistics, ...)

    b <- lapply(out, check_statistics)


    out <- lapply(out, list)  # now the results can be stored in a column of a data.frame

    # create a empty data.frame with colnames to store the results of statistics()
    df <- data.frame(matrix(vector(), 1, length(cols), dimnames = list(c(), cols)),
        stringsAsFactors = FALSE, check.names = FALSE, fix.empty.names = FALSE)


    df[cols] <- out
    return(df)
}


apply_statistics <- function(DD, target_cols, split_cols, ...) {

    splitted <- split(DD, DD[split_cols], drop = FALSE)

    Raw <- plyr::ldply(splitted, apply_statistics_helper, cols = target_cols, ...)


    # i want to keep the group_col and split_cols as they are. split mangles their
    # values in one column called '.id', separated by |
    keys <- expand.grid(lapply(DD[split_cols], levels))

    Raw$.id <- NULL
    Raw <- cbind(keys, Raw)


    return(Raw)
}







format_statistics_caller <- function(x, cols, ...) {
    out <- lapply(x[cols], Vectorize(format_statistics, vectorize.args = "x", SIMPLIFY = FALSE),
        ...)

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

arrange_statistics <- function(formated_statistics_result, split_cols, format_to,
    Alias_mapping, blocks, indent_character, indent) {
    formated_statistics_result <- replace_NA(formated_statistics_result)

    return(arrange_helper(formated_statistics_result, split_cols, format_to, Alias_mapping, blocks, indent_character, indent))
}


arrange_statistics_and_tests <- function(formated_statistics_result, formated_tests_result,
    group_col, split_cols, format_to, Alias_mapping, blocks, indent_character, indent) {
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


    return(arrange_helper(tabr, split_cols, format_to, Alias_mapping, blocks, indent_character, indent))
}

arrange_helper <- function(tab, split_cols, format_to, Alias_mapping, blocks, indent_character, indent) {



    # map cols and blocks to aliases
    tab[[atable_options("colname_for_variable")]] <- plyr::mapvalues(x = tab[[atable_options("colname_for_variable")]],
        from = Alias_mapping$old, to = Alias_mapping$new, warn_missing = FALSE)

    blocks <- if(!is.null(blocks)){
      lapply(blocks, plyr::mapvalues,
             from = Alias_mapping$old,
             to = Alias_mapping$new,
             warn_missing = FALSE)}
    else{blocks}



    if(isTRUE(indent)){


      switch(format_to, Word = {
        indent_character = if(is.null(indent_character)){atable_options("indent_character_Word")}else{indent_character}

        tab <- indent_data_frame_helper(DD = tab,
                                        split_cols = split_cols,
                                        blocks = blocks,
                                        indent_character = indent_character)
        return(tab)
      }, Latex = {
        indent_character = if(is.null(indent_character)){atable_options("indent_character_Latex")}else{indent_character}

        # the order is important: translate_to_LaTeX may change the colnames of tab. So
        # it is applied second.
        tab <- indent_data_frame_helper(DD = tab,
                                        split_cols = split_cols,
                                        blocks = blocks,
                                        indent_character = indent_character)
        tab <- translate_to_LaTeX(tab)

        return(tab)
      }, HTML = {
        indent_character = if(is.null(indent_character)){atable_options("indent_character_HTML")}else{indent_character}

        tab <- indent_data_frame_helper(DD = tab,
                                        split_cols = split_cols,
                                        blocks = blocks,
                                        indent_character = indent_character)
        return(tab)
      }, Console = {
        indent_character = if(is.null(indent_character)){atable_options("indent_character_Console")}else{indent_character}

        tab <- indent_data_frame_helper(DD = tab,
                                        split_cols = split_cols,
                                        blocks = blocks,
                                        indent_character = indent_character)
        tab[is.na(tab)] <- ""

        class(tab) <- c("atable", class(tab))

        return(tab)
      }, markdown = {
        indent_character = if(is.null(indent_character)){atable_options("indent_character_markdown")}else{indent_character}

        tab <- indent_data_frame_helper(DD = tab,
                                        split_cols = split_cols,
                                        blocks = blocks,
                                        indent_character = indent_character)
        return(tab)
      },
      {
        stop("format_to ", format_to, " unknown.")
      })

    }else{
      return(tab)}



}

#' @export
print.atable <- function(x, ...) print(as.data.frame(x), right = FALSE, ...)


atable_splitted_grouped <- function(DD, target_cols, group_col, split_cols, format_to,
    Alias_mapping, blocks, indent_character, indent, ...) {

    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)


    statistics_result <- apply_statistics(DD, Observation_and_target_cols, c(group_col,
        split_cols), ...)

    tests_result <- apply_tests(DD, target_cols, group_col, split_cols, ...)



    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result, tests_result = tests_result))
    }

    formated_statistics_result <- plyr::ddply(statistics_result, c(group_col, split_cols),
        format_statistics_caller, cols = Observation_and_target_cols, ...)

    formated_tests_result <- plyr::ddply(tests_result, split_cols, format_tests_caller,
        cols = target_cols, ...)


    atable_result <- arrange_statistics_and_tests(formated_statistics_result, formated_tests_result,
        group_col, split_cols, format_to, Alias_mapping, blocks, indent_character, indent)
    return(atable_result)
}

atable_unsplitted_grouped <- function(DD, target_cols, group_col, split_cols, format_to,
    Alias_mapping, blocks, indent_character, indent, ...) {

    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)

    statistics_result <- apply_statistics(DD, Observation_and_target_cols, c(group_col,
        split_cols), ...)

    # cannot do ddply as in atable_splitted_ungrouped because split_col=NULL
    tests_result <- apply_tests_helper(DD, target_cols, group_col, ...)


    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result, tests_result = tests_result))
    }

    formated_statistics_result <- plyr::ddply(statistics_result, c(group_col, split_cols),
        format_statistics_caller, cols = Observation_and_target_cols, ...)

    formated_tests_result <- format_tests_caller(tests_result, cols = target_cols,
        ...)

    atable_result <- arrange_statistics_and_tests(formated_statistics_result, formated_tests_result,
        group_col, split_cols, format_to, Alias_mapping, blocks, indent_character, indent)
    return(atable_result)
}

atable_splitted_ungrouped <- function(DD, target_cols, split_cols, format_to, Alias_mapping, blocks, indent_character, indent,
    ...) {

    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)

    statistics_result <- apply_statistics(DD, Observation_and_target_cols, split_cols,
        ...)

    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result))
    }


    formated_statistics_result <- plyr::ddply(statistics_result, split_cols, format_statistics_caller,
        cols = Observation_and_target_cols, ...)

    atable_result <- arrange_statistics(formated_statistics_result, split_cols, format_to,
        Alias_mapping, blocks, indent_character, indent)


    return(atable_result)
}

atable_unsplitted_ungrouped <- function(DD, target_cols, format_to, Alias_mapping, blocks, indent_character, indent,
    ...) {

    Observation_and_target_cols <- c(atable_options("colname_for_observations"),
        target_cols)

    statistics_result <- apply_statistics_helper(DD, Observation_and_target_cols,
        ...)



    if (identical(format_to, "Raw")) {
        return(list(statistics_result = statistics_result))
    }


    formated_statistics_result <- format_statistics_caller(statistics_result, Observation_and_target_cols,
        ...)

    split_cols <- NULL  # i do not want to write another helper function.
    # Just use arrange_statistics, which uses split_cols.
    atable_result <- arrange_statistics(formated_statistics_result, split_cols, format_to,
        Alias_mapping, blocks, indent_character, indent)
    return(atable_result)
}


mockup_format_numbers = function(x) {
  # replaces digits with 'x'
  # removes -
  # replaces NA by 0

  x[is.na(x)] = 0

  y = abs(x)
  y = gsub(pattern="[[:digit:]]", replacement = "x", x = y)
  y = substring(y, first = 1, last = 4)


  return(y)
}

indent_data_frame_helper = function(DD, split_cols, blocks, character_empty = "",
                                    numeric_empty = NA, indent_character = "\\quad", colname_indent = "Group")
{
  # checks if blocking is applicable and calls the appropriate indent_data_frame()

  out <- if(  is.null(split_cols) && !is.null(blocks) ) {


    indent_data_frame_with_blocks(DD = DD,
                                  blocks = blocks,
                                  character_empty = character_empty,
                                  numeric_empty = numeric_empty,
                                  indent_character = indent_character,
                                  colname_indent = colname_indent)
  }else{

    keys = c(split_cols, atable_options("colname_for_variable"), "tag")

    indent_data_frame(DD = DD,
                      keys = keys,
                      values = setdiff(colnames(DD), keys),
                      character_empty = character_empty,
                      numeric_empty = numeric_empty,
                      indent_character = indent_character,
                      colname_indent = colname_indent)
  }

  return(out)
}

indent_data_frame_with_blocks = function(DD, blocks, character_empty = "", numeric_empty = NA, indent_character = "\\quad", colname_indent = "Group")
{




  stopifnot(!is.null(blocks))
  # block to data.frame
  name_adder = function(x, name){data.frame(target_cols = x,
                                            block_name = name,
                                            stringsAsFactors = FALSE)}

  bb = mapply(name_adder, x=blocks, name=names(blocks),
              SIMPLIFY = FALSE)

  bb = do.call(rbind, bb)

  bb = doBy::renameCol(bb, "target_cols", atable_options("colname_for_variable"))

  bb$block_name = factor(bb$block_name, levels = names(blocks)) # the order of the blocks is necessary

  bb = doBy::renameCol(bb, "block_name", atable_options("colname_for_blocks"))

  the_block_name = atable_options("colname_for_blocks")
  if(the_block_name %in% colnames(DD)){stop(the_block_name , " already in the data.
                                            Consider changing the data or atable_options('colname_for_blocks')")}



  DD = merge(DD, bb,
             all.x = TRUE) # merge with blocks

  # order with blocks
  ff <- c(atable_options("colname_for_variable"), the_block_name, "tag")
  ff <- stats::as.formula(paste("~", paste(ff, collapse = "+"), collapse = ""))

  DD <- doBy::orderBy(ff, DD)

  if(atable_options("colname_for_order") %in% colnames(DD)){stop(atable_options("colname_for_order") , " already in the data.
                                            Consider changing the data or atable_options('colname_for_order')")}





  DD[[atable_options("colname_for_order")]] = 1:nrow(DD) #  xxx order in atable options schreiben. naming conflict stopifnot


  # line_adder

  line_adder <- function(DD, keys, values, the_key, character_empty = "", numeric_empty = NA)
  {
    # adds a new line on top of the data frame with keys, value are empty.

    to_add <- DD[1, , drop = FALSE]

    # empty columns empty key columns
    empty_keys <- setdiff(keys, keys[1:which(keys == the_key)])
    to_add[empty_keys] <- character_empty

    # empty value columns

    return_empty_value <- function(x) {
      type <- class(x)
      switch(type, character = character_empty, numeric = numeric_empty, integer = numeric_empty,
             factor = character_empty)
    }

    to_add[values] <- lapply(to_add[values], return_empty_value)


    return(rbind(to_add, DD))
  }




  # Apply line_adder for every key combination, Order: backwards
  keys_indent = c(the_block_name, atable_options("colname_for_variable"), "tag")
  keys <- keys_indent
  values <- setdiff(setdiff(colnames(DD), atable_options("colname_for_order")), keys)
  # line_adder adds new lines with empty values. But I want to keep the column order. So I remove this column from values

  for (index in seq(from = length(keys) - 1, to = 1, by = -1)) {
    DD <- plyr::ddply(DD, keys[1:index], line_adder, keys = keys, values = values,
                      the_key = keys[index])
  }



  ff <- stats::as.formula(paste("~", paste(c(atable_options("colname_for_order"), keys_indent ), collapse = "+"), collapse = "")) # xxx order aus atable options

  DD <- doBy::orderBy(ff, DD)

  # line adder adds new lines even when block is na. remove these lines
  b <- is.na(DD[[the_block_name]]) & nchar(DD[[ atable_options("colname_for_variable")]])==0
  DD <- DD[!b, ]



  fun_for_identical <- function(x,y) !is.na(x) && !is.na(y) && identical(x,y)


  # casts everything to character and then replace_consecutive
  DD[keys] <- lapply(DD[keys], as.character)
  DD[keys] <- lapply(DD[keys], replace_consecutive,
                     by = indent_character,
                     fun_for_identical = fun_for_identical)


  # some NA in blocks still remain. Set to "" for 'no indent'
  b <- is.na(DD[[the_block_name]])
  DD[b, the_block_name] <- ""

  # indent

  indent <- DD[keys]  # keys has length 2 and no duplicates. So DD[keys] is a data.frame, even witout drop=FALSE for '['
  indent <- apply(indent, 1, paste, collapse = " ")



  DD[[colname_indent]] <- indent

  DD <- DD[c(colname_indent, values)]


  rownames(DD) <- NULL
  return(DD)

}


check_blocks <- function(blocks, target_cols)
{

  # blocks <- NULL is ok:
  if(is.null(blocks)){return(TRUE)}

  # check that variables are in a single block

  stopifnot(is.list(blocks),
            sapply(blocks, is.character))


  n <- names(blocks)
  if (is.null(n)) stop("names(blocks) must not be NULL")

  b <- is.na(n)
  if (any(b)) stop("names(blocks) must not be NA")

  b <- duplicated(n)
  if (any(b)) stop("names(blocks) must be unique: ", paste(n[b], collapse = ", "))


  ublock <- unlist(blocks)

  b <- duplicated(ublock)
  if (any(b)) stop("names in blocks not unique: ", paste(ublock[b], collapse = ", "))


  b = ublock %in% target_cols

  if(any(!b)){stop("names in blocks must be in target_cols: ", paste(ublock[!b], collapse = ", "))}

  # check that variables are together
  lapply(blocks, function(x){
    # consecutive variables
    w <- which(target_cols %in% x)
    y <- min(w):max(w)

    if (length(y) != length(w)) stop("block variables appear to be separated: ", paste(x, collapse = ", "))
    if (any(w != y)) stop("block variables appear to be in a different order to target_cols: ", paste(x, collapse = ", "))

    # order as in target_cols
    xo = ordered(x, levels = target_cols)
    b = (sort(xo) == xo)
    if(any(!b)) stop("block variables appear to be in a different order to target_cols: ", paste(x[!b], collapse = ", "))

  })
  return(TRUE)
}



format_compact_numeric = function(x,...)
{

  the_mean <- atable_options("format_numbers")(x$mean)
  the_sd <- atable_options("format_numbers")(x$sd)

  values <- c(Mean_SD = paste0(the_mean, " (", the_sd, ")") )

  format_statistics_out <- data.frame(tag = factor("remove_me", levels = "remove_me"),
                                      value = values, row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE,
                                      fix.empty.names = FALSE)

  return(format_statistics_out)

}


format_compact_factor = function(x,...)
{

  nn <- names(x)

  value <- unlist(x)
  total <- sum(value)


  percent <- 100 * value/total

  if(length(nn)<=3){
    # return only first level, ignore the others
    # As atable::statistics.factor calls table(..., useNA='always'), there is always NA in nn and thus three
    # levels are the minimum, not two levels
    # The counts of missing values will not be displayed, but are included in the percent-calculation

    value <- paste0(atable_options("format_percent")(percent[1]), "% (", atable_options("format_numbers")(value[1]), ")")


    format_statistics_out <- data.frame(tag = factor(nn[1], levels = nn[1]), value = value[1],
                                        row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    return(format_statistics_out)
  }  else{


    value <- paste0(atable_options("format_percent")(percent), "% (", atable_options("format_numbers")(value), ")")



    format_statistics_out <- data.frame(tag = factor(nn, levels = nn),
                                        value = value,
                                        row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    return(format_statistics_out)
  }
}


indent_tag_value = function(x, indent_character)
{

  stopifnot(
    is.data.frame(x),
    # all(sapply(x, is.character)),
    nrow(x)>=1,
    ncol(x)>=2
  )

  x[] = lapply(x, as.character)

  index_variable = which(colnames(x) == atable_options("colname_for_variable") )
  index_tag = which(colnames(x) == "tag")

  stopifnot(index_variable>0,
            index_tag==index_variable+1)

  stopifnot(
    is.character(indent_character),
    length(indent_character)==1
  )

  # vorher:
  # variable | tag
  # nachher
  # variable

  if(nrow(x)==1)
  {
    # format_compact_numeric returns a data.frame wite nrow=1 and sets tag="remove_me"
    # format_compact_factor may also return a data.frame wite nrow=1, but without tag="remove_me"

    if(x$tag=="remove_me"){

      out = x[-index_tag]
      return(out)
    }
    else{
      out = x[-index_variable]
      colnames(out)[index_variable] = atable_options("colname_for_variable")
      return(out)
    }

  }

  # create an empty line an rbind it on top. Keep blocking-column if available.

  empty_headline = x[1, -index_tag]
  empty_headline[1,index_variable:ncol(empty_headline)]=NA
  # set name of heading
  empty_headline[1,index_variable] = x[1,index_variable]
  # set block name of heading if available
  empty_headline = if(atable_options("colname_for_blocks") %in% colnames(empty_headline))
  {
    empty_headline[[atable_options("colname_for_blocks") ]] = x[1, atable_options("colname_for_blocks") ]
    empty_headline

  }else{empty_headline}

  indent = x
  indent[ ,index_variable] = paste(indent_character, indent[,index_tag])
  indent = indent[-index_tag]

  out = rbind(empty_headline, indent,
              stringsAsFactors = FALSE)

  return(out)

}

fill_blocks = function(x)
{
  # replaces NA in a vector x with "no_blocking___1", "no_blocking___2", "no_blocking___3",...
  # 1,2,3 counts the connected components of NA in x
  # x=c(NA, NA, NA, "a","a", NA, NA, "b", "b", NA)
  # fill_blocks(x)

  block_prefix = "no_blocking___"
  x
  y=x
  index=1

  if(is.na(x[1])) {y[1] = paste0(block_prefix, index) }

  for(i in 2:length(x))
  {
    if(is.na(x[i]) ){y[i] = paste0(block_prefix, index) }
    else{
      if(is.na(x[i-1])){index = index + 1}
    }
  }

  return(y)
}


indent_blocks = function(DD, indent_character)
{

  stopifnot(colnames(DD)[1] == atable_options("colname_for_blocks"))

  DD$block_name___ = fill_blocks(DD$block_name___)

  DD$block_name___ = factor(DD$block_name___,
                            levels = DD$block_name___[!duplicated(DD$block_name___)])



  Block_indent = plyr::ddply(DD,
                             atable_options("colname_for_blocks"),
                             function(x){
                               if( substr(x[[atable_options("colname_for_blocks")]][1] , 1,14) == "no_blocking___" )  {return(x)}
                               empty = x[1, 2:ncol(x)]
                               empty[1,1] = as.character(x[1,1])
                               empty[1, 2:ncol(empty)]=NA

                               x[,2] = paste(indent_character, x[,2])

                               rbind(empty,
                                     x[, 2:ncol(x)],
                                     stringsAsFactors = FALSE)
                             })

  return(Block_indent[-1])


}

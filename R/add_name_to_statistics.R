#' add_name_to_statistics
#'
#' add a column to a data.frame x with value name as character.
#' Helper Function. Not intended to be called by the user.
#'
#' checks if the new field already exists
#' @param x an object
#' @param name a value
#' @param colname_for_variable a character length 1. Default is defined in atable_options
#' @param ... passed to methods
#'
#' @return x now with new field colname_for_variable


#' @export
add_name_to_statistics <- function(x, name, ...) {
  # add a column to a data.frame x with value name as character
  UseMethod("add_name_to_statistics")
}

#' @export
#' @describeIn add_name_to_statistics apply add_name_to_statistics to all field of the list
add_name_to_statistics.list <- function(x, name, ...) {
  return(lapply(x, add_name_to_statistics, name, ...))
}

#' @export
#' @describeIn add_name_to_statistics add field colname_for_variable to the data.frame. chekc for a name clash as this field as there are many user-defined fields
add_name_to_statistics.data.frame <- function(x, name, colname_for_variable = atable_options("colname_for_variable"), ...) {

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

#' Reset atable_options to default
#'
#' See also \code{\link{atable_options}}.
#'
#'
#' @examples
#' atable_options('replace_NA_by') # show options
#' atable_options('replace_NA_by' = 'foo bar') # set a new value
#' atable_options('replace_NA_by') # show options
#' atable_options_reset() # restore defaults
#' atable_options('replace_NA_by') # as before
#'
#' @export
atable_options_reset <- function() settings::reset(atable_options)

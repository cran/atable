## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(knitr.kable.NA = '')
library(atable)

## -------------------------------------------------------------------------------------------------
data(mtcars)
# factors
mtcars$am <- factor(mtcars$am, c(0, 1), c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, c(0, 1), c("V-shaped", "straight"))
# ordered
mtcars$cyl <- ordered(mtcars$cyl)
# set format_to
atable_options(format_to = "md")

## ---- results='asis'------------------------------------------------------------------------------
knitr::kable(atable(vs + cyl + hp + disp ~ am, mtcars))

## -------------------------------------------------------------------------------------------------
new_two_sample_htest_numeric <- function(value, group, ...){
  d <- data.frame(value = value, group = group)
  group_levels <- levels(group)
  x <- subset(d, group %in% group_levels[1], select = "value", drop = TRUE)
  y <- subset(d, group %in% group_levels[2], select = "value", drop = TRUE)
  ks_test_out <- stats::ks.test(x, y)
  t_test_out <- stats::t.test(x, y)
  out <- list(p_ks = ks_test_out$p.value,
              p_t = t_test_out$p.value)
  return(out)
}

## -------------------------------------------------------------------------------------------------
new_statistics_numeric <- function(x, ...){
  statistics_out <- list(Median = median(x, na.rm = TRUE),
                         MAD = mad(x, na.rm = TRUE),
                         Mean = mean(x, na.rm = TRUE),
                         SD = sd(x, na.rm = TRUE))
  class(statistics_out) <- c("statistics_numeric", class(statistics_out))
  # We will need this new class later to specify the format
  return(statistics_out)
}

## -------------------------------------------------------------------------------------------------
new_format_statistics_numeric <- function(x, ...){
  Median_MAD <- paste(round(c(x$Median, x$MAD), digits = 1), collapse = "; ")
  Mean_SD <- paste(round(c(x$Mean, x$SD), digits = 1), collapse = "; ")
  levs <- c("Median; MAD", "Mean; SD")
  out <- data.frame(tag = factor(levs, 
                                 levels = levs),
                    # the factor needs levels for the non-alphabetical order
                    value = c(Median_MAD, Mean_SD),
                    stringsAsFactors = FALSE)
  return(out)
}

## -------------------------------------------------------------------------------------------------
utils::assignInNamespace(x = "two_sample_htest.numeric",
                         value = new_two_sample_htest_numeric,
                         ns = "atable")

## -------------------------------------------------------------------------------------------------
atable_options("statistics.numeric" = new_statistics_numeric)

## -------------------------------------------------------------------------------------------------
knitr::kable(atable(hp + disp ~ am, mtcars, 
                    format_statistics.statistics_numeric = new_format_statistics_numeric))

## -------------------------------------------------------------------------------------------------
atable_options_reset()

## -------------------------------------------------------------------------------------------------
require(Hmisc)
label(mtcars$hp) <- "Horse power"
units(mtcars$hp) <- "hp"
knitr::kable(atable(hp + disp ~ 1, mtcars))

## -------------------------------------------------------------------------------------------------
get_alias.labelled <- function(x, ...){
    out <- attr(x, "label", exact = TRUE)
    Units <- attr(x, "units", exact = TRUE)
    out = if(!is.null(Units)){
      paste0(out, " (", Units, ")")}else{out}
    return(out)
}
atable_options("get_alias.labelled" = get_alias.labelled)
knitr::kable(atable(hp + disp ~ 1, mtcars))

## -------------------------------------------------------------------------------------------------
attr(mtcars$disp, "label") <- "Displacement"
get_alias.default <- function(x, ...){
    attr(x, "label", exact = TRUE)
}
atable_options("get_alias.default" = get_alias.default)
knitr::kable(atable(hp + disp ~ 1, mtcars))

## -------------------------------------------------------------------------------------------------
atable_options("format_p_values")(0.12)
atable_options("format_p_values")(0.012)
atable_options("format_p_values")(0.0012)
atable_options("format_p_values")(0.0009)

## -------------------------------------------------------------------------------------------------
fn <- function(x){
  txt <- sprintf("%3.3f", x)
  if(x < 0.001) txt <- "<0.001"
  return(txt)
}
atable_options("format_p_values" = fn)

## -------------------------------------------------------------------------------------------------
atable_options("format_p_values")(0.12)
atable_options("format_p_values")(0.012)
atable_options("format_p_values")(0.0012)
atable_options("format_p_values")(0.0009)

## -------------------------------------------------------------------------------------------------
knitr::kable(atable(vs + cyl + hp + disp ~ am, mtcars))


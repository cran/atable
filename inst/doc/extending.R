## ----setup, include = FALSE---------------------------------------------------

require(knitr)
require(Hmisc)
require(datasets)
require(atable)
require(utils)
knitr::opts_chunk$set(warning=FALSE)

## ----mtcars-------------------------------------------------------------------
data(mtcars)
# factors
mtcars$am <- factor(mtcars$am, c(0, 1), c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, c(0, 1), c("V-shaped", "straight"))
# ordered
mtcars$cyl <- ordered(mtcars$cyl)
# set format_to
atable_options(format_to = "Latex")

## ----mtcars print, results='asis'---------------------------------------------
Hmisc::latex(atable(vs + cyl + hp + disp ~ am, mtcars),
             file = "",
             title = "",
             rowname = NULL,
             table.env = FALSE)

## ----statistics Date----------------------------------------------------------
statistics.Date <- function(x, ...){
  out <- list(min = min(x, na.rm = TRUE),
              med = median(x, na.rm = TRUE),
              max = max(x, na.rm = TRUE))
  class(out) <- c("statistics_date", class(out))
  out
}

## ----format statistics Date---------------------------------------------------
format_statistics.statistics_date <- function(x, ...){
  z <- c("Min ; Max", "Median")
  out <- data.frame(tag = factor(z, z),
                    value = c(paste(x$min, x$max, sep = " ; "),
                              as.character(x$med)),
                    stringsAsFactors = FALSE)
  return(out)
}

## ----Date print, results='asis'-----------------------------------------------
# add a date variable to mtcars
mtcars$date <- as.Date(runif(nrow(mtcars), 0, 365*10), "1990-01-01")

Hmisc::latex(atable(mtcars, "date"),
             file = "",
             title = "",
             rowname = NULL,
             table.env = FALSE)

## ----add some survival data---------------------------------------------------
# add some survival data (use 'date' as the timepoint)
if (requireNamespace("survival", quietly = TRUE)) {
  mtcars$date2 <- mtcars$date + round(rnorm(nrow(mtcars), 10, 4)) # end date
  mtcars$time <- as.numeric(mtcars$date2 - mtcars$date) # time
  mtcars$not_road_worthy <- rbinom(nrow(mtcars), 1, .2) # 'survived'?
  mtcars$surv <- with(mtcars, survival::Surv(time, not_road_worthy))
} else {
  ## do nothing
}

## ----statistics and tests for Surv--------------------------------------------
if (requireNamespace("survival", quietly = TRUE)) {
  # statistics function
  statistics.Surv <- function(x, ...){
    survfit_object <- survival::survfit(x ~ 1)
    # copied from survival::print.survfit
    out <- survival:::survmean(survfit_object, rmean = "common")
    return(list(mean_survival_time = out$matrix["*rmean"],
                SE = out$matrix["*se(rmean)"]))
  }
  # testing function
  two_sample_htest.Surv <- function(value, group, ...){
    survdiff_result <- survival::survdiff(value~group, rho=0)
    # copy from survival::print.survdiff
    etmp <- survdiff_result$exp
    df <- (sum(1 * (etmp > 0))) - 1
    p <- 1 - stats::pchisq(survdiff_result$chisq, df)
    return(list(p = p,stat = survdiff_result$chisq))
  }
} else {
  ## do nothing
}

## ----Surv print, results='asis'-----------------------------------------------
if (requireNamespace("survival", quietly = TRUE)) {
  Hmisc::latex(atable(surv ~ am, mtcars),
               file = "",
               title = "",
               rowname = NULL,
               table.env = FALSE)
} else {
  ## do nothing
}

## ----numeric2 class-----------------------------------------------------------
# add numeric2 to the class of disp
class(mtcars$disp) <- c("numeric2", class(mtcars$disp))

# subsetting function for numeric2 class
'[.numeric2' <- function(x, i, j, ...){
  y <- unclass(x)[i, ...]
  class(y) <- c("numeric2", class(y))
  y
}


## ----statistics numeric2------------------------------------------------------
# statistics function
statistics.numeric2 <- function(x, ...){
  statistics_out <- list(Median = median(x, na.rm = TRUE),
                         p25 = quantile(x, 0.25, na.rm = TRUE),
                         p75 = quantile(x, 0.75, na.rm = TRUE))
  class(statistics_out) <- c("statistics_numeric2", class(statistics_out))
  # We will need this new class later to specify the format
  return(statistics_out)
}
# testing function
two_sample_htest.numeric2 <- function(value, group, ...){
  d <- data.frame(value = value, group = group)
  test_out <- stats::wilcox.test(value ~ group, d)
  return(test_out)
}

## ----numeric2 print, results='asis'-------------------------------------------
Hmisc::latex(atable(vs + cyl + hp + disp ~ am, mtcars),
             file = "",
             title = "",
             rowname = NULL,
             table.env = FALSE)

## ----format statistics numeric2-----------------------------------------------
format_statistics.statistics_numeric2 <- function(x, ...){
  out <- data.frame(
    tag = factor(c("Median [Quartiles]")),
    value = sprintf("%2.1f [%2.1f ; %2.1f]", x$Median, x$p25, x$p75),
    stringsAsFactors = FALSE)
  return(out)
}



## ----format statistics numeric2 print, results='asis'-------------------------

Hmisc::latex(atable(vs + cyl + hp + disp ~ am, mtcars),
             file = "",
             title = "",
             rowname = NULL,
             table.env = FALSE)



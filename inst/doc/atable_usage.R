## ----require, echo=FALSE, results='hide', message=FALSE, warning=FALSE--------

require(knitr)
require(Hmisc)
require(datasets)
require(utils)
require(atable)


## ----global_chunk_options, echo = FALSE, results='hide'-----------------------

opts_chunk$set(message = FALSE)
opts_chunk$set(echo = FALSE)
opts_chunk$set(warning = FALSE)
opts_chunk$set(results = "hide")


## ----ToothGrowth atable, echo=TRUE, results='asis'----------------------------

# apply atable
the_table <- atable::atable(ToothGrowth,
                            target_cols = "len",
                            group_col = "supp",
                            split_cols = "dose",
                            format_to = "Latex")

# send to LaTeX
Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:ToothGrowthatable",
             caption = "ToothGrowth analysed by atable.",
             caption.lot = "ToothGrowth analysed by atable",
             rowname = NULL)


## ----mtcars atable, echo=TRUE, results='asis'---------------------------------

# all columns of mtcars are numeric, although some are
# better represented as factors
mtcars <- within(datasets::mtcars, {gear <- factor(gear)})

# Add labels and units.
attr(mtcars$mpg, "alias") = "Consumption [Miles (US)/ gallon]"
Hmisc::label(mtcars$qsec) = "Quarter Mile Time"
units(mtcars$qsec) = "s"

# apply atable
the_table <- atable::atable(mpg + hp + gear + qsec ~ cyl | vs,
                            mtcars,
                            format_to = "Latex")
# atable also has a formula method.
# The left side contains the target columns, the right side contains grouping
# and splitting columns separated by the pipe |

# send to LaTeX
Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:mtcarsatable",
             caption = "mtcars analysed by atable.",
             caption.lot = "mtcars analysed by atable",
             rowname = NULL)

## ----Extract specific values from the table, echo=TRUE, results='markup'------

unformatted <- atable::atable(mpg + hp + gear + qsec ~ cyl | vs,
                              mtcars,
                              format_to = "Raw")
# format_to = "Raw" tells atable to skip formatting.

# Extract specific values
unformatted$statistics_result$mpg[[2]]$mean
unformatted$statistics_result$mpg[[2]]$sd

## ----Localisation, echo=TRUE, results='asis'----------------------------------

# Set german words for the table:
atable::atable_options(labels_TRUE_FALSE = c("Ja", "Nein"),
                       labels_Mean_SD = "Mittelwert (SD)" ,
                       labels_valid_missing = "Ok (fehlend)",
                       colname_for_observations = "N",
                       colname_for_value = "Wert",
                       colname_for_group = "",
                       replace_NA_by = "fehlend")

attr(mtcars$mpg, "alias German") = "Verbrauch [Miles (US)/ gallon]"
attr(mtcars$hp, "alias German") = "PS"

# Tell atable to look for attribute "alias German"
atable_options('get_alias.default' = function(x, ...)
  {attr(x, "alias German", exact = TRUE)})

# apply atable
the_table <- atable::atable(mtcars,
             target_cols = c("mpg", "hp"))

# reset all options to default
atable_options_reset()

# send to LaTeX
Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:Localisation",
             caption = "Localised atable. All identifiers produced by atable are
                        now translated to german; also the user can add aliasees
                        to all variables for localisation.",
             caption.lot = "Localised atable",
             rowname = NULL)



## ----Word, echo=TRUE----------------------------------------------------------

for_Word <- atable::atable(mpg + hp + gear + qsec ~ cyl | vs, mtcars,
                           format_to = "Word")

# print in Word with packages flextable and officer

# MyFTable <- flextable::regulartable(data = for_Word)
# left aligned first column:
# MyFTable <- flextable::align(MyFTable, align = "left", j = 1)

# save on disc. Not run here:
# doc <- officer::read_docx()
# doc <- flextable::body_add_flextable(doc, value = MyFTable)
# print(doc, target = "atable and Word.docx")


## ----HTML, echo=TRUE----------------------------------------------------------

for_HTML <- atable::atable(mpg + hp + gear + qsec ~ cyl | vs,
                           mtcars,
                           format_to = "HTML")

options(knitr.kable.NA = '')
# knitr::kable(for_HTML, caption="HTML table with atable") # not run.


## ----Console, echo=TRUE-------------------------------------------------------
atable::atable(mpg + hp + gear + qsec ~ cyl | vs,
                           mtcars,
                           format_to = "Console")

## ----atable_options, echo=TRUE, eval=FALSE------------------------------------
#  atable_options(format_to = "Console")

## ----mockup table, echo=TRUE, results='asis'----------------------------------

# set the formating of numbers so that only 'x' is returned instead of digits.

atable_options("format_p_values" = atable:::mockup_format_numbers)
atable_options("format_numbers" = atable:::mockup_format_numbers)
atable_options("format_percent" = atable:::mockup_format_numbers)

# now apply atable to mtcars as above
the_table <- atable::atable(mpg + hp + gear + qsec ~ cyl | vs,
                            mtcars,
                            format_to = "Latex")

# send to LaTeX
Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:mtcarsatablemockup",
             caption = "mockup table of the mtcars analysis, filled with xxx instead of numbers.
                        Compare with table \\ref{tab:mtcarsatable}.",
             caption.lot = "mockup table of the mtcars analysis",
             rowname = NULL)

# back to normal:
atable_options_reset()


## ----blocks, echo=TRUE, results='asis'----------------------------------------

the_table <- atable::atable(datasets::mtcars,
                     target_cols = c("cyl", "disp", "hp", "am", "gear", "qsec") ,
                     blocks = list("Engine" = c("cyl", "disp", "hp"),
                                   "Gearbox" = c("am", "gear")),
                     format_to = "Latex")

# send to LaTeX
Hmisc::latex(
  the_table,
  file = "",
  title = "",
  label = "tab:mtcarsblocking",
  caption = "Blocking shown with datasets::mtcars: Variables cyl, disp and mpg are
in block Engine and variables am and gear in block gearbox. Variable qsec is not
blocked and thus not indented.",
caption.lot = "Blocking of the mtcars analysis",
rowname = NULL)


## ----Tabelle with stats and test, results='asis', echo=FALSE------------------



DD <- matrix(
  c(
    c('scale_of_measurement', 'nominal', 'ordinal', 'interval'),

    c('statistic', 'counts occurences of every level', 'as factor', 'Mean and standard deviation'),

    c('two_sample_test', 'chi^2 test', 'Wilcoxon Rank-Sum test', 'Kolmogorov-Smirnov Test'),

    c('effect_size', "two levels: odds ratio, else Cramér's phi", "Cliff's Delta", "Cohen's d"),

    c('multi_sample_test', 'chi^2 test', 'Kruskal-Wallis test', 'Kruskal-Wallis test')
  ),
  ncol = 4,
  byrow = TRUE,
  dimnames = list(NULL, c('R class', 'factor', 'ordered', 'numeric'))
)


DD <- as.data.frame(DD)

DD[] <- lapply(DD, gsub, pattern = "_", replacement = " ")

DD <- atable::translate_to_LaTeX(DD, greek = TRUE)

Hmisc::latex(DD,
      file = "",
      title = "",
      label = "tab:classesandatable",
      caption = "Classes and atable. Table shows the descriptive statistics and hypothesis tests, that are
      applied to the three R classes factor, ordered and numeric. Table also shows the appropriate scale
      of measurement. Class character and logical are treated as nominal scaled variables.",
      caption.lot = "Classes and atable",
      rowname = NULL,
      first.hline.double = FALSE,
      collabel.just = c("l|", "l", "l", "l"),
      col.just = c("p{3.5cm}|", "p{3.5cm}", "p{4cm}", "p{4cm}"),
      where="!htbp",
      multicol = FALSE,
      longtable = FALSE,
      booktabs = FALSE)




## ----Replace two_sample_htest.numeric, results='markup', echo=TRUE------------
# write a new function:
new_two_sample_htest <- function(value, group, ...){

  d <- data.frame(value = value, group = group)

  group_levels <- levels(group)
  x <- subset(d, group %in% group_levels[1], select = "value", drop = TRUE)
  y <- subset(d, group %in% group_levels[2], select = "value", drop = TRUE)

  ks_test_out <- stats::ks.test(x, y)
  t_test_out <- stats::t.test(x, y)
  cohen_d_out <- effsize::cohen.d(x, y, na.rm = TRUE)

  # return p-values of both tests
  out <- list(p_ks = ks_test_out$p.value,
              p_t = t_test_out$p.value,
              cohens_d = cohen_d_out$estimate)

  return(out)
}


## ----Modify statistics numeric, results='markup', echo=TRUE-------------------


new_stats <- function(x, ...){

  statistics_out <- list(Median = median(x, na.rm = TRUE),
                         MAD = mad(x, na.rm = TRUE),
                         Mean = mean(x, na.rm = TRUE),
                         SD = sd(x, na.rm = TRUE))

  return(statistics_out)
}


## ----replace via atable_options, results='markup', echo=TRUE------------------
atable_options("statistics.numeric" = new_stats)

## ----replace via atable, , results='markup', echo=TRUE------------------------
the_table <-  atable::atable(atable::test_data,
                             target_cols = "Numeric",
                             group_col = "Group",
                             split_cols = "Split1",
                             format_to = "Latex",
                             two_sample_htest.numeric = new_two_sample_htest)



## ----Print modify numeric, echo=TRUE, results='asis'--------------------------

Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:modifynumeric",
             caption = "Modified atable also calculates the median, MAD,
             t-test and KS-test.",
             caption.lot = "Modified atable",
             rowname = NULL)


## ----Methods for statistics, results='markup', echo=TRUE----------------------

statistics.Date <- function(x, ...){

  out <- list(
    Min = min(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )

  class(out) <- c("statistics_Date", class(out))
  # We will need this new class later to specify the format

  return(out)
}


## ----Methods for format, results='markup', echo=TRUE--------------------------
format_statistics.statistics_Date <- function(x, ...){

  min_max <- paste0(x$Min, "; ", x$Max)
  Median <- as.character(x$Median)


  out <- data.frame(
    tag = factor(c("Min Max", "Median"), levels = c("Min Max", "Median")),
    value = c(min_max, Median),
    stringsAsFactors = FALSE)
  # the factor needs levels for the non-alphabetic order
  return(out)
}


## ----Methods for Date print, echo=TRUE, results='asis'------------------------
the_table <-  atable::atable(atable::test_data,
                             target_cols = "Date",
                             format_to = "Latex")

Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:addedDate",
             caption = "atable with added methods for class Date. Now calculates
             minimum, maximum and median for this class",
             caption.lot = "atable with added methods for class Date",
             rowname = NULL)

## ----atable compact, echo=TRUE, results='asis'--------------------------------

atable_options_reset()


tab = atable_compact(atable::test_data,
                     target_cols = c("Numeric", "Numeric2", "Split2", "Factor",
                                     "Ordered"),
                     group_col = "Group2",
                     blocks = list("Primary Endpoint" = "Numeric",
                                   "Secondary  Endpoint" = c("Numeric2", "Split2")),
                     indent_character = "\\quad")

tab = atable::translate_to_LaTeX(tab)

Hmisc::latex(tab,
             file = "",
             title = "",
             label = "tab:atable compact",
             caption = Hmisc::latexTranslate("atable compact. The data.frame is
             grouped by group_col and the summary statistcs of the target_cols
             are calculated: mean, sd for numeric, counts and percentages for
             factors. The target_cols are blocked: the first block 'Primary Endpoint'
             contains the variable Numeric. The second block 'Secondary  Endpoint'
             contains the variables 'Numeric2' and 'Split2'. The blocks are
             intended. For variable Split2 only its first level 'b' is reported, as
             the variable has only two levels and the name 'Split2' does not appear
             in the table. The variables Factor and Ordered have more than two levels,
             so all of them are reported and appropriately intended."),
             caption.lot = "atable compact",
             rowname = NULL)


## ----atable longitudinal, echo=TRUE, results='asis'---------------------------

x = atable::test_data

# create timepoint of measurement
set.seed(42)
x = within(x, {time = sample(paste0("time_", 1:6), size=nrow(x), replace = TRUE)})

tab = atable_longitudinal(x,
       target_cols = "Split2",
       group_col = "Group2",
       split_cols = "time",
       add_margins = TRUE)

tab = atable::translate_to_LaTeX(tab)

Hmisc::latex(tab,
             file = "",
             title = "",
             label = "tab:atable longitudinal",
             caption = Hmisc::latexTranslate("atable longitudinal. Table shows
             statistics of variable Split2 measured at six time points in in three
             groups and the p-values for a comparison of the groups. The name of
             the variable 'Split2' does not show up in the table, so the user should
             add it to the caption of the table. Also only statistics of the first
             level of 'Split2' are shown, as 'Split2' has only two levels.
             Format of the statistics is percent % (n/total)."),
             caption.lot = "atable longitudinal",
             rowname = NULL)



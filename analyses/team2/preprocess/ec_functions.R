# Functions to help with data preprocessing

#' Clean the sentencing period data and split it into a minimum and maximum.
#'
#' When only a single sentence length (e.g., "2 years") is given, the minimum
#' will be left blank. The `min_period` and `max_period` columns are used to
#' fill missing data (when `period` is "Other") and are ignored otherwise.
#'
#' @param period A character vector giving messy sentencing periods
#' @param min_period A character vector giving some minimum periods
#' @param max_period A character vector giving some minimum periods
#'
#' @return A two-column character matrix with the minimum and maximum period.
#'   Missing data is returned as a blank string.
#' @export
clean_dispositions_periods <- function(period, min_period, max_period) {
  clean_periods <- period
  
  period_is_other <- !is.na(clean_periods) &
    tolower(clean_periods) == "other"
  clean_periods[period_is_other &
                  !is.na(min_period) &
                  !is.na(max_period)] <-
    paste(min_period[period_is_other &
                       !is.na(min_period) &
                       !is.na(max_period)],
          "-",
          max_period[period_is_other &
                       !is.na(min_period) &
                       !is.na(max_period)])
  clean_periods[period_is_other &
                  is.na(min_period) &
                  !is.na(max_period)] <-
    max_period[period_is_other &
                 is.na(min_period) &
                 !is.na(max_period)]
  clean_periods[period_is_other &
                  !is.na(min_period) &
                  is.na(max_period)] <-
    min_period[period_is_other &
                 !is.na(min_period) &
                 is.na(max_period)]
  clean_periods[period_is_other] <- stringr::str_replace_all(clean_periods[period_is_other],
                                                             "\\.00", "")
  
  # Do basic cleanup
  clean_periods <- tolower(clean_periods)
  clean_periods <- stringr::str_replace_all(clean_periods, "[[:blank:]]+", " ")
  clean_periods <- stringr::str_replace(clean_periods, "^ ", "")
  clean_periods <- stringr::str_replace(clean_periods, " $", "")
  
  # FOR NOW we're going to throw out extra information about IPP
  clean_periods <- stringr::str_replace(clean_periods,
                                        "day modification of ipp sentence", "days")
  
  # Make life without parole more token-like
  clean_periods <- stringr::str_replace(clean_periods,
                                        "life without the possibility of parole",
                                        "life_no_parole")
  
  # Deal with the many ways of writing ".5"
  clean_periods <- stringr::str_replace_all(clean_periods, "([[:digit:]]+) - 1/2", "\\1.5")
  clean_periods <- stringr::str_replace_all(clean_periods, "([[:digit:]]+) 1/2", "\\1.5")
  clean_periods <- stringr::str_replace_all(clean_periods, "([[:digit:]]+) and a half", "\\1.5") 
  
  # Tokenize "time in"/"time-in", "time served" and some typos
  clean_periods <- stringr::str_replace(clean_periods, "tiime", "time")
  clean_periods <- stringr::str_replace(clean_periods, "tim\\b", "time")
  clean_periods <- stringr::str_replace(clean_periods, "time[ -]?(served|in)", "time_served")
  clean_periods <- stringr::str_replace(clean_periods, "balance of backtime", "time_served")
  clean_periods <- stringr::str_replace(clean_periods, "back time", "time_served")
  
  # Standardize units
  clean_periods <- stringr::str_replace_all(clean_periods, "hrs", "hours")
  clean_periods <- stringr::str_replace_all(clean_periods, "hour(?!s)",  "hours")
  clean_periods <- stringr::str_replace_all(clean_periods, "day(?!s)",   "days")
  clean_periods <- stringr::str_replace_all(clean_periods, "month(?!s)", "months")
  clean_periods <- stringr::str_replace_all(clean_periods, "year(?!s)",  "years")
  
  # Remove "flat"
  clean_periods <- stringr::str_replace(clean_periods, " flat", "")
  
  # Remove "BCP"
  clean_periods <- stringr::str_replace(clean_periods, " bcp", "")
  
  # Standardize the "time arithmetic"
  clean_periods <- stringr::str_replace_all(clean_periods, ", ", " plus ")
  clean_periods <- stringr::str_replace_all(clean_periods, " and ", " plus ")
  clean_periods <- stringr::str_replace_all(clean_periods, " less ", " minus ")
  
  # This one's tricky, in part because str_replace_all is greedier than it should
  # be. Turn entries like "2 years 6 months" into "2 years plus six months".
  operator_insertion_regexp <- "^([[:digit:]]+) ([[:alpha:]]+) ([[:digit:]]+) ([[:alpha:]]+)$"
  needs_operator_inserted <- !is.na(clean_periods) &
    stringr::str_detect(clean_periods, operator_insertion_regexp) & 
    (stringr::str_replace(clean_periods, operator_insertion_regexp, "\\2") != "to")
  clean_periods[needs_operator_inserted] <- stringr::str_replace_all(clean_periods[needs_operator_inserted],
                                                                     operator_insertion_regexp,
                                                                     "\\1 \\2 plus \\3 \\4")
  
  # Split the periods using "-" or "to"
  clean_periods_split <- stringr::str_split_fixed(clean_periods, "\\s?((\\bto\\b)|-)\\s?", 2)
  
  # If the beginning period is missing a time unit (days, months, years), grab it from the end period
  is_missing_units <- stringr::str_detect(clean_periods_split[, 1], "^[[:digit:]]+(\\.5)?$")
  # Pulling out the first units word is a bit tricky
  missing_units <-  stringr::str_extract(stringr::str_extract(clean_periods_split[is_missing_units, 2], 
                                                              "^[[:digit:]]+(\\.5)? [[:alpha:]]+"), 
                                         "[[:alpha:]]+$")
  clean_periods_split[is_missing_units, 1] <- paste(clean_periods_split[is_missing_units, 1],
                                                    missing_units, sep = " ")
  
  # Last thing: if there's only one entry, str_split puts it in the first column,
  # have it swap spaces with the second.
  is_missing_max <- clean_periods_split[, 2] == ""
  clean_periods_split[is_missing_max, 2] <- clean_periods_split[is_missing_max, 1]
  clean_periods_split[is_missing_max, 1] <- ""
  
  clean_periods_split
}

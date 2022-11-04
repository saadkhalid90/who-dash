## read in the required libraries

library(shiny)
library(DT)
library(formattable)
library(readr)
library(dplyr)
library(ggplot2)

unit.scale = function(x)
  return(x / 1)

osub_formatter <- formatter("span",
                            style = x ~ style (
                              display = "block",
                              `background-color` = ifelse(x > 1, "#F48FB1", "none")
                            ))

str_formatter <- formatter("span",
                           style = x ~ style (
                             display = "block",
                             `background-color` = ifelse(x <= 10, "#F48FB1", "none")
                           ))

nd_formatter <- formatter("span",
                          style = x ~ style (
                            display = "block",
                            `background-color` = ifelse(x <= 5, "#F48FB1", "none")
                          ))

disc_formatter <- formatter("span",
                            style = x ~ style (
                              display = "block",
                              `background-color` = ifelse(x < 0.75 |
                                                            x > 1.25, "#F48FB1", "none")
                            ))

seg1_formatter <- formatter("span",
                           style = ~ style(
                             `background-color` = ifelse(log1 == 1, "#BBDEFB", "none")
                           ))
seg2_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log2 == 1, "#BBDEFB", "none")
                            ))
seg3_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log3 == 1, "#BBDEFB", "none")
                            ))
seg4_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log4 == 1, "#BBDEFB", "none")
                            ))
seg5_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log5 == 1, "#BBDEFB", "none")
                            ))
seg6_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log6 == 1, "#BBDEFB", "none")
                            ))
seg7_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log7 == 1, "#BBDEFB", "none")
                            ))
seg8_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log8 == 1, "#BBDEFB", "none")
                            ))
seg9_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log9 == 1, "#BBDEFB", "none")
                            ))
seg10_formatter <- formatter("span",
                            style = ~ style(
                              `background-color` = ifelse(log10 == 1, "#BBDEFB", "none")
                            ))

## read in the required data
summary <- read_csv('Data/summary.csv', col_types = "icccciiiiiiiniiic")
listing <- read_csv('Data/listing.csv')
segments <- read_csv('Data/segments.csv') %>% select(-c("id"))
timeUpdated <- readRDS('timeUpdated.RDS')

sample_wide <- read_csv('Data/sample-wide.csv', col_types = "iiiiiiii")
sample_long <- read_csv('Data/sample-long.csv', col_types = "iccicc")

## subsetted summary
summary_app <- summary %>% filter(review_state == "approved")
summary_rej <- summary %>% filter(review_state == "rejected")
summary_hi <- summary %>% filter(review_state == "hasIssues")


## format the data
formattedSummary <- formattable(
  summary,
  list(
    n_subs = osub_formatter,
    n_str = str_formatter,
    max_str = str_formatter,
    disc_hh = disc_formatter,
    n_non_dwelling = nd_formatter,
    est_hh_eff = FALSE
  )
)

formattedSummaryApp <- formattable(
  summary_app,
  list(
    n_subs = osub_formatter,
    n_str = str_formatter,
    max_str = str_formatter,
    disc_hh = disc_formatter,
    n_non_dwelling = nd_formatter,
    est_hh_eff = FALSE
  )
)

formattedSummaryHi <- formattable(
  summary_hi,
  list(
    n_subs = osub_formatter,
    n_str = str_formatter,
    max_str = str_formatter,
    disc_hh = disc_formatter,
    n_non_dwelling = nd_formatter,
    est_hh_eff = FALSE
  )
)

formattedSummaryRej <- formattable(
  summary_rej,
  list(
    n_subs = osub_formatter,
    n_str = str_formatter,
    max_str = str_formatter,
    disc_hh = disc_formatter,
    n_non_dwelling = nd_formatter,
    est_hh_eff = FALSE
  )
)

formattedSegments <- formattable(segments,
                                 list(
                                   seg1_name = seg1_formatter,
                                   seg2_name = seg2_formatter,
                                   seg3_name = seg3_formatter,
                                   seg4_name = seg4_formatter,
                                   seg5_name = seg5_formatter,
                                   seg6_name = seg6_formatter,
                                   seg7_name = seg7_formatter,
                                   seg8_name = seg8_formatter,
                                   seg9_name = seg9_formatter,
                                   seg10_name = seg10_formatter,
                                   log1 = FALSE,
                                   log2 = FALSE,
                                   log3 = FALSE,
                                   log4 = FALSE,
                                   log5 = FALSE,
                                   log6 = FALSE,
                                   log7 = FALSE,
                                   log8 = FALSE,
                                   log9 = FALSE,
                                   log10 = FALSE
                                 ))

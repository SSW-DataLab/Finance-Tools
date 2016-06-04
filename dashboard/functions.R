library(googlesheets)
library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)


#### data preparation ####

#
PrepareData <- function(url) {
  workbook <- gs_url(url)

  # cleanup Primary
  # remove lines without shortcodes
  primary_df <- workbook %>%
    gs_read(ws = "Primary") %>%
    CleanPrimarySheet

  # cleanup FinancialStatus
  # only want most recent month for each shortcode
  financial_df <- workbook %>%
    gs_read(ws = "FinancialStatus") %>%
    CleanFinancialStatusSheet

  # join together
  df <- inner_join(primary_df, financial_df, by = "Shortcode") %>%
    mutate(ContractName = factor(ContractName, levels = unique(ContractName)))

  df
}


#
CleanPrimarySheet <- function(df) {

  df <- df %>%
    mutate(Shortcode = as.integer(gsub("[^0-9]", "", Shortcode))) %>%
    filter(!is.na(Shortcode)) %>%
    mutate_each(funs(gsub("[^0-9/]", "", .) %>% as.Date(format = "%m/%d/%Y")), contains("Date")) %>%
    mutate_each(funs(gsub("[^0-9.]", "", .) %>% as.numeric), TotalAwardAmount, CurrentYearDirect, CurrentYearIndirect) %>%
    select(
      Shortcode,
      ContractName,
      ContractStartDate,
      ContractEndDate,
      CurrentYearStartDate,
      CurrentYearEndDate,
      TotalAwardAmount,
      CurrentYearDirect,
      CurrentYearIndirect
    )

  df
}


#
CleanFinancialStatusSheet <- function(df) {

  df <- df %>%
    mutate(MonthYear = as.yearmon(MonthYear, format = "%b%y")) %>%  # convert to first-of-month dates
    select(
      Shortcode,
      MonthYear,
      Spent,
      Encumbered,
      AmountRemaining
    )

  df
}


#### plotting ####

#
CreateGatheredDateFrame <- function(df) {

  gathered_df <- df %>%
    select(ContractName, CurrentYearStartDate, CurrentYearEndDate, Spent, Encumbered, Remaining = AmountRemaining) %>%
    gather("category", "dollars", Spent, Encumbered, Remaining) %>%
    mutate(
      ContractName = factor(ContractName, levels = rev(levels(ContractName))),
      category     = factor(category, levels = c("Spent", "Encumbered", "Remaining"))
    ) %>%
    group_by(ContractName) %>%
    mutate(
      length = as.integer(CurrentYearEndDate - CurrentYearStartDate) * (dollars / sum(dollars)),
      start  = ifelse(category == "Spent", CurrentYearStartDate,
                      ifelse(category == "Encumbered", CurrentYearStartDate + lag(length, 1, order_by = category),
                             ifelse(category == "Remaining", CurrentYearStartDate + lag(length, 1, order_by = category) + lag(length, 2, order_by = category), NA)
                      )
      ) %>% as.Date,
      end    = start + length,
      width  = sum(dollars) %>% format(big.mark = ",", nsmall = 2) %>% paste0("$", .)
    ) %>%
    ungroup %>%
    arrange(ContractName, category)

  gathered_df
}

#
CreateGanttChart <- function(gathered_df) {

  new_levels <- gathered_df %>% arrange(desc(CurrentYearStartDate)) %$% ContractName %>% unique %>% as.character

  gantt_chart <- gathered_df %>%
    mutate(ContractName = factor(ContractName, levels = new_levels)) %>%
    ggplot(aes(color = category)) +
    geom_segment(aes(x = start, xend = end, y = ContractName, yend = ContractName, size = width)) +
    geom_vline(xintercept = as.integer(Sys.Date())) +
    ggtitle("Spending Progress Gantt Chart") +
    xlab("Date") +
    ylab("Contract")

  gantt_chart
}

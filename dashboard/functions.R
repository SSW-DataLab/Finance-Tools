library(googlesheets)
library(dplyr)
library(zoo)
library(tidyr)
library(ggvis)
library(scales)


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


# prepare raw data for display on Current tab
# one row per contract, displays accounting error
PrepareRawData <- function(df) {
  df <- df %>%
    mutate(Unaccounted = CurrentYearDirect - (Spent + Encumbered + AmountRemaining)) %>%
    mutate_at(vars(CurrentYearDirect, Spent, Encumbered, AmountRemaining, Unaccounted), funs(dollar)) %>%
    select(
      "Contract Name"           = ContractName,
      Shortcode,
      "Report Date"             = MonthYear,
      "Current Year Start Date" = CurrentYearStartDate,
      "Current Year End Date"   = CurrentYearEndDate,
      CurrentYearDirect,
      Spent,
      Encumbered,
      Remaining                 = AmountRemaining,
      Unaccounted
    )

  df
}

#### plotting ####

#
CreateGanttChart <- function(df) {

  # prepare data
  df <- df %>%
    select(ContractName, CurrentYearStartDate, CurrentYearEndDate, Spent, Encumbered, Remaining = AmountRemaining) %>%
    gather("category", "dollars", Spent, Encumbered, Remaining) %>%
    mutate(category = factor(category, levels = c("Spent", "Encumbered", "Remaining"))) %>%
    group_by(ContractName) %>%
    mutate(
      total  = sum(dollars),
      length = as.integer(CurrentYearEndDate - CurrentYearStartDate) * (dollars / total),
      start  = ifelse(
        category == "Spent",
        CurrentYearStartDate,
        ifelse(
          category == "Encumbered",
          CurrentYearStartDate + lag(length, 1, order_by = category),
          ifelse(
            category == "Remaining",
            CurrentYearStartDate + lag(length, 1, order_by = category) + lag(length, 2, order_by = category),
            NA
          ))) %>% as.Date,
      end    = start + length,
      height = sum(dollars)
    ) %>%
    ungroup %>%
    arrange(ContractName, category)

  # function to generate tooltips for each rectangle
  tooltip <- function(x) {
    if (is.null(x)) return()

    # x doesn't contain dollars, so get it from the original data
    dollars <- filter(df, ContractName == x$ContractName & category == x$category)$dollars

    lines <- c(
      x$ContractName,
      paste(dollar(dollars), x$category)
    ) %>% paste0(collapse = "<br />")
  }

  # create a ggvis plot
  plot <- df %>%
    ggvis(y = ~ContractName, fill = ~category) %>%
    layer_rects(x = ~start, x2 = ~end, height = band()) %>%
    add_tooltip(tooltip, "hover") %>%
    add_axis("x", title = "Date") %>%
    add_axis("y", title = "Contract", properties = axis_props(title = list(dy = -50)))

  plot
}

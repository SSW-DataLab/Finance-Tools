library(shiny)
library(dplyr)
library(ggvis)
library(DT)
library(scales)

source("functions.R")

GS_URL <- "https://docs.google.com/spreadsheets/d/1nga7F6MC3eQ3JdrPG9yV7ZBsipPOxmZLlvidrNeFEQM/"


shinyServer(function(input, output) {

  # reload data each connection
  withProgress(message = "loading data...", value = 1, {
    df <- PrepareData(GS_URL)
  })


  #### current tab ####

  # subset and prepare most recent data
  monthly_df <- filter(df, MonthYear == max(MonthYear))

  # generate ggvis plot
  monthly_df %>%
    CreateGanttChart %>%
    set_options(width = "auto", height = 600) %>%
    bind_shiny("gantt_chart")

  output$current_data <- renderDataTable({
    PrepareRawData(monthly_df)
  }, options = list(dom = 't'))


  #### historical tab ####

})

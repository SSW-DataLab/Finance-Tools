shinyServer(function(input, output) {

  # reload data each connection
  df       <- PrepareData(GS_URL)
  dates    <- unique(df$MonthYear)
  max_date <- max(dates)


  #### current tab ####

  output$gantt_chart <- renderPlot(

    height = 800,

    df %>%
      filter(MonthYear == max_date) %>%
      CreateGatheredDateFrame %>%
      CreateGanttChart
  )


  #### historical tab ####

})

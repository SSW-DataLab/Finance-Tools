library(shiny)
library(shinydashboard)
library(DT)
library(ggvis)

header <- dashboardHeader(
  title = "CADL Contracts"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Current Progress",
      icon    = icon("bar-chart", lib = "font-awesome"),
      tabName = "current"
    ),
    menuItem(
      "Historical Data",
      icon    = icon("clock-o", lib = "font-awesome"),
      tabName = "historical"
    )
  )
)

body <- dashboardBody(

  includeCSS("./custom.css"),

  tabItems(
    tabItem(
      "current",
      fluidRow(
        box(
          "Gantt Chart of Current Progress",
          width = 12,
          ggvisOutput("gantt_chart")
        )
      ),
      fluidRow(
        box(
          "Raw Data",
          width = 12,
          dataTableOutput("current_data"),
          br(),
          p("Chart and data account for <em>direct</em> expenses only."),
          p("'Unaccounted' is the difference between the yearly amount for the contract ('Current Year Direct') and the sum of 'Spent', 'Encumbered', and 'Remaining' from the most recent UM-provided report. If it isn't zero, there may be a problem.")
        )
      )
    ),

    tabItem(
      "historical",
      h2("Under Construction!")
    )
  )
)

dashboardPage(header, sidebar, body)

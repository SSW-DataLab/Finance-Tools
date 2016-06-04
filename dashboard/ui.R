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

  tabItems(
    tabItem(
      "current",
      box(
        "Gantt Chart of Current Progress",
        width = 12,
        plotOutput("gantt_chart")
      )
    ),

    tabItem(
      "historical",
      h2("Under Construction!")
    )
  )
)

dashboardPage(header, sidebar, body)

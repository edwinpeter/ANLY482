library(shiny)
library(shinydashboard)

sidebars <- (sidebarMenu(
  menuItem("Upload", tabName = "upload", icon = icon("upload")),
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Schedules", tabName = "schedules", icon = icon("calendar"))
))

tab1 <- tabItem(tabName = "upload",
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file1", "Choose CSV File",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    ),
                    tags$hr(),
                    checkboxInput("header", "Header", TRUE)
                  ),
                  mainPanel(
                    tableOutput("contents")
                  )
                )
)
tab2 <- tabItem(tabName = "dashboard",
                fluidPage(
                  
                  title = 'Select Table Rows',
                  
                  h1('Market Basket Data'),
                  
                  fluidRow(
                    column(width = 6,
                           box(
                             title = "MBA Data", width = NULL, status = "primary",
                             div(style = 'overflow-x: scroll', DT::dataTableOutput('x1'))
                           )),
                    column(6, plotOutput('x2', height = 500))
                  ),
                  
                  fluidRow(
                    #column(9, DT::dataTableOutput('x3')),
                    #column(3, verbatimTextOutput('x4'))
                  )
                  
                )
)


tab3 <- tabItem(tabName = "schedules",
                fluidPage(
                  
                  title = 'Select Table Rows',
                  
                  fluidRow(
                    column(width = 12,
                           box(
                             title = "Time Series Data", width = NULL, status = "primary",
                             div(style = 'overflow-x: scroll', DT::dataTableOutput('ts'))
                           ))
                  ),
                  
                  fluidRow(
                    #column(9, DT::dataTableOutput('x3')),
                    #column(3, verbatimTextOutput('x4'))
                  )
                  
                )
)
tablebody <- dashboardBody(
  tabItems(
    tab1,
    tab2,
    tab3
  )
)



dashboardPage(
  skin= c("red"),
  dashboardHeader(title = "Erumpere"),
  dashboardSidebar(sidebars),
  tablebody
)











#Sort this out 
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyTime)
library(timevis)

library(WriteXLS)
library(readxl)

library(dplyr)

library(DT)


########################################### CLIENT DB - Helper, CRUD methods ###########################################
source('clienthelpers.R')

########################################### Programs DB - Helper, CRUD methods ###########################################
source('programhelpers.R')

########################################### Activity DB - Helper , CRUD methods ###########################################
source('activityhelpers.R')

########################################### User Interface ###########################################



tab1 <- tabItem(tabName = "dashboard",
                fluidPage(
                  
                  title = 'Select Table Rows',
                  
                  h1('Overview'),
                  
                  fluidRow(
                    # column(width = 6,
                    #        box(
                    #          title = "MBA Data", width = NULL, status = "primary",
                    #          div(style = 'overflow-x: scroll', DT::dataTableOutput('x1'))
                    #        )),
                    # column(6, plotOutput('x2', height = 500))
                    #selectInput("program_client_name", "Select Client",c("")),
                    valueBoxOutput("value1"),
                    valueBoxOutput("value2"),
                    valueBoxOutput("value3")
                    
                  ),
                  
                  fluidRow(
                    #column(9, DT::dataTableOutput('x3')),
                    #column(3, verbatimTextOutput('x4'))
                    timevisOutput("timeline")
                    
                    
                  )
                  
                )
)


tab2 <- tabItem(tabName = "mba",
                fluidPage(
                  
                  title = 'Select Table Rows',
                  
                  fluidRow(
                    column(width = 12,
                           box(
                             title = "Market Basket Analysis", width = NULL, status = "primary",
                             div(style = 'overflow-x: scroll', DT::dataTableOutput('ts'))
                           ))
                  ),
                  
                  fluidRow(
                    #column(9, DT::dataTableOutput('x3')),
                    #column(3, verbatimTextOutput('x4'))
                  )
                  
                )
)

tab3 <- tabItem(tabName = "clientdb",
                fluidPage(
                  #use shiny js to disable the ID field
                  h1('Client Database'),
                  
                  shinyjs::useShinyjs(),
                  fluidRow(
                    column(width = 4,  
                           #input fields
                           tags$hr(),
                           h2('Enter new client:'),
                           shinyjs::hidden(textInput("id", "Id", "0")),
                           textInput("cname", "Client Name", ""),
                           textInput("address", "Address", ""),
                           textInput("postal", "Postal", ""),
                           textInput("officecontact", "Office Contact", ""),
                           textInput("centrecode", "Centre & Org Code", ""),
                           
                           #action buttons
                           actionButton("submitclient", "Submit"),
                           actionButton("updateclient", "Update")
                           
                    ),
                    column(width = 8,                  
                           DT::dataTableOutput("clientdb")
                           
                    )
                  )
                )
)

tab4 <- tabItem(tabName = "program",
                fluidPage(
                  #use shiny js to disable the ID field
                  h1('Programs Database'),

                  shinyjs::useShinyjs(),
                  fluidRow(
                    column(width = 5,
                           #input fields
                           tags$hr(),
                           h3('Enter POC details:'),
                           shinyjs::hidden(textInput("pid", "Id", "0")),
                           selectInput("program_client_name", "Select Client",c("")),
                           textInput("poc_name", "POC Name", ""),
                           textInput("poc_designation", "POC Designation", ""),
                           textInput("poc_email", "POC Email", ""),
                           textInput("poc_contact", "POC Contact", "")
                    ),
                    column(width = 5,
                           #input fields
                           tags$hr(),
                           h3('Enter Program details:'),
                           textInput("prog_location", "Program Location", ""),
                           selectInput("prog_age", "Select Ages",c("K1", "K2", "Mixed")),
                           textInput("prog_est_popn", "Program Estimated Size", ""),
                           textInput("prog_type", "Program Type", ""),
                           textInput("prog_duration", "Program Duration", ""),
                           shinyjs::hidden(textInput("client_date_created", "client_date_created", "")),
                           
                           
                           #action buttons
                           actionButton("submitprogram", "Submit"),
                           actionButton("updateprogram", "Update")
                    ),
                    column(width = 12,
                           DT::dataTableOutput("programdb", width="100%")

                    )
                  )
                )
)

tab5 <- tabItem(tabName = "activity",
                fluidPage(
                  #use shiny js to disable the ID field
                  h1('Enter Program Information'),
                  shinyjs::useShinyjs(),
                  fluidRow(
                    column(width = 4,  
                           #input fields
                           tags$hr(),
                           h2('Choose Prep Day Activities:'),
                           shinyjs::hidden(textInput("ppid", "Id", "0")),
                           selectInput("activity_client_date", "Select Program",c("")),
                           tags$hr(),
                           dateInput("activity_date", "Choose Date", "", format = "dd-mm-yy"),
                           selectInput("prep_id","Choose Prep Activity", prepList), #this is where we do drop down list
                           #timeInput("activity_start_time", "Select Start Time:", seconds = FALSE),
                           textInput("activity_start_time", "Select Start Time:", ""),
                           textInput("activity_duration", "Select Duration", ""),
                           #timeInput("activity_end_time", "Select End Time:", seconds = FALSE),
                           textInput("activity_end_time", "Select End Time:", ""),
                           textInput("activity_location", "Select Location", ""),
                           # dateRangeInput('dateRange',
                           #                label = 'Date range input: yyyy-mm-dd',
                           #                start = "", end = ""
                           # ),
                           
                           
                           #action buttons for prep day
                           actionButton("submitprepact", "Submit"),
                           actionButton("updateprepact", "Update"),

                           h2('Choose Actual Day Activities:'),
                           textInput("activity_date", "Choose Date", ""),
                           selectInput("prep_id","Choose Actual Day Activity",actList), #this is where we do drop down list
                           textInput("activity_start_time", "Select Start Time", ""),
                           textInput("activity_duration", "Select Duration", ""),
                           #activity_end_time
                           #activity_location
                           
                           #action buttons for prep day
                           actionButton("submitactlact", "Submit"),
                           actionButton("updateactlact", "Update")

                           
                           
                    ),
                    column(width = 8,                  
                           DT::dataTableOutput("prepprogramdb")
                           
                    ),
                    column(width = 8,                  
                           DT::dataTableOutput("actldb")
                           
                    )
                  )
                )
)

tablebody <- dashboardBody(
  tabItems(
    tab1,
    tab2,
    tab3,
    tab4,
    tab5
  )
)

sidebars <- (sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Analysis 1 - MBA", tabName = "mba", icon = icon("signal")),
  menuItem("Client Database", tabName = "clientdb", icon = icon("users")),
  menuItem("Programs Database", tabName = "program", icon = icon("list-ol")),
  menuItem("Activity", tabName = "activity", icon = icon("pied-piper-alt"))
  
))

ui <- dashboardPage(
  skin= c("red"),
  dashboardHeader(title = "Erumpere"),
  dashboardSidebar(sidebars),
  tablebody
)


########################################### Server ###########################################

source('server.R')

########################################### Start the whole thing ###########################################

shinyApp(ui = ui, server = server)
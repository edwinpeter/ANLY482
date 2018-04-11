# Libraries Used:

# For shiny visualization
library(shiny)
library(shinydashboard)
library(shinyjs)
library(timevis)

# For reading and write XLS files to local database
library(WriteXLS)
library(readxl)

# To pull data from XLS
library(dplyr)

# To put data into data table
library(DT)

# Plotting libraries for graphs
library(plotly)
library(ggplot2)

# For time function
library(lubridate)

# For rules and market basket analysis
library(arules)
library(reshape2)
library(arulesViz)
library(viridisLite)
library(tcltk)
library(plyr)
library(reshape2)

########################################### CLIENT DB - Helper, CRUD methods ###########################################
source('clienthelpers.R')

########################################### Programs DB - Helper, CRUD methods ###########################################
source('programhelpers.R')

########################################### Activity DB - Helper , CRUD methods ###########################################
source('activityhelpers.R')

########################################### Activity DB - Helper , CRUD methods ###########################################
source('mbahelpers.R')

########################################### Dashboard - Helper ###########################################
source('dashboardhelpers.R')

########################################### User Interface ###########################################



tab1 <- tabItem(tabName = "dashboard",
                fluidPage(
                  
                  title = 'Select Table Rows',
                  
                  h1('Overview'),
                  
                  fluidRow(
                    valueBoxOutput("value1"),
                    valueBoxOutput("value2"),
                    valueBoxOutput("value3")
                    
                  ),
                  
                  fluidRow(
                    timevisOutput("timeline"),
                    DT::dataTableOutput("timetable")
                  ),
                  tags$hr(),
                  fluidRow(
                    column(6, plotlyOutput("frequencyplotprep")),
                    column(6, plotlyOutput("frequencyplotact"))
                    
                  )
                )
)


tab2 <- tabItem(tabName = "mba",
                fluidPage(
                  
                  h1('Market Basket Analysis'),
                  
                  # Sidebar layout with input and output definitions ----
                  sidebarLayout(
                    
                    # Sidebar to demonstrate various slider options ----
                    sidebarPanel(
                      
                      # Input: Decimal interval with step value ----
                      sliderInput("supp", "Support:",
                                  min = 0.01, max = 1,
                                  value = 0.01, step = 0.01),
                      
                      # Input: Decimal interval with step value ----
                      sliderInput("conf", "Confidence:",
                                  min = 0.1, max = 1,
                                  value = 0.5, step = 0.1)
                      
                    ),
                    mainPanel(
                      fluidRow(
                        column(width = 12,                  
                               plotlyOutput("mbagraph")
                               
                        )
                      )
                      
                    )
                  )
                )
)

tab3 <- tabItem(tabName = "clientdb",
                fluidPage(
                  h1('Client Database'),
                  
                  shinyjs::useShinyjs(),
                  fluidRow(
                    column(width = 4,  
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

tab4 <- tabItem(tabName = "booking",
                fluidPage(
                  h1('Bookings Database'),
                  
                  shinyjs::useShinyjs(),
                  fluidRow(
                    column(width = 5,
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
                  h1('Enter Program Information'),
                  shinyjs::useShinyjs(),
                  fluidRow(
                    column(width = 4,  
                           tags$hr(),
                           h2('Choose Prep Day Activities:'),
                           shinyjs::hidden(textInput("ppid", "Id", "0")),
                           selectInput("activity_client_date", "Select Program",c("")),
                           tags$hr(),
                           dateInput("activity_date", "Choose Date", "", format = "dd-mm-yy"),
                           selectInput("prep_id","Choose Prep Activity", prepList), #this is where we do drop down list
                           textInput("activity_start_time", "Select Start Time:", ""),
                           textInput("activity_duration", "Select Duration", ""),
                           textInput("activity_location", "Select Location", ""),
                           
                           #action buttons for prep day
                           actionButton("submitprepact", "Submit"),
                           actionButton("updateprepact", "Update")
                    ),
                    column(width = 8,                  
                           DT::dataTableOutput("prepprogramdb")
                           
                    )
                  ),
                  tags$hr(),
                  
                  fluidRow(
                    column(width = 4,  
                           tags$hr(),
                           h2('Choose Actual Day Activities:'),
                           shinyjs::hidden(textInput("aid", "Id", "0")),
                           
                           selectInput("actual_id","Choose Actual Day Activity", actList), #this is where we do drop down list
                           dateInput("actual_start_date", "Choose Start Date", "", format = "dd-mm-yy"),
                           textInput("actual_start_time", "Select Start Time:", ""),
                           dateInput("actual_end_date", "Choose End Date", "", format = "dd-mm-yy"),
                           textInput("actual_end_time", "Select End Time:", ""),
                           textInput("actual_activity_location", "Select Location", ""),
                           
                           #action buttons for prep day
                           actionButton("submitactprogram", "Submit"),
                           actionButton("updateactprogram", "Update")
                    ),
                    column(width = 8,                  
                           DT::dataTableOutput("actprogramdb")
                           
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
  menuItem("Bookings Database", tabName = "booking", icon = icon("list-ol")),
  menuItem("Activity Database", tabName = "activity", icon = icon("pied-piper-alt"))
  
))

ui <- dashboardPage(
  skin= c("red"),
  dashboardHeader(title = "Erumpere"),
  dashboardSidebar(sidebars),
  tablebody
)


########################################### Server ###########################################

source('server.R')

########################################### Link Server and UI ###########################################

shinyApp(ui = ui, server = server)
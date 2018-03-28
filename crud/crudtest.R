library(shiny)
library(shinyjs)
library(WriteXLS)
library(readxl)
library(dplyr)

########################################### CLIENT DB - Helper, CRUD methods ###########################################
source('clienthelpers.R')

########################################### Programs DB - Helper, CRUD methods ###########################################
#source('programhelpers.R')

########################################### Programs DB - Helper, CRUD methods ###########################################
source('samplecode.R')




########################################### User Interface ###########################################
tab1 <- tabItem(tabName = "dashboard",
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


tab2 <- tabItem(tabName = "schedules",
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
                           shinyjs::disabled(textInput("id", "Id", "0")),
                           textInput("cname", "Client Name", ""),
                           textInput("address", "Address", ""),
                           textInput("postal", "Postal", ""),
                           textInput("officecontact", "Office Contact", ""),
                           textInput("centrecode", "Centre & Org Code", ""),
                           
                           #action buttons
                           actionButton("submit", "Submit"),
                           #actionButton("new", "New"),
                           #actionButton("delete", "Delete"),
                           actionButton("update", "Update")
                           
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
                  h1('NOT DONE YET Programs Database'),

                  shinyjs::useShinyjs(),
                  fluidRow(
                    column(width = 4,
                           #input fields
                           tags$hr(),
                           h2('Enter new Program:'),
                           shinyjs::disabled(textInput("id", "Id", "0")),
                           textInput("name", "Client Name", ""),
                           textInput("address", "Address", ""),
                           textInput("postal", "Postal", ""),
                           textInput("officecontact", "Office Contact", ""),
                           textInput("centrecode", "Centre & Org Code", ""),

                           #action buttons
                           actionButton("submit", "Submit"),
                           actionButton("new", "New"),
                           actionButton("delete", "Delete")

                    ),
                    column(width = 8
                           #DT::dataTableOutput("responses")

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
                           textInput("activity_date", "Choose Date", ""),
                           selectInput("prep_id","Choose Prep Activity",prepList), #this is where we do drop down list
                           textInput("activity_start_time", "Select Start Time", ""),
                           textInput("activity_duration", "Select Duration", ""),
                           #activity_end_time
                           #activity_location
                           
                           h2('Choose Prep Day Activities:'),
                           textInput("activity_date", "Choose Date", ""),
                           selectInput("prep_id","Choose Actual Day Activity",actList), #this is where we do drop down list
                           textInput("activity_start_time", "Select Start Time", ""),
                           textInput("activity_duration", "Select Duration", ""),
                           #activity_end_time
                           #activity_location
                           
                           #action buttons
                           actionButton("submit", "Submit"),
                           actionButton("new", "New"),
                           actionButton("delete", "Delete")
                           
                    ),
                    column(width = 8                  
                           #DT::dataTableOutput("responses")
                           
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
    tab5,
  )
)

sidebars <- (sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Schedules", tabName = "schedules", icon = icon("calendar")),
  menuItem("Client Database", tabName = "clientdb", icon = icon("users")),
  menuItem("Programs Database", tabName = "program", icon = icon("list-ol")),
  menuItem("Activity", tabName = "activity", icon = icon("dolly"))
  
))

ui <- dashboardPage(
  skin= c("red"),
  dashboardHeader(title = "Erumpere"),
  dashboardSidebar(sidebars),
  tablebody
)


########################################### Server ###########################################

server <- function(input, output, session) {
  shinyjs::disable('update')
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata_client()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id == "0") {
      CreateData_client(formData())
      UpdateInputs_client(CreateDefaultRecord_client(), session)
    }
  }, priority = 1)
  
  #UPDATE
  observeEvent(input$update, {
    if (input$id != "0") {
      UpdateData_client(formData())
      UpdateInputs_client(CreateDefaultRecord_client(), session)
    } 
  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$clientdb_rows_selected, {
    shinyjs::enable('update')
    if (length(input$clientdb_rows_selected) > 0) {
      x <- ReadData_client()[input$clientdb_rows_selected,]
      data <- ReadData_client()[input$clientdb_rows_selected, ]
      UpdateInputs_client(data, session)
    }
    
  })
  
  # display table
  output$clientdb <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$update
    ReadData_client()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata_client()$fields)[-1]
  )     
}

########################################### Start the whole thing ###########################################

shinyApp(ui = ui, server = server)
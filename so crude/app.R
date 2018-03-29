library(shiny)
library(readxl)
library(WriteXLS)
library(shinydashboard)
# library(dplyr)

CastData <- function(data) {
  datar <- data.frame(cname = data["cname"], 
                      address = data["address"], 
                      postal = data["postal"], 
                      officecontact = data["officecontact"], 
                      centrecode = data["centrecode"], 
                      stringsAsFactors = FALSE)
  
  #rownames(datar) <- data["id"]
  return (datar)
}


CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", cname = "haha", address="addressLOL", postal="postalLOL", officecontact="officecontactLOL", centrecode="centrecodeLOL"))
  return (mydefault)
}

UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "cname", value = unname(data["cname"]))
  updateTextInput(session, "address", value = unname(data["address"]))
  updateTextInput(session, "postal", value = unname(data["postal"]))
  updateTextInput(session, "officecontact", value = unname(data["officecontact"]))
  updateTextInput(session, "centrecode", value = unname(data["centrecode"]))
  # updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
  # updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
}

GetNextId <- function() {
  responses <- loadData()
  if (exists("responses")) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}


CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
    print("succ")
  } else {
    responses <<- data
  }
  saveData(responses)
}


ReadData <- function() {
  #if (!exists("responses")){
  responses <- loadData()  
  #}
}

UpdateData <- function(data) {
  data <- CastData(data)
  
  responses[row.names(responses) == row.names(data), ] <<- data
  saveData(data)
}


DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
  saveData(data)
}


GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              cname = "cname", 
              address = "address", 
              postal = "postal", 
              officecontact = "officecontact",
              centrecode = "centrecode"
  )
  
  result <- list(fields = fields)
  return (result)
}



# outputDir <- "/Users/Edwin/Desktop/z/crud/responses"

#Shafiq's Directory
outputDir <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/so crud/responses"

saveData <- function(responses) {
  #responses <- t(responses)
  # Create a unique file name
  #fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  fileName <- sprintf("%s_%s.xls", as.integer(Sys.time()), "data")
  # Write the file to the local system
  # write.csv(
  #   x = responses,
  #   file = file.path(outputDir, fileName), 
  #   row.names = FALSE, quote = TRUE
  # )
  WriteXLS::WriteXLS(
    x = responses,
    ExcelFileName = file.path(outputDir, fileName)
  )
  
}


loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  # Just read latest file
  #data <- read.table(tail(files, n=1))
  data <- readxl::read_xls(tail(files, n=1))
  
  #data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  #data <- do.call(rbind, data)
  data
}

loadPrepAct <- function() {
  # Read all the files into a list
  # Just read latest file
  #data <- read.table(tail(files, n=1))
  data <- readxl::read_xls("D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/so crude/activity_data/prep_activities.xls")
  
  #data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  #data <- do.call(rbind, data)
  data
}

prepList <- pull(loadPrepAct(),"activity_name")

loadActualAct <- function() {
  # Read all the files into a list
  # Just read latest file
  #data <- read.table(tail(files, n=1))
  data <- readxl::read_xls("D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/so crude/activity_data/actual_activities.xls")
  
  #data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  #data <- do.call(rbind, data)
  data
}

actList <- pull(loadActualAct(),"activity_name")

# Define UI for application that draws a histogram
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
                           #shinyjs::disabled(textInput("id", "Id", "0")),
                           textInput("cname", "Client Name", ""),
                           textInput("address", "Address", ""),
                           textInput("postal", "Postal", ""),
                           textInput("officecontact", "Office Contact", ""),
                           textInput("centrecode", "Centre & Org Code", ""),
                           
                           #action buttons
                           actionButton("submit", "Submit"),
                           actionButton("new", "New"),
                           actionButton("delete", "Delete")
                           
                    ),
                    column(width = 8,                  
                           DT::dataTableOutput("responses")
                           
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
                    column(width = 4,  
                           #input fields
                           tags$hr(),
                           h2('Enter new Program:'),
                           shinyjs::disabled(textInput("id", "Id", "0")),
                           textInput("cname", "Client Name", ""),
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
                           selectInput("prep_id","Choose Prep Activity",prepList),
                           textInput("activity_start_time", "Select Start Time", ""),
                           textInput("activity_duration", "Select Duration", ""),
                           #activity_end_time
                           #activity_location
                           
                           h2('Choose Prep Day Activities:'),
                           textInput("activity_date", "Choose Date", ""),
                           selectInput("prep_id","Choose Actual Day Activity",actList),
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
    tab5
  )
)

sidebars <- (sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Schedules", tabName = "schedules", icon = icon("calendar")),
  menuItem("Client Database", tabName = "clientdb", icon = icon("users")),
  menuItem("Programs Database", tabName = "program", icon = icon("list-ol")),
  menuItem("Activity Management", tabName = "activity", icon = icon("cog", lib = "glyphicon"))
  
))

ui <- dashboardPage(
  skin= c("red"),
  dashboardHeader(title = "Erumpere"),
  dashboardSidebar(sidebars),
  tablebody
)


server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    # if (input$id != "0") {
    #   UpdateData(formData())
    # } else {
    #   CreateData(formData())
    #   UpdateInputs(CreateDefaultRecord(), session)
    # }
    CreateData(formData())
    #UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  shinyjs::disable("id")
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
}


# Run the application 
shinyApp(ui = ui, server = server)


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

###################################################################################################################
# the pull function is used to return all the values in the specified column name and put is in a list format
# pull function requires dplyr library
actList <- pull(loadActualAct(),"activity_name")
prepList <- pull(loadPrepAct(),"activity_name")
###################################################################################################################


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
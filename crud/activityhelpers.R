########################################### Activity - Helper, CRUD methods ###########################################

#directoryprepact <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/prep_activities.xls"
#directoryactualact <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/actual_activities.xls"
#directoryprepprog <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/prep_list.xls"
#directoryactualprog <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/actual_list.xls"

directoryprepact <- "/Users/Edwin/ANLY482/crud/Data/prep_activities.xls"
directoryactualact <- "/Users/Edwin/ANLY482/crud/Data/actual_activities.xls"

directoryprepprog <- "/Users/Edwin/ANLY482/crud/Data/prep_list.xls"
directoryactualprog <- "/Users/Edwin/ANLY482/crud/Data/actual_list.xls"


loadPrepAct <- function() {
  # Read all the files into a list
  # Just read latest file
  data <- readxl::read_xls(directoryprepact)
  data
}


loadActualAct <- function() {
  # Read all the files into a list
  # Just read latest file
  data <- readxl::read_xls(directoryactualact)
  data
}

###################################################################################################################
# the pull function is used to return all the values in the specified column name and put is in a list format
# pull function requires dplyr library
actList <- pull(loadActualAct(),"activity_name")
actldb <- as.data.frame(loadActualAct())
prepList <- pull(loadPrepAct(),"activity_name")
prepdb <- as.data.frame(loadPrepAct())

#Table methods


GetTableMetadata_actl <- function() {
  fields <- c(id = "Id", 
              actual_id = "Activity ID", 
              activity_name = "Activity Name", 
              activity_desc = "Description", 
              proj_loc = "Location",
              loc_name = "Location Name"
  )
  
  result <- list(fields = fields)
  return (result)
}

GetTableMetadata_prep <- function() {
  fields <- c(id = "Id", 
              actual_id = "Activity ID", 
              activity_name = "Activity Name", 
              activity_desc = "Description"
  )
  
  result <- list(fields = fields)
  return (result)
}
#Read
ReadData_prep <- function() {
  if (exists("prepdb")) {
    prepdb
  }
}

ReadData_actl <- function() {
  if (exists("actldb")) {
    actldb
  }
}

###################################### Manage Prep Day Programs #########################################################

outputDir_prepprogram <- "/Users/Edwin/ANLY482/crud/PrepActivityDB/"

saveData_prepprogram <- function(prepprogramdb) {
  fileName <- sprintf("%s_%s.xls", as.double(format(Sys.time(), "%y%m%d%H%M%S")), "prepprog")
  WriteXLS::WriteXLS(
    x = prepprogramdb,
    ExcelFileName = file.path(outputDir_prepprogram, fileName)
  )
  
}

loadData_prepprogram <- function() {
  files <- list.files(outputDir_prepprogram, full.names = TRUE)
  data <- readxl::read_xls(tail(files, n=1))
  data
}

prepprogramdb <- loadData_prepprogram()
prepprogramdb <- as.data.frame(prepprogramdb)

GetTableMetadata_prepprogram <- function() {
  fields <- c(ppid = "Id", 
              activity_client_date = "Centre - Program Created", 
              prep_id = "Activity ID",
              activity_date = "Date of Activity", 
              activity_start_time = "Activity Start Time", 
              activity_duration = "Activity Duration",
              activity_end_time = "Activity End Time",
              activity_location = "Location"
  )
  
  result <- list(fields = fields)
  return (result)
}

GetNextId_prepprogram <- function() {
  if (exists("prepprogramdb") && nrow(prepprogramdb) > 0) {
    max(as.integer(rownames(prepprogramdb))) + 1
  } else {
    return (1)
  }
}

#Create
CreateData_prepprogram <- function(data) {
  # drops <- c('activity_start_time.sec',
  #            'activity_start_time.min',
  #            'activity_start_time.mday',
  #            'activity_start_time.mon',
  #            'activity_start_time.year')
  # data[ , !(names(data) %in% drops)]
  
  data <- CastData_prepprogram(data)
  

  rownames(data) <- GetNextId_prepprogram()
  
  if (exists("prepprogramdb")) {
    prepprogramdb <<- rbind(prepprogramdb, data)
  } else {
    prepprogramdb <<- data
  }
  print(prepprogramdb)
  saveData_prepprogram(prepprogramdb)
}

#Read
ReadData_prepprogram <- function() {
  if (exists("prepprogramdb")) {
    prepprogramdb
  }
}

ReadData_prepprogram_filter <- function(filter_prog){
  if (exists("prepprogramdb")) {
    prepprogramdb
    filtered <- filter(prepprogramdb, activity_client_date == filter_prog)
  }
}

#Update
UpdateData_prepprogram <- function(data) {
  data <- CastData_prepprogram(data)
  prepprogramdb[row.names(prepprogramdb) == row.names(data), ] <<- data
  shinyjs::disable('updateprepprogram')
  shinyjs::enable('submitprepprogram')
  saveData_prepprogram(prepprogramdb)
  
}

CastData_prepprogram <- function(data) {
  act_date <- as.Date("1970-01-01") + as.double(data['activity_date'])
  datar <- data.frame(
                      prep_id = data["prep_id"], 
                      activity_client_date = data["activity_client_date"],
                      activity_date = as.character(act_date), 
                      activity_start_time = data["activity_start_time"], 
                      activity_duration = data["activity_duration"], 
                      activity_end_time = data["activity_end_time"], 
                      activity_location = data["activity_location"], 
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["ppid"]
  return (datar)
}

CreateDefaultRecord_prepprogram <- function(d) {
  mydefault <- CastData_prepprogram_default(list(ppid = "0", activity_client_date = d['activity_client_date'], prep_id = "", activity_date="", activity_start_time="", activity_duration= "", activity_end_time = "", activity_location =""))
  print(mydefault)
  return (mydefault)
}

UpdateInputs_prepprogram <- function(data, session) {
  updateDateInput(session, "activity_date", value=NA)
  updateTextInput(session, "activity_location", value="")
  updateTextInput(session, "activity_start_time", value="")
  updateTextInput(session, "activity_duration", value="")
  updateTextInput(session, "activity_end_time", value="")
  
  # print(data)
  # updateTextInput(session, "ppid", value = unname(rownames(data)))
  # #updateTextInput(session, "activity_client_date", value = unname(data["activity_client_date"]))
  # updateSelectInput(session, "activity_client_date", selected = data["activity_client_date"])
  # #updateTextInput(session, "prep_id", value = unname(data["prep_id"]))
  # updateTextInput(session, "prep_id", value = "")
  # updateTextInput(session, "activity_date", value = unname(data["activity_date"]))
  # #updateTextInput(session, "activity_start_time", value = unname(data["activity_start_time"]))
  # updateTextInput(session, "activity_start_time", value = "")
  # updateTextInput(session, "activity_duration", value = unname(data["activity_duration"]))
  # updateTextInput(session, "activity_end_time", value = unname(data["activity_end_time"]))
  # updateTextInput(session, "activity_location", value = unname(data["activity_location"]))
}
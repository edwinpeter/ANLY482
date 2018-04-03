
directoryprepact <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/prep_activities.xls"
directoryactualact <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/actual_activities.xls"
directoryprepprog <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/prep_list.xls"
directoryactualprog <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Data/actual_list.xls"


# directoryprepact <- "/Users/Edwin/ANLY482/ANLY482/crud/Data/prep_activities.xls"
# directoryactualact <- "/Users/Edwin/ANLY482/ANLY482/crud/Data/actual_activities.xls"
# directoryprepprog <- "/Users/Edwin/ANLY482/ANLY482/crud/Data/prep_list.xls"
# directoryactualprog <- "/Users/Edwin/ANLY482/ANLY482/crud/Data/actual_list.xls"


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
GetTableMetadata_prep <- function() {
  fields <- c(id = "Id", 
              actual_id = "Activity ID", 
              activity_name = "Activity Name", 
              activity_desc = "Description"
  )
  
  result <- list(fields = fields)
  return (result)
}

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

loadPrepProg <- function() {
  # Read all the files into a list
  # Just read latest file
  data <- readxl::read_xls(directoryprepprog)
  data
}

prepprogdb <- as.data.frame(loadPrepProg())

#Read
ReadData_prepprog <- function() {
  if (exists("prepprogdb")) {
    prepprogdb
  }
}

#Filter by centre code and prog_date_time_created
prepprogdb <- filter(prepprogdb, client_center_org_code == "D.C-G8")
prepprogdb <- filter(prepprogdb, prog_date_time_created == "11:00:00 22/08/2017")

#Table methods
GetTableMetadata_prepprog <- function() {
  fields <- c(id = "Id", 
              client_center_org_code = "Centre & Org Code", 
              prog_date_time_created = "Program Date Created", 
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

###################################### Manage Actual Day Programs #########################################################

loadActualProg <- function() {
  # Read all the files into a list
  # Just read latest file
  data <- readxl::read_xls(directoryactualprog)
  data
}

actlprogdb <- as.data.frame(loadActualProg())

#Read
ReadData_actlprog <- function() {
  if (exists("actlprogdb")) {
    actlprogdb
  }
}

# filter(prepprogdb, client_center_org_code == "@thepark-LSH")

#Filter by centre code and prog_date_time_created
actlprogdb <- filter(actlprogdb, client_center_org_code == "D.C-G8")
actlprogdb <- filter(actlprogdb, prog_date_time_created == "11:00:00 22/08/2017")


#Table methods
GetTableMetadata_actlprog <- function() {
  fields <- c(id = "Id", 
              client_center_org_code = "Centre & Org Code", 
              prog_date_time_created = "Program Date Created", 
              actual_id = "Activity ID",
              activity_start_date = "Activity Start Date", 
              activity_start_time = "Activity Start Time", 
              activity_duration = "Activity Duration",
              activity_end_date = "Activity End Date", 
              activity_end_time = "Activity End Time",
              activity_location = "Location"
  )
  
  result <- list(fields = fields)
  return (result)
}

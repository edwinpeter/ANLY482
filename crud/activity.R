
#directoryprep <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/so crude/activity_data/prep_activities.xls"
#directoryactual <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/so crude/activity_data/actual_activities.xls"

directoryprep <- "/Users/Edwin/ANLY482/ANLY482/crud/Data/prep_activities.xls"
directoryactual <- "/Users/Edwin/ANLY482/ANLY482/crud/Data/actual_activities.xls"

loadPrepAct <- function() {
  # Read all the files into a list
  # Just read latest file
  data <- readxl::read_xls(directoryprep)
  data
}


loadActualAct <- function() {
  # Read all the files into a list
  # Just read latest file
  data <- readxl::read_xls(directoryactual)
  data
}

###################################################################################################################
# the pull function is used to return all the values in the specified column name and put is in a list format
# pull function requires dplyr library
actList <- pull(loadActualAct(),"activity_name")
prepList <- pull(loadPrepAct(),"activity_name")
###################################################################################################################
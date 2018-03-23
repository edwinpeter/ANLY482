library(dplyr)

######################################################################
# Create the first quadrant class
#
# This is used to represent a coordinate in the first quadrant.
FirstQuadrant <- setClass(
  # Set the name for the class
  "FirstQuadrant",
  
  # Define the slots
  slots = c(
    x = "numeric",
    abc = "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    x = 0.0,
    y = 0.0
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    # symbools are accessed using @
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)

x <- FirstQuadrant()


y <- FirstQuadrant(x=5,abc=7)
y
y@x
y@abc

PrepList <- setClass(
  #Set name for class
  "PrepList",
  
  #Define variables
  slots = c(
    client_center_org_code = "character",
    prog_date_time_created = "character",
    prep_id = "character",
    activity_date = "character",
    activity_start_time = "character",
    activity_duration = "character",
    activity_end_time = "character",
    activity_location = "character"
  ),
  
  # #set default values (optional)
  # prototype = list(
  #   client_center_org_code = "na"
  # ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  #can be called using validObject(theObject)
  # validity=function(object)
  # {
  #   if(sum(object@velocity^2)>100.0) {
  #     return("The velocity level is out of bounds.")
  #   }
  #   return(TRUE)
  # }
  
)

###########################################R######################################
#create methods to get data

x <- import("D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/so crude/activity_data/actual_activities.xls")


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

prepList <- loadPrepAct()

actual_id <- prepList[1]
activity_name <- prepList[2]
activity_desc <- prepList[3]

for(i in actual_id){
  # print(i)
  # print(typeof(i))
  if('t&d' == i){
    print(i)
  }
}

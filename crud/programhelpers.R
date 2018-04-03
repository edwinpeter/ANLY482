########################################### Programs DB - Helper, CRUD methods ###########################################

#outputDir_client <- "ProgramDB"
# outputDir_program <- "/Users/Edwin/ANLY482/ANLY482/crud/ProgramDB"
outputDir_program <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/ProgramDB"


saveData_program <- function(programdb) {
  fileName <- sprintf("%s_%s.xls", as.double(format(Sys.time(), "%y%m%d%H%M%S")), "program")
  WriteXLS::WriteXLS(
    x = programdb,
    ExcelFileName = file.path(outputDir_program, fileName)
  )
  
}

loadData_program <- function() {
  files <- list.files(outputDir_program, full.names = TRUE)
  data <- readxl::read_xls(tail(files, n=1))
  data
}

programdb <- loadData_program()
programdb <- as.data.frame(programdb)

GetTableMetadata_program <- function() {
  fields <- c(pid = "Id", 
              program_client_name = "Client", 
              poc_name = "POC Name", 
              poc_designation = "POC Designation", 
              poc_email = "POC Email",
              poc_contact = "POC Contact",
              prog_location = "Program Location",
              prog_age = "Age Group",
              prog_est_popn = "Estimated Size",
              prog_type = "Program Type",
              prog_duration = "Program Day(s)"
              
  )
  
  result <- list(fields = fields)
  return (result)
}

GetNextId_program <- function() {
  if (exists("programdb") && nrow(programdb) > 0) {
    max(as.integer(rownames(programdb))) + 1
  } else {
    return (1)
  }
}

#Create
CreateData_program <- function(data) {
  
  data <- CastData_program(data)
  rownames(data) <- GetNextId_program()
  if (exists("programdb")) {
    programdb <<- rbind(programdb, data)
  } else {
    programdb <<- data
  }
  
  saveData_program(programdb)
}

#Read
ReadData_program <- function() {
  if (exists("programdb")) {
    programdb
  }
}

#Update
UpdateData_program <- function(data) {
  data <- CastData_program(data)
  programdb[row.names(programdb) == row.names(data), ] <<- data
  shinyjs::disable('updateprogram')
  shinyjs::enable('submitprogram')
  saveData_program(programdb)
  
}

CastData_program <- function(data) {
  print(data)
  datar <- data.frame(program_client_name = data["program_client_name"],
                      poc_name = data["poc_name"], 
                      poc_designation = data["poc_designation"], 
                      poc_email = data["poc_email"], 
                      poc_contact = data["poc_contact"], 
                      prog_location = data["prog_location"], 
                      prog_age = data["prog_age"], 
                      prog_est_popn = data["prog_est_popn"], 
                      prog_type = data["prog_type"], 
                      prog_duration = data["prog_duration"], 
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["pid"]
  return (datar)
}

CreateDefaultRecord_program <- function() {
  mydefault <- CastData_program(list(pid = "0", program_client_name= "", poc_name = "", poc_designation="", poc_email="", poc_contact= "", prog_location = "", prog_age ="", prog_est_popn ="", prog_type ="", prog_duration =""))
  return (mydefault)
}

UpdateInputs_program <- function(data, session) {
  updateTextInput(session, "pid", value = unname(rownames(data)))
  updateTextInput(session, "poc_name", value = unname(data["poc_name"]))
  updateTextInput(session, "poc_designation", value = unname(data["poc_designation"]))
  updateTextInput(session, "poc_email", value = unname(data["poc_email"]))
  updateTextInput(session, "poc_contact", value = unname(data["poc_contact"]))
  updateTextInput(session, "prog_location", value = unname(data["prog_location"]))
  updateCheckboxInput(session, "prog_age", value = as.logical(data["prog_age"]))
  updateTextInput(session, "prog_est_popn", value = unname(data["prog_est_popn"]))
  updateTextInput(session, "prog_type", value = unname(data["prog_type"]))
  updateTextInput(session, "prog_duration", value = unname(data["prog_duration"]))
}







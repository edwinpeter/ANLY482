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
              pname = "Program Name", 
              paddress = "Address", 
              ppostal = "Postal", 
              pofficecontact = "officecontact",
              pcentrecode = "centrecode"
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
  datar <- data.frame(pname = data["pname"], 
                      paddress = data["paddress"], 
                      ppostal = data["ppostal"], 
                      pofficecontact = data["pofficecontact"], 
                      pcentrecode = data["pcentrecode"],
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["pid"]
  return (datar)
}

CreateDefaultRecord_program <- function() {
  mydefault <- CastData_program(list(pid = "0", pname = "", paddress="", ppostal="", pofficecontact= "", pcentrecode=""))
  return (mydefault)
}

UpdateInputs_program <- function(data, session) {
  updateTextInput(session, "pid", value = unname(rownames(data)))
  updateTextInput(session, "pname", value = unname(data["pname"]))
  updateTextInput(session, "paddress", value = unname(data["paddress"]))
  updateTextInput(session, "ppostal", value = unname(data["ppostal"]))
  updateTextInput(session, "pofficecontact", value = unname(data["pofficecontact"]))
  updateTextInput(session, "pcentrecode", value = unname(data["pcentrecode"]))
}


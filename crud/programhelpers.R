########################################### Programs DB - Helper, CRUD methods ###########################################


outputDir_program <- "/Users/Edwin/Desktop/z/crud/ProgramDB"

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
  fields <- c(id = "Id", 
              cname = "Client Name", 
              address = "Address", 
              postal = "Postal", 
              officecontact = "officecontact",
              centrecode = "centrecode"
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
  print(rownames)
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
  shinyjs::disable('update')
  saveData_program(programdb)
  
}

#Delete
DeleteData_program <- function(data) {
  programdb <<- programdb[row.names(programdb) != unname(data["id"]), ]
}

CastData_program <- function(data) {
  datar <- data.frame(name = data["cname"], 
                      address = data["address"], 
                      postal = data["postal"], 
                      officecontact = data["officecontact"], 
                      centrecode = data["centrecode"],
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

CreateDefaultRecord_program <- function() {
  mydefault <- CastData_program(list(id = "0", cname = "", address="", postal="", officecontact= "", centrecode=""))
  return (mydefault)
}

UpdateInputs_program <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "cname", value = unname(data["cname"]))
  updateTextInput(session, "address", value = unname(data["address"]))
  updateTextInput(session, "postal", value = unname(data["postal"]))
  updateTextInput(session, "officecontact", value = unname(data["officecontact"]))
  updateTextInput(session, "centrecode", value = unname(data["centrecode"]))
}
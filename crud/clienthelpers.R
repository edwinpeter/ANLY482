########################################### CLIENT DB - Helper, CRUD methods ###########################################

#outputDir_client <- "ClientDB"
outputDir_client <- "/Users/Edwin/ANLY482/crud/ClientDB"
#outputDir_client <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/ClientDB"

saveData_client <- function(clientdb) {
  fileName <- sprintf("%s_%s.xls", as.double(format(Sys.time(), "%y%m%d%H%M%S")), "client")
  WriteXLS::WriteXLS(
    x = clientdb,
    ExcelFileName = file.path(outputDir_client, fileName)
  )
  
}

loadData_client <- function() {
  files <- list.files(outputDir_client, full.names = TRUE)
  data <- readxl::read_xls(tail(files, n=1))
  data
}

clientdb <- loadData_client()
clientdb <- as.data.frame(clientdb)


GetTableMetadata_client <- function() {
  fields <- c(id = "Id", 
              cname = "Client Name", 
              address = "Address", 
              postal = "Postal", 
              officecontact = "Office Contact",
              centrecode = "Centre & Org Code"
  )
  
  result <- list(fields = fields)
  return (result)
}

GetNextId_client <- function() {
  if (exists("clientdb") && nrow(clientdb) > 0) {
    max(as.integer(rownames(clientdb))) + 1
  } else {
    return (1)
  }
}

#Create
CreateData_client <- function(data) {
  data <- CastData_client(data)
  rownames(data) <- GetNextId_client()
  if (exists("clientdb")) {
    clientdb <<- rbind(clientdb, data)
  } else {
    clientdb <<- data
  }
  saveData_client(clientdb)
}

#Read
ReadData_client <- function() {
  if (exists("clientdb")) {
    clientdb
  }
}

#Update
UpdateData_client <- function(data) {
  data <- CastData_client(data)
  clientdb[row.names(clientdb) == row.names(data), ] <<- data
  shinyjs::disable('updateclient')
  shinyjs::enable('submitclient')
  saveData_client(clientdb)
}

#Delete
DeleteData_client <- function(data) {
  clientdb <<- clientdb[row.names(clientdb) != unname(data["id"]), ]
}

CastData_client <- function(data) {
  datar <- data.frame(cname = data["cname"], 
                      address = data["address"], 
                      postal = data["postal"], 
                      officecontact = data["officecontact"], 
                      centrecode = data["centrecode"],
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

CreateDefaultRecord_client <- function() {
  mydefault <- CastData_client(list(id = "0", cname = "", address="", postal="", officecontact= "", centrecode=""))
  return (mydefault)
}

UpdateInputs_client <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "cname", value = unname(data["cname"]))
  updateTextInput(session, "address", value = unname(data["address"]))
  updateTextInput(session, "postal", value = unname(data["postal"]))
  updateTextInput(session, "officecontact", value = unname(data["officecontact"]))
  updateTextInput(session, "centrecode", value = unname(data["centrecode"]))
}
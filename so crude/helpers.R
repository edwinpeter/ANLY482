library(readxl)
library(WriteXLS)

CastData <- function(data) {
  datar <- data.frame(cname = data["cname"], 
                      address = data["address"], 
                      postal = data["postal"], 
                      officecontact = data["officecontact"], 
                      used_shiny = as.logical(data["used_shiny"]), 
                      r_num_years = as.integer(data["r_num_years"]),
                      stringsAsFactors = FALSE)
  
  #rownames(datar) <- data["id"]
  return (datar)
}

CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", cname = "haha", address="addressLOL", postal="postalLOL", officecontact="officecontactLOL", used_shiny = FALSE, r_num_years = 2))
  return (mydefault)
}

UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "cname", value = unname(data["cname"]))
  updateTextInput(session, "address", value = unname(data["address"]))
  updateTextInput(session, "postal", value = unname(data["postal"]))
  updateTextInput(session, "officecontact", value = unname(data["officecontact"]))
  updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
  updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
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
              used_shiny = "Used Shiny", 
              r_num_years = "R Years")
  
  result <- list(fields = fields)
  return (result)
}



outputDir <- "/Users/Edwin/Desktop/z/crud/responses"

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

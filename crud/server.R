library(shiny)
library(shinyjs)

server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
  CastData <- function(data) {
    datar <- data.frame(name = data["name"], 
                        used_shiny = as.logical(data["used_shiny"]), 
                        r_num_years = as.integer(data["r_num_years"]),
                        stringsAsFactors = FALSE)
    
    rownames(datar) <- data["id"]
    return (datar)
  }
  
  CreateDefaultRecord <- function() {
    mydefault <- CastData(list(id = "0", name = "", used_shiny = FALSE, r_num_years = 2))
    return (mydefault)
  }
  
  UpdateInputs <- function(data, session) {
    updateTextInput(session, "id", value = unname(rownames(data)))
    updateTextInput(session, "name", value = unname(data["name"]))
    updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
    updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
  }
 
  GetNextId <- function() {
    if (exists("responses") && nrow(responses) > 0) {
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
    } else {
      responses <<- data
    }
  }
  ReadData <- function() {
    if (exists("responses")) {
      responses
    }
  }
  
  UpdateData <- function(data) {
    data <- CastData(data)
    responses[row.names(responses) == row.names(data), ] <<- data
  }
  
  DeleteData <- function(data) {
    responses <<- responses[row.names(responses) != unname(data["id"]), ]
  }
  
  GetTableMetadata <- function() {
    fields <- c(id = "Id", 
                name = "Name", 
                used_shiny = "Used Shiny", 
                r_num_years = "R Years")
    
    result <- list(fields = fields)
    return (result)
  }
  
  
}
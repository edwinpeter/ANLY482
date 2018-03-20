library(shiny)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  #data table
  DT::dataTableOutput("responses", width = 500), 
  
  #input fields
  tags$hr(),
  textInput("id", "Id", "0"),
  textInput("cname", "cname", ""),
  textInput("address", "address", ""),
  textInput("postal", "postal", ""),
  textInput("officecontact", "officecontact", ""),
  checkboxInput("used_shiny", "Used Shiny", FALSE),
  sliderInput("r_num_years", "R Years", 0, 25, 2, ticks = FALSE),
  
  #action buttons
  actionButton("submit", "Submit"),
  actionButton("new", "New"),
  actionButton("delete", "Delete")
)


server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    # if (input$id != "0") {
    #   UpdateData(formData())
    # } else {
    #   CreateData(formData())
    #   UpdateInputs(CreateDefaultRecord(), session)
    # }
    CreateData(formData())
    #UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  shinyjs::disable("id")
  
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
  
}


# Run the application 
shinyApp(ui = ui, server = server)


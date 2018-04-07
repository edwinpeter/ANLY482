datax <- data.frame(
  id      = 1:2,
  content = c("D.C.G8 Camp 1", "Cambri-M.H Prep Day"),
  start   = c("2017-01-10", "2017-01-14"),
  end     = c("2017-01-11", NA)
)

server <- function(input, output, session) {
  ###################################### Server for Dashboard #########################################################
  
  output$value1 <- renderValueBox({
    valueBox(
      #formatC(prof.prod$value, format="d", big.mark=','),
      #paste('Top Product:',prof.prod$Product),
      value = 2,
      subtitle = "Upcoming Programs",
      icon = icon("bell",lib='glyphicon'),
      color = "yellow")   
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      #formatC(prof.prod$value, format="d", big.mark=','),
      #paste('Top Product:',prof.prod$Product),
      value = 5,
      subtitle = "Prep Day activities",
      icon = icon("modal-window",lib='glyphicon'),
      color = "blue")   
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      #formatC(prof.prod$value, format="d", big.mark=','),
      #paste('Top Product:',prof.prod$Product),
      value = 5,
      subtitle = "Actual Day activities",
      icon = icon("modal-window",lib='glyphicon'),
      color = "red")   
  })
  output$timeline <- renderTimevis({
    timevis(datax)
  })
  
  ###################################### Server for Client Database #########################################################
  shinyjs::disable('updateclient')
  formData_client <- reactive({
    sapply(names(GetTableMetadata_client()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submitclient, {
    if (input$id == "0") {
      CreateData_client(formData_client())
      UpdateInputs_client(CreateDefaultRecord_client(), session)
    }
  }, priority = 1)
  
  #UPDATE
  observeEvent(input$updateclient, {
    if (input$id != "0") {
      UpdateData_client(formData_client())
      UpdateInputs_client(CreateDefaultRecord_client(), session)
    }

  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$clientdb_rows_selected, {
    shinyjs::enable('updateclient')
    shinyjs::disable('submitclient')
    if (length(input$clientdb_rows_selected) > 0) {
      data <- ReadData_client()[input$clientdb_rows_selected, ]
      UpdateInputs_client(data, session)
    }
    
  })
  
  # display table
  output$clientdb <- DT::renderDataTable({
    #update after submit is clicked
    input$submitclient
    #update after delete is clicked
    input$updateclient
    ReadData_client()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata_client()$fields)[-1]
  )
  
  
  ###################################### Server for Programs Database #########################################################
  
  shinyjs::disable('updateprogram')
  formData_program <- reactive({
    sapply(names(GetTableMetadata_program()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submitprogram, {
    if (input$id == "0") {
      CreateData_program(formData_program())
      UpdateInputs_program(CreateDefaultRecord_program(), session)
    }
  }, priority = 1)
  
  #UPDATE
  observeEvent(input$updateprogram, {
    if (input$pid != "0") {
      UpdateData_program(formData_program())
      UpdateInputs_program(CreateDefaultRecord_program(), session)
    } 
  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$programdb_rows_selected, {
    shinyjs::enable('updateprogram')
    shinyjs::disable('submitprogram')
    if (length(input$programdb_rows_selected) > 0) {
      data <- ReadData_program()[input$programdb_rows_selected, ]
      UpdateInputs_program(data, session)
    }
    
  })
  
  # display table
  output$programdb <- DT::renderDataTable({
    #update after submit is clicked
    input$submitprogram
    #update after update is clicked
    input$updateprogram
    ReadData_program()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata_program()$fields)[-1],
  options = list(scrollX = TRUE)
  )
  
  
  
  #Update client list on programs page
  observe({
    input$submitclient
    input$updateclient
    x <- pull(loadData_client(),"centrecode")

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "program_client_name",
                      label = paste("Select Client (", length(x), ")"),
                      choices = x,
                      selected = ''
    )
  })
  
  ###################################### Server for Prep Activities Database #########################################################
  
  shinyjs::disable('updateprepact')
  formData_prepprogram <- reactive({
    sapply(names(GetTableMetadata_prepprogram()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submitprepact, {
    if (input$id == "0") {
      CreateData_prepprogram(formData_prepprogram())
      UpdateInputs_prepprogram(CreateDefaultRecord_prepprogram(formData_prepprogram()), session)
    }
  }, priority = 1)
  
  #UPDATE
  observeEvent(input$updateprepact, {
    if (input$pid != "0") {
      UpdateData_prepprogram(formData_prepprogram())
      UpdateInputs_prepprogram(CreateDefaultRecord_prepprogram(), session)
    } 
  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$prepprogramdb_rows_selected, {
    shinyjs::enable('updateprepact')
    shinyjs::disable('submitprepact')
    if (length(input$prepprogramdb_rows_selected) > 0) {
      data <- ReadData_prepprogram()[input$prepprogramdb_rows_selected, ]
      UpdateInputs_prepprogram(data, session)
    }
    
  })
  
  # display table
  output$prepprogramdb <- DT::renderDataTable({
    #update after submit is clicked
    input$submitprepact
    #update after update is clicked
    input$updateprepact
    
    input$activity_client_date
    ReadData_prepprogram()
    if(input$activity_client_date == ""){
      ReadData_prepprogram()
    }else{
      ReadData_prepprogram_filter(input$activity_client_date)
    }
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata_prepprogram()$fields)[-1]
  )
  
  
  
  #Update program list on activity page
  observe({
    input$submitprogram
    input$updateprogram
    x <- pull(loadData_program(),"client_date_created")

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

    # Can also set the label and select items
    updateSelectInput(session, "activity_client_date",
                      label = paste("Select Program (", length(x), ")"),
                      choices = x,
                      selected = ''
    )
  })

}
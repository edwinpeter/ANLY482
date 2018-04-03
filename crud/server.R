server <- function(input, output, session) {
  
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
      print("ADAS")
      
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
  colnames = unname(GetTableMetadata_program()$fields)[-1]
  )
  
  
  
  #Update client list on programs page
  observe({
    input$submitclient
    input$updateclient
    x <- pull(loadData_client(),"centrecode")
    print(x)
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "inSelect",
                      label = paste("Select Client (", length(x), ")"),
                      choices = x,
                      selected = tail(x, 1)
    )
  })
  
  ###################################### Server for Prep Activities Database #########################################################
  
  shinyjs::disable('updateprepact')
  formData_prepact <- reactive({
    sapply(names(GetTableMetadata_program()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submitprepact, {
    if (input$id == "0") {
      CreateData_program(formData_prepact())
      UpdateInputs_program(CreateDefaultRecord_program(), session)
    }
  }, priority = 1)
  
  #UPDATE
  observeEvent(input$updateprepact, {
    if (input$pid != "0") {
      print("ADAS")
      
      UpdateData_program(formData_prepact())
      UpdateInputs_program(CreateDefaultRecord_program(), session)
    } 
  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$prepdb_rows_selected, {
    shinyjs::enable('updateprepact')
    shinyjs::disable('submitprepact')
    if (length(input$prepdb_rows_selected) > 0) {
      data <- ReadData_prepprog()[input$prepdb_rows_selected, ]
      UpdateInputs_program(data, session)
    }
    
  })
  
  # display table
  output$prepdb <- DT::renderDataTable({
    #update after submit is clicked
    input$submitprepact
    #update after update is clicked
    input$updateprepact
    ReadData_prepprog()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata_prepprog()$fields)[-1]
  )
  
  
  
  # #Update client list on programs page
  # observe({
  #   input$submitclient
  #   input$updateclient
  #   x <- pull(loadData_client(),"centrecode")
  #   print(x)
  #   
  #   # Can use character(0) to remove all choices
  #   if (is.null(x))
  #     x <- character(0)
  #   
  #   # Can also set the label and select items
  #   updateSelectInput(session, "inSelect",
  #                     label = paste("Select Client (", length(x), ")"),
  #                     choices = x,
  #                     selected = tail(x, 1)
  #   )
  # })
  
  ###################################### Server for Prep Activities Database #########################################################

  shinyjs::disable('updateactlact')
  formData_actlact <- reactive({
    sapply(names(GetTableMetadata_program()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submitactlact, {
    if (input$id == "0") {
      CreateData_program(formData_actlact())
      UpdateInputs_program(CreateDefaultRecord_program(), session)
    }
  }, priority = 1)
  
  #UPDATE
  observeEvent(input$updateactlact, {
    if (input$pid != "0") {
      print("ADAS")
      
      UpdateData_program(formData_actlact())
      UpdateInputs_program(CreateDefaultRecord_program(), session)
    } 
  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$actldb_rows_selected, {
    shinyjs::enable('updateprepact')
    shinyjs::disable('submitprepact')
    if (length(input$actldb_rows_selected) > 0) {
      data <- ReadData_actlprog()[input$actldb_rows_selected, ]
      UpdateInputs_program(data, session)
    }
    
  })
  
  # display table
  output$actldb <- DT::renderDataTable({
    #update after submit is clicked
    input$submitactlact
    #update after update is clicked
    input$updateactlact
    ReadData_actlprog()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata_actlprog()$fields)[-1]
  )
  
  
  
  # #Update client list on programs page
  # observe({
  #   input$submitclient
  #   input$updateclient
  #   x <- pull(loadData_client(),"centrecode")
  #   print(x)
  #   
  #   # Can use character(0) to remove all choices
  #   if (is.null(x))
  #     x <- character(0)
  #   
  #   # Can also set the label and select items
  #   updateSelectInput(session, "inSelect",
  #                     label = paste("Select Client (", length(x), ")"),
  #                     choices = x,
  #                     selected = tail(x, 1)
  #   )
  # })
  
}
server <- function(input, output, session) {
  ###################################### Server for Dashboard #########################################################
  
  output$value1 <- renderValueBox({
    valueBox(
      value = nrow(datax),
      subtitle = "Upcoming Programs",
      icon = icon("bell",lib='glyphicon'),
      color = "red")   
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      value = nrow(GetPrep()),
      subtitle = "Prep Day activities",
      icon = icon("modal-window",lib='glyphicon'),
      color = "orange")   
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      value = nrow(GetActual()),
      subtitle = "Actual Day activities",
      icon = icon("modal-window",lib='glyphicon'),
      color = "navy")   
  })
  
  output$timeline <- renderTimevis({
    timevis(GetDashboardCalendar())
  })
  
  observe({
    input$submitprepact
    input$updateprepact
    input$submitactprogram
    input$submitactprogram
    output$timeline <- renderTimevis({
      timevis(GetDashboardCalendar())
    })
  })
  
  output$timetable <- DT::renderDataTable({
    input$timeline_data
  }, options=list(columnDefs = list(list(visible=FALSE, targets=c(1,5,6,7)))))
  
  output$frequencyplotprep <- renderPlotly({
    plot_ly(prepfreq, x = ~prepx, y = ~prepy, type = 'bar', 
            text = prepy, textposition = 'auto',
            marker = list(color = '#ff9749',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Frequency of Prep Activities",
             margin = list(b = 160),
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$frequencyplotact <- renderPlotly({
    plot_ly(prepfreq, x = ~actx, y = ~acty, type = 'bar', 
            text = acty, textposition = 'auto',
            marker = list(color = '#660066',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Frequency of Actual Activities",
             margin = list(b = 160),
             xaxis = list(title = ""),
             yaxis = list(title = ""))
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
  
  
  
  #Update client list on bookings page
  observe({
    input$submitclient
    input$updateclient
    x <- pull(loadData_client(),"centrecode")

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
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
  
  ###################################### Server for Actual Activities Database #########################################################
  
  shinyjs::disable('updateactprogram')
  formData_actprogram <- reactive({
    sapply(names(GetTableMetadata_actprogram()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submitactprogram, {
    if (input$aid == "0") {
      CreateData_actprogram(formData_actprogram())
      UpdateInputs_actprogram(CreateDefaultRecord_actprogram(formData_actprogram()), session)
    }
  }, priority = 1)
  
  #UPDATE
  observeEvent(input$updateactprogram, {
    if (input$aid != "0") {
      UpdateData_actprogram(formData_actprogram())
      UpdateInputs_actprogram(CreateDefaultRecord_actprogram(), session)
    } 
  }, priority = 1)
  
  
  # Select row in table -> show details in inputs
  observeEvent(input$actprogramdb_rows_selected, {
    shinyjs::enable('updateactprogram')
    shinyjs::disable('submitactprogram')
    if (length(input$actprogramdb_rows_selected) > 0) {
      data <- ReadData_actprogram()[input$actprogramdb_rows_selected, ]
      UpdateInputs_actprogram(data, session)
    }
    
  })
  
  # display table
  output$actprogramdb <- DT::renderDataTable({
    #update after submit is clicked
    input$submitactprogram
    #update after update is clicked
    input$updateactprogram
    
    input$activity_client_date
    ReadData_actprogram()
    if(input$activity_client_date == ""){
      ReadData_actprogram()
    }else{
      ReadData_actprogram_filter(input$activity_client_date)
    }
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata_actprogram()$fields)[-1]
  )
  
  
  
  ###################################### Server for MBA Analysis #########################################################

  sliderValues <- reactive({
    prepareBasket()
    rules <- apriori(transactions.data, parameter = list(supp=input$supp, conf=input$conf))
  })
  
  output$mbagraph = renderPlotly({
    # plot(rules, method="graph", engine = 'interactive')
    sliderValues()
    rules <- apriori(transactions.data, parameter = list(supp=input$supp, conf=input$conf))
    plotly_arules(rules)
  })
}
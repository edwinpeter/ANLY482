library(shiny)
library(DT)

  
shinyServer(function(input, output, session) {
  directory <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/Dashbored/"
  mbafilename <- "prep_day_data.csv"
  timeseriesfilename <- "Time Series Data.csv"
  mbafile_directory <- paste(directory, mbafilename, sep="")
  tsfile_directory <- paste(directory, timeseriesfilename, sep="")
  
  # read.excel
  #Read in csv file 
  transactions <- read.csv(mbafile_directory, sep=",")
  ts <- read.csv(tsfile_directory, sep=",")
  date()
  
  output$x1 <- DT::renderDataTable(transactions, server = FALSE)
  output$ts <- DT::renderDataTable(ts, server = FALSE)
  
  
  
})

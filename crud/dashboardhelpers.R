hrs <- function(u) {
  x <- u * 3600
  return(x)
}

mns <- function(m) {
  x <- m * 60
  return(x)
}

GetPrep <- function(){
  listed <- merge(x=prepprogramdb, y=prepdb, by.x=colnames(prepprogramdb)[2], by.y=colnames(prepdb)[1],  all.x = TRUE)
  this_month <- month(Sys.Date())
  this_year <- year(Sys.Date())
  
  # Filter only this month and this year to take out relevant data to display on dashboard
  # Can modify this part to determine what to show on dashboard
  dashboard_filtered <- dplyr::filter(listed, month(as.Date(listed$activity_date)) == this_month)
  dashboard_filtered <- dplyr::filter(dashboard_filtered, year(as.Date(dashboard_filtered$activity_date)) == this_year)
  return(dashboard_filtered)
}

GetActual <- function(){
  act <- merge(x=actprogramdb, y=actldb, by.x=colnames(actprogramdb)[2], by.y=colnames(actldb)[1],  all.x = TRUE)
  this_month <- month(Sys.Date())
  this_year <- year(Sys.Date())
  
  # Filter only this month and this year to take out relevant data to display on dashboard
  # Can modify this part to determine what to show on dashboard
  dashboard_filtered_act <- filter(act, month(as.Date(act$actual_start_date)) == this_month)
  dashboard_filtered_act <- filter(dashboard_filtered_act, year(as.Date(dashboard_filtered_act$actual_start_date)) == this_year)
  return(dashboard_filtered_act)
}

GetDashboardCalendar <- function(){
  content = character()
  start = character()
  end = character()
  type = character()
  style = character()
  title = character()
  
  
  dashboard_filtered <- GetPrep()
  
  if (nrow(dashboard_filtered) != 0){
    # Loop through all prep activities to pick out and append to content, start and end
    for(i in 1:nrow(dashboard_filtered)) {
      id <- dashboard_filtered[i,]$prep_id
      client <- unlist(strsplit(dashboard_filtered[i,]$activity_client_date, " "))[1]
      
      startdate <- dashboard_filtered[i,]$activity_date
      starttime <- format(strptime(dashboard_filtered[i,]$activity_start_time, format="%H%M"), format = "%H:%M")
      startdatetime <- paste(startdate, starttime)
      startdatetime <- strptime(startdatetime, "%Y-%m-%d %H:%M")
      
      enddatetime <- startdatetime + hrs(as.numeric(dashboard_filtered[i,]$activity_duration))
      temptitle <- paste(id, client, startdatetime, enddatetime)
      
      #Client in brackets
      client_naming <- paste("(", client, ")", sep="")
      
      combined <- paste(id, client_naming)
      content <- append(content, combined)
      start <- append(start, as.character(startdatetime))
      end <- append(end, as.character(enddatetime))
      type <- append(type, "box")
      style <- append(style, "background-color: #ff9749; color: #38160a; border-color: black")
      title <- append(title, temptitle)
    }
  }
  
  dashboard_filtered_act <- GetActual()
  
  #Loop through all prep activities to pick out and append to content, start and end
  if (nrow(dashboard_filtered_act) != 0){
    for(i in 1:nrow(dashboard_filtered_act)) {
      id <- dashboard_filtered_act[i,]$actual_id
      client <- unlist(strsplit(dashboard_filtered_act[i,]$activity_client_date, " "))[1]
      
      startdate <- dashboard_filtered_act[i,]$actual_start_date
      starttime <- format(strptime(dashboard_filtered_act[i,]$actual_start_time, format="%H%M"), format = "%H:%M")
      startdatetime <- paste(startdate, starttime)
      startdatetime <- strptime(startdatetime, "%Y-%m-%d %H:%M")
      
      enddate <- dashboard_filtered_act[i,]$actual_end_date
      endtime <- format(strptime(dashboard_filtered_act[i,]$actual_end_time, format="%H%M"), format = "%H:%M")
      enddatetime <- paste(enddate, endtime)
      enddatetime <- strptime(enddatetime, "%Y-%m-%d %H:%M")
      
      duration <- difftime(enddatetime, startdatetime, units="hours")

      enddatetime <- startdatetime + hrs(as.numeric(duration))

      type <- append(type, "box")
      temptitle <- paste(id, client, startdatetime, enddatetime)
      
      #Brackets for client
      client_naming <- paste("(", client, ")", sep="")
      
      combined <- paste(id, client_naming)
      content <- append(content, combined)
      start <- append(start, as.character(startdatetime))
      end <- append(end, as.character(enddatetime))
      
      style <- append(style, "background-color: #660066; color: #ff9900; border-color: black")
      title <- append(title, temptitle)
      
    }
  }
  if (length(content) != 0){
    datax <- data.frame(
      id      = 1:length(content),
      content = content,
      start   = start,
      end     = end,
      type    = type,
      style   = style,
      title   = title
    )
    return(datax)  
  }else{
    return(df <- data.frame())
  }
  
}

datax <- GetDashboardCalendar()

################################################FREQUENCY PLOT###########################################

#First, merge datasets to get the mapping
listed <- merge(x=prepprogramdb, y=prepdb, by.x=colnames(prepprogramdb)[2], by.y=colnames(prepdb)[2],  all.x = TRUE)
act <- merge(x=actprogramdb, y=actldb, by.x=colnames(actprogramdb)[2], by.y=colnames(actldb)[2],  all.x = TRUE)

#Set data for prep
prepfreqtable <- as.data.frame(table(listed$prep_id))
prepx <- prepfreqtable$Var1
prepy <- prepfreqtable$Freq
prepfreq <- data.frame(prepx, prepy)

#Set data for actual
actfreqtable <- as.data.frame(table(act$actual_id))
actx <- actfreqtable$Var1
acty <- actfreqtable$Freq
actfreq <- data.frame(actx, acty)





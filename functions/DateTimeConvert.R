#Script to convert time and date from WPS format
#Rachel O'Leary
#2016-03-02 
#v1.0

#-----Set working drectory and input filename-----#


setwd("Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0051 RSV cobas Liat test  - clinical validity study")      #Sets appropriate working directory
#inputraw="Raw_data/GNCH-2017-06-08_11-54-11-AM.csv"            #States file name to be read in
inputraw="Raw_data/SRH-2017-06-08_02-25-14-PM.csv"            #States file name to be read in



library(stringr)#load the stringr package

#-----DEFINE FUNCTION TO CONVERT TIME COLUMNS TO HH:MM TIME-----#



timeconv <- function(input) {
  
  if(is.logical(input)) input <- as.numeric(input)
  
  
  if (is.numeric(input)){
    
    #-----reformats times formatted by as a decimal-----#
    input <- (input*86400)
    
    input <- format(round(as.POSIXct(input, origin="1970-01-01", "GMT")), "%H:%M")
    #input <- strptime(input, format='%I:%M %p')
    #input <- format(round(as.POSIXct(input, origin="1970-01-01", "GMT")), "%I:%M %p")
    
    print(input)
    
    
  }  else if(is.logical(input)) {
    
    input <- as.numeric(input) 
    
    #-----reformats times formatted by as a decimal-----#
    input <- (input*86400)
   
    input <- format(round(as.POSIXct(input, origin="1970-01-01", "GMT")), "%H:%M")
    
    #input <- format(round(as.POSIXct(input, origin="1970-01-01", "GMT")), "%I:%M %p")
    
    print(input)
    
  } else {
    
    #-----reformats times formatted by Excel 1899-12-30 default-----#
    
    #input <- format(as.POSIXct(strptime(input,"%m/%d/%Y %I:%M %p")), "%H:%M")
    
    input <- format(as.POSIXct(strptime(input,"%m/%d/%Y %I:%M %p")), "%I:%M %p")
    
    print(input)
    
  }
  
  return(input)
}

#-----END OF FUNCTION-----#  



#-----Read in previously specified filename-----#


DataChecking=read.csv(file=inputraw,header=TRUE,stringsAsFactors=FALSE,strip.white = TRUE)


#-----Reformats all columns containing dates, identified by "date" in the column header-----#


DataChecking[, datecol <- grep("date", names(DataChecking))] <- lapply(DataChecking[, datecol <- grep("date", names(DataChecking))], as.Date, format = "%m/%d/%Y")


#-----Reformats all columns containing times, identified by "time" in the column header (uses timeconv function)-----#

DataChecking[, (timecol <- grep("time", names(DataChecking)))] <- lapply(DataChecking[, (timecol <- grep("time", names(DataChecking)))], timeconv)

#-----Writes reformated dataset to file-----#

write.csv(DataChecking,file="Raw_data/SRH_datetimeresolved.csv",row.names=FALSE)

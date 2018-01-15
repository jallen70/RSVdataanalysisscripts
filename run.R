# R script run diagnostic accuracy stats 

# clear the global environment
rm(list = ls())
#path to the working directory
#wd<- "~/Documents/DEC WORK/NCL_DEC0051 RSV cobas Liat test  - clinical validity study"
wd <- "Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0051 RSV cobas Liat test  - clinical validity study"
setwd(wd)

#load in packages required
source("loadpackages.R")
loadpackages()

#load functions script
source("functions/functions.R")

#load script to read in data
source("functions/read_clean.R")
readingfiles()

library(gridExtra)

########################################################################

Cohort <- T
alpha <- 0.05 # confidence level
dig <- 3 # number of decimals points 
tabfilepath <- "output/pilot/"
figfilepath <- "figures/"
save_tables  = "T" 
now <- format(Sys.time(), "%b%d%H%M%S")

################## ANALYSIS PRINT OUT ##################################


################## DIAGNOSTIC ACCURACY PER SITE ###############################

#create a data set to calculate Tps, Tns, Fps, Fns

  cutoffdate <- maxdate
  
 
  rsvTxT <- TxTanalysis2(total_study$cLoutcome,total_study$PCRoutcome,"RSV")
 
  Tp <- rsvTxT[,1]
  Fp <- rsvTxT[,2]
  Tn <- rsvTxT[,3]
  Fn <- rsvTxT[,4]
  
  #Display overall 2x2 table
  rsvtable <- Dx2by2fun()
  filename <- paste0( tabfilepath,now,"rsvaccuracyresults.csv")
  write.table(rsvtable, file = filename, sep = ",",  col.names=T, row.names=T)
  
  message(paste("Combined site sensitivity",round(100*SnSp(Tp,Fn),dig)),"% (",
          round(100*SnSpCIs(alpha,SnSp(Tp,Fn),Tp+Fn)$lower,dig),"-", 
          round(100*SnSpCIs(alpha,SnSp(Tp,Fn),Tp+Fn)$upper,dig),
          ")%")
  
  message(paste("Combined site specificity",round(100*SnSp(Tn,Fp),dig)),"% (",
          round(100*SnSpCIs(alpha,SnSp(Tn,Fp),Tn+Fp)$lower,dig),"-", 
          round(100*SnSpCIs(alpha,SnSp(Tn,Fp),Tn+Fp)$upper,dig),
          ")%")
  
  
  virus <- "RSV"
  data <- total_study
  
  if (virus == "RSV") {
    pcrvirus <- "RSV/Rhino"
  } else {
    pcrvirus <- "pcrvirus"
  }
  
  rsvTxT_loc <- TxTanalysis_loc(total_study, virus, pcrvirus)
  rownames(rsvTxT_loc) <- rsvTxT_loc[,1]  
  rsvTxT_loc
  
 
  
  #GNCH 
  Tp <- rsvTxT_loc$Tp[1]
  Fp <- rsvTxT_loc$Fp[1]
  Tn <- rsvTxT_loc$Tn[1]
  Fn <- rsvTxT_loc$Fn[1]
  
  #Display overall 2x2 table
  rsvtable_gnch <- Dx2by2fun()
  rsvtable_gnch
  
  #SRH 
  Tp <- rsvTxT_loc$Tp[2]
  Fp <- rsvTxT_loc$Fp[2]
  Tn <- rsvTxT_loc$Tn[2]
  Fn <- rsvTxT_loc$Fn[2]
  
  #Display overall 2x2 table
  rsvtable_srh <- Dx2by2fun()
  rsvtable_srh
  
  # need to now conduct 2x2 analysis for repeat cobas testing. 
  
  repeat_testing <- testing_outcome(total_study$rcobasresult_rsv, total_study$rcobasresult_flua, total_study$rcobasresult_flub)
  
  total_study$rcLoutcome <- repeat_testing$outcome
  
  total_study$cLoutcome_overall <- total_study$rcLoutcome 
  total_study$cLoutcome_overall[total_study$cobastest != "invalid retest "] <- total_study$cLoutcome
  rsvTxT_repeat <- TxTanalysis2(total_study$cLoutcome_overall,total_study$PCRoutcome,"RSV")
  rsvTxT_repeat
  
  Tp <- rsvTxT_repeat[,1]
  Fp <- rsvTxT_repeat[,2]
  Tn <- rsvTxT_repeat[,3]
  Fn <- rsvTxT_repeat[,4]
  
  
  rsvtable2 <- Dx2by2fun()
  filename <- paste0( tabfilepath,now,"rsvaccuracyresults_withinvalids.csv")
  write.table(rsvtable2, file = filename, sep = ",",  col.names=T, row.names=T)
  
  # also 2x2 analysis for discrepany testing
  
  # only one for discrepant testing in pilot study
  
  
  
  
#   source("functions/accstats.R")   # writes out sens and spec tables, and ppv and npv tables with CI to csv files
#   accstats(results)
#   
#   # plot of predictive values over time and with prevalence
#   source("functions/predvalues_time.R")
#   
#   ###### AVERAGE LENGTH OF TIME FOR PCR RESULT #######################
  source("functions/timediff.R")
  
  total_study <- total_study %>%  
    dplyr::rowwise() %>% dplyr::mutate(difftime = timedifferences(pcr_dateresultavail, pcr_timeresultavail, cobasdate, cobastime))
  
#   
   timediff(total_study)
#   
#   #average time between results per location
#   averagedifftime= ddply(total_study,~Location, summarise, 
#                          AvNumDaysBetwRes = round(mean(difftime, na.rm=T),digits = 0))
#   row.names(averagedifftime) = averagedifftime[,1]
#   averagedifftime$Location <- NULL
#   
#   averagedifftime
#   
  ### demographic - average age
  
  ### diagnosis on admission
  
  ### diagnosis on discharge
  
  ### type of swab used
  
  ### isolation usage
  
  ### change in isolation usage following PCR result
  
  ### barrier nursing
  
  ### numbers for antibiotic/atimicrobial prescription
  
  ### numbers for indication for testing
  
  ### other tests conducted?
  
  ### average length of stay for those discharged before end of study
  
  




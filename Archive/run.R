# R script run diagnostic accuracy stats 
#test new git
# clear the global environment
rm(list = ls())
#path to the working directory
wd<- "Z:/DEC methods/tools - R/Working_project_folders/NCL_DEC0051 RSV cobas Liat test  - clinical validity study"
setwd(wd)

#load in packages required
source("loadpackages.R")
loadpackages()

#load functions script
source("functions/functions.R")

#load script to read in data
source("functions/read_clean.R")
readingfiles()

########################################################################

Cohort <- T
alpha <- 0.05 # confidence level
dig <- 3 # number of decimals points 

################## ANALYSIS PRINT OUT ##################################


################## DIAGNOSTIC ACCURACY PER SITE ###############################

#create a data set to calculate Tps, Tns, Fps, Fns

  cutoffdate <- maxdate
  source("functions/2x2analysis.R")
  rsvTxT <- TxTanalysis(total_study,cutoffdate,"RSV")
  
  Tp <- rsvTxT[,1]
  Fp <- rsvTxT[,2]
  Tn <- rsvTxT[,3]
  Fn <- rsvTxT[,4]
  
  #Display overall 2x2 table
  rsvtable <- Dx2by2fun()
  
  message(paste("Combined site sensitivity",round(100*SnSp(Tp,Fn),dig)),"% (",
          round(100*SnSpCIs(alpha,SnSp(Tp,Fn),Tp+Fn)$lower,dig),"-", 
          round(100*SnSpCIs(alpha,SnSp(Tp,Fn),Tp+Fn)$upper,dig),
          ")%")
  
  message(paste("Combined site specificity",round(100*SnSp(Tn,Fp),dig)),"% (",
          round(100*SnSpCIs(alpha,SnSp(Tn,Fp),Tn+Fp)$lower,dig),"-", 
          round(100*SnSpCIs(alpha,SnSp(Tn,Fp),Tn+Fp)$upper,dig),
          ")%")
  
  
  rsvTxT_loc <- TxTanalysis_loc(total_study,cutoffdate,"RSV")
  rownames(rsvTxT_loc) <- rsvTxT_loc[,1]  
  
  #GNCH 
  Tp <- rsvTxT_loc$Tp[1]
  Fp <- rsvTxT_loc$Fp[1]
  Tn <- rsvTxT_loc$Tn[1]
  Fn <- rsvTxT_loc$Fn[1]
  
  #Display overall 2x2 table
  rsvtable_gnch <- Dx2by2fun()
  
  #SRH 
  Tp <- rsvTxT_loc$Tp[2]
  Fp <- rsvTxT_loc$Fp[2]
  Tn <- rsvTxT_loc$Tn[2]
  Fn <- rsvTxT_loc$Fn[2]
  
  #Display overall 2x2 table
  rsvtable_srh <- Dx2by2fun()
  

#   source("functions/accstats.R")   # writes out sens and spec tables, and ppv and npv tables with CI to csv files
#   accstats(results)
#   
#   # plot of predictive values over time and with prevalence
#   source("functions/predvalues_time.R")
#   
  ###### AVERAGE LENGTH OF TIME FOR PCR RESULT #######################
  source("functions/timediff.R")
  
  timediff(total_study,difftime2)
  
  #average time between results per location
  averagedifftime= ddply(total_study,~Location, summarise, 
                         AvNumDaysBetwRes = round(mean(difftime, na.rm=T),digits = 0))
  row.names(averagedifftime) = averagedifftime[,1]
  averagedifftime$Location <- NULL
  
  averagedifftime
  
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
  
  




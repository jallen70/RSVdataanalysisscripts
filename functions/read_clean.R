# R function script to read in data and clean up by removing missing 
# or ineligible data

readingfiles <- function(){
  
# read in both csv files
  inputraw1="Raw_data/GNCH-2017-06-08_11-54-11-AM.csv"  
  inputraw2="Raw_data/SRH-2017-06-08_02-25-14-PM.csv"  
    
gnchfull = read.csv(file = inputraw1, strip.white = TRUE) #### fill in with file details
srhfull = read.csv(file = inputraw2, strip.white = TRUE)  ### fill in with file details

# add factor for location
gnchfull$Location <- "GNCH"
srhfull$Location <- "SRH"  

gnchfull <- gnchfull[, -grep("X.", colnames(gnchfull))]
srhfull <- srhfull[, -grep("X.", colnames(srhfull))]


total_study_full <- rbind.data.frame(gnchfull,srhfull)
total_study_full$Location <-as.factor(total_study_full$Location)

total_study_full[, grep("date", names(total_study_full))] <- lapply(total_study_full[,  grep("date", names(total_study_full))], as.Date, format = "%m/%d/%Y")

total_study_full[, (timecol <- grep("time", names(total_study_full)))] <- lapply(total_study_full[, (timecol <- grep("time", names(total_study_full)))], timeconv)
total_study_full$Location <-as.factor(total_study_full$Location)

# total_study_full$admissiondiag <- as.factor(total_study_full$admissiondiag)
# total_study_full$isolprecaut <- as.factor(total_study_full$isolprecaut)
# 
# total_study_full$antibioprecaut <- as.factor(total_study_full$antibioprecaut)
# total_study_full$antibioticname <- as.factor(total_study_full$antibioticname)
# 
# total_study_full$antiviralprecaut <- as.factor(total_study_full$antiviralprecaut)
total_study_full$antiviralname <- as.factor(total_study_full$antiviralname)

total_study_full$dxOther <- as.character(total_study_full$dxOther)
total_study_full$antibxOther <- as.character(total_study_full$antibxOther)

total_study_full$antiviralname <- as.character(total_study_full$antiviralname)
total_study_full$otherdx <- as.character(total_study_full$otherdx)
total_study_full$pcrfailurereason <- as.character(total_study_full$pcrfailurereason)

total_study_full$rpcrcycno <- as.integer(total_study_full$rpcrcycno)

################################################################################################

# How many initial participants do we have?  Before exclusions?
initial_partnum = nrow(total_study_full)

# check class of data
sapply(total_study_full,class)

# set the rownames to equal participant ID
rownames(total_study_full)  = total_study_full[,1]
total_study_full$ParticipantId <- NULL

#how many missing pcrs and cobas liat tests did we have?
# these many need adapting if other reasons are given for 
missing_cobas = nmissing(total_study_full$cobastest == "" & is.na(total_study_full$cobastime) )
missing_pcrs = nmissing((total_study_full$pcrresult == ""| total_study_full$pcrresult == "missing") & is.na(total_study_full$pcrtime))


removed4missing <- nmissing(total_study_full$cobastest == ""
                           | total_study_full$pcrresult == ""| total_study_full$pcrresult == "missing")

list_missing <- subset(total_study_full, total_study_full$cobastest == ""
                       | total_study_full$pcrresult == ""| total_study_full$pcrresult == "missing")
write.csv(list_missing,"output/list_missing.csv")
  
################ HERE WE ACTUALLY REMOVED THE PARTICIPANTS WITH MISSING
################ RESULTS AND THE INVALIDS #############################

# remove missing results
total_study1 <- subset(total_study_full, total_study_full$cobastest != ""
                                          & total_study_full$pcrresult != ""& total_study_full$pcrresult != "missing")

removed4failure = initial_partnum - removed4missing 

# nmissing_pcrs = nrow(subset(total_study_full, total_study_full$PCRsuccess == "yes" &
#                             (total_study_full$PCRresult == "missing" | 
#                                total_study_full$PCRresult == "NA") ))
# 
# nmissing_cobas = nrow(subset(total_study_full,  total_study_full$cobassuccess == "yes" &
#                                (total_study_full$cobasresult == "missing" |
#                                 total_study_full$cobasresult == "NA")))


# remove PCR and cobas test failures from primary analysis

# total_study1 <- subset(total_study1, total_study1$PCRsuccess == "yes")
# npcrfailure = nrow(subset(total_study1, total_study1$PCRsuccess == "no"))
# 
# total_study1 <- subset(total_study1, total_study1$cobassuccess == "yes")
# ncobasfailure = nrow(subset(total_study1, total_study1$cobassuccess == "no"))

## overall result needed

total_study1$cLoutcome <- ""

total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV negative" & 
                         total_study1$cobasresult_flua == "Flu A negative" & 
                         total_study1$cobasresult_flub == "Flu B negative"] <- "Neg"
total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV positive" & 
                         total_study1$cobasresult_flua == "Flu A negative" & 
                         total_study1$cobasresult_flub == "Flu B negative"] <- "RSV"
total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV negative" & 
                         total_study1$cobasresult_flua == "Flu A positive" & 
                         total_study1$cobasresult_flub == "Flu B negative"] <- "FluA"
total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV negative" & 
                         total_study1$cobasresult_flua == "Flu A negative" & 
                         total_study1$cobasresult_flub == "Flu B positive"] <- "FluB"

# are there any combined results?
total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV positive" & 
                         total_study1$cobasresult_flua == "Flu A negative" & 
                         total_study1$cobasresult_flub == "Flu B positive"] <- "RSV/FluB"
total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV positive" & 
                         total_study1$cobasresult_flua == "Flu A positive" & 
                         total_study1$cobasresult_flub == "Flu B negative"] <- "RSV/FluA"
total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV negative" & 
                         total_study1$cobasresult_flua == "Flu A positive" & 
                         total_study1$cobasresult_flub == "Flu B positive"] <- "FluA/FluB"
total_study1$cLoutcome[total_study1$cobasresult_rsv == "RSV positive" & 
                         total_study1$cobasresult_flua == "Flu A positive" & 
                         total_study1$cobasresult_flub == "Flu B positive"] <- "RSV/FluA/FluB"

#do number of invalids match the empty outcomes?
if(sum(total_study1$cobastest == "invalid retest") != sum(total_study1$cLoutcome == "")){
  message("Problem with cLoutcome column, number of invalid tests do not agree, recheck cLoutcome column calculations")
}

# summary of cobas results
cLsummary <- ddply(total_study1,~Location, summarise, 
                 RSV = sum(cLoutcome == "RSV"), 
                 FluA = sum(cLoutcome == "FluA"), 
                 FluB = sum(cLoutcome == "FluB"),
                 Neg = sum(cLoutcome == "Neg"), 
                 invalids = sum(cLoutcome == ""))


## how many invalids per site?
cL_testing <- ddply(total_study1,~Location,summarise, cL_1stinvalid = sum(cobastest == "invalid retest"),
                    repeatcL = sum(rcobasresult == "done") + sum(rcobasresult == "invalid"), 
                    repeatcL_ntdone = sum(rcobasresult == "not done"),
                    repeatcL_invalid = sum(rcobasresult == "invalid") )

### PCR testing


# overall PCR results 
total_study1$PCRoutcome <- ""

total_study1$PCRoutcome[total_study1$pcradeno == "yes"] <- "Adeno"
total_study1$PCRoutcome[total_study1$pcrinfa == "yes"] <- "FluA"
total_study1$PCRoutcome[total_study1$pcrinfb == "yes"] <- "FluB"
total_study1$PCRoutcome[total_study1$pcrpneumo == "yes"] <- "Pneumo"
total_study1$PCRoutcome[total_study1$pcrpara1 == "yes"] <- "Para1"
total_study1$PCRoutcome[total_study1$pcrpara2 == "yes"] <- "Para2"
total_study1$PCRoutcome[total_study1$pcrpara3 == "yes"] <- "Para3"
total_study1$PCRoutcome[total_study1$pcrpara4 == "yes"] <- "Para4"
total_study1$PCRoutcome[total_study1$pcrrhino == "yes"] <- "Rhino"
total_study1$PCRoutcome[total_study1$pcrrsv == "yes"] <- "RSV"
total_study1$PCRoutcome[total_study1$pcrrsv == "yes" & total_study1$pcrrhino == "yes"] <- "RSV/Rhino"
total_study1$PCRoutcome[total_study1$pcradeno == "yes" & total_study1$pcrrhino == "yes"] <- "Adeno/Rhino"

# any combined?
sum(total_study1$pcrrsv == "yes" & total_study1$pcrinfa == "no" & total_study1$pcrinfb == "no") 
sum(total_study1$pcrrsv == "yes" & total_study1$pcrrhino == "yes")
sum(total_study1$pcrinfa == "yes" & total_study1$pcrrhino == "yes")
sum(total_study1$pcrrsv == "yes" & total_study1$pcrpneumo == "yes")
sum(total_study1$pcrrsv == "yes" & total_study1$pcrpneumo == "yes")
sum(total_study1$pcradeno == "yes" & total_study1$pcrpneumo == "yes")
sum(total_study1$pcradeno == "yes" & total_study1$pcrrhino == "yes")
sum(total_study1$pcrpneumo== "yes" & total_study1$pcrrhino == "yes")


#PCR_testing  <- count(total_study1$PCRoutcome)


negs <- sum(total_study1$pcrrsv == "no" & total_study1$pcrinfa == "no" & total_study1$pcrinfb == "no" & 
        total_study1$pcrpneumo== "no" & total_study1$pcrrhino == "no" & total_study1$pcradeno == "no" &
        total_study1$pcrpara1 == "no" & total_study1$pcrpara2 == "no" & total_study1$pcrpara3 == "no" & 
        total_study1$pcrpara4 == "no")

if(sum(total_study1$PCRoutcome == "") != negs){
  message("Number of missing data in PCR_testing does not equal number of negs - check calculations!")
}

# # remove cobas invalids or indeterminates on first test from primary analysis
# 
# total_study1 <- subset(total_study1, total_study1$cobastest != "invalid retest")
#                                     #total_study1$cobasresult != "invalid")
# ncobasinvalid <- nrow(subset(total_study1, total_study1$cobasresult != "invalid"))
# ncobasindeterminate <- nrow(subset(total_study1, total_study1$cobasresult != "indeterminate"))
# 
total_study <- total_study1
final_participantnum = nrow(total_study1)


# #initial_partnum - removed4missing - removed4invalid
# if (final_participantnum != initial_partnum - removed4missing  - 
#     removed4failure - ncobasinvalid - ncobasindeterminate) 
#     message("WARNING final number of partipants with removals
#                                does not match initial number")

message(paste(removed4missing, "total removed for missing results" ))
message(paste(missing_pcrs, "participants with missing PCR results"))
message(paste(missing_cobas, "participants with missing cobas results"))
# message(paste(npcrfailure, "participants with failed PCR test"))
# message(paste(ncobasfailure, "participants with failed cobas test"))
# message(paste(removed4failure, "total participants removed with failed tests"))
# message(paste(ncobasinvalid, "participants removed for invalid cobas results"))
# message(paste(ncobasindeterminate, "participants removed for indeterminate cobas results"))
 
message(paste(nrow(total_study), "participants remaining in the study"))

########################################################################
#number of participants in each location
countpart = ddply(total_study,~Location, summarise, NumberofParticipants = length(Location))

countpart = totalscol_table(countpart)
assign("countpart",countpart,envir = .GlobalEnv)

nrow(total_study)
########################################################################
assign("total_study",total_study,envir = .GlobalEnv)

#####  STUDY DURATION ######

dates_ordered = total_study$admissiondate[order(total_study$admissiondate)]
mindate = min(dates_ordered)
mindate

maxdate = max(dates_ordered)

assign("mindate",mindate,envir = .GlobalEnv)
assign("maxdate",maxdate,envir = .GlobalEnv)

# write out IDs of all included participants
write.csv(row.names(total_study), "includedparticipants.csv")
write.csv(total_study,"output/total_study.csv")


 
}
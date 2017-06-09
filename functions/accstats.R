
accstats <- function(results)

  accuracystats = 0
  stats = 0
  results_display = 0
  results_display = results
  stats <- data.frame(results)
  assign("stats",stats,envir = .GlobalEnv)


row.names(results_display) = results_display[,1]
results_display$Location <- NULL
results_display

# create a data frame with these values in order to calculate Diganostic accuracy stats
# allow this data frame to be in the global environment


#Diagnostic Performance for all Locations

#calculate the test performance, sens, spec etc.
source("functions/Dx2b2StatsFntest.R")
accuracystats <- Dx2b2Statstest(alpha, Cohort)

message("Summarising the Sensitivity per site")
accuracystats_Sn = ddply(DxAfull,~Location,summarise, LCI = round(100*SnLCI,dig), 
                         Sensitivity = round(100*sens,dig), UCI = round(100*SnUCI,dig))
row.names(accuracystats_Sn) = accuracystats_Sn[,1]
accuracystats_Sn$Location <- NULL

accuracystats_Sn

message("Summarising the Specificity per site")
accuracystats_Sp = ddply(DxAfull,~Location,summarise, LCI = round(100*SpLCI,dig), 
                         Specificity = round(100*spec,dig), UCI = round(100*SpUCI,dig))
row.names(accuracystats_Sp) = accuracystats_Sp[,1]
accuracystats_Sp$Location <- NULL


accuracystats_Sp
################# ALL SITES - SENS AND SPEC ######################################
acc_stats = ddply(DxAfull,~Location,summarise, Sensitivity = round(100*sens,dig),
                  LCI = round(100*SnLCI,dig), UCI = round(100*SnUCI,dig),
                  Specificity = round(100*spec,dig), 
                  lCI = round(100*SpLCI,dig), uCI = round(100*SpUCI,dig))
row.names(acc_stats) = acc_stats[,1]
acc_stats$Location <- NULL


Combined <-c(round(100*SnSp(Tp,Fn),dig),round(100*SnSpCIs(alpha,SnSp(Tp,Fn),Tp+Fn)$lower,dig), 
             round(100*SnSpCIs(alpha,SnSp(Tp,Fn),Tp+Fn)$upper,dig), round(100*SnSp(Tn,Fp),dig),
             round(100*SnSpCIs(alpha,SnSp(Tn,Fp),Tn+Fp)$lower,dig), 
             round(100*SnSpCIs(alpha,SnSp(Tn,Fp),Tn+Fp)$upper,dig))



acc_stats <-  rbind (acc_stats,Combined)
rownames(acc_stats)[5]<- "Combined"

acc_statsC <- acc_stats


################# ALL SITES - ppv and npv ######################################
acc_stats_pv = ddply(DxAfull,~Location,summarise, PPV = round(100*ppv,dig),
                  LCI = round(100*ppvLCI,dig), UCI = round(100*ppvUCI,dig),
                  NPV = round(100*npv,dig), 
                  lCI = round(100*npvLCI,dig), uCI = round(100*npvUCI,dig))
row.names(acc_stats_pv) = acc_stats_pv[,1]
acc_stats_pv$Location <- NULL


  Combined <-c(round(100*Tp/(Tp + Fp),dig),round(100*SnSpCIs(alpha,Tp/(Tp + Fp),Tp+Fp)$lower,dig), 
               round(100*SnSpCIs(alpha,Tp/(Tp + Fp),Tp+Fp)$upper,dig), round(100*Tn/(Tn+Fn),dig),
               round(100*SnSpCIs(alpha,Tn/(Tn+Fn),Tn+Fn)$lower,dig), 
               round(100*SnSpCIs(alpha,Tn/(Tn+Fn),Tn+Fn)$upper,dig)       
  )



acc_stats_pv <-  rbind (acc_stats_pv,Combined)
rownames(acc_stats_pv)[5]<- "Combined"



acc_stats_pvC = acc_stats_pv 

######## write tables to csv files  for inclusion into report ###############

write.csv(acc_statsC, "output/accuracy_statistics_SnSp.csv")
write.csv(acc_stats_pvC, "output/accuracy_statistics_PPVNPV.csv")




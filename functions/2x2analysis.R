# R function script for  analysis for RSV
rsvanalysis <- function(cutoffdate)
  
################SET UP DATA FRAME FOR ANALYSIS #########################
df_npt = 0

agreement_tps = 0
disagreement_fps = 0
disagreement_fns = 0
agreement_tns = 0

Location <- total_study$Location
x <- rownames(total_study)

#RSV TPs
TP = sum(total_study$cLoutcome == "RSV" & total_study$PCRoutcome == "RSV")



#create a data frame with just NPT results
df_npt <-  data.frame(x,total_study$cLoutcome, agreement_tps,
                     disagreement_fps,disagreement_fns,agreement_tns, Location)

df_npt$agreement_tps[(total_study$cL == "RSV" & total_study$PCRvirustype == "RSV") &
                       total_study$admissiondate <= cutoffdate] <- 1
df_npt$disagreement_fps[(total_study$cobasresult == "RSVpos" & total_study$PCRvirustype != "RSV")&
                          total_study$admissiondate <= cutoffdate] <- 1
df_npt$disagreement_fns[(total_study$cobasresult == "RSVneg" & total_study$PCRvirustype == "RSV")&
                          total_study$admissiondate <= cutoffdate] <- 1
df_npt$agreement_tns[(total_study$cobasresult == "RSVneg" & total_study$PCRvirustype != "RSV")&
                       total_study$admissiondate <= cutoffdate] <- 1

discrepant_fps <- subset(total_study, (total_study$cobasresult == "RSVpos" & total_study$PCRvirustype != "RSV"))
discrepant_fns <- subset(total_study, (total_study$cobasresult == "RSVneg" & total_study$PCRvirustype == "RSV"))

write.csv(discrepant_fps, file="output/discrepant_fps.csv")
write.csv(discrepant_fns, file="output/discrepant_fns.csv")

# will need to do further analysis here on virus type with PCR

# calculate  Tp, Fp, Tn, Fn
length(df_npt$agreement_tps)
Tp = sum(df_npt$agreement_tps)
length(df_npt$disagreement_fps)
Fp = sum(df_npt$disagreement_fps)
length(df_npt$disagreement_fns)
Fn = sum(df_npt$disagreement_fns)
length(df_npt$agreement_tns)
Tn = sum(df_npt$agreement_tns)


# return list to be used in main program
result <- list(df_npt = df_npt,Tp = Tp, Fp = Fp, Tn = Tn, Fn = Fn)

return(result)



# script to create table to summarize results

#load functions script
source("functions/functions.R")

rnames <- c("Prevalence", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
              "False positive rate", "False negative rate")

# combined flu A and B

  resultssum = results


Tp_dri = resultssum[1,2]
Fn_dri = resultssum[1,5]
Fp_dri = resultssum[1,3]
Tn_dri = resultssum[1,4]
num_dri = resultssum[1,6]

prev_dri = paste(round(100*(Tp_dri+Fn_dri)/num_dri,dig),"%")

sens_dri = paste((round(100*SnSp(Tp_dri,Fn_dri),dig)),"% (",
round(100*SnSpCIs(alpha,SnSp(Tp_dri,Fn_dri),Tp_dri+Fn_dri)$lower,dig),"-", 
round(100*SnSpCIs(alpha,SnSp(Tp_dri,Fn_dri),Tp_dri+Fn_dri)$upper,dig), ")")

spec_dri = paste((round(100*SnSp(Tn_dri,Fp_dri),dig)),"% (",
                   round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fp_dri),Tn_dri+Fp_dri)$lower,dig),"-", 
                   round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fp_dri),Tn_dri+Fp_dri)$upper,dig),")")

ppv_dri = paste((round(100*SnSp(Tp_dri,Fp_dri),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tp_dri,Fp_dri),Tp_dri+Fp_dri)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tp_dri,Fp_dri),Tp_dri+Fp_dri)$upper,dig),")")

npv_dri = paste((round(100*SnSp(Tn_dri,Fn_dri),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fn_dri),Tn_dri+Fn_dri)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fn_dri),Tn_dri+Fn_dri)$upper,dig),")")

fpr_dri = paste((round(100*SnSp(Fp_dri,Tp_dri),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fp_dri,Tp_dri),Tp_dri+Fp_dri)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fp_dri,Tp_dri),Tp_dri+Fp_dri)$upper,dig),")")

fnr_dri = paste((round(100*SnSp(Fn_dri,Tn_dri),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fn_dri,Tn_dri),Tn_dri+Fn_dri)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fn_dri,Tn_dri),Tn_dri+Fn_dri)$upper,dig),")")



DRI_CI <- c(prev_dri,sens_dri, spec_dri, ppv_dri, npv_dri, fpr_dri,fnr_dri )


Tp_ngh = resultssum[2,2]
Fn_ngh = resultssum[2,5]
Fp_ngh = resultssum[2,3]
Tn_ngh = resultssum[2,4]
num_ngh = resultssum[2,6]


prev_ngh = paste(round(100*(Tp_ngh+Fn_ngh)/num_ngh,dig),"%")

sens_ngh = paste((round(100*SnSp(Tp_ngh,Fn_ngh),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tp_ngh,Fn_ngh),Tp_ngh+Fn_ngh)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tp_ngh,Fn_ngh),Tp_ngh+Fn_ngh)$upper,dig), ")")

spec_ngh = paste((round(100*SnSp(Tn_ngh,Fp_ngh),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fp_ngh),Tn_ngh+Fp_ngh)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fp_ngh),Tn_ngh+Fp_ngh)$upper,dig),")")

ppv_ngh = paste((round(100*SnSp(Tp_ngh,Fp_ngh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tp_ngh,Fp_ngh),Tp_ngh+Fp_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tp_ngh,Fp_ngh),Tp_ngh+Fp_ngh)$upper,dig),")")

npv_ngh = paste((round(100*SnSp(Tn_ngh,Fn_ngh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fn_ngh),Tn_ngh+Fn_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fn_ngh),Tn_ngh+Fn_ngh)$upper,dig),")")

fpr_ngh = paste((round(100*SnSp(Fp_ngh,Tp_ngh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fp_ngh,Tp_ngh),Tp_ngh+Fp_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fp_ngh,Tp_ngh),Tp_ngh+Fp_ngh)$upper,dig),")")

fnr_ngh = paste((round(100*SnSp(Fn_ngh,Tn_ngh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fn_ngh,Tn_ngh),Tn_ngh+Fn_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fn_ngh,Tn_ngh),Tn_ngh+Fn_ngh)$upper,dig),")")


row.names<- c("Prevalence", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
              "False positive rate", "False negative rate")
NGH_CI <- c(prev_ngh,sens_ngh, spec_ngh, ppv_ngh, npv_ngh, fpr_ngh,fnr_ngh )
NGH_CI


Tp_rhh = resultssum[3,2]
Fn_rhh = resultssum[3,5]
Fp_rhh = resultssum[3,3]
Tn_rhh = resultssum[3,4]
num_rhh = resultssum[3,6]

prev_rhh = paste(round(100*(Tp_rhh+Fn_rhh)/num_rhh,dig),"%")

sens_rhh = paste((round(100*SnSp(Tp_rhh,Fn_rhh),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tp_rhh,Fn_rhh),Tp_rhh+Fn_rhh)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tp_rhh,Fn_rhh),Tp_rhh+Fn_rhh)$upper,dig), ")")

spec_rhh = paste((round(100*SnSp(Tn_rhh,Fp_rhh),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tn_rhh,Fp_rhh),Tn_rhh+Fp_rhh)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tn_rhh,Fp_rhh),Tn_rhh+Fp_rhh)$upper,dig),")")

ppv_rhh = paste((round(100*SnSp(Tp_rhh,Fp_rhh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tp_rhh,Fp_rhh),Tp_rhh+Fp_rhh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tp_rhh,Fp_rhh),Tp_rhh+Fp_rhh)$upper,dig),")")

npv_rhh = paste((round(100*SnSp(Tn_rhh,Fn_rhh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tn_rhh,Fn_rhh),Tn_rhh+Fn_rhh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tn_rhh,Fn_rhh),Tn_rhh+Fn_rhh)$upper,dig),")")

fpr_rhh = paste((round(100*SnSp(Fp_rhh,Tp_rhh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fp_rhh,Tp_rhh),Tp_rhh+Fp_rhh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fp_rhh,Tp_rhh),Tp_rhh+Fp_rhh)$upper,dig),")")

fnr_rhh = paste((round(100*SnSp(Fn_rhh,Tn_rhh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fn_rhh,Tn_rhh),Tn_rhh+Fn_rhh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fn_rhh,Tn_rhh),Tn_rhh+Fn_rhh)$upper,dig),")")


row.names<- c("Prevalence", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
              "False positive rate", "False negative rate")
RHH_CI <- c(prev_rhh,sens_rhh, spec_rhh, ppv_rhh, npv_rhh, fpr_rhh,fnr_rhh )
RHH_CI

Tp_rvi = resultssum[4,2]
Fn_rvi = resultssum[4,5]
Fp_rvi = resultssum[4,3]
Tn_rvi = resultssum[4,4]
num_rvi = resultssum[4,6]

prev_rvi = paste(round(100*(Tp_rvi+Fn_rvi)/num_rvi,dig),"%")

sens_rvi = paste((round(100*SnSp(Tp_rvi,Fn_rvi),dig)),"% (",round(100*SnSpCIs(alpha,SnSp(Tp_rvi,Fn_rvi),Tp_rvi+Fn_rvi)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tp_rvi,Fn_rvi),Tp_rvi+Fn_rvi)$upper,dig), ")")

spec_rvi = paste((round(100*SnSp(Tn_rvi,Fp_rvi),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tn_rvi,Fp_rvi),Tn_rvi+Fp_rvi)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tn_rvi,Fp_rvi),Tn_rvi+Fp_rvi)$upper,dig),")")

ppv_rvi = paste((round(100*SnSp(Tp_rvi,Fp_rvi),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tp_rvi,Fp_rvi),Tp_rvi+Fp_rvi)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tp_rvi,Fp_rvi),Tp_rvi+Fp_rvi)$upper,dig),")")

npv_rvi = paste((round(100*SnSp(Tn_rvi,Fn_rvi),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tn_rvi,Fn_rvi),Tn_rvi+Fn_rvi)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tn_rvi,Fn_rvi),Tn_rvi+Fn_rvi)$upper,dig),")")

fpr_rvi = paste((round(100*SnSp(Fp_rvi,Tp_rvi),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fp_rvi,Tp_rvi),Tp_rvi+Fp_rvi)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fp_rvi,Tp_rvi),Tp_rvi+Fp_rvi)$upper,dig),")")

fnr_rvi = paste((round(100*SnSp(Fn_rvi,Tn_rvi),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fn_rvi,Tn_rvi),Tn_rvi+Fn_rvi)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fn_rvi,Tn_rvi),Tn_rvi+Fn_rvi)$upper,dig),")")


row.names<- c("Prevalence", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
              "False positive rate", "False negative rate")
RVI_CI <- c(prev_rvi,sens_rvi, spec_rvi, ppv_rvi, npv_rvi, fpr_rvi,fnr_rvi )
RVI_CI


Tpc = Tp_dri+ Tp_ngh + Tp_rhh + Tp_rvi
Fpc = Fp_dri+ Fp_ngh + Fp_rhh + Fp_rvi
Tnc = Tn_dri+ Tn_ngh + Tn_rhh + Tn_rvi
Fnc = Fn_dri+ Fn_ngh + Fn_rhh + Fn_rvi

prev_combo = paste(round(100*(Tp+Fn)/(num_dri + num_rhh + num_ngh + num_rvi),dig),"%")

sens_comb = paste((round(100*SnSp(Tpc,Fnc),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tpc,Fnc),Tpc+Fnc)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tpc,Fnc),Tpc+Fnc)$upper,dig), ")")

spec_comb = paste((round(100*SnSp(Tnc,Fpc),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tnc,Fpc),Tnc+Fpc)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tnc,Fpc),Tnc+Fpc)$upper,dig),")")

ppv_comb = paste((round(100*SnSp(Tpc,Fpc),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tpc,Fpc),Tpc+Fpc)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tpc,Fpc),Tpc+Fpc)$upper,dig),")")

npv_comb = paste((round(100*SnSp(Tnc,Fnc),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tnc,Fnc),Tnc+Fnc)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tnc,Fnc),Tnc+Fnc)$upper,dig),")")

fpr_comb = paste((round(100*SnSp(Fpc,Tpc),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fpc,Tpc),Tpc+Fpc)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fpc,Tpc),Tpc+Fpc)$upper,dig),")")

fnr_comb = paste((round(100*SnSp(Fnc,Tnc),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fnc,Tnc),Tnc+Fnc)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fnc,Tnc),Tnc+Fnc)$upper,dig),")")


row.names<- c("Prevalence", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
              "False positive rate", "False negative rate")
Comb_CI <- c(prev_combo,sens_comb, spec_comb, ppv_comb, npv_comb, fpr_comb,fnr_comb)
Comb_CI



summary_results <-cbind(DRI_CI, NGH_CI, RHH_CI, RVI_CI, Comb_CI )
row.names(summary_results) <- rnames
colnames(summary_results) <- c("DRI", "NGH", "RHH", "RVI", "Combined")
summary_results

if (flutype== "combined"){
    write.csv(summary_results,file = ("output/combo_results.csv"))
}

if (flutype== "A"){
  write.csv(summary_results,file = ("output/a_results.csv"))
}

if (flutype== "B"){
  write.csv(summary_results,file = ("output/b_results.csv"))
}

#discrepancy analysis table
# DRI one Fps -> Tps
prev_dri2 = paste(round(100*(Tp_dri + 1 +Fn_dri)/num_dri,dig),"%")

sens_dri2 = paste((round(100*SnSp(Tp_dri + 1,Fn_dri),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tp_dri + 1,Fn_dri),Tp_dri + 1+Fn_dri)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tp_dri+ 1,Fn_dri),Tp_dri+ 1 +Fn_dri)$upper,dig), ")")

spec_dri2 = paste((round(100*SnSp(Tn_dri,Fp_dri - 1),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fp_dri- 1),Tn_dri+Fp_dri- 1)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fp_dri- 1),Tn_dri+Fp_dri- 1)$upper,dig),")")

ppv_dri2 = paste((round(100*SnSp(Tp_dri+ 1,Fp_dri- 1),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tp_dri+ 1,Fp_dri - 1),Tp_dri+ 1+Fp_dri - 1)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tp_dri+ 1,Fp_dri-1),Tp_dri+Fp_dri)$upper,dig),")")

npv_dri2 = paste((round(100*SnSp(Tn_dri,Fn_dri),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fn_dri),Tn_dri+Fn_dri)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tn_dri,Fn_dri),Tn_dri+Fn_dri)$upper,dig),")")

fpr_dri2 = paste((round(100*SnSp(Fp_dri- 1,Tp_dri+ 1),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fp_dri + 1,Tp_dri - 1),Tp_dri+Fp_dri)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fp_dri + 1,Tp_dri - 1),Tp_dri+Fp_dri)$upper,dig),")")

fnr_dri2 = paste((round(100*SnSp(Fn_dri,Tn_dri),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fn_dri,Tn_dri),Tn_dri+Fn_dri)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fn_dri,Tn_dri),Tn_dri+Fn_dri)$upper,dig),")")



DRI_CI2 <- c(prev_dri2,sens_dri2, spec_dri2, ppv_dri2, npv_dri2, fpr_dri2,fnr_dri2 )


#NGH 3 FPs -> TPs
prev_ngh2 = paste(round(100*(Tp_ngh + 3+Fn_ngh)/num_ngh,dig),"%")

sens_ngh2 = paste((round(100*SnSp(Tp_ngh + 3,Fn_ngh),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tp_ngh+ 3,Fn_ngh),Tp_ngh + 3+Fn_ngh)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tp_ngh+ 3,Fn_ngh),Tp_ngh + 3+Fn_ngh)$upper,dig), ")")

spec_ngh2 = paste((round(100*SnSp(Tn_ngh,Fp_ngh-3),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fp_ngh-3),Tn_ngh+Fp_ngh-3)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fp_ngh-3),Tn_ngh+Fp_ngh-3)$upper,dig),")")

ppv_ngh2 = paste((round(100*SnSp(Tp_ngh+3,Fp_ngh-3),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tp_ngh+3,Fp_ngh-3),Tp_ngh+Fp_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tp_ngh+3,Fp_ngh-3),Tp_ngh+Fp_ngh)$upper,dig),")")

npv_ngh2 = paste((round(100*SnSp(Tn_ngh,Fn_ngh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fn_ngh),Tn_ngh+Fn_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Tn_ngh,Fn_ngh),Tn_ngh+Fn_ngh)$upper,dig),")")

fpr_ngh2 = paste((round(100*SnSp(Fp_ngh-3,Tp_ngh+3),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fp_ngh-3,Tp_ngh+3),Tp_ngh+Fp_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fp_ngh-3,Tp_ngh+3),Tp_ngh+Fp_ngh)$upper,dig),")")

fnr_ngh2 = paste((round(100*SnSp(Fn_ngh,Tn_ngh),dig)),"% (",
                round(100*SnSpCIs(alpha,SnSp(Fn_ngh,Tn_ngh),Tn_ngh+Fn_ngh)$lower,dig),"-", 
                round(100*SnSpCIs(alpha,SnSp(Fn_ngh,Tn_ngh),Tn_ngh+Fn_ngh)$upper,dig),")")


row.names<- c("Prevalence", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
              "False positive rate", "False negative rate")
NGH_CI2 <- c(prev_ngh2,sens_ngh2, spec_ngh2, ppv_ngh2, npv_ngh2, fpr_ngh2,fnr_ngh2 )
NGH_CI2


Tpc2 = Tp_dri + 1 + Tp_ngh + 3 + Tp_rhh + Tp_rvi
Fpc2 = Fp_dri - 1 + Fp_ngh - 3 + Fp_rhh + Fp_rvi
Tnc2 = Tn_dri+ Tn_ngh + Tn_rhh + Tn_rvi
Fnc2 = Fn_dri+ Fn_ngh + Fn_rhh + Fn_rvi

prev_combo2 = paste(round(100*(Tp + 4 +Fn)/(num_dri + num_rhh + num_ngh + num_rvi),dig),"%")

sens_comb2 = paste((round(100*SnSp(Tpc2,Fnc2),dig)),"% (",
                  round(100*SnSpCIs(alpha,SnSp(Tpc2,Fnc2),Tpc2+Fnc2)$lower,dig),"-", 
                  round(100*SnSpCIs(alpha,SnSp(Tpc2,Fnc2),Tpc2+Fnc2)$upper,dig), ")")

spec_comb2 = paste((round(100*SnSp(Tnc2,Fpc2),dig)),"% (",
                  round(100*SnSpCIs(alpha,SnSp(Tnc2,Fpc2),Tnc2+Fpc2)$lower,dig),"-", 
                  round(100*SnSpCIs(alpha,SnSp(Tnc2,Fpc2),Tnc2+Fpc2)$upper,dig),")")

ppv_comb2 = paste((round(100*SnSp(Tpc2,Fpc2),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tpc2,Fpc2),Tpc2+Fpc2)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tpc2,Fpc2),Tpc2+Fpc2)$upper,dig),")")

npv_comb2 = paste((round(100*SnSp(Tnc2,Fnc2),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Tnc2,Fnc2),Tnc2+Fnc2)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Tnc2,Fnc2),Tnc2+Fnc2)$upper,dig),")")

fpr_comb2 = paste((round(100*SnSp(Fpc2,Tpc2),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Fpc2,Tpc2),Tpc2+Fpc2)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Fpc2,Tpc2),Tpc2+Fpc2)$upper,dig),")")

fnr_comb2 = paste((round(100*SnSp(Fnc2,Tnc2),dig)),"% (",
                 round(100*SnSpCIs(alpha,SnSp(Fnc2,Tnc2),Tnc2+Fnc2)$lower,dig),"-", 
                 round(100*SnSpCIs(alpha,SnSp(Fnc2,Tnc2),Tnc2+Fnc2)$upper,dig),")")


row.names<- c("Prevalence", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value",
              "False positive rate", "False negative rate")
Comb_CI2 <- c(prev_combo2,sens_comb2, spec_comb2, ppv_comb2, npv_comb2, fpr_comb2,fnr_comb2)
Comb_CI2

summary_results <-cbind(DRI_CI2, NGH_CI2, RHH_CI, RVI_CI, Comb_CI2 )
row.names(summary_results) <- rnames
colnames(summary_results) <- c("DRI", "NGH", "RHH", "RVI", "Combined")
summary_results

if (flutype== "combined"){
  write.csv(summary_results,file = ("output/discrepant_results.csv"))
}

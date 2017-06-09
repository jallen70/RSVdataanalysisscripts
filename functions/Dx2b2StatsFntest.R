Dx2b2Statstest <- function(alpha, Cohort) 
  
  Total = 0
  WithCondition <- stats$tps + stats$fns
  WithoutCondition <- stats$fps + stats$tns
  TestPos <- stats$tps + stats$fps
  TestNeg <- stats$fns + stats$tns
  Total <- stats$total

  tps = stats$tps
  fps = stats$fps
  tns = stats$tns
  fns = stats$fns
  Location = stats$Location
    
  #if (Total != TestPos + TestNeg) message("Warning, inconsistency in total numbers of particpants")
  if (Cohort) Prevalence <- WithCondition/Total else Prevalence <- NA
  
  DxA <- data.frame(Location,tps, fns, WithCondition, 
                           fps, tns, WithoutCondition, 
                           TestPos, TestNeg, Total)
  
  #check total numbers are the same as in stats data frame 
   if (all(DxA$Total != (DxA$TestPos + DxA$TestNeg)))
    message("Warning, inconsistency in total numbers of particpants")
  
  
  source("functions/functions.R")  
  
DxAfull = mutate(DxA, summarize, 
       sens = SnSp(tps,fns),  SnLCI = SnSpCIs(alpha,sens,WithCondition)$lower,
       SnUCI = SnSpCIs(alpha,sens,WithCondition)$upper,
       spec = SnSp(tns, fps), SpLCI = SnSpCIs(alpha,spec,WithoutCondition)$lower,
       SpUCI = SnSpCIs(alpha,spec,WithoutCondition)$upper,
       ppv = tps/TestPos, npv = tns/TestNeg, ppvLCI = SnSpCIs(alpha,ppv,TestPos)$lower,
       ppvUCI = SnSpCIs(alpha,ppv,TestPos)$upper, 
       npv = tns/TestNeg, npvLCI = SnSpCIs(alpha,npv,TestNeg)$lower,
       npvUCI = SnSpCIs(alpha,npv,TestNeg)$upper,
       fpspv = fps / TestPos, fpspvLCI = SnSpCIs(alpha,fpspv,TestPos)$lower,
       fpspvUCI = SnSpCIs(alpha,fpspv,TestPos)$upper,
       fnpv = fns / TestNeg, fnpvLCI = SnSpCIs(alpha,fnpv,TestNeg)$lower,
       fnpvUCI = SnSpCIs(alpha,fnpv,TestNeg)$upper, 
       LRplus= LRp(sens,spec), LRplusCI = LRCIs(alpha,LRplus,WithCondition,WithoutCondition,tps,fps)$lower,
       LRplusCI = LRCIs(alpha,LRplus,WithCondition,WithoutCondition,tps,fps)$upper,
       LRneg = LRn(sens,spec), LRNegCI = LRCIs(alpha,LRneg,WithCondition,WithoutCondition,fns,tns)$lower,
       LRplusCI = LRCIs(alpha,LRplus,WithCondition,WithoutCondition,fns,tns)$upper,
       DOR = LRplus/LRneg
         )

  
  if (! Cohort) message("Warning: This is not a cohort study, so the prevalance and predictive values do not make sense")
  
  
  
  return(DxAfull = DxAfull)




predvalues_time <- function()
 
  days = difftime(maxdate,mindate,units="days")
  
  n = seq(0,120,20)
  maxdate
  time = as.Date(mindate +n)
  timecut = as.Date(mindate + 2*n)
  time
  time
  time2 = as.Date(mindate)
  prevalence = 0
  
  ppv = 0
  ppvl = 0
  ppvu = 0
  
  npv = 0
  npvl = 0
  npvu = 0
  
  numparticipants = 0
  
  for (i in 1:7){
  # if(time[i] <= maxdate) {
     
      cutoffdate = time[i]
      time2[i] = as.Date(time[i], "%m,%d,%Y")
     
      rm(Tp, Fp, Tn, Fn, TpA, FpA, TnA, FnA, TpB, FpB, TnB, FnB)
      
      source("functions/rsvanalysis.R")
      rsvstudy <- rsvanalysis(cutoffdate)
      
      ppv[i] = SnSp(Tp,Fp)
      ppvl[i] = SnSpCIs(alpha,SnSp(Tp,Fp), Tp + Fp)$lower
      ppvu[i] = SnSpCIs(alpha,SnSp(Tp,Fp), Tp + Fp)$upper
      
      npv[i] = SnSp(Tn,Fn)
      npvl[i] = SnSpCIs(alpha,SnSp(Tn,Fn), Tn + Fn)$lower
      npvu[i] = SnSpCIs(alpha,SnSp(Tn,Fn), Tn + Fn)$upper
      
      numparticipants[i] = Tp + Fp + Tn + Fn
      prevalence[i] = (Tp+Fn)/(Tp + Fp + Tn + Fn)
      
  #  } 
  }
  
  prevalence

  ppv_time = data.frame(time2, ppv, L = ppvl, U = ppvu)
  ppv_graph = ggplot(ppv_time, aes(time2, ppv)) +
    scale_x_date(breaks = "month", labels = date_format("%b"), 
                 limits = c(mindate,maxdate)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymax = U, ymin = L)) + ylab("PPV") +
    xlab("Time during study")+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=16,face="bold"))
  ppv_graph
  
  npv_time = data.frame(time2, npv, L = npvl, U = npvu)
  npv_graph = ggplot(npv_time, aes(time2, npv)) +
    scale_x_date(breaks = "month", labels = date_format("%b"), 
                 limits = c(mindate,maxdate)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymax = U, ymin = L)) + ylab("NPV") +
    xlab("Time during study")+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=16,face="bold"))
  
  num_time = data.frame(time2, numparticipants)
  num_time = subset(num_time, numparticipants != "0")
  
  num_graph = ggplot(num_time, aes(time2, numparticipants)) +
    scale_x_date(breaks = "month", labels = date_format("%b"), 
                 limits = c(mindate,maxdate)) +
    geom_point(size = 3) + ylab("Number of participants") +
    xlab("Time during study") +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=16,face="bold"))
  num_graph
  
  prev_time = data.frame(time2, prevalence)
  prev_graph = ggplot(prev_time, aes(time, prevalence)) +
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b"), 
                 limits = c(mindate,maxdate)) +
    geom_point(size = 2) +
    ylab("Prevalence") +
    xlab("Time during study")+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=16,face="bold"))
    prev_graph
    
    prevppv= data.frame(prevalence, ppv, L = ppvl, U = ppvu)
    prevppv_graph = ggplot(prevppv, aes(prevalence, ppv)) +
      scale_y_continuous(limits = c(0.5,1)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymax = U, ymin = L)) + ylab("PPV") +
      xlab("Prevalence")+
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=16,face="bold"))
    prevppv_graph
    
    prevnpv= data.frame(prevalence, npv, L = npvl, U = npvu)
    prevnpv_graph = ggplot(prevnpv, aes(prevalence, npv)) +
      scale_y_continuous(limits = c(0.5,1)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymax = U, ymin = L)) + ylab("NPV") +
      xlab("Prevalence")+
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=16,face="bold"))
    prevnpv_graph
   
    
  mypath <- file.path( paste("figures/predictive_values", ".pdf", sep=""))
  pdf(mypath, paper = "a4r", width = 11, height = 8)
  grid.arrange(num_graph, prev_graph, ppv_graph, npv_graph, ncol = 4)
  dev.off()
  
  mypath <- file.path( paste("figures/prevppv_graph", ".pdf", sep=""))
  pdf(mypath, paper = "a4r", width = 11, height = 8)
  grid.arrange(prevppv_graph, prevnpv_graph, ncol = 2)
  dev.off()
  
  
  
  
  
  
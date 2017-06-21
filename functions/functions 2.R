#Useful functions

nmissing <- function(x) sum(x)

SnSp <- function(x,y) x/(y+x)

LRp <- function(x,y) x/ (1 - y) # Likelihood ratio of a positive test Sensitivity / (1 - Specificity) # Likelihood ratio of a positive test
LRn <- function(x,y) (1 - x) / y  # Likelihood ratio of a negative test, FNR / TNR, (1 - Sensitivity) / Specificity 
#DOR <- LRplus / LRneg


#confidence intervals
SnSpCIs <- function(alpha, x, y) {
  # x is either Sn or Sp
  # y is either withCondition or WithoutCondition
  #zSensitivity = qnorm(1-alpha/2)*sqrt((1-Sensitivity)*Sensitivity/WithCondition)
  #SensitivityCI <- c(Sensitivity - zSensitivity, Sensitivity + zSensitivity)
  zSnSp = qnorm(1-alpha/2)*sqrt((1-x)*x/y)
  SnSpCI = c(x - zSnSp, x + zSnSp)
return(list(lower = x - zSnSp, upper = x+ zSnSp))
}

LRCIs <- function(alpha,lr,w,w2,x,y){
 #lr
 #w = WithCondition
 #w2 = WithoutCondition
 #x = tps for LR+, fns for LR-
 #y = fps for LR+, tns for LR-
  zLR = qnorm(1-alpha/2)*sqrt(1/x - 1/w + 1/y - 1/w2)
  LRCI <- c( exp(log(lr) - zLR), exp(log(lr) + zLR))
  return(list(lower = exp(log(lr) - zLR), upper = exp(log(lr) + zLR)))
}


#function to create 2x2 table
Dx2by2fun <- function(){
  
Dx2by2 <- data.frame(c(Tp, Fn, Tp+Fn), c(Fp, Tn, Fp+Tn), c(Tp+Fp, Tn+Fn, Tp+Fp+Tn+Fn))
colnames(Dx2by2) <- c("Lab RT-PCR +ve", "  -ve", "Totals") # can also use names()
rownames(Dx2by2) <- c("cobas Liat +ve", "cobas Liat -ve", "Totals") # can also use row.names()

return(Dx2by2)
}

#function to create data frame and then x table for reading into LaTex.  Used for
# tables which count up a summarise data items.  
totalscol_table <-function(dataframe){
  row.names(dataframe) = dataframe[,1]
  dataframe$Location <- NULL
  
  #Summary data frame for creating xtables in latex document
  dataframe = data.frame(dataframe)
  Totals = colSums(Filter(is.integer,dataframe)) 
  dataframe = rbind(dataframe, Totals)
  num <- nlevels(dataframe$Location)
  rownames(dataframe)[num + 1]<- "Total"  # works here as there are 4 locations
  return(dataframe)

}

col_table <-function(dataframe){
  row.names(dataframe) = dataframe[,1]
  dataframe$Location <- NULL
  
  #Summary data frame for creating xtables in latex document
  dataframe = data.frame(dataframe)
  #Totals = colSums(dataframe, na.rm  = T)#colSums(Filter(is.real,dataframe)) 
  #dataframe = rbind(dataframe, Totals)
  #rownames(dataframe)[5]<- "Total"  # works here as there are 4 locations
  return(dataframe)
  
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

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


TxTanalysis <- function(data,cutoffdate,virus){
  
  
  if (virus == "RSV") pcrvirus <- "RSV/Rhino"
  
  Tp = sum(data$cLoutcome == virus & (data$PCRoutcome == virus | data$PCRoutcome == pcrvirus))
  Fp = sum(data$cLoutcome == virus & data$PCRoutcome != virus & data$PCRoutcome != pcrvirus)
  Tn = sum(data$cLoutcome != virus & data$PCRoutcome != virus & data$PCRoutcome != pcrvirus)
  Fn = sum(data$cLoutcome != virus & (data$PCRoutcome == virus | data$PCRoutcome == pcrvirus))

  result = cbind(Tp,Fp,Tn,Fn)
# return list to be used in main program
#result <- list(Tp = Tp, Fp = Fp, Tn = Tn, Fn = Fn)

return(result)

}




# function to create final outcome column for index and reference tests for TxT analysis

testing_outcome <- function(col1, col2, col3){
  
  mydata <- cbind.data.frame(col1,col2,col3)
  colnames(mydata) <- c("col1", "col2", "col3")
  mydata$outcome <- ""
  
  mydata$outcome[mydata$col1 == "RSV negative" & 
                   mydata$col2 == "Flu A negative" & 
                   mydata$col3 == "Flu B negative"] <- "Neg"
  mydata$outcome[mydata$col1 == "RSV positive" & 
                   mydata$col2 == "Flu A negative" & 
                   mydata$col3 == "Flu B negative"] <- "RSV"
  mydata$outcome[mydata$col1 == "RSV negative" & 
                   mydata$col2 == "Flu A positive" & 
                   mydata$col3 == "Flu B negative"] <- "FluA"
  mydata$outcome[mydata$col1 == "RSV negative" & 
                   mydata$col2 == "Flu A negative" & 
                   mydata$col3 == "Flu B positive"] <- "FluB"
  
  # are there any combined results?
  mydata$outcome[mydata$col1 == "RSV positive" & 
                   mydata$col2 == "Flu A negative" & 
                   mydata$col3 == "Flu B positive"] <- "RSV/FluB"
  mydata$outcome[mydata$col1 == "RSV positive" & 
                   mydata$col2 == "Flu A positive" & 
                   mydata$col3 == "Flu B negative"] <- "RSV/FluA"
  mydata$outcome[mydata$col1 == "RSV negative" & 
                   mydata$col2 == "Flu A positive" & 
                   mydata$col3 == "Flu B positive"] <- "FluA/FluB"
  mydata$outcome[mydata$col1 == "RSV positive" & 
                   mydata$col2 == "Flu A positive" & 
                   mydata$col3 == "Flu B positive"] <- "RSV/FluA/FluB"
  
  
  
  return(mydata)
}

TxTanalysis2 <- function(outcome1, outcome2, virus){
  
  
  if (virus == "RSV") pcrvirus <- "RSV/Rhino"
  
  Tp = sum(outcome1 == virus & (outcome2 == virus | outcome2 == pcrvirus))
  Fp = sum(outcome1 == virus & outcome2 != virus & outcome2 != pcrvirus)
  Tn = sum(outcome1 != virus & outcome2 != virus & outcome2 != pcrvirus)
  Fn = sum(outcome1 != virus & (outcome2 == virus | outcome2 == pcrvirus))
  
  result = cbind(Tp,Fp,Tn,Fn)
  # return list to be used in main program
  #result <- list(Tp = Tp, Fp = Fp, Tn = Tn, Fn = Fn)
  
  return(result)
  
}


TxTanalysis_loc <- function(data,cutoffdate,virus){
  
  
  if (virus == "RSV") pcrvirus <- "RSV/Rhino"
  
  df = ddply(data, ~Location, summarise, Tp = sum(cLoutcome == virus & (PCRoutcome == virus | PCRoutcome == pcrvirus)),
             Fp = sum(cLoutcome == virus & PCRoutcome != virus & PCRoutcome != pcrvirus), 
             Tn = sum(cLoutcome != virus & PCRoutcome != virus & PCRoutcome != pcrvirus),
             Fn = sum(cLoutcome != virus & (PCRoutcome == virus | PCRoutcome == pcrvirus)))
  
  
  # return list to be used in main program
  #result <- list(Tp = Tp, Fp = Fp, Tn = Tn, Fn = Fn)
  
  return(df)
  
}



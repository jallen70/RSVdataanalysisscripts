# function to change the time format in excel spreadsheet and 
# then look at the time difference between pcr and cobas

timediff <- function(total_study, difftime2){

total_study$PCR_date<- as.Date((total_study$PCR_date), "%d/%m/%Y")
total_study$cobas_date<- as.Date((total_study$cobas_date), "%d/%m/%Y")


total_study$pcrtime2 <- strptime(total_study$PCR_time, format='%I:%M %p')
total_study$pcrtime2 <- substr(total_study$pcrtime2, 11, 19) 
total_study$pcrtime2 <- str_c(total_study$PCR_date,total_study$pcrtime2) 
total_study$pcrtime2 <- as.POSIXlt(total_study$pcrtime2, tz = "GMT", format='%Y-%m-%d %H:%M:%S') 


total_study$npttime2 <- strptime(total_study$cobas_time, format='%I:%M %p')
total_study$npttime2
total_study$npttime2 <- substr(total_study$npttime2, 11, 19) 
total_study$npttime2 <- str_c(total_study$cobas_date,total_study$npttime2) 
total_study$npttime2 <- as.POSIXlt(total_study$npttime2,tz = "GMT", format='%Y-%m-%d %H:%M:%S') 

difftime2 = as.numeric(difftime(total_study$pcrtime2,total_study$npttime2, units = "days")) #, "%Y-%m-%d %H:%M:%S")
total_study$difftime = difftime2

assign("total_study",total_study,envir = .GlobalEnv)

#################PLOT BOX PLOTS######################################

total_study_timediff <- total_study

p_med = round(median(total_study_timediff$difftime),1)

box1 <- ggplot(data = total_study_timediff, aes(x= 1, y =difftime)) + 
geom_boxplot() + scale_y_continuous(limits = c(0, 15)) + 
  scale_x_continuous(limits = c(0, 2), breaks=c(-1,3)) + 
xlab("All sites") +  ylab("Time to availability of results (days)") + 
  geom_text(data=NULL, x = 1, y = p_med, 
                label = p_med, 
            size = 3, vjust = -1.5) +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=16,face="bold"))

box1

p_meds = ddply(total_study_timediff, .(Location), summarise, med =  round(median(difftime),1))


box2 <- ggplot(data = total_study_timediff, aes(x = Location, y =difftime)) + 
  geom_boxplot() + scale_y_continuous(limits = c(0, 15)) + 
  xlab("Site") +  ylab("Time to availability of results (days)") + 
  geom_text(data = p_meds, aes(x = Location, y = med, label = med), 
     size = 3, vjust = -0.5) +
  theme(axis.text=element_text(size=10), 
  axis.title=element_text(size=16,face="bold")) 

box2

mypath =file.path(paste("figures/boxdiff", ".pdf", sep=""))
pdf(mypath, paper = "a4r", width = 11, height = 8)
grid.arrange(box1, box2, ncol = 2)
dev.off()

box3 <- box2 + geom_boxplot(aes(x= 5.0, y =difftime)) +
  xlab("All sites") +    
  geom_text(data=NULL, x = 5.0, y = p_med, 
            label = p_med, 
            size = 3, vjust = -1.5) + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=16,face="bold")) 
    
box3 <- box3 + scale_x_discrete(limits=c("GNCH", "SRH", "Combined")) 
box3
###############SAVE THE PLOTS ######################################

mypath =file.path(paste("figures/boxdiffcombo", ".pdf", sep=""))
pdf(mypath, paper = "a4r", width = 11, height = 8)
grid.arrange(box3)
dev.off()




}
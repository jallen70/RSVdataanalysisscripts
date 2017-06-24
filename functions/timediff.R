# function to change the time format in excel spreadsheet and 
# then look at the time difference between pcr and cobas

timediff <- function(total_study){

total_study_timediff <- total_study
# exclude those with longer than 5 days as need to investigate whether the date the 
# pcr results became available was actually the time it was looked at rather than when it became 
# available 
total_study_timediff <- subset(total_study_timediff, total_study_timediff$difftime <= 5)

p_med = round(median(total_study_timediff$difftime, na.rm = T),1)

box1 <- ggplot(data = total_study_timediff, aes(x= 1, y =difftime)) + 
geom_boxplot() + scale_y_continuous(limits = c(0, 5)) + 
  scale_x_continuous(limits = c(0, 2), breaks=c(-1,3)) + 
xlab("All sites") +  ylab("Time to availability of results (days)") + 
  geom_text(data=NULL, x = 1, y = p_med, 
                label = p_med, 
            size = 5, vjust = -1.5) +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=16,face="bold"))

box1

p_meds = ddply(total_study_timediff, .(Location), summarise, med =  round(median(difftime, na.rm = T),1))

combo <- c("combo", p_med)
box2 <- ggplot(data = total_study_timediff, aes(x = Location, y =difftime)) + 
  geom_boxplot() + scale_y_continuous(limits = c(0, 5)) + 
  xlab("Site") +  ylab("Time to availability of results (days)") + 
  geom_text(data = p_meds, aes(x = Location, y = med, label = med), 
     size = 5, vjust = -0.5) +
  theme(axis.text=element_text(size=10), 
  axis.title=element_text(size=16,face="bold")) 

box2

mypath =file.path(paste("figures/boxdiff", ".pdf", sep=""))
pdf(mypath, paper = "a4r", width = 11, height = 8)
grid.arrange(box1, box2, ncol = 2)
dev.off()

box3 <- box2 + geom_boxplot(aes(x= 3.0, y =difftime)) +
  xlab("All sites") +    
  geom_text(data=NULL, x = 3.0, y = p_med, 
            label = p_med, 
            size = 5, vjust = -1.5) + 
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
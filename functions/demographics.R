

# demographic data - analysis carried out before removing missing data from
# accuracy analysis


demographics <- function(total_study_full){

srh <- subset(total_study_full, Location == "SRH")
gnch <- subset(total_study_full, Location == "GNCH")
# demographics  - this needs to be put into a script and data outputed.  

mean(total_study_full$age, na.rm = T)
sd(total_study_full$age, na.rm = T)
min(total_study_full$age)
max(total_study_full$age)

# #age
# dev.copy(png,paste0(figfilepath,'boxplotage.png'))
# p <- boxplot(total_study_full$age, xlab="Age (months)", cex.lab=2)
# print(p)
# dev.off()

median(total_study_full$age, na.rm = T)
age_table <- ddply(total_study_full, ~Location, summarise, mean_age = mean(age, na.rm = T), sd = sd(age, na.rm = T), median_age = median(age, na.rm = T))


#timedifferences(admissiondate, admissiontime, discdate, disctime)
total_study_full <- total_study_full %>%  
  dplyr::rowwise() %>% dplyr::mutate(los = timedifferences(discdate, disctime, admissiondate, admissiontime))

mean(total_study_full$los, na.rm = T)
sd(total_study_full$los, na.rm = T) 
los_table <- ddply(total_study_full, ~Location, summarise, mean_los = mean(los, na.rm = T), sd = sd(los, na.rm = T),
                   median_los = median(los, na.rm = T))


# diagnosis on admission
admin_dx <- count(total_study_full$admissiondiag)
srhadmin_dx <- count(srh$admissiondiag)
gnchadmin_dx <- count(gnch$admissiondiag)

#other diagnoses
admin_dxO <- count(total_study_full$dxOther)
srhadmin_dxO <- count(srh$dxOther)
gnchadmin_dxO <- count(gnch$dxOther)


# antibiotics prescribed

abx <- count(total_study_full$antibioprecaut)
srhabx <- count(srh$antibioprecaut)
gnchabx <- count(gnch$antibioprecaut)

# antiviral prescribed

avx <- count(total_study_full$antiviralprecaut)
srhavx <- count(srh$antiviralprecaut)
gnchavx <- count(gnch$antiviralprecaut)

# discharge dx

# diagnosis on admission
disc_dx <- count(total_study_full$discdx)
srhdisc_dx <- count(srh$discdx)
gnchdisc_dx <- count(gnch$discdx)

}

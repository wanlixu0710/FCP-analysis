setwd("/Volumes/efs/Nursing/Research/Maps Project/data analysis/FCP-analysis/")
library (plyr)
library (dplyr)
library (ggplot2)
library (tidyr)
library (reshape2)
library ("lme4")
library("car")
library("Rmisc")
library(simr)
rm(list=ls())

## get ready the feeding data ##
feeding_excel <- read.csv(file="/Volumes/efs/Nursing/Research/Maps Project/MAPS/Data/clinical data/feeding_first_50.csv")%>% select(1:5)
str(feeding_excel)
count <- feeding_excel %>% filter(as.numeric(PNA)<28) %>% group_by(Subject_ID,PNA)%>%summarise(summbm=sum(MBM, na.rm=TRUE), sumdbm=sum(DBM, na.rm=TRUE), sumformula=sum(Formula, na.rm=TRUE))
feeding_excel <- count %>% mutate(pmbm=summbm/(summbm+sumdbm+sumformula),pdbm=sumdbm/(summbm+sumdbm+sumformula),pform=sumformula/(summbm+sumdbm+sumformula)) %>% mutate (source="excel")

feeding_redcap <- read.csv(file="/Volumes/efs/Nursing/Research/Maps Project/MAPS/Data/clinical data/feeding_redcap.csv")
feeding_redcap <- dplyr::rename(feeding_redcap, MBM=mbm_number, DBM=dbm_number, Formula=formula_number )
str(feeding_redcap)
count <- feeding_redcap %>% filter(as.numeric(PNA)<28) %>% group_by(Subject_ID,PNA)%>%summarise(summbm=sum(MBM, na.rm=TRUE), sumdbm=sum(DBM, na.rm=TRUE), sumformula=sum(Formula, na.rm=TRUE))
feeding_redcap <- count %>% mutate(pmbm=summbm/(summbm+sumdbm+sumformula),pdbm=sumdbm/(summbm+sumdbm+sumformula),pform=sumformula/(summbm+sumdbm+sumformula)) %>% mutate (source="redcap")

feeding <- rbind(feeding_redcap, feeding_excel)



## Getting the Phical data
phical <- read.csv(file="/Volumes/home/wax11001/R_directory/phical/results_phical.csv",stringsAsFactors = F)%>%dplyr::rename(Fecal_Sample_ID_OLD=Sample.ID)
sample <- read.csv(file="/Volumes/efs/Nursing/Research/Maps Project/MAPS/Data/Stool Sample/bacoded stool sample sheet (long form).csv")
sample <- sample[,c("Subject_ID","Fecal_Sample_ID_OLD","PNA")]%>%unique()
phical <- inner_join(sample[,c("Subject_ID","Fecal_Sample_ID_OLD","PNA")],phical)
str(phical)

phical.cal <- phical %>% select(Subject_ID,Fecal_Sample_ID_OLD,PNA, FecalCal..ug.g.)%>%group_by(Subject_ID,PNA, Fecal_Sample_ID_OLD) %>%summarise(FecalCal..ug.g.=mean(FecalCal..ug.g.))

phical.cal$week[phical.cal$PNA <= 7] <- 1
phical.cal$week[phical.cal$PNA> 7 & phical.cal$PNA <= 14] <- 2
phical.cal$week[phical.cal$PNA> 14 & phical.cal$PNA <= 21] <- 3
phical.cal$week[phical.cal$PNA >21 ] <- 4

#plot
ggplot(phical.cal,aes(x=as.factor(week), y=FecalCal..ug.g., fill=factor(week)))+geom_boxplot()+theme_bw()+xlab("Postnatal age (week)") +ylab("FC level (µg/g)") + expand_limits(y=0) + scale_y_continuous(breaks=seq(0,1800, by=200)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



##antibiotics
anti_redcap <- read.csv(file="/Volumes/efs/Nursing/Research/Maps Project/MAPS/Data/clinical data/antibiotics_wx.csv")
anti_redcap$antibiotic.use  <- 1
anti_redcap$ampi[anti_redcap$ampicillin.factor > 0] <- 1
anti_redcap$gent[anti_redcap$gentamicin.factor > 0] <- 1
anti_redcap$clin [anti_redcap$clindamycin.factor > 0] <- 1
anti_redcap$cefo [anti_redcap$cefotaxime.factor > 0] <- 1
anti_redcap$ceft [anti_redcap$ceftazidime.factor  > 0] <- 1
anti_redcap$sulf [anti_redcap$sulfamethoxazoletrimethoprim.factor >0] <- 1
anti_redcap$number_anti_use <- anti_redcap$ampi+anti_redcap$gent+anti_redcap$clin+anti_redcap$cefo+anti_redcap$ceft+anti_redcap$sulf

anti_redcap$Numbers.of.antibiotic <- rowSums(cbind(anti_redcap$ampi,anti_redcap$gent,anti_redcap$clin,anti_redcap$cefo,anti_redcap$ceft,anti_redcap$sulf), na.rm=T)
anti_redcap <- anti_redcap %>% select(Subject_ID, PNA, antibiotic.use, Numbers.of.antibiotic)

anti_excel <- read.csv(file="/Volumes/efs/Nursing/Research/Maps Project/MAPS/Data/clinical data/Daily_antibiotic.40.50.csv")
anti_excel <- anti_excel %>% select(Subject_ID, PNA, antibiotic.use, Numbers.of.antibiotic)
anti <- rbind(anti_excel,anti_redcap)
anti <- unique(anti) %>% filter(Subject_ID %in% phical$Subject_ID)
anti.mean <- anti %>% select(Subject_ID, PNA) %>% unique()%>%group_by(Subject_ID)%>%count()%>%dplyr::rename(days.of.antibioitcs=n)


##number of samples
c <- phical.cal%>%select(Subject_ID,PNA)%>% group_by(Subject_ID)%>%count()
mean(c$n)
sd(c$n)

#phical value
mean(phical.cal$FecalCal..ug.g.)
sd(phical.cal$FecalCal..ug.g.)
min(phical.cal$FecalCal..ug.g.)
max(phical.cal$FecalCal..ug.g.)


#demo
demo <- read.csv(file="/Volumes/efs/Nursing/Research/Maps Project/MAPS/Data/clinical data/demo_12_17_17.csv", stringsAsFactors = F) %>% filter(Subject_ID %in% phical.cal$Subject_ID)
str(demo)
(demo$infants_ga_age_at_birth)
mean(demo$infants_ga_age_at_birth,na.rm = T)
sd(demo$infants_ga_age_at_birth,na.rm = T)
min(demo$infants_ga_age_at_birth,na.rm=T)
max(demo$infants_ga_age_at_birth,na.rm=T)

(demo$birth_weight)
mean(demo$birth_weight ,na.rm = T)
sd(demo$birth_weight ,na.rm = T)
min(demo$birth_weight ,na.rm=T)
max(demo$birth_weight ,na.rm=T)

mean(demo$birth_length ,na.rm = T)
sd(demo$birth_length,na.rm = T)
min(demo$birth_length ,na.rm=T)
max(demo$birth_length ,na.rm=T)

mean(demo$birth_head_circumference ,na.rm = T)
sd(demo$birth_head_circumference ,na.rm = T)
min(demo$birth_head_circumference ,na.rm=T)
max(demo$birth_head_circumference ,na.rm=T)

mean(demo$mother_age ,na.rm = T)
sd(demo$mother_age ,na.rm = T)
min(demo$mother_age ,na.rm=T)
max(demo$mother_age ,na.rm=T)

(demo$snapeii)
mean(demo$snapeii ,na.rm = T)
sd(demo$snapeii ,na.rm = T)
min(demo$snapeii ,na.rm=T)
max(demo$snapeii ,na.rm=T)

table(demo$gender)
table(demo$race)
table(demo$hispanic.factor)
table(demo$twins_triplets)
table(demo$prom)
table(demo$delivery)
table(demo$prom)
table(demo$resuscitation.factor)
combined <- inner_join(phical.cal, demo)


## phical by week
combined$week[combined$PNA <= 7] <- 1
combined$week[combined$PNA> 7 & combined$PNA <= 14] <- 2
combined$week[combined$PNA> 14 &combined$PNA <= 21] <- 3
combined$week[combined$PNA >21 ] <- 4

ggplot(combined,aes(x=as.factor(week), y=FecalCal..ug.g.))+geom_boxplot()+theme_bw()
table(combined$week)

mean (combined$FecalCal..ug.g.[combined$week==1])
mean (combined$FecalCal..ug.g.[combined$week==2])
mean (combined$FecalCal..ug.g.[combined$week==3])
mean (combined$FecalCal..ug.g.[combined$week==4])

sd (combined$FecalCal..ug.g.[combined$week==1])
sd (combined$FecalCal..ug.g.[combined$week==2])
sd (combined$FecalCal..ug.g.[combined$week==3])
sd (combined$FecalCal..ug.g.[combined$week==4])


# anova for the 4 weeks
a1 <- aov(combined$FecalCal..ug.g.~as.factor(combined$week))
summary(a1)
pairwise.t.test(combined$FecalCal..ug.g., combined$week, p.adj="bonf")
TukeyHSD(a1)


#????????? intra, inter class variation

summary(aov(combined$FecalCal..ug.g.~as.factor(combined$Subject_ID)))#????
# intra-indiviual CV
a <- phical.cal%>% group_by(Subject_ID)%>%summarise(mean.sub =mean(FecalCal..ug.g.), sd.sub=sd(FecalCal..ug.g.))%>%mutate(cv.sub=sd.sub/mean.sub*100)
mean(a$cv.sub)

# inter-indiviual CV
a <- phical.cal%>% group_by(PNA)%>%summarise(mean.sub =mean(FecalCal..ug.g.), sd.sub=sd(FecalCal..ug.g.))%>%mutate(cv.sub=sd.sub/mean.sub*100)
mean(a$cv.sub, na.rm=T)
str(combined)



## niss
niss <- read.csv(file="/Volumes/efs/Nursing/Research/Maps Project/MAPS/Data/clinical data/niss_wx.csv", stringsAsFactors = F) 
niss <- niss %>% filter(Subject_ID %in% phical$Subject_ID)
str(niss)

niss$week[niss$PNA <= 7] <- 1
niss$week[niss$PNA> 7 & niss$PNA <= 14] <- 2
niss$week[niss$PNA> 14 &niss$PNA <= 21] <- 3
niss$week[niss$PNA >21 ] <- 4

#plot the niss and ANOVA between groups
ggplot(niss,aes(x=as.factor(week), y=acute.freq, fill=as.factor(week)))+geom_bar(stat = "summary", fun.y = "mean")+theme_bw()
a1 <- aov(niss$acute~as.factor(niss$week))
summary(a1)
ggplot(niss,aes(x=as.factor(week), y=acute, fill=as.factor(week)))+geom_bar(stat = "summary", fun.y = "mean")+theme_bw()

combined.c <- summarySE(niss[,c( "acute", "week")], measurevar="acute", groupvars=c("week"))

ggplot(combined.c,aes(x=as.factor(week), y=acute, fill=factor(week))) +geom_bar(stat = "summary", fun.y = "mean")+theme_bw()+ geom_errorbar(aes(ymin=acute-se, ymax=acute+se), width=.3, size=1, color="black") 

a1 <- aov(niss$chronic.freq~niss$PNA)
summary(lm(chronic~PNA, data=niss))
combined.c <- summarySE(niss[,c( "chronic", "week")], measurevar="chronic", groupvars=c("week"))
ggplot(combined.c,aes(x=as.factor(week), y=chronic, fill=factor(week))) +geom_bar(stat = "summary", fun.y = "mean")+theme_bw()+ geom_errorbar(aes(ymin=chronic-se, ymax=chronic+se), width=.3, size=1, color="black") 

hist(niss$acute)
summary(lm(niss$acute~niss$PNA))

packageDescription("lme4")
citation("lme4")
packageDescription("car")$Version
citation("car")


key <- phical.cal%>%select(Subject_ID,PNA, Fecal_Sample_ID_OLD)
niss.2 <- left_join(key,niss)
niss.week.mean <- niss.2 %>%select(-PNA)%>% group_by(Subject_ID,week) %>%dplyr::summarise_each(funs(mean))
niss.week.mean <- niss.week.mean %>% rename(acute.week=acute, chronic.week=chronic, mom_breastfeed.week=mom_breastfeed, kangaroo.week=kangaroo, contact_total.week=contact_total)%>% select(-acute.freq, -chronic.freq)
niss.week.mean$Fecal_Sample_ID_OLD <- as.character(niss.week.mean$Fecal_Sample_ID_OLD)

niss.mean <- niss%>%select(-PNA) %>%select(-week) %>% group_by(Subject_ID) %>%dplyr::summarise_each(funs(mean))
niss.mean <- niss.mean %>% dplyr::rename(acute.mean=acute, chronic.mean=chronic, mom_breastfeed.mean=mom_breastfeed, kangaroo.mean=kangaroo, contact_total.mean=contact_total) %>% select(-acute.freq, -chronic.freq)
str(niss.week.mean)

str(combined)


combined <- left_join(combined, niss.week.mean)
combined <- left_join(combined, niss.mean)
combined <- left_join(combined, niss)
str(combined$acute.freq)



#descriptive
mean(combined$acute.freq, na.rm=TRUE)
sd(combined$acute.freq, na.rm=TRUE)

mean(combined$chronic.freq, na.rm=TRUE)*8
sd(combined$chronic.freq, na.rm=TRUE)*8

#anova not best, linear regression with log transformation

summary(lm(log(combined$FecalCal..ug.g.)~combined$acute+as.factor(combined$week)))
plot(lm(log(combined$FecalCal..ug.g.)~combined$acute+as.factor(combined$week)))
summary(lm(log(combined$FecalCal..ug.g.)~combined$acute+combined$PNA))



summary(lm(log(combined$FecalCal..ug.g.)~combined$chronic+as.factor(combined$week)))




#plot new
combined.c <- summarySE(combined[,c("FecalCal..ug.g.", "chronic.cut","week")], measurevar="FecalCal..ug.g.", groupvars=c("chronic.cut","week"))
ggplot(combined.c[!is.na(combined.c$chronic.cut),],aes(x=as.factor(week), y=FecalCal..ug.g., fill=factor(chronic.cut),group=(chronic.cut)))+geom_bar(stat = "summary", fun.y = "mean",  position = "dodge")+ geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, position=position_dodge(.9))  +theme_bw()+xlab("Postnatal age (week)") +ylab("FC level (µg/g)") + scale_fill_manual("Chronic pain/stress", values = c("Low" = "pink","High" = "hotpink"))+ expand_limits(y=0) + scale_y_continuous(breaks=seq(0,1000, by=50)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



ggplot(combined.c[!is.na(combined.c$chronic.cut),],aes(x=week, y=FecalCal..ug.g., colour=factor(chronic.cut),group=factor(chronic.cut)))+geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, size=1) + geom_line(size=1) +geom_point( size=3) +xlab("Postnatal age (week)") +ylab("FC level (µg/g)")+ scale_color_manual("Chronic pain/stress",values = c("High" = "red","Low" = "blue")) + expand_limits(y=0) + scale_y_continuous(breaks=0:1800*50) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

combined.c <- summarySE(combined[,c("FecalCal..ug.g.", "acute.cut","week")], measurevar="FecalCal..ug.g.", groupvars=c("acute.cut","week"))
ggplot(combined.c[!is.na(combined.c$acute.cut),],aes(x=as.factor(week), y=FecalCal..ug.g., fill=factor(acute.cut),group=(acute.cut)))+geom_bar(stat = "summary", fun.y = "mean",  position = "dodge")+ geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, position=position_dodge(.9))  +theme_bw()+xlab("Postnatal age (week)") +ylab("FC level (µg/g)") + scale_fill_manual("Acute pain/stress ", values = c("Low" = "lightblue","High" = "steelblue"))+ expand_limits(y=0) + scale_y_continuous(breaks=seq(0,1000, by=50)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(combined.c[!is.na(combined.c$acute.cut),],aes(x=week, y=FecalCal..ug.g., colour=factor(acute.cut),group=factor(acute.cut)))+geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, size=1) + geom_line(size=1) +geom_point( size=3) +xlab("Postnatal age (week)") +ylab("FC level (µg/g)")+ scale_color_manual("Acute pain/stress",values = c("High" = "red","Low" = "blue")) + expand_limits(y=0) + scale_y_continuous(breaks=0:1800*50) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





#antibiotics
combined <- left_join(combined, anti.mean)
combined$days.of.antibioitcs[is.na(combined$days.of.antibioitcs)] <- 0
anti.count <- combined[,c("Subject_ID", "days.of.antibioitcs") ]%>%unique()
mean(anti.count$days.of.antibioitcs)
sd(anti.count$days.of.antibioitcs)
max(anti.count$days.of.antibioitcs)

combined$anti.cut[combined$days.of.antibioitcs==0] <- "No"
combined$anti.cut[combined$days.of.antibioitcs>0] <- "Yes"

ggplot(combined,aes(x=as.factor(anti.cut), y=FecalCal..ug.g., colour=factor(anti.cut)))+geom_boxplot()+theme_bw()+facet_grid(~week)
table(combined$anti.cut,combined$week)
ggplot(combined,aes(x=PNA, y=FecalCal..ug.g., colour=factor(Subject_ID)))+geom_point()+geom_line()+facet_wrap(~anti.cut)+theme_bw()
mean(combined$FecalCal..ug.g.[combined$anti.cut=="No"])
sd(combined$FecalCal..ug.g.[combined$anti.cut=="No"])

mean(combined$FecalCal..ug.g.[combined$anti.cut=="Yes"])
sd(combined$FecalCal..ug.g.[combined$anti.cut=="Yes"])

t.test(combined$FecalCal..ug.g.~combined$anti.cut)
t.test(combined[combined$week==1,]$FecalCal..ug.g.~combined[combined$week==1,]$anti.cut)
t.test(combined[combined$week==2,]$FecalCal..ug.g.~combined[combined$week==2,]$anti.cut)
t.test(combined[combined$week==3,]$FecalCal..ug.g.~combined[combined$week==3,]$anti.cut)
t.test(combined[combined$week==4,]$FecalCal..ug.g.~combined[combined$week==4,]$anti.cut)
sd(combined[combined$week==4&combined$anti.cut=="Yes",]$FecalCal..ug.g.)
sd(combined[combined$week==4&combined$anti.cut=="No",]$FecalCal..ug.g.)

a3 <- aov(combined$FecalCal..ug.g.~combined$days.of.antibioitcs*as.factor(combined$week))
summary(a3)
TukeyHSD(a3)
summary(lm(combined$FecalCal..ug.g.~combined$days.of.antibioitcs+as.factor(combined$week)))

combined.c <- summarySE(combined[,c("FecalCal..ug.g.", "anti.cut","week")], measurevar="FecalCal..ug.g.", groupvars=c("anti.cut","week"))
ggplot(combined.c,aes(x=week, y=FecalCal..ug.g., colour=factor(anti.cut),group=factor(anti.cut)))+geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, size=1) + geom_line(size=1) +geom_point( size=3) +xlab("Postnatal age (week)") +ylab("FC level (µg/g)")+ scale_color_manual("Postnatal antibiotics",values = c("Yes" = "red","No" = "blue")) + expand_limits(y=0) + scale_y_continuous(breaks=0:1800*50) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


table(combined$anti.cut,combined$week)



## feeding
feeding$week[feeding$PNA <= 7] <- 1
feeding$week[feeding$PNA> 7 & feeding$PNA <= 14] <- 2
feeding$week[feeding$PNA> 14 &feeding$PNA <= 21] <- 3
feeding$week[feeding$PNA >21 ] <- 4

feeding.2 <- left_join(key,feeding)
feeding.week.mean <- feeding.2%>%select(-PNA,-Fecal_Sample_ID_OLD)%>% group_by(Subject_ID,week) %>%dplyr::summarise_each(funs(mean))
feeding.week.mean <- feeding.week.mean %>% mutate(pmbm.week=summbm/(summbm+sumdbm+sumformula), pdbm.week=sumdbm/(summbm+sumdbm+sumformula),pform.week=sumformula/(summbm+sumdbm+sumformula)) %>% select(Subject_ID, week, pmbm.week, pdbm.week, pform.week)

feeding.mean <- feeding%>%select(-PNA) %>%select(-week) %>% group_by(Subject_ID) %>%dplyr::summarise_each(funs(mean))
feeding.mean <- feeding.mean %>% mutate(pmbm.mean=summbm/(summbm+sumdbm+sumformula),pdbm.mean=sumdbm/(summbm+sumdbm+sumformula),pform.mean=sumformula/(summbm+sumdbm+sumformula))%>% select(Subject_ID,  pmbm.mean, pdbm.mean, pform.mean)
#enteral feeding introduced
feeding <- feeding %>% filter(Subject_ID %in% phical.cal$Subject_ID)
feeding$sum <- feeding$summbm+feeding$sumdbm+feeding$sumformula  
summary(feeding$sum)
a <- feeding%>%filter(sum>0)
summary(a)
b <- a %>% group_by(Subject_ID)%>%summarise(minfeeding =min(PNA))
mean(b$minfeeding)
sd(b$minfeeding)
min(b$minfeeding)
max(b$minfeeding)


# percentage of feeding
feeding.mean <- feeding.mean %>% filter(Subject_ID %in% phical.cal$Subject_ID)
mean(feeding.mean$pmbm.mean)
sd(feeding.mean$pmbm.mean)
min(feeding.mean$pmbm.mean)
max(feeding.mean$pmbm.mean)

mean(feeding.mean$pdbm.mean)
sd(feeding.mean$pdbm.mean)
min(feeding.mean$pdbm.mean)
max(feeding.mean$pdbm.mean)

mean(feeding.mean$pform.mean)
sd(feeding.mean$pform.mean)
min(feeding.mean$pform.mean)
max(feeding.mean$pform.mean)

# combine
combined <- left_join(combined, feeding)
combined <- left_join(combined, feeding.mean)
combined <- left_join(combined, feeding.week.mean)

hist(combined$FecalCal..ug.g.)
hist(log(combined$FecalCal..ug.g.))
combined$trans.fecal <- log(combined$FecalCal..ug.g.)

summary(lm(combined$trans.fecal~combined$pmbm+combined$pform+as.factor(combined$week)))

#plot
combined$pmbm.cut <- cut(combined$pmbm, breaks=c(quantile(combined$pmbm, c (0, .6, 1),na.rm=TRUE)), include.lowest=TRUE,  right = FALSE)
table(combined$pmbm.cut,combined$week)
ggplot(combined[!is.na(combined$pmbm),],aes(x=as.factor(pmbm.cut), y=FecalCal..ug.g., fill=factor(pmbm.cut)))+geom_boxplot()+theme_bw()+facet_grid(~week)
summary(lm(log(combined$FecalCal..ug.g.)~combined$pmbm+as.factor(combined$week)))
summary(lm(log(combined$FecalCal..ug.g.)~combined$pmbm.cut+as.factor(combined$week)))

combined$pdbm.cut <- cut(combined$pdbm, breaks=c(quantile(combined$pdbm, c (0, .7, 1),na.rm=TRUE)), include.lowest=TRUE, right = FALSE)
table(combined$pdbm.cut,combined$week)
ggplot(combined[!is.na(combined$pdbm),],aes(x=as.factor(pdbm.cut), y=FecalCal..ug.g., fill=factor(pdbm.cut)))+geom_boxplot()+theme_bw()+facet_grid(~week)
summary(lm(log(combined$FecalCal..ug.g.)~combined$pdbm+as.factor(combined$week)))


combined$pform.cut <- cut(combined$pform, breaks=c(quantile(combined$pform, c (0, .6, 1),na.rm=TRUE)), include.lowest=TRUE,  right = FALSE)
table(combined$pform.cut,combined$week)
ggplot(combined[!is.na(combined$pform),],aes(x=as.factor(pform.cut), y=FecalCal..ug.g., fill=factor(pform.cut)))+geom_boxplot()+theme_bw()+facet_grid(~week)
summary(lm(log(combined$FecalCal..ug.g.)~combined$pform+as.factor(combined$week)))

#plot with 70%
combined$pmbm.cut <- "non-MOM"
combined$pmbm.cut[combined$pmbm>=0.7] <-"MOM"
table(combined$pmbm.cut,combined$week)
combined.c <- summarySE(combined[,c("FecalCal..ug.g.", "pmbm.cut","week")], measurevar="FecalCal..ug.g.", groupvars=c("pmbm.cut","week"))
ggplot(combined.c,aes(x=as.factor(week), y=FecalCal..ug.g., fill=factor(pmbm.cut),group=(pmbm.cut)))+geom_bar(stat = "summary", fun.y = "mean",  position = "dodge")+ geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, position=position_dodge(.9))  +theme_bw()+xlab("Postnatal age (week)") +ylab("FC level (µg/g)") + scale_fill_manual("Feeding type", values = c("non-MOM" = "blue", "MOM" = "light blue"))+ expand_limits(y=0) + scale_y_continuous(breaks=seq(0,1000, by=50)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

combined.c <- summarySE(combined[,c("FecalCal..ug.g.", "pmbm.cut","week")], measurevar="FecalCal..ug.g.", groupvars=c("pmbm.cut","week"))
ggplot(combined.c,aes(x=week, y=FecalCal..ug.g., colour=factor(pmbm.cut),group=factor(pmbm.cut)))+geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, size=1) + geom_line(size=1) +geom_point( size=3) +xlab("Postnatal age (week)") +ylab("FC level (µg/g)")+ scale_color_manual("Feeding type",values = c("MOM" = "red","non-MOM" = "blue")) + expand_limits(y=0) + scale_y_continuous(breaks=0:1800*50) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#plot 3 groups based on with 70% # sample size for formula and HDM is too small to draw conclusion
combined$feeding.cut[combined$pmbm>0.5]="MOM"
combined$feeding.cut[combined$pdbm>0.5]="HDM"
combined$feeding.cut[combined$pform>0.5]="Formula"
table(combined$feeding.cut,combined$week)
combined.c <- summarySE(combined[,c("FecalCal..ug.g.", "feeding.cut","week")], measurevar="FecalCal..ug.g.", groupvars=c("feeding.cut","week"))
ggplot(combined.c[!is.na(combined.c$feeding.cut),],aes(x=as.factor(week), y=FecalCal..ug.g., fill=factor(feeding.cut),group=(feeding.cut)))+geom_bar(stat = "summary", fun.y = "mean",  position = "dodge")+ geom_errorbar(aes(ymin=FecalCal..ug.g.-se, ymax=FecalCal..ug.g.+se), width=.2, position=position_dodge(.9))  +theme_bw()+xlab("Postnatal age (week)") +ylab("FC level (µg/g)") + scale_fill_manual("Feeding type", values = c("MOM" = "dark blue","HDM" = "blue", "Formula" = "light blue"))+ expand_limits(y=0) + scale_y_continuous(breaks=seq(0,1000, by=50)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



## combined model

lm1 <- lm(combined$trans.fecal~combined$anti.cut+as.factor(combined$week)+combined$pmbm+combined$pform+combined$chronic+combined$infants_ga_age_at_birth+combined$gender+combined$prom+combined$delivery+combined$snapeii)
summary(lm1)
vif(lm1)
plot(lm1)


### mixed effect model
fit.null <- lmer(trans.fecal~(1|Subject_ID)+(1|PNA) , data=combined)
summary(fit.null)
AIC(fit.null)

fit1 <- lmer(trans.fecal~chronic+anti.cut+acute+pmbm+pmbm.mean+pform+pform.mean+infants_ga_age_at_birth+prom+PNA+(1+pmbm+pform+PNA|Subject_ID)+(1|PNA) , data=combined)
summary(fit1)
coefs <- data.frame(coef(summary(fit1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
AIC(fit1)
plot(fit1)


fit2 <- lmer(trans.fecal~chronic+pdbm+pdbm.mean+delivery+mom_breastfeed+infants_ga_age_at_birth+prom+(1+pdbm+PNA+anti.cut|Subject_ID)+(1|PNA) , data=combined)
summary(fit2)
coefs <- data.frame(coef(summary(fit2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
AIC(fit2)
plot(fit2)

anova(fit1, fit2)

combined$days.of.antibioitcs

##### week
phical <- phical.cal %>% select(Subject_ID,Fecal_Sample_ID_OLD,PNA, FecalCal..ug.g.)%>%group_by(Subject_ID,PNA, Fecal_Sample_ID_OLD) %>%summarise(FecalCal..ug.g.=mean(FecalCal..ug.g.))
phical$week[phical$PNA <= 7] <- 1
phical$week[phical$PNA> 7 & phical$PNA <= 14] <- 2
phical$week[phical$PNA> 14 &phical$PNA <= 21] <- 3
phical$week[phical$PNA >21 ] <- 4

phical.week <- phical %>% select(Subject_ID, week, FecalCal..ug.g.)%>%group_by(Subject_ID, week) %>%summarise(fecal.week=mean(FecalCal..ug.g.))

combined.week <- left_join(phical.week, demo)
combined.week <- left_join(combined.week, niss.week.mean)
combined.week <- left_join(combined.week, feeding.week.mean)
combined.week <- left_join(combined.week,nnnsnew )
combined.week <- left_join(combined.week, anti.mean)
combined.week$days.of.antibioitcs[is.na(combined.week$days.of.antibioitcs)] <- 0
combined.week$anti.cut[combined.week$days.of.antibioitcs==0] <- "No"
combined.week$anti.cut[combined.week$days.of.antibioitcs>0] <- "Yes"

hist(combined.week$fecal.week)
hist(log(combined.week$fecal.week))
combined.week$trans.fecal <- log(combined.week$fecal.week)

fit.null <- lmer(trans.fecal~(1|Subject_ID)+(1|week) , data=combined.week)
summary(fit.null)

fit1 <- lmer(trans.fecal~chronic.week+pdbm.week+prom+ anti.cut+week+(1+week|Subject_ID)+(1|week) , data=combined.week)
summary(fit1)

coefs <- data.frame(coef(summary(fit1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
AIC(fit1)
plot(fit1)
combined.week$week <- as.factor(combined.week$week)

write.csv(combined.week, file="test.csv")
fit2 <- lmer(trans.fecal~chronic.week+pform.week+pdbm.week+prom+anti.cut+infants_ga_age_at_birth+gender+delivery+as.factor(week)+(1+pdbm.week|Subject_ID)+(1|week) , data=combined.week) # best so far
summary(fit2)
AIC (fit2)
plot(fit2)
car::Anova(fit2)

coefs <- data.frame(coef(summary(fit2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

combined.week$pmbm.cut="non-MOM"
combined.week$pmbm.cut[combined.week$pmbm.week>=0.7] <-"MOM"
fit2 <- lmer(trans.fecal~chronic.week+pmbm.cut+prom+anti.cut+infants_ga_age_at_birth+gender+delivery+as.factor(week)+(1+pmbm.cut|Subject_ID)+(1|week) , data=combined.week) # best so far
summary(fit2)
AIC (fit2)
plot(fit2)
car::Anova(fit2)

coefs <- data.frame(coef(summary(fit2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs



vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }


summary(lm(combined$trans.fecal~combined$infants_ga_age_at_birth+combined$week))



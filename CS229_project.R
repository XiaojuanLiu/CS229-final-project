# -------------------
# Title: CS229 project
# Editor: Xiaojuan 
# Date created: 4/15/2021
# Date edited: 
#----------------------

library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(lubridate)

setwd("D:/Course/CS229/project/")
rm(list=ls())

# read in dataset
# dt <- read.csv("COVID-19_Case_Surveillance_Public_Use_Data.csv")
dt <- read.csv("./data/United States.csv")
dt1.21 <- read.csv("./data/2021VAERSData/2021VAERSDATA.csv")
dt2.21 <- read.csv("./data/2021VAERSData/2021VAERSVAX.csv")
dt3.21 <- read.csv("./data/2021VAERSData/2021VAERSSYMPTOMS.csv")

dt1.20 <- read.csv("./data/2020VAERSData/2020VAERSDATA.csv")
dt2.20 <- read.csv("./data/2020VAERSData/2020VAERSVAX.csv")
dt3.20 <- read.csv("./data/2020VAERSData/2020VAERSSYMPTOMS.csv")


# merge by id and restric to covid
df21 <- dt2.21 %>% filter(VAX_TYPE=="COVID19") %>% left_join(dt1.21, by="VAERS_ID")%>% left_join(dt3.21, by="VAERS_ID")
df20 <- dt2.20 %>% filter(VAX_TYPE=="COVID19") %>% left_join(dt1.20, by="VAERS_ID")%>% left_join(dt3.20, by="VAERS_ID")
df <- rbind(df21,df20)
write.csv(df21,"./data/2021vaers.csv",row.names = F,fileEncoding = "UTF-8")
write.csv(df20,"./data/2020vaers.csv",row.names = F,fileEncoding = "UTF-8")

# dur
df$date <- as_date(df$ONSET_DATE, format="%m/%d/%Y")
df <- df %>% fill(date) %>% fill(date)
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
table(year(df$date), useNA="always")

df$date.vax <- as_date(df$VAX_DATE, format="%m/%d/%Y")
df <- df %>% fill(date.vax) %>% fill(date.vax)
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))

table(year(df$date.vax), useNA="always")
dfout <- df[df$dur>=0&df$dur<60,]
df$dur <- as.Date(df$date)-as.Date(df$date.vax)


# daily events
df21$date <- as_date(df21$ONSET_DATE, format="%m/%d/%Y")
df21 <- df21 %>% fill(date) %>% fill(date)
table(year(df21$date), useNA="always")

daily <- df %>% group_by(date) %>% summarise(count=n())
ggplot(daily[55:nrow(daily),], aes(x=date, y=count))+geom_line(col=2)+
  labs(x="Date", y="Adverse effect events (n)")+
  theme_minimal()

daily <- df %>% group_by(date,VAX_MANU) %>% summarise(count=n())
table(daily$VAX_MANU)
daily <- daily[daily$VAX_MANU!="UNKNOWN MANUFACTURER",]
ggplot(daily[96:nrow(daily),], aes(x = date, y = count)) + 
  geom_line(aes(color = VAX_MANU)) + 
  labs(x="Date", y="Adverse effect events (n)")+
  labs(colour="VAC Manu",linetype="VAC Manu",shape="VAC Manu")+
  scale_color_manual(values = c(7,2,4)) +
  theme_minimal()
# "darkred", "steelblue",
# barplot 
hist <- c(df21$SYMPTOM1,df21$SYMPTOM2,df21$SYMPTOM3,df21$SYMPTOM4,df21$SYMPTOM5)
hist <- gsub('Injection site', '', hist)
hist1 <- data.frame(table(hist))[-1,]
hist1 <- hist1[order(hist1$Freq,decreasing=T),]
# hist1[1:15,]$hist
# hist1$hist2 <- ifelse(hist1$hist=="Injection site erythema", "Erythema",ifelse(hist1$hist=="Injection site swelling", "Swelling",hist1$hist))
# hist1$hist[11] <- "Erythema"
# hist1$hist[15] <- "Swelling"
ggplot(hist1[1:15,], aes(x = reorder(hist, -Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill=2) + 
  labs(x="", y="Frequency") + 
  scale_x_discrete(guide=guide_axis(n.dodge=2))
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# 
#
  
#scale_x_discrete(labels = abbreviate)

# dur
df21$date.vax <- as_date(df21$VAX_DATE, format="%m/%d/%Y")
df21 <- df21 %>% fill(date.vax) %>% fill(date.vax)
table(year(df21$date.vax), useNA="always")
df21$dur <- df21$date-df21$date.vax
df <- df21[df21$dur<0&df21$dur>=0,]
hist(as.numeric(df$dur))

summary(as.numeric(df21$dur))
daily <- df21 %>% group_by(date) %>% summarise(count=n())


# rate
dt$date.vax <- as_date(dt$date, "%Y-%m-%d")
df$date.vax <- as_date(df$VAX_DATE, format="%m/%d/%Y")
daily <- df %>% group_by(date.vax) %>% summarise(count=n())
dt <- dt %>% left_join(daily, by="date.vax")
dt$rate <- dt$count/dt$people_vaccinated * 10000

ggplot(dt, aes(x=date.vax, y=rate))+geom_line(col=2)+
  labs(x="Date", y="Adverse effect events (per 10000 person)")+
  theme_minimal()
summary(df$dur)




hist <- list(paste0("df21$SYMPTOM", 1:5))
hist <- c(df21$SYMPTOM1,df21$SYMPTOM2,df21$SYMPTOM3,df21$SYMPTOM4,df21$SYMPTOM5)
sort(table(hist),decreasing=T)[1:5]
barplot(prop.table(sort(table(hist),decreasing=T)[1:10]))



### problem 
# recode history 
#Taking Other Medicine,Prior Vaccine,Allergic History,Type-2 Diabetes, Hypertension, Arthritis, Asthma, Migraine,
#High cholesterol, Abnormal Blood Pressure,Chronic Obstructive Pulmonary Disease,GERD, 
#Anxiety, Obesity, depression,Thyroid Disorder,Anemia, Dementia, Cancer, Kidney Disease,
#Hyperlipidemia, Heart Disease, Covid-19 Positive History, Atrial Fibrillation, Pain Symptoms,Hospitalized
for (i in 1:nrow(df)){
  df$Allergic_his[i] <- ifelse(grepl("[Aa]llergic", df$HISTORY[i], fixed = TRUE),1,0)
  df$diabetes[i] <- ifelse(grepl("[Dd]iabete", df$HISTORY[i], fixed = TRUE),1,0)
  df$hypertension[i] <- ifelse(grepl("[Hy]ypertension|high blood", df$HISTORY[i], fixed = TRUE),1,0)
  df$arthritis[i] <- ifelse(grepl("[Aa]rthritis", df$HISTORY[i], fixed = TRUE),1,0)
  df$Asthma[i] <- ifelse(grepl("[Aa]sthma", df$HISTORY[i], fixed = TRUE),1,0)
  df$Migraine[i] <- ifelse(grepl("[Mm]igraine", df$HISTORY[i], fixed = TRUE),1,0)
  df$copd[i] <- ifelse(grepl("[Cc]opd|COPD", df$HISTORY[i], fixed = TRUE),1,0)
  df$Anxiety[i] <- ifelse(grepl("[Aa]nxiety", df$HISTORY[i], fixed = TRUE),1,0)
  df$obesity[i] <- ifelse(grepl("[Oo]besity", df$HISTORY[i], fixed = TRUE),1,0)
  df$depression[i] <- ifelse(grepl("[Dd]epression", df$HISTORY[i], fixed = TRUE),1,0)
  df$Thyroid[i] <- ifelse(grepl("[Tt]hyroid", df$HISTORY[i], fixed = TRUE),1,0)
  df$Anemia[i] <- ifelse(grepl("[Aa]nemia", df$HISTORY[i], fixed = TRUE),1,0)
  df$Dementia[i] <- ifelse(grepl("[Dd]ementia", df$HISTORY[i], fixed = TRUE),1,0)
  df$Cancer[i] <- ifelse(grepl("[Cc]ancer", df$HISTORY[i], fixed = TRUE),1,0)
  df$Kidney[i] <- ifelse(grepl("[Kk]idney|CKD|ckd", df$HISTORY[i], fixed = TRUE),1,0)
  df$Hyperlipidemia[i] <- ifelse(grepl("[Hy]yperlipidemia", df$HISTORY[i], fixed = TRUE),1,0)
  df$CVD[i] <- ifelse(grepl("[Hh]eart|[Cc]vd|[Ss]troke|HF", df$HISTORY[i], fixed = TRUE),1,0)
  df$AF[i] <- ifelse(grepl("[Aa]trial [Ff]ibrillation|AF", df$HISTORY[i], fixed = TRUE),1,0)
}

df$Allergic_his[grepl("[Aa]llergic", df$HISTORY)] <- 1
df$diabetes[grepl("[Dd]iabete", df$HISTORY)] <- 1
df$hypertension[grepl("[Hy]ypertension|high blood", df$HISTORY)]<- 1
df$arthritis[grepl("[Aa]rthritis", df$HISTORY)]<- 1
df$Asthma[grepl("[Aa]sthma", df$HISTORY)]<- 1
df$Migraine[grepl("[Mm]igraine", df$HISTORY)]<- 1
df$copd[grepl("[Cc]opd|COPD", df$HISTORY)]<- 1
df$Anxiety[grepl("[Aa]nxiety", df$HISTORY)]<- 1
df$obesity[grepl("[Oo]besity", df$HISTORY)]<- 1
df$depression[grepl("[Dd]epression", df$HISTORY)]<- 1
df$Thyroid[grepl("[Tt]hyroid", df$HISTORY)]<- 1
df$Anemia[grepl("[Aa]nemia", df$HISTORY)]<- 1
df$Dementia[grepl("[Dd]ementia", df$HISTORY)]<- 1
df$Cancer[grepl("[Cc]ancer", df$HISTORY)]<- 1
df$Kidney[grepl("[Kk]idney|CKD|ckd", df$HISTORY)]<- 1
df$Hyperlipidemia[grepl("[Hy]yperlipidemia", df$HISTORY)]<- 1
df$CVD[grepl("[Hh]eart|[Cc]vd|[Ss]troke|HF", df$HISTORY)]<- 1
df$AF[grepl("[Aa]trial [Ff]ibrillation|AF", df$HISTORY)]<- 1



df$othermeds <- ifelse(!is.na(df$OTHER_MEDS)&df$OTHER_MEDS!=""&df$OTHER_MEDS!=" "&df$OTHER_MEDS!="Na"&df$OTHER_MEDS!="NA",1,0)
table(df$othermeds)
df$currill <- ifelse(!is.na(df$CUR_ILL)&df$CUR_ILL!=""&df$CUR_ILL!=" "&df$CUR_ILL!="Na"&df$CUR_ILL!="NA",1,0)
table(df$currill)
df$allergies <- ifelse(!is.na(df$ALLERGIES)&df$ALLERGIES!=""&df$ALLERGIES!=" "&df$ALLERGIES!="Na"&df$ALLERGIES!="NA",1,0)
table(df$allergies)
df$sex <- ifelse(df$SEX=="F",1,0)
df$disable <- ifelse(df$DISABLE!="",1,0)
df$manu <- ifelse(df$VAX_MANU=="MODERNA",0,1)
df <- df[df$NUMDAYS<=50,]

df0 <- df[is.finite(df$VAERS_ID),]
write.csv(df0,"./data/vaers.csv",row.names = F,fileEncoding = "UTF-8")

df0 <- df[!is.na(df$VAERS_ID),]
sum(is.na(df$VAERS_ID))

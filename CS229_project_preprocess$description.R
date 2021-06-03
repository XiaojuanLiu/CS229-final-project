#---
# Title: CS229 project
# Editor: Xiaojuan 
# Date created: 4/15/2021
# Date edited: 6/02/2021
#---

library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(lubridate)

setwd("D:/Course/CS229/project/")
rm(list=ls())

#---------------- read in dataset---------------------------------
dt <- read.csv("./data/United States.csv")
dt1.21 <- read.csv("./data/2021VAERSData/2021VAERSDATA.csv")
dt2.21 <- read.csv("./data/2021VAERSData/2021VAERSVAX.csv")
dt3.21 <- read.csv("./data/2021VAERSData/2021VAERSSYMPTOMS.csv")

dt1.20 <- read.csv("./data/2020VAERSData/2020VAERSDATA.csv")
dt2.20 <- read.csv("./data/2020VAERSData/2020VAERSVAX.csv")
dt3.20 <- read.csv("./data/2020VAERSData/2020VAERSSYMPTOMS.csv")


# merge by id and restrict to covid vaccine
df21 <- dt2.21 %>% filter(VAX_TYPE=="COVID19") %>% left_join(dt1.21, by="VAERS_ID")%>% left_join(dt3.21, by="VAERS_ID")
df20 <- dt2.20 %>% filter(VAX_TYPE=="COVID19") %>% left_join(dt1.20, by="VAERS_ID")%>% left_join(dt3.20, by="VAERS_ID")
df <- rbind(df21,df20)

#write.csv(df21,"./data/2021vaers.csv",row.names = F,fileEncoding = "UTF-8")
#write.csv(df20,"./data/2020vaers.csv",row.names = F,fileEncoding = "UTF-8")

#---------------- pre-processing---------------------------------
## remove unknown sex/manufactor or age <12 
# df <- df[df$SEX!="U"&df$VAX_MANU!="UNKNOWN MANUFACTURER"&df$AGE_YRS>11,]

## redefine categorical variables 
df$othermeds <- ifelse(!is.na(df$OTHER_MEDS)&df$OTHER_MEDS!=""&df$OTHER_MEDS!=" "&df$OTHER_MEDS!="Na"&df$OTHER_MEDS!="NA",1,0)
# table(df$othermeds)
df$curr_ill <- ifelse(!is.na(df$CUR_ILL)&df$CUR_ILL!=""&df$CUR_ILL!=" "&df$CUR_ILL!="Na"&df$CUR_ILL!="NA",1,0)
# table(df$currill)
df$allergies <- ifelse(!is.na(df$ALLERGIES)&df$ALLERGIES!=""&df$ALLERGIES!=" "&df$ALLERGIES!="Na"&df$ALLERGIES!="NA",1,0)
# table(df$allergies)
df$sex <- ifelse(df$SEX=="F",1,0)
df$disable <- ifelse(df$DISABLE!="",1,0)
df$manu <- ifelse(df$VAX_MANU=="MODERNA",0,ifelse(df$VAX_MANU=="PFIZER\\BIONTECH",2,3))
df$AGE_YRS <- as.numeric(df$AGE_YRS)

## manipulate date variable
# 1. onset date and imputation 
df$date <- as_date(df$ONSET_DATE, format="%m/%d/%Y")
df <- df %>% fill(date) %>% fill(date)
# if year< 2020 then impute by date of the previous record 
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
df$date <- ifelse(year(df$date)<2020, lag(format(df$date)),format(df$date))
table(year(df$date), useNA="always")# no missing

# 2. vaccination date and imputation 
df$date.vax <- as_date(df$VAX_DATE, format="%m/%d/%Y")
df <- df %>% fill(date.vax) %>% fill(date.vax)
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))
df$date.vax <- ifelse(year(df$date.vax)<2020, lag(format(df$date.vax)),format(df$date.vax))
table(year(df$date.vax), useNA="always")# no missing

# define duration
df$dur <- as.Date(df$date)-as.Date(df$date.vax)
table(df$dur)
table(df[df$NUMDAYS<=50,]$NUMDAYS)
table(df[df$NUMDAYS<=50,]$dur)
#----------------description plots---------------------------------

# histogram showing frequency of top 15 common symptom 
symptom <- c(df$SYMPTOM1,df$SYMPTOM2,df$SYMPTOM3,df$SYMPTOM4,df$SYMPTOM5)
symptom <- gsub('Injection site', '', symptom)
hist <- data.frame(table(symptom))[-1,]
hist <- hist[order(hist$Freq,decreasing=T),]
# 
ggplot(hist[1:15,], aes(x = reorder(symptom, -Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill=2) + 
  labs(x="Symptoms", y="Frequency") + 
  scale_x_discrete(guide=guide_axis(n.dodge=2))
# theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# line chart showing daily events over time
daily <- df %>% group_by(date) %>% summarise(count=n())
ggplot(daily[55:nrow(daily),], aes(x=as_date(date), y=count))+geom_line(col=2)+
  labs(x="Date", y="Adverse effect events (n)")+
  theme_minimal()
# daily events by manufacturer over time
daily <- df %>% group_by(date,VAX_MANU) %>% summarise(count=n())
table(daily$VAX_MANU)
daily <- daily[daily$VAX_MANU!="UNKNOWN MANUFACTURER",]# remove unknown
ggplot(daily[96:nrow(daily),], aes(x = as_date(date), y = count)) + 
  geom_line(aes(color = VAX_MANU)) + 
  labs(x="Date", y="Adverse effect events (n)")+
  labs(colour="VAC Manu",linetype="VAC Manu",shape="VAC Manu")+
  scale_color_manual(values = c(7,2,4)) +
  theme_minimal()


# compute event rate
dt$date.vax <- as_date(dt$date, "%Y-%m-%d") # total vaccine data
df$date.vax <- as_date(df$VAX_DATE, format="%m/%d/%Y") # event data
daily <- df %>% group_by(date.vax) %>% summarise(count=n())
dt <- dt %>% left_join(daily, by="date.vax") # merge by date
dt$rate <- dt$count/dt$people_vaccinated * 10000

# line plot showing event rate over time
ggplot(dt, aes(x=date.vax, y=rate))+geom_line(col=2)+
  labs(x="Date", y="Adverse effect events (per 10000 person)")+
  theme_minimal()
summary(df$dur)

##----------- identify baseline features ---------------------------

#Taking Other Medicine,Prior Vaccine,Allergic History,Type-2 Diabetes, Hypertension, Arthritis, Asthma, Migraine,
#High cholesterol, Abnormal Blood Pressure,Chronic Obstructive Pulmonary Disease,GERD, 
#Anxiety, Obesity, depression,Thyroid Disorder,Anemia, Dementia, Cancer, Kidney Disease,
#Hyperlipidemia, Heart Disease, Covid-19 Positive History, Atrial Fibrillation, Pain Symptoms,Hospitalized
# create a feature (n=17) space of history 
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

df <- df[df$NUMDAYS<=50,]
df0 <- df[is.finite(df$VAERS_ID),]

# output data
write.csv(df0,"./data/vaers1.csv",row.names = F,fileEncoding = "UTF-8")


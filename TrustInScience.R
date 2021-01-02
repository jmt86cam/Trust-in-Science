# clear the environment: rm(list = ls())
# clear the console: ctrl+L
# clear all the plots: dev.off()

# Set where plots will be saved to:
setwd("Path/to/working/directory")

#### Packages ####
library(tidyr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(gridExtra)
#### end ####

#### Read in the data ####
Surveydata <- read.csv("/location/of/survey/results.csv", na.strings=c(""," ","NA"))

# Only work with completed data
Surveydata <- subset(Surveydata,Finished == "True")
Surveydata$Finished <- NULL

Surveydata <- unite(Surveydata, "EducationLevel", c(Q2:Q2_5_TEXT),na.rm = TRUE)
Surveydata <- unite(Surveydata, "Institute", c(Q4:Q4_3_TEXT),na.rm = TRUE)

#Trying to write for loop to sort political question
#x=1:16
#for (val in x) {
#  column = paste("Surveydata$Q5_",val,sep = "")
#  column[1][column[1] == 'On'] <- val
#}
Surveydata$Q5_1[Surveydata$Q5_1 == 'On'] <- '1'
Surveydata$Q5_2[Surveydata$Q5_2 == 'On'] <- '2'
Surveydata$Q5_3[Surveydata$Q5_3 == 'On'] <- '3'
Surveydata$Q5_4[Surveydata$Q5_4 == 'On'] <- '4'
Surveydata$Q5_5[Surveydata$Q5_5 == 'On'] <- '5'
Surveydata$Q5_6[Surveydata$Q5_6 == 'On'] <- '6'
Surveydata$Q5_7[Surveydata$Q5_7 == 'On'] <- '7'
Surveydata$Q5_8[Surveydata$Q5_8 == 'On'] <- '8'
Surveydata$Q5_9[Surveydata$Q5_9 == 'On'] <- '9'
Surveydata$Q5_10[Surveydata$Q5_10 == 'On'] <- '10'
Surveydata$Q5_11[Surveydata$Q5_11 == 'On'] <- '11'
Surveydata$Q5_12[Surveydata$Q5_12 == 'On'] <- '12'
Surveydata$Q5_13[Surveydata$Q5_13 == 'On'] <- '13'
Surveydata$Q5_14[Surveydata$Q5_14 == 'On'] <- '14'
Surveydata$Q5_15[Surveydata$Q5_15 == 'On'] <- '15'
Surveydata$Q5_16[Surveydata$Q5_16 == 'On'] <- '16'
Surveydata$Q5_1[Surveydata$Q5_1 == 'Off'] <- NA
Surveydata$Q5_2[Surveydata$Q5_2 == 'Off'] <- NA
Surveydata$Q5_3[Surveydata$Q5_3 == 'Off'] <- NA
Surveydata$Q5_4[Surveydata$Q5_4 == 'Off'] <- NA
Surveydata$Q5_5[Surveydata$Q5_5 == 'Off'] <- NA
Surveydata$Q5_6[Surveydata$Q5_6 == 'Off'] <- NA
Surveydata$Q5_7[Surveydata$Q5_7 == 'Off'] <- NA
Surveydata$Q5_8[Surveydata$Q5_8 == 'Off'] <- NA
Surveydata$Q5_9[Surveydata$Q5_9 == 'Off'] <- NA
Surveydata$Q5_10[Surveydata$Q5_10 == 'Off'] <- NA
Surveydata$Q5_11[Surveydata$Q5_11 == 'Off'] <- NA
Surveydata$Q5_12[Surveydata$Q5_12 == 'Off'] <- NA
Surveydata$Q5_13[Surveydata$Q5_13 == 'Off'] <- NA
Surveydata$Q5_14[Surveydata$Q5_14 == 'Off'] <- NA
Surveydata$Q5_15[Surveydata$Q5_15 == 'Off'] <- NA
Surveydata$Q5_16[Surveydata$Q5_16 == 'Off'] <- NA
Surveydata <- unite(Surveydata, "PoliticalOrientation", c(Q5_1:Q5_16),na.rm = TRUE)
Surveydata$DateCompleted <- Surveydata$RecordedDate

Surveydata <- unite(Surveydata, "Source", c(Q7:Q7_8_TEXT),na.rm = TRUE)
Surveydata <- unite(Surveydata, "OpenSourceSites", c(Q9:Q9_9_TEXT),na.rm = TRUE)
Surveydata <- unite(Surveydata, "OpenSourceLimiter", c(Q11:Q11_6_TEXT),na.rm = TRUE)

Surveydata <- rename(Surveydata,c("Age" = "Q1", "SourceBias" = "Q8", "DataInteraction" = "Q6","OpenSourceFrequency" = "Q10",
                                  "JournalistMediaEndorsement" = "Q12_1", "GovernmentEndorsement" = "Q12_2",
                                  "ScienceInstituteEndorsement" = "Q12_3", "CommunityEndorsement" = "Q12_4",
                                  "RawToProcessedScale" = "Q12_1.1"))

Surveydata$StartDate <- NULL
Surveydata$EndDate <- NULL
Surveydata$Status <- NULL
Surveydata$Progress <- NULL
Surveydata$RecordedDate <- NULL
Surveydata$ResponseId <- NULL
Surveydata$Duration..in.seconds. <- NULL
Surveydata$DistributionChannel <- NULL
Surveydata$UserLanguage <- NULL
Surveydata$Q17 <- NULL
Surveydata$Q14 <- NULL
Surveydata$Q13 <- NULL
Surveydata$Q15 <- NULL

# Create data frame with DataInteraction answers each given their own row
NoInt <- Surveydata %>% filter(str_detect(DataInteraction, "told"))
NoInt$DataInteraction <- "I do not interact with data myself, preferring to be told about conclusions from the latest research"
Processed <- Surveydata %>% filter(str_detect(DataInteraction, "processed"))
Processed$DataInteraction <- "I like to see data presented in a processed form when reading about research"
Interrogate <- Surveydata %>% filter(str_detect(DataInteraction, "interrogate"))
Interrogate$DataInteraction <- "I often download data and interrogate it myself to see if I agree with others' interpretations"
Produce <- Surveydata %>% filter(str_detect(DataInteraction, "produce"))
Produce$DataInteraction <- "I produced new data as part of my work"
Interpret <- Surveydata %>% filter(str_detect(DataInteraction, "interpret my own"))
Interpret$DataInteraction <- "I interpret my own or other peoples' data as part of my work"
None <- Surveydata %>% filter(str_detect(DataInteraction, "None"))
None$DataInteraction <- "None of these describe me"
SurveydataDataInt <- rbind(NoInt,Processed,Interrogate,Produce,Interpret,None)
rm(NoInt,Processed,Interrogate,Produce,Interpret,None)

# Create data frame with Source answers each given their own row
Aa <- Surveydata %>% filter(str_detect(Source, "Newspapers"))
Aa$Source <- "Newspapers"
Ab <- Surveydata %>% filter(str_detect(Source, "Social media"))
Ab$Source <- "Social media"
Ac <- Surveydata %>% filter(str_detect(Source, "Blogs"))
Ac$Source <- "Blogs"
Ad <- Surveydata %>% filter(str_detect(Source, "Government website"))
Ad$Source <- "Government website"
Ae <- Surveydata %>% filter(str_detect(Source, "TV"))
Ae$Source <- "TV/radio news"
Af <- Surveydata %>% filter(str_detect(Source, "Open source data"))
Af$Source <- "Open source data"
Ag <- Surveydata %>% filter(str_detect(Source, "Friends"))
Ag$Source <- "Friends/family"
Ah <- Surveydata %>% filter(str_detect(Source, "Other"))
Ah$Source <- "Other"
SurveydataSource <- rbind(Aa,Ab,Ac,Ad,Ae,Af,Ag,Ah)
rm(Aa,Ab,Ac,Ad,Ae,Af,Ag,Ah)

# Create data frame with OpenSourceSites answers each given their own row
Aa <- Surveydata %>% filter(str_detect(OpenSourceSites, "Google News"))
Aa$OpenSourceSites <- "Google News - Coronavirus (Covid-19)"
Aab <- Surveydata %>% filter(str_detect(OpenSourceSites, "earlyAlert"))
Aab$OpenSourceSites <- "earlyAlert - Global Common Operational Picture - Coronavirus (Covid-19 / 2019 nCoV)"
Ab <- Surveydata %>% filter(str_detect(OpenSourceSites, "Baidu"))
Ab$OpenSourceSites <- "Baidu - Real-time update: map of the novel coronavirus pneumonia epidemic"
Ac <- Surveydata %>% filter(str_detect(OpenSourceSites, "WHO Coronavirus Disease"))
Ac$OpenSourceSites <- "WHO Coronavirus Disease (COVID-19) Dashboard"
Ad <- Surveydata %>% filter(str_detect(OpenSourceSites, "Johns Hopkins"))
Ad$OpenSourceSites <- "COVID-19 Dashboard by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)"
Ae <- Surveydata %>% filter(str_detect(OpenSourceSites, "European Centre"))
Ae$OpenSourceSites <- "European Centre for Disease Prevention and Control: COVID-19 Situation and Dashboard"
Af <- Surveydata %>% filter(str_detect(OpenSourceSites, "GOV.UK: Coronavirus"))
Af$OpenSourceSites <- "GOV.UK: Coronavirus (COVID-19) in the UK"
Ag <- Surveydata %>% filter(str_detect(OpenSourceSites, "RIVM: Current"))
Ag$OpenSourceSites <- "RIVM: Current information about COVID-19 (novel coronavirus)"
Ah <- Surveydata %>% filter(str_detect(OpenSourceSites, "Other"))
Ah$OpenSourceSites <- "Other"
Aj <- Surveydata %>% filter(str_detect(OpenSourceSites, "do not access"))
Aj$OpenSourceSites <- "I do not access open source data/dashboards on Covid-19"
SurveydataOpenSourceSites <- rbind(Aa,Aab,Ab,Ac,Ad,Ae,Af,Ag,Ah,Aj)
rm(Aa,Aab,Ab,Ac,Ad,Ae,Af,Ag,Ah,Aj)

# Create data frame with OpenSourceLimiter answers each given their own row
Aa <- Surveydata %>% filter(str_detect(OpenSourceLimiter, "I had more time"))
Aa$OpenSourceLimiter <- "I had more time"
Ab <- Surveydata %>% filter(str_detect(OpenSourceLimiter, "I was directed to them"))
Ab$OpenSourceLimiter <- "I was directed to them (from journalism/social media) more often"
Ac <- Surveydata %>% filter(str_detect(OpenSourceLimiter, "felt more comfortable"))
Ac$OpenSourceLimiter <- "I felt more comfortable handling data"
Ad <- Surveydata %>% filter(str_detect(OpenSourceLimiter, "more contextual knowledge"))
Ad$OpenSourceLimiter <- "I had more contextual knowledge from the medical field"
Af <- Surveydata %>% filter(str_detect(OpenSourceLimiter, "I do not see myself accessing open source"))
Af$OpenSourceLimiter <- "I do not see myself accessing open source data on Covid-19 more often than I currently do"
Ag <- Surveydata %>% filter(str_detect(OpenSourceLimiter, "Other"))
Ag$OpenSourceLimiter <- "Other"
SurveydataOpenSourceLimiter <- rbind(Aa,Ab,Ac,Ad,Af,Ag)
rm(Aa,Ab,Ac,Ad,Af,Ag)

#### end ####

#### Context graphics ####
# Pie chart of education level 
EducationSummary <- ggplot(Surveydata, aes(x=factor(1), fill=EducationLevel))+
  geom_bar(width = 1)+
  coord_polar("y")+
  scale_fill_brewer(palette="Blues")+labs(title="Participant Education level")+
  theme(plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
      axis.ticks = element_blank(), legend.title = element_blank())
ggsave("EducationSummaryPie.png",EducationSummary)
rm(EducationSummary)

# Pie chart of Age 
AgeSummary <- ggplot(Surveydata, aes(x=factor(1), fill=Age))+
  geom_bar(width = 1)+
  coord_polar("y")+
  scale_fill_brewer(palette="Blues")+labs(title="Age groups of Participants")+
  theme(plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.title = element_blank())
ggsave("AgeSummaryPie.png",AgeSummary)
rm(AgeSummary)

# Data interaction
DataInteractionPlot <- ggplot(SurveydataDataInt, aes(x=DataInteraction, fill=DataInteraction))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Participants' interaction with data")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "vertical", legend.title = element_blank(),
        legend.justification = "center")
ggsave("DataInteractionPlot.png",DataInteractionPlot)
rm(DataInteractionPlot)

# Information sources
SourcePlot <- ggplot(SurveydataSource, aes(x=Source, fill=Source))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Participants' sources of information")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "horizontal", legend.title = element_blank(),
        legend.justification = "center")
ggsave("SourcePlot.png",SourcePlot)
rm(SourcePlot)

# Trust in sources
SourceBiasPlot <- ggplot(Surveydata, aes(x=SourceBias, fill=SourceBias))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Participants' trust in their sources of information")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "vertical", legend.title = element_blank(),
        legend.justification = "center")
ggsave("SourceBiasPlot.png",SourceBiasPlot)
rm(SourceBiasPlot)

# Open source sites
OpenSourceSitesPlot <- ggplot(SurveydataOpenSourceSites, aes(x=OpenSourceSites, fill=OpenSourceSites))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Open source data/dashboards accessed by participants")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "vertical", legend.title = element_blank(),
        legend.justification = "center")
ggsave("OpenSourceSitesPlot.png",OpenSourceSitesPlot)
rm(OpenSourceSitesPlot)

# Open source frequency bar chart
OpenSourceFrequencyPlot <- ggplot(data = subset(Surveydata, !is.na(OpenSourceFrequency)), aes(x=OpenSourceFrequency, fill=OpenSourceFrequency))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="How often do participants' use open source data/dashboards")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "horizontal", legend.title = element_blank(),
        legend.justification = "center")
ggsave("OpenSourceFrequencyPlot.png",OpenSourceFrequencyPlot)
rm(OpenSourceFrequencyPlot)

# Open source limiter bar chart
OpenSourceLimiterPlot <- ggplot(SurveydataOpenSourceLimiter, aes(x=OpenSourceLimiter, fill=OpenSourceLimiter))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Factors that would increase participants use of open source data/dashboards")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "vertical", legend.title = element_blank(),
        legend.justification = "center")
ggsave("OpenSourceLimiterPlot.png",OpenSourceLimiterPlot)
rm(OpenSourceLimiterPlot)

# Endorsement bar chart
p1 <- ggplot(data = subset(Surveydata, !is.na(JournalistMediaEndorsement)), aes(x=JournalistMediaEndorsement, fill=JournalistMediaEndorsement))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Journalists and Media outlets")+ 
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "horizontal", legend.title = element_blank(),
        legend.justification = "center")
p2 <- ggplot(data = subset(Surveydata, !is.na(GovernmentEndorsement)), aes(x=GovernmentEndorsement, fill=GovernmentEndorsement))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="National/government level organisations")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "horizontal", legend.title = element_blank(),
        legend.justification = "center")
p3 <- ggplot(data = subset(Surveydata, !is.na(ScienceInstituteEndorsement)), aes(x=ScienceInstituteEndorsement, fill=ScienceInstituteEndorsement))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Scientific institutions")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "horizontal", legend.title = element_blank(),
        legend.justification = "center")
p4 <- ggplot(data = subset(Surveydata, !is.na(CommunityEndorsement)), aes(x=CommunityEndorsement, fill=CommunityEndorsement))+
  geom_bar(width = 1)+ scale_fill_brewer(palette="Blues")+
  labs(title="Community")+
  theme(plot.title = element_text(hjust=0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_line(),
        legend.position = "bottom",legend.direction = "horizontal", legend.title = element_blank(),
        legend.justification = "center")
EndorsementsPlot <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)
ggsave("EndorsementsPlot.png",EndorsementsPlot)
rm(p1, p2, p3, p4, EndorsementsPlot)

#### end ####

# Education level vs desire for data to be processed


# Education level vs open source data and limiter


# Political views within source


# Political views within bias


# Political views within endorsements


# Age views within political views


# Age views within source


# Age views within bias


ggplot(Surveydata, aes(x=DataInteraction, fill=Age))+
  geom_bar(width = 1)

# Reorder x axis
# scale_x_discrete(limits=c("Not at all important", "Preferable", "Essential"))+

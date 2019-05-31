setwd("~/home/RWork/dissertation/data/")
library(tidyverse) 
library(DescTools)
library(stringr) 
library(glmmTMB)

#Load raw data
rawdata <- read_tsv("EyeTracking/SeductiveDetails.subset.all.final.tsv")

#Remove data with missing timestamps
rawdata <-
  rawdata %>%
  drop_na(EyeTrackerTimestamp)
str(rawdata, list.len=ncol(rawdata))

#Set up trackloss
#rawdata$trackloss <- rawdata$ValidityLeft > 1 & rawdata$ValidityRight > 1

bindata <-
  rawdata %>%
  dplyr::select(ParticipantName, SegmentName,"AOI[E.IMG.1.POS]Hit",
                "AOI[E.W.1.NEG]Hit",
                "AOI[E.W.2.NEG]Hit",
                "AOI[E.AREA.1.NEG]Hit",
                "AOI[E.IMG.2.POS]Hit",
                "AOI[E.W.3.POS]Hit",
                "AOI[E.AREA.2.POS]Hit",
                "AOI[E.AREA.3.POS]Hit",
                "AOI[E.IMG.3.NEG]Hit",
                "AOI[E.IMG.4.NEG]Hit",
                "AOI[E.W.4.NEG]Hit",
                "AOI[E.W.5.NEG]Hit",
                "AOI[E.W.6.NEG]Hit",
                "AOI[E.AREA.4.NEG]Hit",
                "AOI[E.IMG.5.NEG]Hit",
                "AOI[E.W.7.NEG]Hit",
                "AOI[E.W.8.NEG]Hit",
                "AOI[E.W.9.NEG]Hit",
                "AOI[E.AREA.5.NEG]Hit",
                "AOI[E.IMG.6.NEG]Hit",
                "AOI[E.W.10.POS]Hit",
                "AOI[E.W.11.POS]Hit",
                "AOI[E.AREA6.POS]Hit",
                "AOI[N.IMG.1]Hit",
                "AOI[N.AREA.1]Hit",
                "AOI[N.IMG.2]Hit",
                "AOI[N.AREA.2]Hit",
                "AOI[N.AREA.3]Hit",
                "AOI[N.IMG.3]Hit",
                "AOI[N.IMG.4]Hit",
                "AOI[N.AREA.4]Hit",
                "AOI[N.IMG.5]Hit",
                "AOI[N.Area.5]Hit",
                "AOI[N.IMG.6]Hit",
                "AOI[N.AREA5]Hit",
                "AOI[ND.IMG.1]Hit",
                "AOI[ND.AREA.1]Hit",
                "AOI[ND.IMG.2]Hit",
                "AOI[ND.AREA.2]Hit",
                "AOI[ND.AREA.3]Hit",
                "AOI[ND.IMG.3]Hit",
                "AOI[ND.IMG.4]Hit",
                "AOI[ND.AREA.4]Hit",
                "AOI[ND.IMG.5]Hit",
                "AOI[ND.AREA.5]Hit",
                "AOI[ND.AREA.6]Hit",
                "AOI[ND.IMG6]Hit",
                "AOI[E.IMG.1.NEG]Hit",
                "AOI[E.IMG.2.NEG]Hit",
                "AOI[E.IMG.3.NEG]Hit_1",
                "AOI[E.IMG.4.NEG]Hit_1",
                "AOI[N.IMG.1]Hit_1",
                "AOI[N.IMG.2]Hit_1",
                "AOI[N.IMG.3]Hit_1",
                "AOI[N.IMG.4]Hit_1",
                "AOI[ND.IMG.1]Hit_1",
                "AOI[ND.IMG.2]Hit_1",
                "AOI[ND.IMG.3]Hit_1",
                "AOI[ND.IMG.4]Hit_1"
                ) %>%
  gather(key = "condition", value = "response", -ParticipantName, -SegmentName, na.rm = T) 

bindata <-
  bindata %>% 
  mutate(condition = case_when(
    str_detect(condition, "\\[ND") ~ "NODETAILS",
    str_detect(condition, "\\[N") ~ "NEUTRAL", 
    str_detect(condition, "\\[E") ~ "EMOTION"
  )) 

bindata$condition <- fct_relevel(bindata$condition, ref="NODETAILS")

bindata %>% print(n=20)

######################################
#Add a single success to every panel
#

bindata <- 
unique(bindata[1:3]) %>%
  mutate(response = 1) %>%
  rbind(bindata)


betadata <-
  bindata %>%
  group_by(ParticipantName, SegmentName, condition) %>%
  summarise(hits = sum(response),
            n = n(),
            rate = hits / n)

betadata$condition <- fct_relevel(betadata$condition, ref="NODETAILS")

betadata[which(betadata$ParticipantName == 200),] %>%print(n=200)

####################################################
# Additional parameters

groups <- read_csv("Participants.groups.csv")
iceages <- read_csv("IceAgesTest.csv")
lightning <- read_csv("LightningTest.csv")

#####################################
#Fix groups
groups <-
  groups %>%
  mutate(treatment = as.factor(treatment))

groups$treatment <- fct_relevel(groups$treatment, ref="NoDetails")

#####################################
#Calclate prior knowledge ice ages
iceages.scores <-
  iceages %>%
  tail(-2) %>%
  drop_na(ID) %>%
  mutate(ID = as.numeric(ID),
         ice.prior = (as.numeric(P.1) +
                        as.numeric(P.2) + 
                        as.numeric(P.3) +
                        as.numeric(P.4) +
                        as.numeric(P.5)) / 5) %>%
  dplyr::select(ID, ice.prior)

iceages.scores

#####################################
#Calclate prior knowledge lightning
lightning.scores <-
  lightning %>%
  tail(-2) %>%
  drop_na(ID) %>%
  mutate(ID = as.numeric(ID),
         lightning.prior = (as.numeric(P.1) +
                              as.numeric(P.2) + 
                              as.numeric(P.3) +
                              as.numeric(P.4) +
                              as.numeric(P.5)) / 5) %>%
  dplyr::select(ID, lightning.prior)

lightning.scores

####################################
#Join tables

sc.data <-
  groups %>%
  left_join(iceages.scores) %>%
  left_join(lightning.scores) 


sc.data <-
  sc.data %>%
  mutate(prior = ice.prior + lightning.prior) %>%
  dplyr::select(ID, prior)

sc.data %>%
  print(n=100)


betadata <- 
  betadata %>%
  left_join(sc.data, by=c("ParticipantName"="ID"))

####################################################
#Beta Regression Model 

glmmmod <- glmmTMB(rate ~ condition, family = beta_family(link = "logit"), data = betadata)
summary(glmmmod)

hist(betadata$rate, breaks=20)

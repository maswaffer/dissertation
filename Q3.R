setwd("~/kitematic/lt/RWork/Spring2019/FirstWeek")
library(tidyverse) 
library(DescTools)
library(stringr) 
library(MASS)
library(betareg)


#comment
#Load raw data
rawdata <- read_tsv("EyeTrackingData2.tsv")

#Remove data with missing timestamps
rawdata <-
  rawdata %>%
  drop_na(EyeTrackerTimestamp)
spec(rawdata)
str(rawdata, list.len=ncol(rawdata))

#Set up trackloss
rawdata$trackloss <- rawdata$ValidityLeft > 1 & rawdata$ValidityRight > 1

bindata <-
  rawdata %>%
  dplyr::select(ParticipantName, SegmentName,
                "AOI[E.IMG.1.POS]Hit",
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
  gather(key = "condition", value = "response", -ParticipantName, -SegmentName, na.rm = T) %>%
  mutate(condition = case_when(
    str_detect(condition, "\\[ND") ~ "NODETAILS",
    str_detect(condition, "\\[N") ~ "NEUTRAL", 
    str_detect(condition, "\\[E") ~ "EMOTION"
  )) 

bindata$condition <- fct_relevel(bindata$condition, ref="NODETAILS")

head(bindata)
nrow(bindata)
Desc(bindata$response)
####################################################
#Negative Binomial Model
nbmod <- glm.nb(response ~ condition, data = bindata)
summary(nbmod)


####################################################
#Beta Regression Model 
betadata <-
  bindata %>%
  group_by(ParticipantName, condition) %>%
  summarise(hits = sum(response),
            n = n(),
            rate = hits / n)

betadata$condition <- fct_relevel(betadata$condition, ref="NODETAILS")

head(betadata)
nrow(betadata)

betamod <- betareg(rate ~ condition, data = betadata)
summary(betamod)



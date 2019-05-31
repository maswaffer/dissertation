setwd("~/home/RWork/dissertation/data/")

library(tidyverse)
library(ggplot2)
library(car)
library(mvoutlier)

groups <- read_csv("Participants.groups.csv")
iceages <- read_csv("Assessments/IceAges.csv")
lightning <- read_csv("Assessments/Lightning.csv")

#####################################
#Factor groups
groups <-
  groups %>%
  mutate(treatment = as.factor(treatment)) 

groups$treatment <- fct_collapse(groups$treatment,
                                  NoDetails = "NoDetails",
                                  Details = c("Neutral", "Emotion")
                                )

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
                 as.numeric(P.5)) / 5,
         ice.total = as.numeric(SC0), 
         ice.transfer = as.numeric(SC1),
         ice.recall = as.numeric(SC2)) %>%
  dplyr::select(ID, ice.total, ice.transfer, ice.recall, ice.prior)

iceages.scores %>% print(n=50)

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
                        as.numeric(P.5)) / 5,
         lightning.total = as.numeric(SC0), 
         lightning.transfer = as.numeric(SC1),
         lightning.recall = as.numeric(SC2)) %>%
  dplyr::select(ID, lightning.total, lightning.transfer, lightning.recall, lightning.prior)

lightning.scores %>%print(n=50)

####################################
#Join tables

q1.data <-
groups %>%
  left_join(iceages.scores) %>%
  left_join(lightning.scores) 


q1.data <-
  q1.data %>%
  mutate(total = ice.total + lightning.total, 
         transfer = ice.transfer + lightning.transfer,
         recall = ice.recall + lightning.recall, 
         prior = ice.prior + lightning.prior)

q1.data %>%
  print(n=100)


####################################
# Descriptives

#Box plots
ggplot(q1.data, aes(x = treatment, y = recall)) +
  geom_boxplot()

ggplot(q1.data, aes(x = treatment, y = transfer)) +
  geom_boxplot()

ggplot(q1.data, aes(x = treatment, y = prior)) +
  geom_boxplot()

aq.plot(q1.data[c("transfer", "recall", "prior")])

#ASSUMPTION - Linear relationship between DV and Covariate
#Line Plots 

ggplot(q1.data, aes(x = prior)) +
  geom_point(aes(y=recall, colour="recall")) +
  geom_smooth(aes(y=recall, colour="recall"), method='lm', se=F) +
  geom_point(aes(y=transfer, colour="transfer")) + 
  geom_smooth(aes(y=transfer, colour="transfer"), method='lm', se=F) +
  geom_point(aes(y=prior, colour="prior")) +
  geom_smooth(aes(y=prior, colour="prior"), method='lm', se=F) 

recallmod <- lm(recall ~ prior, data=q1.data)
summary(recallmod)

transfermod <- lm(transfer ~ prior, data=q1.data)
summary(transfermod)

#####################################################
#With no linear relationship between recall and prior
#Recall will be an ANOVA, Transfer will be ANCOVA

################
#TRANSFER ANCOVA
################
#ASSUMPTION - Homogeneity of Slopes (Interaction between treatment and covariate)
mod.transfer <- aov(transfer ~ treatment + prior + treatment:prior, data = q1.data)
summary(mod.transfer)

#ASSUMPTION - Groups are not different on covariate
mod.covar <- aov(prior ~ treatment, data = q1.data)
summary(mod.covar)

#ASSUMPTION - Homogeneity of variance
leveneTest(transfer ~ treatment, center = "mean", data = q1.data)

#MODEL
q1.transfer.mod <- lm(transfer ~ treatment + prior, data = q1.data)

#ASSUMPTION - Normality of residuals
q1.transfer.resid <- resid(q1.transfer.mod)
shapiro.test(q1.transfer.resid)

#Model summaries
summary(q1.transfer.mod)
Anova(q1.transfer.mod)


#############
#Recall ANOVA
#############

#ASSUMPTION - Homogeneity of variance
leveneTest(recall ~ treatment, center = "mean", data = q1.data)

#MODEL
q1.recall.mod <- lm(recall ~ treatment, data = q1.data)

#ASSUMPTION - Normality of residuals
q1.recall.resid <- resid(q1.recall.mod)
shapiro.test(q1.recall.resid)

#Model summaries
summary(q1.recall.mod)
Anova(q1.recall.mod)

library(readxl)
library(car)
library(lavaan)
library(DHARMa)
library(ggplot2)
library(countreg)
library(MASS)
# 1
victims <- read_excel("victims.xlsx")
View(victims)
victims$race<-as.factor(victims$race)
poimod<-glm(resp~race, family = poisson(link="log"),data=victims)
spoimod<-summary(poimod)
spoimod
Anova(poimod,test="Wald",type=3)
## RR 2 
glm.RR <- function(GLM.RESULT, digits = 2) {
  
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}


glm.RR(poimod, 3)

#  3 ratio means
mean(subset(victim, race == "black")$resp)/mean(subset(victim, race == "white")$resp)

#  prediction 4
newdat1<-victims[c("race")]
newdat2<-newdat1

## Predicted number of cases per person
newdat1$race <- "black"
victims$pred.race <- exp(predict(poimod, newdat1))

## Predicted number of cases per actual population
victims$pred.race <- exp(predict(poimod))

#  5 
# Simulate the residuals
sim.poimod <- simulateResiduals(poimod, plot = T)
hist(sim.poimod)
rootogram(poimod,ylab="Root Square of Frequency",main="Poisson")

# GOODNESS OF FIT (GOF)
testUniformity(sim.poimod)
testDispersion(sim.poimod)
testOverdispersion(sim.poimod)

#  maybe we should add code R squared.
# significace of the predictor



# 6 Negative Binomial

poimodnb<-glm.nb(resp~race,data=victims)
spoimodnb<-summary(poimodnb)
spoimodnb

#  7 Quasi-Likelihood
poimodQua<-glm(resp~race,family= quasipoisson,data=victims)
summary(poimodQua)


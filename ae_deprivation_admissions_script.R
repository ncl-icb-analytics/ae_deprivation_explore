library(readxl)
library(NCLRtemplates)
library(Hmisc)
library(AER)
library(lme4)
library(tidyverse)
library(broom)
library(broom.mixed)


# Wilson score binomial ci function
prop_ci <- function(o, n, ci=0.95){
  
  z <- qnorm(ci + ((1-ci)/2))
  p <- o/n
  q <- 1-p
  
  plower <- ((2*o + z^2) - z * sqrt(z^2 + (4*o*q))) / (2*(n+z^2))
  
  pupper <- ((2*o + z^2) + z * sqrt(z^2 + (4*o*q))) / (2*(n+z^2))
  
  return(c(o/n,plower, pupper))
  
}


# overdispersion test
od_test<-function(model, ...){
  sum(residuals(model, type="pearson")^2) / df.residual(model)
}



#read in
AE_admissions <- read_excel("data/BS-AE-IP.xlsx")

AE_ad_year <-
  AE_admissions %>% 
  group_by(Quintile) %>% 
  summarise(PERSONS = sum(PERSONS, na.rm = TRUE)/12,
            AE_ATTENDS = sum(AE_ATTENDS, na.rm = TRUE)/12,
  ) %>% 
  mutate(prop = AE_ATTENDS/ PERSONS,
         #binconf(AE_ATTENDS, PERSONS),
         lcl = binconf(AE_ATTENDS, PERSONS)[,2],
         ucl = binconf(AE_ATTENDS, PERSONS)[,3]
  )


AE_ad_year %>% 
  ggplot(aes(Quintile, prop))+
  geom_point()+
  geom_errorbar(aes(ymax=ucl, ymin=lcl), col="purple") + 
  theme_nclicb()



##### Create a couple of features
AE_admissions$deprived <- ifelse(AE_admissions$Quintile <2, 1,0)
AE_admissions$age_cat <- factor(AE_admissions$ageband)
AE_admissions$GP_Borough_Name <- factor(AE_admissions$GP_Borough_Name)


levels(AE_admissions$age_cat)
levels(AE_admissions$age_cat)

AE_admissions %>% 
  group_by(age_cat) %>% 
  summarise(sum(PERSONS),
            sum(AE_ATTENDS))

# Date range
min(AE_admissions$month)
max(AE_admissions$month)

AE_admissions %>% 
  group_by(month) %>% 
  count()

#### Simple Poisson model first ####

model21 <- glm(AE_ATTENDS ~ age_cat + gender + deprived + offset(log(PERSONS)), data=AE_admissions, family="poisson", na.action = na.omit)

summary(model21)

od_test(model21)



##### Random-intercept for clustering at borough ####
# This is confounded with quantiles though, as there is a geographical component here.
# Wider problem is quantiles in general though, as a composite indicator.

model22 <- glmer(AE_ATTENDS ~ (1|GP_Borough_Name) + age_cat + gender + deprived + offset(log(PERSONS))
                , data=AE_admissions, family="poisson", na.action = na.omit
                , control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model22)
model22

od_test(model22)


#### Negative Binomial ####
library(MASS)
model23 <- glm.nb(IP_FROM_AE ~ age_cat + gender + deprived 
                  + offset(log(PERSONS))
                  , data=AE_admissions, na.action = na.omit)

summary(model23)

od_test(model23)

exp(coef(model23))

exp(confint(model23))

tidy(model23, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

library(glmmTMB)
model24 <- glmmTMB(AE_ATTENDS ~ (1|GP_Borough_Name) + age_cat + gender + deprived + offset(log(PERSONS))
                   , family = nbinom2(link = "log")
                   , data=AE_admissions, na.action = na.omit)

summary(model24)

od_test(model24)

exp(coef(model24))

exp(confint(model24))

tidy(model24, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

# Incidence rate ratio: deprived = 1.3536108 
# The A&E admission rate is 1.35 (1.34 - 1.37) times higher, by rolling 12-month periods ending 01/01/2022 - 01/07/2023

lapply(list(model23, model24), AIC)

library(sjPlot)

plot_model(model3, ci.lvl = 0.95, line.size = 2)



plot_model(model23, ci.lvl = 0.95, line.size = 2)


plot_model(model24, ci.lvl = 0.95, line.size = 2)


# planned stuff

# primary care and community service data.
# Check in with David Egan about community services.  ethnicity in wards?  
# 
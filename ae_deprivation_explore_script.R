library(readxl)
library(NCLRtemplates)
library(Hmisc)
library(AER)
library(lme4)
library(tidyverse)
library(broom)


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
AE_balanced_scorecard <- 
  read_excel("data/AE balanced scorecard.xlsx"
             , col_types = c("date", "numeric", "text", 
                             "numeric", "numeric", "text", "numeric", 
                              "numeric")
             , na = "NULL"
             )

AE_year <-
AE_balanced_scorecard %>% 
  group_by(Quintile) %>% 
  summarise(PERSONS = sum(PERSONS, na.rm = TRUE),
            AE_ATTENDS = sum(AE_ATTENDS, na.rm = TRUE),
            ) %>% 
  mutate(prop = AE_ATTENDS/ PERSONS,
         #binconf(AE_ATTENDS, PERSONS),
         lcl = binconf(AE_ATTENDS, PERSONS)[,2],
         ucl = binconf(AE_ATTENDS, PERSONS)[,3]
         )

AE_year %>% 
  ggplot(aes(Quintile, prop))+
  geom_point()+
  # Why is this drawing in the wrong orientation?  Answer: they aren't it's the long hats and almost invisibly small bars
  geom_errorbar(aes(ymax=ucl, ymin=lcl), col="red") + 
  theme_nclicb()


##### Create a couple of features
AE_balanced_scorecard$deprived <- ifelse(AE_balanced_scorecard$Quintile <2, 1,0)
AE_balanced_scorecard$age_cat <- factor(AE_balanced_scorecard$ageband)
AE_balanced_scorecard$GP_Borough_Name <- factor(AE_balanced_scorecard$GP_Borough_Name)


levels(AE_balanced_scorecard$age_cat)
levels(AE_balanced_scorecard$age_cat)

AE_balanced_scorecard %>% 
  group_by(age_cat) %>% 
  summarise(sum(PERSONS),
            sum(AE_ATTENDS))

# Date range
min(AE_balanced_scorecard$month)
max(AE_balanced_scorecard$month)

AE_balanced_scorecard %>% 
  group_by(month) %>% 
  count()

#### Simple Poisson model first ####

model1 <- glm(AE_ATTENDS ~ age_cat + gender + deprived + offset(log(PERSONS)), data=AE_balanced_scorecard, family="poisson", na.action = na.omit)

summary(model1)

od_test(model1)



##### Random-intercept for clustering at borough ####
# This is confounded with quantiles though, as there is a geographical component here.
# Wider problem is quantiles in general though, as a composite indicator.

model2 <- glmer(AE_ATTENDS ~ (1|GP_Borough_Name) + age_cat + gender + deprived + offset(log(PERSONS))
                , data=AE_balanced_scorecard, family="poisson", na.action = na.omit
                , control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model2)
model2

od_test(model2)


#### Negative Binomial ####
library(MASS)
model3 <- glm.nb(AE_ATTENDS ~ age_cat + gender + deprived + offset(log(PERSONS)), data=AE_balanced_scorecard, na.action = na.omit)

summary(model3)

od_test(model3)

exp(coef(model3))

exp(confint(model3))

tidy(model3, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

# Incidence rate ratio: deprived = 1.3536108 
# The A&E admission rate is 1.35 (1.34 - 1.37) times higher, by rolling 12-month periods ending 01/01/2022 - 01/07/2023


library(sjPlot)

plot_model(model3, ci.lvl = 0.95, line.size = 2)




#### Now per borough ###

mod_out1 <- 
  AE_balanced_scorecard %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ age_cat + gender + deprived + offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE))

mod_out1 %>% 
  filter(term == "deprived")




mod_out2 <- 
  AE_balanced_scorecard %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ deprived + offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE))

mod_out2 %>% 
  filter(term == "deprived")



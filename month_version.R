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
AE_balanced_scorecard_mn<- 
  read_excel("data/AE BS by month and 3-mth rolling.xlsx"
             , col_types = c("date", "numeric", "text", 
                             "numeric", "numeric", "text", "numeric", 
                             "numeric")
             , na = "NULL"
  )

# See below for diagnostic script, but missing August A&E data despite having population data
# Removing to repeat below

AE_balanced_scorecard_mn <-
  AE_balanced_scorecard_mn %>% 
  filter((year(month) == 2023 & month(month)==7))
  #filter(!(year(month) == 2023 & month(month)==8))
  

AE_year_mn <-
  AE_balanced_scorecard_mn %>% 
  group_by(Quintile) %>% 
  summarise(PERSONS = sum(PERSONS, na.rm = TRUE),
            AE_ATTENDS = sum(AE_ATTENDS, na.rm = TRUE),
  ) %>% 
  mutate(prop = AE_ATTENDS/ PERSONS,
         #binconf(AE_ATTENDS, PERSONS),
         lcl = binconf(AE_ATTENDS, PERSONS)[,2],
         ucl = binconf(AE_ATTENDS, PERSONS)[,3]
  )

AE_year_mn %>% 
  ggplot(aes(Quintile, prop))+
  geom_point()+
  # Why is this drawing in the wrong orientation?  Answer: they aren't it's the long hats and almost invisibly small bars
  geom_errorbar(aes(ymax=ucl, ymin=lcl), col="red") + 
  theme_nclicb()


##### Create a couple of features
AE_balanced_scorecard_mn$deprived <- ifelse(AE_balanced_scorecard_mn$Quintile <2, 1,0)
AE_balanced_scorecard_mn$age_cat <- factor(AE_balanced_scorecard_mn$ageband)
AE_balanced_scorecard_mn$GP_Borough_Name <- factor(AE_balanced_scorecard_mn$GP_Borough_Name)


levels(AE_balanced_scorecard_mn$age_cat)
levels(AE_balanced_scorecard_mn$age_cat)

AE_balanced_scorecard_mn %>% 
  group_by(age_cat) %>% 
  summarise(sum(PERSONS),
            sum(AE_ATTENDS))

# Date range
min(AE_balanced_scorecard_mn$month)
max(AE_balanced_scorecard_mn$month)

AE_balanced_scorecard_mn %>% 
  group_by(month) %>% 
  count()

#### Simple Poisson model first ####

model1_mn <- glm(AE_ATTENDS ~ age_cat + gender + deprived 
                 + offset(log(PERSONS))
                 , data=AE_balanced_scorecard_mn
                 , family="poisson", na.action = na.omit)

summary(model1_mn)

od_test(model1_mn)



##### Random-intercept for clustering at borough ####
# This is confounded with quantiles though, as there is a geographical component here.
# Wider problem is quantiles in general though, as a composite indicator.

model2_mn2 <- glmer(AE_ATTENDS ~ (1|GP_Borough_Name) + age_cat + factor(gender) + deprived #
                   # + nsk(month_no, knots = c(4,8))+
                    +offset(log(PERSONS))
                , data=AE_balanced_scorecard_mn, family="poisson", na.action = na.omit
                , control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e8)))

summary(model2_mn2)
#model2

modelfit.all <- lme4::allFit(model2_mn2)
ss <- summary(modelfit.all)

od_test(model2_mn2)

tidy(model2_mn2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

# Diagnosd the key issue here which is that we can't use the random intercept with this parameterisation
# , as both the PERSONS variable is based of GP populations, and the random intercept is GP.  This means they are 
# correlated and model becomes nearly unidentifiable.  Better to pursue with NB2 structure without explict random effect
# because we are using PERSONS as a denomincator here in the offset.


#### Negative Binomial ####
library(MASS)
library(splines2)
model3_mn <- glm.nb(AE_ATTENDS ~ age_cat + gender + deprived + offset(log(PERSONS)), data=AE_balanced_scorecard_mn, na.action = na.omit)

model3_mn_s <- glm.nb(AE_ATTENDS ~ age_cat + factor(gender) + deprived 
                       #+ nsk(month_no, knots = c(4,8))
                       + offset(log(PERSONS)) 
                       , data=AE_balanced_scorecard_mn, na.action = na.omit)


summary(model3_mn)
summary(model3_mn_s)

anova(model3_mn_s)

od_test(model3_mn)

AIC(model2_mn2)
AIC(model3_mn_s)

od_test(model3_mn_s)

exp(coef(model3_mn))

exp(confint(model3_mn))

tidy(model3_mn, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

tidy(model3_mn_s, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

# Incidence rate ratio: deprived = 1.3536108 
# The A&E admission rate is 1.35 (1.34 - 1.37) times higher, by rolling 12-month periods ending 01/01/2022 - 01/07/2023


library(sjPlot)

plot_model(model3_mn, ci.lvl = 0.95, line.size = 2)

library(glmmTMB)
model3_mn_s_rint <- glmmTMB(AE_ATTENDS ~ (1|GP_Borough_Name) + age_cat + gender + deprived +
                              #nsk(month_no, knots = c(4,8))+
                              offset(log(PERSONS)) 
                            , data=AE_balanced_scorecard_mn, na.action = na.omit)

summary(model3_mn_s)

library(cAIC4)
cAIC(model2_mn2)
cAIC(model3_mn)
cAIC(model3_mn_s)
cAIC(model3_mn_s_rint)
# This wont' converge and calculated here due to the correlation between offset and random intercept.

od_test(model3_mn)
od_test(model3_mn_s)
od_test(model3_mn_s_rint)


#### Now per borough ###

mod_out1_mn <- 
  AE_balanced_scorecard_mn %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ age_cat + gender + deprived + 
                             nsk(month_no, knots = c(4,8)) + offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE))  %>% 
  mutate(adjustment = "Age, Sex, Seasonality")

mod_out1_mn %>% 
  filter(term == "deprived")



# What's going on here?  Iteration limit reached, nothing for Enfield. Is there something missing in data?
a <- AE_balanced_scorecard_mn %>% 
  filter(GP_Borough_Name == "NHS Islington CCG")

b<- a %>% 
  group_by(month, age_cat, gender, deprived) %>% 
  summarise(ct = sum(AE_ATTENDS)) %>% 
  pivot_wider(names_from = c(age_cat, gender, deprived) , values_from = ct )

AE_balanced_scorecard_mn %>% 
  group_by(month, GP_Borough_Name) %>% 
  summarise(ATTENDS = sum(AE_ATTENDS),
            PERSONS = sum(PERSONS)) %>% 
  filter(year(month) == 2023 & month(month) == 8)

# So it appears in this sample like there's no AE data from August, but we have denominator.
# Looping back up to top to add this a s filter.

mod_out2_mn <- 
  AE_balanced_scorecard_mn %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ deprived +
                             nsk(month_no, knots = c(4,8))+ offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)) %>% 
  mutate(adjustment = "Seasonality")

mod_out2_mn %>% 
  filter(term == "deprived")


mod_out3_mn <- 
  AE_balanced_scorecard_mn %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ deprived +
                             offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)) %>% 
  mutate(adjustment = "None")



mod_out3_mn %>% 
  filter(term == "deprived")

mod_out1_mn %>% 
  bind_rows(mod_out2_mn, mod_out3_mn)  %>% 
  filter(term == "deprived") %>% 
  ggplot(aes(x=GP_Borough_Name, y=estimate, colour=adjustment, fill = adjustment))+
  geom_point(position = position_dodge(width = 0.8))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, group=adjustment), position = position_dodge(width = 0.8))+
  labs(y="Incidence Rate Ratio (IRR)", x="GP Borough Name",
       colour = "Standardisation applied", fill ="Standardisation applied",
       title = "Relative A&E attendance rate for deprived populations against others",
       subtitle = "IMD Quntile 1 vs. 2-5, where 1 = rate is the same, >1 = deprived group is higher") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  scale_y_continuous(limits=c(1, 1.6))+
  scale_fill_ncl()+
  scale_colour_ncl()+
  theme_nclics()+
  theme(text = element_text(size=16))

ggsave("./output/ae_attend_IRR.png", width = (758*2), height = (471*2), units = "px")




####### Same deal, but applying the quintiles 1 & 2 instead of just 1. ####

AE_balanced_scorecard_mn$deprived2 <- ifelse(AE_balanced_scorecard_mn$Quintile <3, 1,0)

model1_mn2 <- glm(AE_ATTENDS ~ age_cat + gender + deprived2 + offset(log(PERSONS))
                  , data=AE_balanced_scorecard_mn, family="poisson", na.action = na.omit)

summary(model1_mn2)

od_test(model1_mn2)


tidy(model1_mn2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived2")

##### Random-intercept for clustering at borough ####
# This is confounded with quantiles though, as there is a geographical component here.
# Wider problem is quantiles in general though, as a composite indicator.

model2_mn2 <- glmer(AE_ATTENDS ~ (1|GP_Borough_Name) + age_cat + gender + deprived2 +
                      nsk(month_no, knots = c(4,8))+ offset(log(PERSONS))
                    , data=AE_balanced_scorecard_mn, family="poisson", na.action = na.omit
                    , control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model2_mn2)
model2

od_test(model2_mn2)

tidy(model2_mn2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived2")


#### Negative Binomial ####
library(MASS)
library(splines2)
model3_mn2 <- glm.nb(AE_ATTENDS ~ age_cat + gender + deprived2 + offset(log(PERSONS)), data=AE_balanced_scorecard_mn, na.action = na.omit)
model3_mn223 <- glm.nb(AE_ATTENDS ~ age_cat + gender + deprived2 + GP_Borough_Name +
                       nsk(month_no, knots = c(4,8)) +
                       offset(log(PERSONS)) 
                     , data=AE_balanced_scorecard_mn, na.action = na.omit)

summary(model3_mn2)
summary(model3_mn223)

anova(model3_mn22)

od_test(model3_mn2)

AIC(model3_mn)
AIC(model3_mn223)

od_test(model3_mn2)

exp(coef(model3_mn))

exp(confint(model3_mn))

tidy(model3_mn2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived2")

tidy(model3_mn22, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived2")

# Incidence rate ratio: deprived = 1.3536108 
# The A&E admission rate is 1.35 (1.34 - 1.37) times higher, by rolling 12-month periods ending 01/01/2022 - 01/07/2023


library(sjPlot)

plot_model(model3_mn, ci.lvl = 0.95, line.size = 2)




#### Now per borough ###

mod_out1_mn2 <- 
  AE_balanced_scorecard_mn %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ age_cat + gender + deprived2 + 
                             nsk(month_no, knots = c(4,8)) + offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE))  %>% 
  mutate(adjustment = "Age, Sex, Seasonality")

mod_out1_mn2 %>% 
  filter(term == "deprived2")


mod_out2_mn2 <- 
  AE_balanced_scorecard_mn %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ deprived2 +
                             nsk(month_no, knots = c(4,8))+ offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)) %>% 
  mutate(adjustment = "Seasonality")

mod_out2_mn2 %>% 
  filter(term == "deprived2")


mod_out3_mn2 <- 
  AE_balanced_scorecard_mn %>%
  nest_by(GP_Borough_Name) %>%
  mutate(mod = list(glm.nb(AE_ATTENDS ~ deprived2 +
                             offset(log(PERSONS))
                           , data=data))) %>% 
  mutate(ci = list(confint(mod))) %>% 
  reframe(tidy(mod, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)) %>% 
  mutate(adjustment = "None")



mod_out3_mn2 %>% 
  filter(term == "deprived2")


mod_out1_mn2 %>% 
  bind_rows(mod_out2_mn2, mod_out3_mn2)  %>% 
  filter(term == "deprived2") %>% 
  ggplot(aes(x=GP_Borough_Name, y=estimate, colour=adjustment, fill = adjustment))+
  geom_point(position = position_dodge(width = 0.8))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, group=adjustment), position = position_dodge(width = 0.8))+
  labs(y="Incidence Rate Ratio (IRR)", x="GP Borough Name",
       colour = "Standardisation applied", fill ="Standardisation applied",
       title = "Relative A&E attendance rate for deprived populations against others",
       subtitle = "IMD Quntile 1-2 vs. 3-5, where 1 = rate is the same, >1 = deprived group is higher") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  scale_y_continuous(limits=c(1, 1.6))+
  scale_fill_ncl(reverse = FALSE)+
  scale_colour_ncl(reverse = FALSE)+
  theme_nclics()+
  theme(text = element_text(size=16))


ggsave("./output/ae_attend_IRR_IMD12.png", width = (758*2), height = (471*2), units = "px")

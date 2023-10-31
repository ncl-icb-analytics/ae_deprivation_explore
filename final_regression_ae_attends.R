# Final tidy version of script for monthly data for deprivation indicators
library(dplyr)
library(MASS)
library(broom)

# Database connection.  Pulls the connection string from your environmental variable called SANDPIT
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = Sys.getenv("SANDPIT"))

sql<- "-- output most recent month
select 
A.month, 
A.month_no,
A.GP_Borough_Name, 
A.[PCN_NAME],
A.[PRACTICE_CODE],
A.[PRACTICE_NAME],A.Quintile, A.ageband, A.gender, sum(PERSONS) as PERSONS, 
case when sum(AE_ATTENDS) is null then 0 else sum(AE_ATTENDS) end as AE_ATTENDS
from [Data_Lab_SBI].[dbo].[ML_BS_MPI] A
left join [Data_Lab_SBI].[dbo].[ML_BS_AE] B
on A.month_no = B.month_no
       and A.GP_Borough_Name=B.GP_Borough_Name 
       and A.Quintile = B.Quintile
          and A.ageband = B.ageband
          and A.gender = B.gender
                and A.[PCN_NAME] = B.[PCN_NAME]
              and A.[PRACTICE_CODE]= B.[PRACTICE_CODE]
              and A.[PRACTICE_NAME]= B.[PRACTICE_NAME]
       and A.LSOA = B.LSOA
          where A.quintile<>99
              and A.gender <> 'U'
              and A.ageband <105
                        group by 
A.month, 
A.month_no,
A.GP_Borough_Name, 
A.[PCN_NAME],
A.[PRACTICE_CODE],
A.[PRACTICE_NAME], A.Quintile, A.ageband, A.gender
"

# send the query and get the data back
AE_balanced_scorecard_mn <-  dbGetQuery(con,sql)


##### Create a couple of features
AE_balanced_scorecard_mn$deprived <- ifelse(AE_balanced_scorecard_mn$Quintile <2, 1,0)
AE_balanced_scorecard_mn$age_cat <- factor(AE_balanced_scorecard_mn$ageband)

# subset to just the most recent month
sub <- 
  AE_balanced_scorecard_mn %>% 
  filter(month == max(AE_balanced_scorecard_mn$month))

# Build a negative binomial regression model (like a Poisson regression, but deals with overdispersion better)
# The dataset is aggregated, not patient-level, so need to weight each row according to the number of 
# patients it refers to, this is what the 'offset' does.  Poisson / binomial / negative binomial
# all use a 'log' link function, so it's common to log the offset for scaling purposes.

attends_model <- glm.nb(AE_ATTENDS ~ age_cat + gender + deprived 
                    + offset(log(PERSONS))
                    , data=AE_balanced_scorecard_mn
                    , na.action = na.omit)



# Extract the coefficient.  The tidy function from broom package is helpful, as it exponentiates (converts the 
# coefficient to an incidence rate ratio by reversing the link function), and calculates a confidence interval.
# You want the estimate and conf.low & conf.high columns
tidy(attends_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

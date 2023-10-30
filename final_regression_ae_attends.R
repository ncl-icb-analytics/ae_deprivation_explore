# Final tidy version of script for monthly data for deprivation indicators
library(dplyr)
library(MASS)
library(broom)
#read in
AE_balanced_scorecard_mn<- 
  read_excel("data/AE BS by month and 3-mth rolling.xlsx"
             , col_types = c("date", "numeric", "text", 
                             "numeric", "numeric", "text", "numeric", 
                             "numeric")
             , na = "NULL"
  )

library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = Sys.getenv("SANDPIT"))

sql<- "select 
--A.month,
--A.month_no,
A.GP_Borough_Name, 
A.[PCN_NAME],
A.[PRACTICE_CODE],
A.[PRACTICE_NAME],A.Quintile, A.ageband, A.gender, sum(PERSONS) as PERSONS, 
case when sum(AE_ATTENDS) is null then 0 else sum(AE_ATTENDS) end as IP_FROM_AE
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
--A.month,
--A.month_no,
A.GP_Borough_Name, 
A.[PCN_NAME],
A.[PRACTICE_CODE],
A.[PRACTICE_NAME], A.Quintile, A.ageband, A.gender"

# send the query and get the data back
AE_balanced_scorecard_mn <-  dbGetQuery(con,sql)


##### Create a couple of features
AE_balanced_scorecard_mn$deprived <- ifelse(AE_balanced_scorecard_mn$Quintile <2, 1,0)
AE_balanced_scorecard_mn$age_cat <- factor(AE_balanced_scorecard_mn$ageband)


sub <- 
  AE_balanced_scorecard_mn %>% 
  filter(month == max(AE_balanced_scorecard_mn$month))

attends_model <- glm.nb(AE_ATTENDS ~ age_cat + gender + deprived 
                    + offset(log(PERSONS))
                    , data=AE_balanced_scorecard_mn
                    , na.action = na.omit)



tidy(attends_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "deprived")

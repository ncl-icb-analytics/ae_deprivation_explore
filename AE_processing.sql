

-- balanced scorecard - IP
-- CS 5/9/23

use data_lab_Sbi




DROP TABLE if exists [Data_Lab_SBI].[dbo].[ML_BS_MPI]



SELECT 
 [Month],
[Month_no],
OPA.OACode as LSOA,
Legacy_CCG_Name as GP_Borough_Name,
[PCN_NAME],
[PRACTICE_CODE],
[PRACTICE_NAME],
99 as Quintile,
floor(datediff(year,fpro.dateofbirth,getdate())/15)*15 AgeBand,
gend.[GenderCode2] as Gender,
count( distinct fpra.sk_patientid) as PERSONS

into [Data_Lab_SBI].[dbo].[ML_BS_MPI]
FROM [Data_Lab_SBI].[dbo].[CS_months]
  join [Fact].[Patient].[FactPractice] fpra
  on 1=1
  JOIN [Fact].[Patient].[FactProfile] fpro
       on fpra.sk_patientid=fpro.sk_patientid
  join dictionary.dbo.Organisation org1
       on fpra.SK_OrganisationID=org1.sk_organisationid
  join Dictionary.dbo.Organisation org2
       on org1.SK_OrganisationID_ParentOrg=org2.SK_Organisation_ID
  join [Dictionary].[dbo].[Gender] gend
       on fpro.sk_genderid = gend.sk_genderid
  join  [Fact].[Patient].[FactResidence] fres
       on fres.sk_patientid=fpro.sk_patientid
  join [Dictionary].[dbo].[OutputArea] opa
       on fres.SK_OutputAreaID = opa.SK_OutputAreaID
  join [NCL].[_dictionary].[NCL_PCN_2021] pcn
       on org1.organisation_code = pcn.PRACTICE_CODE
    where 
                                           (fres.PeriodStart<[End_Date])
              and                      (fres.PeriodEnd>=[Start_Date] or fres.PeriodEnd='9999-12-31' )
              and                      (fres.DateDetectedEnd>=[Start_Date] or fres.DateDetectedEnd is null)

              and                      (fpra.PeriodStart<[End_Date])
              and                      (fpra.PeriodEnd>=[Start_Date] or fpra.PeriodEnd='9999-12-31' ) 
              and                      (fpra.DateDetectedLeft>=[Start_Date] or fpra.DateDetectedLeft is null)

              and                      (fpro.PeriodStart<[End_Date])
              and                      (fpro.PeriodEnd>=[Start_Date] or fpro.PeriodEnd='9999-12-31' ) 
              and                      (fpro.dateofdeath>=[Start_Date] or fpro.dateofdeath is null)

  and fpra.SK_DataSourceID=7 
  and fpro.SK_DataSourceID=7
  and fres.SK_DataSourceID=7
  and OPA.CensusYear = '2011'
  and Legacy_CCG_Name <> ''


  group by  [Month],
[Month_no],
OPA.OACode,
Legacy_CCG_Name,
[PCN_NAME],
[PRACTICE_CODE],
[PRACTICE_NAME],
floor(datediff(year,fpro.dateofbirth,getdate())/15)*15,
gend.[GenderCode2]

  update A set Quintile = IMD_Quintile
from [Data_Lab_SBI].[dbo].[ML_BS_MPI] A
join  [Data_Lab_SBI].[RD].[IMD2019] B
on A.LSOA = B.LSOA_code_2011

-- now activity

DROP TABLE if exists [Data_Lab_SBI].[dbo].[ML_BS_AE]


SELECT 
 [Month],
[Month_no],
OPA.OACode as LSOA,
Legacy_CCG_Name as GP_Borough_Name,
[PCN_NAME],
[PRACTICE_CODE],
[PRACTICE_NAME],

floor(datediff(year,fpro.dateofbirth,getdate())/15)*15 AgeBand,
gend.[GenderCode2] as Gender,
99 as Quintile,
count( *) as AE_ATTENDS

into [Data_Lab_SBI].[dbo].[ML_BS_AE]
FROM [Data_Lab_SBI].[dbo].[CS_months]
  join SUS.IP.EncounterDenormalised A
  on 1=1
  join [Fact].[Patient].[FactPractice] fpra
  on fpra.sk_patientid=A.sk_patientid
  JOIN [Fact].[Patient].[FactProfile] fpro
       on fpra.sk_patientid=fpro.sk_patientid
  join dictionary.dbo.Organisation org1
       on fpra.SK_OrganisationID=org1.sk_organisationid
  join Dictionary.dbo.Organisation org2
       on org1.SK_OrganisationID_ParentOrg=org2.SK_Organisation_ID
  join [Dictionary].[dbo].[Gender] gend
       on fpro.sk_genderid = gend.sk_genderid
  join  [Fact].[Patient].[FactResidence] fres
       on fres.sk_patientid=fpro.sk_patientid
  join [Dictionary].[dbo].[OutputArea] opa
       on fres.SK_OutputAreaID = opa.SK_OutputAreaID
  join [NCL].[_dictionary].[NCL_PCN_2021] pcn
       on org1.organisation_code = pcn.PRACTICE_CODE
    where 
                                           (fres.PeriodStart<[End_Date])
              and                      (fres.PeriodEnd>=[Start_Date] or fres.PeriodEnd='9999-12-31' )
              and                      (fres.DateDetectedEnd>=[Start_Date] or fres.DateDetectedEnd is null)

              and                      (fpra.PeriodStart<[End_Date])
              and                      (fpra.PeriodEnd>=[Start_Date] or fpra.PeriodEnd='9999-12-31' ) 
              and                      (fpra.DateDetectedLeft>=[Start_Date] or fpra.DateDetectedLeft is null)

              and                      (fpro.PeriodStart<[End_Date])
              and                      (fpro.PeriodEnd>=[Start_Date] or fpro.PeriodEnd='9999-12-31' ) 
              and                      (fpro.dateofdeath>=[Start_Date] or fpro.dateofdeath is null)

  and fpra.SK_DataSourceID=7 
  and fpro.SK_DataSourceID=7
  and fres.SK_DataSourceID=7
  and OPA.CensusYear = '2011'
  and Legacy_CCG_Name <> ''
  and Episode_Start_Date >= Start_Date
  and Episode_Start_Date < End_Date
  and left(Admission_Method_Hospital_Provider_Spell,1) in ('1','2')
  and Episode_Number=1


  group by  [Month],
[Month_no],
OPA.OACode,
Legacy_CCG_Name,
[PCN_NAME],
[PRACTICE_CODE],
[PRACTICE_NAME],
floor(datediff(year,fpro.dateofbirth,getdate())/15)*15,
gend.[GenderCode2]

  update A set Quintile = IMD_Quintile
from [Data_Lab_SBI].[dbo].[ML_BS_AE] A
join  [Data_Lab_SBI].[RD].[IMD2019] B
on A.LSOA = B.LSOA_code_2011


-- output by month

/*
select A.month, A.month_no, A.GP_Borough_Name, A.Quintile, A.ageband, A.gender, sum(PERSONS) as PERSONS, sum(AE_ATTENDS) as AE_ATTENDS
from [Data_Lab_SBI].[dbo].[ML_BS_MPI] A
left join [Data_Lab_SBI].[dbo].[ML_BS_AE] B
on A.month_no = B.month_no
       and A.GP_Borough_Name=B.GP_Borough_Name 
       and A.Quintile = B.Quintile
          and A.ageband = B.ageband
          and A.gender = B.gender
       and A.LSOA = B.LSOA
          where A.quintile<>99
              and A.gender <> 'U'
              and A.ageband <105
group by A.month, A.month_no, A.GP_Borough_Name, A.Quintile, A.ageband, A.gender
*/

-- output most recent month
select A.GP_Borough_Name, 
A.[PCN_NAME],
A.[PRACTICE_CODE],
A.[PRACTICE_NAME],A.Quintile, A.ageband, A.gender, sum(PERSONS) as PERSONS, sum(AE_ATTENDS) as AE_ATTENDS
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
                        and A.month_no = 19
                        group by 
A.GP_Borough_Name, 
A.[PCN_NAME],
A.[PRACTICE_CODE],
A.[PRACTICE_NAME], A.Quintile, A.ageband, A.gender

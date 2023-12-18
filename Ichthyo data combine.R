# main issue here is a few dates duplicating when combining the Access data and 
# excel data in the end. 
# 01-06-20 also loses the program and sampling number associated with WDL data
# 02-10-20 and 03-10-20 no catch should be taken care of in the code to show NO CATCH
# those are last fixes that need to happen for data set to be in better shape combined (6-8-23)

# cleaner version of Access and Excel data combined, with the data overlap from 
# 2019 accounted for
# pivots for columns included
# fixing join errors

# load libraries needed

library(tidyverse)
library(dplyr)
library(ggplot2)
library(moderndive)
library(knitr)
library(gridExtra)
library(stringr)
library(plotly)
library(viridis)
library(kableExtra)
library(lubridate)
library(readxl)
library(readr)
library(anytime)
library(magrittr)
library(janitor)
library(tidylog)



#importing ich sampling data to clean up for comparison with lab data


###################### ACCESS DATA ############################################

PhysData <- read_csv("drift data/TblPhysicalDataAccess.csv") #physical data is shared with drift & ich, tows done at same time
CatchData <- read_csv("ichthyo data/TblLarvalCatchDataAccess.csv")
Species <- read_csv("ichthyo data/TblLarvalLookUpV2.csv")
SamplingData <- read_csv("ichthyo data/TblLarvalSampAccess.csv")
IEPFish <- read_csv("ichthyo data/IEP FISH CODE.csv")

#check column types before renaming and adjusting columns

str(PhysData)
str(Species)
str(SamplingData)
str(CatchData)

### now rename columns for consistency/being able to merge data together later

PhysData <- PhysData %>%
  rename (YSI = 'YSI #',
          Station = 'Station Code',
          FieldCheck = "Field Check",
          FieldEnteredByAccess = 'EnteredBy',
          FieldQAQCByAccess = "QA/QC'dBy",
          FieldCommentsAccess = 'Comments')
View(PhysData)

Species <- Species %>%
  rename (SpeciesCode = "Code",
          CommonName = "Species",
          ScientificName = "Scientific Name")
View(Species)

SamplingData <- SamplingData %>%
  rename (SamplingQAQCByAccess = "QA/QC'dBy",
          SamplingEnteredByAccess = 'EnteredBy')
View(SamplingData)

IEPFish <- IEPFish %>%
  select(-c("...5":"...17"))
View(IEPFish)

#figure out how to match IEP fish code with YBFMP fish code later..some don't have
#a corresponding code

#check column types before trying to combine dataframes

str(PhysData)
str(Species)
str(SamplingData)
str(CatchData)

#create date and time column, put date into correct format

PhysData2 <- mutate(PhysData, Date=mdy(Date),DateTime = ymd_hms(paste(as.character(Date), as.character(Time))))
View(PhysData2)

## 8 failed to parse = 8 where date/time was NA in file - keep eye for later

#removing unneeded columns from view of table
#start meter/end meter refer to zoop tows, not ich, so they can be dropped here

PhysData3 <- PhysData2 %>%
  select(-c(LightData, DriftData, ZoopsData, `50_ZoopsData`, ChlData,
            LarvalData, PhytoData, NutrData, Recorder, FieldCheck, Crew,
            FieldEnteredByAccess, FieldQAQCByAccess, StartMeter, EndMeter,
            MeterSetTime))
View(PhysData3)

#discovered in 2015 tows were done to determine if channel location created bias in sampling
# will need to arrange those columns 

CheckLocation <- filter(CatchData, !is.na(ChannelLocation))

#confirms that channel location/tow location included with speciated data

#combine and update catch/species data for channel location
CatchSpecies <- left_join(CatchData, Species)
View(CatchSpecies)

CatchSpecies2 <- mutate(CatchSpecies, ChannelLocation = case_when(is.na(ChannelLocation) ~ "Center", TRUE ~ ChannelLocation))

IchSample <- left_join(PhysData3, SamplingData, by = "PhysicalDataID")
View(IchSample)

# rename columns to help simplify before pivoting

IchSample2 <- IchSample %>%
  rename(Start_MidWest = "LarvalStartMeter_Mid_West",
         End_MidWest = "LarvalEndMeter_Mid_West",
         Start_NearWest = "LarvalStartMeter_Near_West",
         End_NearWest = "LarvalEndMeter_Near_West",
         Start_MidEast = "LarvalStartMeter_Mid_East",
         End_MidEast = "LarvalEndMeter_Mid_East",
         Start_NearEast = "LarvalStartMeter_Near_East",
         End_NearEast = "LarvalEndMeter_Near_East",
         Start_MidBottom = "LarvalStartMeter_Mid_Bottom",
         End_MidBottom = "LarvalEndMeter_Mid_Bottom") %>%
  select(-c(SamplingEnteredByAccess, SamplingQAQCByAccess,
            StartTime, StopTime, "2ndStartTime_East", "2ndStopTime_East",
            Bottom_StartTime))
View(IchSample2)

StartPivot <- pivot_longer(IchSample2,
                           cols = c(Start_MidWest, Start_NearWest,
                                    Start_MidEast, Start_NearEast,
                                    Start_MidBottom, LarvalStartMeter), 
                           names_to = c("TowLocation"),
                           values_to = c("StartValue"),
                           names_prefix = c("Start_"),
                           values_drop_na = TRUE) %>%
  select(-c(End_MidWest, End_NearWest,
            End_MidEast, End_NearEast, 
            End_MidBottom, LarvalEndMeter)) %>%
  mutate(TowLocation = case_when(TowLocation == "LarvalStartMeter" ~ "Center",
                                 TowLocation == "MidWest" ~ "Mid_West",
                                 TowLocation == "MidEast" ~ "Mid_East",
                                 TowLocation == "NearWest" ~ "Near_West",
                                 TowLocation == "NearEast" ~ "Near_East",
                                 TowLocation == "MidBottom" ~ "Bottom",
                                 TRUE ~ TowLocation))
View(StartPivot)

EndPivot <- pivot_longer(IchSample2,
                         cols = c(End_MidWest, End_NearWest,
                                  End_MidEast, End_NearEast,
                                  End_MidBottom, LarvalEndMeter), 
                         names_to = c("TowLocation"),
                         values_to = c("EndValue"),
                         names_prefix = c("End_"),
                         values_drop_na = TRUE) %>%
  select(-c(Start_MidWest, Start_NearWest,
            Start_MidEast, Start_NearEast, 
            Start_MidBottom, LarvalStartMeter)) %>%
  mutate(TowLocation = case_when(TowLocation == "LarvalEndMeter" ~ "Center",
                                 TowLocation == "MidWest" ~ "Mid_West",
                                 TowLocation == "MidEast" ~ "Mid_East",
                                 TowLocation == "NearWest" ~ "Near_West",
                                 TowLocation == "NearEast" ~ "Near_East",
                                 TowLocation == "MidBottom" ~ "Bottom",
                                 TRUE ~ TowLocation))
View(EndPivot)

Pivot1 <- left_join(StartPivot, EndPivot)
View(Pivot1)

Pivot3 <- filter(Pivot1, !is.na(LarvalDataID)) %>%
  left_join(CatchSpecies2, by = c("LarvalDataID", "TowLocation" = "ChannelLocation"))
View(Pivot3)

# checking values between the two dataframes before joining, ensuring everything matches
# for "channel location" and "tow location" as they are essentially the same

unique(CatchSpecies2$ChannelLocation)
unique(Pivot1$TowLocation)

# larval code mainly utilized for linking things in access database, now that 
# things are linked, not necessary to keep so can be dropped here

IchAccess <- Pivot3 %>%
  filter(Station == "STTD" | Station == "SHR") %>%
  select(-c(LarvalCode.x, LarvalCode.y))
View(IchAccess)

# this ensures looking at data that is in access, not excel
# this means these larvaldataID are not affected by the data gap to address
# between access and excel
# some of these NA are "no catch" and not blanks - think about a solution? 110 values
# early data (i.e. 1998 -2003 not found via contractor data records...)

# narrow down how many can be confirmed "no catch" (2010 onward) versus
# how many must be marked as "no data" (2009 and earlier) due to not being able 
# to confirm with contractor data amd verifying it's not an additional mistaken entry

viewNA <- filter(IchAccess, is.na(LarvalCatchID))
# some include the data gap between access and excel

viewAccessNA <- filter(viewNA, year(Date) < 2019)


AccessNA <- filter(viewAccessNA, year(Date) >= 2010 & year(Date) < 2019)
AccessNoData <- filter(viewAccessNA, year(Date) < 2010)



# now rename each group of data appropriately and ensure # replaced values equal to 
# what was narrowed down between the two date ranges

NOSpecimens = mutate(IchAccess, SpeciesCode = case_when(IchAccess$Date %in% viewAccessNA$Date & year(Date) >= 2010
                                                        & year(Date) < 2019 & is.na(SpeciesCode) ~ 
                                                          "NONE", TRUE ~ SpeciesCode))

NoData = mutate(NOSpecimens, SpeciesCode = case_when(IchAccess$Date %in% viewAccessNA$Date & year(Date) < 2010
                                                     & is.na(SpeciesCode) ~ "NODATA", TRUE ~ SpeciesCode))

IchAccessA <- NoData
#this should have a date/time column for ease of combining with excel data later on

#create a version of this as csv
write_csv(
  IchAccessA,
  "IchAccessEdited.csv",
  na = "NA",
  append = FALSE,
  col_names = TRUE,
  quote = c("needed", "all", "none"),
  escape = c("double", "backslash", "none"),
  eol = "\n",
  num_threads = readr_threads(),
  progress = show_progress())

#next fix the species code lookup table to account for all possibilities
# i.e. Menidia spp and Menidia sp. = same since what was in excel is different from
# how the species lookup is for access 

#TO FILL THE 2019/2020 gap of data between excel and access, now read in data from Excel

####### bring lab data from excel in ##############
IchLabData <- read_csv("ichthyo data/IchLabExcelData.csv", skip=1)

IchLabData <- IchLabData %>%
  rename(Program = 'Measuring program short name',
         Date = 'Sampling Event Date',
         Time = 'Sampling Event Time',
         Station = 'Sampling Area Number',
         SamplingNumber = 'Sampling Event Number',
         SampleID = 'Sample ID',
         OrganismType = 'Observation Type Short Name',
         OrganismGroup = 'Attribute',
         ScientificName = 'Observable',
         TotalCountSpecies = 'Value...10',
         TL = 'Value...11',
         FL = 'Value...12',
         LifeStage = 'Value...13',
         LarvalLifeStage = '...14',
  )

View(IchLabData)

#get rid of descriptor rows

IchLab2 <- filter(IchLabData, !is.na(Program))

View(IchLab2)

#determine column types, create date/time column and fix date format

str(IchLab2)

IchLab2 <- mutate(IchLab2, Date=mdy(Date),DateTime = ymd_hm(paste(as.character(Date), Time)), Time= NULL)

View(IchLab2)

# to fix issue with scientific names and creating consistency, must edit the 
# species lookup table from access to include new versions of scientific name
# to match codes properly. This is to reduce # of "NA" in species code and common name
# when time comes to publish

## use this for fixing the issue with scientific names ######
unique(IchLab2$ScientificName)

unique(Species$ScientificName)

unique(IchLab2$OrganismGroup)

unique(Species$ScientificName)

#per contractor form Menidia sp. = common name silverside
# between lab data in excel and lab data from access, add to the 
# species lookup table to cover everything for joining 

SpeciesUpdate <- Species %>%
  add_row(SpeciesCode = "UNSS" , CommonName = "silverside", ScientificName = "Menidia sp.") %>%
  add_row(SpeciesCode = "POM", CommonName = "Unid Crappie", ScientificName = "Pomoxis sp.") %>%
  add_row(SpeciesCode = "NONE", CommonName = "NoCatch", ScientificName = "No Catch") %>%
  add_row(SpeciesCode = "RAIKIL", CommonName = "Rainwater Killifish", ScientificName = "Lucania parva") %>%
  add_row(SpeciesCode = "KLF", CommonName = "Killifish", ScientificName = "Cyprinodontidae spp.")

SpeciesUpdate2 <- SpeciesUpdate %>%
  filter(!is.na(ScientificName)) %>%
  filter(SpeciesCode != "TSE") %>%
  filter(SpeciesCode != "ASE")

# removing TSE - threadfin shad eggs and ASE american shad eggs in this version 
# since it does not appear contractor has made a comment re: eggs
# also the "animal tissue, baby clam, etc" is removed since those NA don't match
# correctly when tied to the excel data

# filter lab data from excel to the date range that needs to be merged with access

LabGap <- IchLab2 %>%
  filter(Date < "2020-02-24")
View(LabGap)

#ensure FL is correct column type

LabGapA <- LabGap %>%
  mutate(FL = as.numeric(FL))
View(LabGapA)

#combine 2019 lab data with access data to "complete" data set 
#how to match this up correctly? bind rows adds rows at end instead of matching with
#physical data id, but left join does not add in the data that matches correctly? --
# because TL and FL also exist in access data? 

IchAccessFilter <- IchAccessA %>%
  filter(Date > "2019-04-15") %>%
  select(-c(TL, FL, ScientificName, SpeciesCode, Count,
            Comments, CommonName, Stage, DateTime))
View(IchAccessFilter)
##temporary solution is remove date/time column here since it probably has wrong 
## time for 5-6-19 - time for 5/6 should be 9:53 per data sheet. was likely entered
# wrong before being sent to contractors so ID came back with wrong time.

IchAccessFilter <- IchAccessA %>%
  filter(Date > "2019-04-15") %>%
  select(-c(TL, FL, ScientificName, Count,
            Comments, CommonName, Stage, DateTime))
View(IchAccessFilter)

# need to add in species code to lab data before
#lab data from excel also needs to have species code etc joined
# must complete this before joining data frames together to ensure no unnecessary 
# NA filled columns
# note gap in sampling due to COVID-19 emergency in 2020 to 2021
# lab data is only current through 4/11/2022

GapData <- left_join(IchAccessFilter, LabGapA)

## 01-06-20 is no catch, but still has WDL, make sure that the NA is NO CATCH

GapDataNOCATCH <- mutate(GapData, ScientificName = case_when(is.na(ScientificName) 
                                                             ~ "No Catch", TRUE ~ ScientificName))

AccessTestSpecies <- left_join(GapDataNOCATCH, SpeciesUpdate2)

IchAccessFilter2 <- IchAccessA %>%
  filter(Date < "2019-04-22")

# AccessGap2 <- bind_rows(GapData, IchAccessFilter2)
# # this seems to be all the data in the right place for access with the data 
# # overlap issue addressed

# GapTrial <- bind_rows(AccessTestSpecies, IchAccessFilter2)
# # this seems to fix the code issue...without anything repeating or not matching

AccessGap <- bind_rows(AccessTestSpecies, IchAccessFilter2)

# now appropriate lab data has been added to access
# this should now have all the lab data from 2019 and jan 2020 
# matched with the 2019 access data
# now bring in sampling data from excel to merge excel files

################ bring sampling data from excel in #############################

read_csv("icthyo data/IchSampData.csv")

#determine and label column names

IchSampling <- read_csv("ichthyo data/IchSampData.csv", skip =1)

#first rename columns, then remove descriptor row using filter

IchSampling <- IchSampling %>%
  rename(Program = 'Measuring program short name',
         Date = 'Sampling Event Date',
         Time = 'Sampling Event Time',
         Station = 'Sampling Area Number',
         SamplingNumber = 'Sampling Event Number',
         PhysicalDataID = '...6',
         ConditionCode = 'Condition Code',
         SamplingAltered = 'Sampling Altered',
         MeterSetTime = 'Set Time',
         FlowMeterStart = 'Flow Meter Start',
         FlowMeterEnd = 'Flow Meter End',
         FlowMeter50Start = 'Flow Meter Start (50)',
         FlowMeter50End = 'Flow Meter End (50)',
         FlowMeterSpeed = 'Flow Meter Speed',
         PhysicalDataIDx = 'Physical Data ID',
         EnteredBy = 'Entered by',
         QAQCBy = "QAQC'd by",
         FieldComments = 'Field Comments',
         LabComments = 'Lab Comments',
         SampleVolume = 'Sample Volume',
         SubsampleNumber = 'Subsample Number',
         DilutionVolume = 'Dilution Volume',
         SlideCount = 'Slide Count',
         MeshSize = 'Observation Area Name',
         Observation = 'Observation Area Number',
         SpotNumber = 'Spot Number',
         SpotCode = 'Spot Code (original/duplicate)')

View(IchSampling)

#filtering the na rows removes the excess at the end of the file and the descriptor row without losing column names

IchSampling2 <- filter(IchSampling, !is.na(Program))

View(IchSampling2)

#structure helps show what types your columns are so if any need to change it's easier to find

str(IchSampling2)

#mutating date and time into date/time column for ease of combining data

IchSampling2 <- mutate(IchSampling2, Date=mdy(Date),DateTime = ymd_hm(paste(as.character(Date), Time)), Time = NULL)

#add Time = NULL to get rid of character vs time issue

#filtering the rows helps to remove the demonstration data that was in the file

IchSampling2 <- filter(IchSampling2, year(Date)>2018)
View(IchSampling2)


IchSampling3 <- IchSampling2 %>%
  select(-c(PhysicalDataID, PhysicalDataIDx, FlowMeter50Start, FlowMeter50End,
            EnteredBy, QAQCBy, SpotCode, SpotNumber, Observation))
View(IchSampling3)

str(IchSampling3)

#####  Bringing in water Quality from Excel #######################################
# must remember to bring in water quality data - housed separately from sampling and 
# lab data in lower trophic excel databases. combine water quality with sampling data
# (1:1 combine) and then combine lab data #name columns appropriately

read_csv("drift data/LTWQ2020on.csv")

#determine and label column names

WaterQuality <- read_csv("drift data/LTWQ2020on.csv", skip =1)

View(WaterQuality)

WaterQuality <- WaterQuality %>%
  rename(Program = "Measuring Program Name",
         Station = "Station Name",
         StationNumber = "Station Number",
         SamplingNumber = "Sampling Number (event)",
         Date = "WDL SAM_COLLECTION_DATE",
         Time = "Collection Time",
         SampleNumber = "Sample Number",
         SampleDate = "Sample Date",
         SampleTime = "Sample Time",
         FieldCheck = "Field Check",
         SecchiDiskDepth = "secchi",
         WaterTemperature = "water.temp",
         DO = "DO.probe",
         SpCnd = "sp.cond",
         MicrocystisVisualRank = "microcyst",
         VegetationRank = "VegRank",
         Turbidity = "turb")

View(WaterQuality)

str(WaterQuality)

#need to filter out first row - descriptor row

WaterQuality2 <- filter(WaterQuality, !is.na(Program))

View(WaterQuality2)

str(WaterQuality2)

#make sure to change time to proper formats before adjusting rows and columns

WaterQuality3 <- mutate(WaterQuality2, Date=mdy(Date),DateTime = ymd_hm(paste(as.character(Date), Time)), Time= NULL)

View(WaterQuality3)

#need to filter out columns that are blank, and columns that can be left out for 
# QAQC and publishing

WaterQuality3 <- WaterQuality3 %>%
  select(-c(`Run Number`, `Run Name`, SampleNumber, SampleDate, SampleTime,
            Recorder, FieldChecker, Crew, LightData, LarvalData, DriftData,
            `150_ZoopsData`, `50_ZoopsData`, PhytoData, ChlData, NutrData,
            EnteredBy, QAQCBy, FieldCheck, StationNumber, Depth1Irr, Depth2Irr, 
            Depth3Irr, Depth4Irr,SurfaceIrr, SubIrr1, SubIrr2, SubIrr3, SubIrr4,
            LabOrField))
View(WaterQuality3)

#will need to filter out information that is for NDFA since
#no ich tows done during NDFA/NDFS

WaterQuality4 <- WaterQuality3 %>%
  filter(Program == "YBFMP" | Program == "Shared")
View(WaterQuality4)

# this still includes LIS data, which no tows are done at LIS for ich for YBFMP
# no SHR data from 2017 on is needed...hopefully left join takes care of this 
# otherwise will need to filter that out later

WaterQuality5 <- WaterQuality3 %>%
  filter(Program == "NDFA")
View(WaterQuality5)

WaterQuality6 <-WaterQuality3 %>%
  filter(Program == "NDFS")
View(WaterQuality6)

#previous 2 df created to double check the total number of objects against original
#to ensure nothing was missed in filtering (5 and 6)

IchSamplingWater <- left_join(IchSampling3, WaterQuality4)
View(IchSamplingWater)

#should not be any tows from SHR included since SHR was not sampled 2017 onward

SHR <- filter(IchSamplingWater, Station == "SHR")
View(SHR)
# the two points found show in comments that the ich net was used for comparison, 
# not for an actual tow


IchSamplingWater2 <- IchSamplingWater %>%
  filter(Station == "STTD") %>%
  rename(StartValue = "FlowMeterStart",
         EndValue = "FlowMeterEnd")
View(IchSamplingWater2)

# since data is incomplete for 2022, filter out 2022

IchSamplingWater3 <- IchSamplingWater2 %>%
  filter(year(Date) < 2022)
View(IchSamplingWater3)

# 2-10-2020 no catch, 3-10-2020 no catch. ensure NA is taken care of being joining
# with species data

IchExcelSpecies <- left_join(IchSamplingWater3, IchLab2)
View(IchExcelSpecies)

NAScientificName <- filter(IchExcelSpecies, is.na(ScientificName))

IchExcelSpeciesA <- mutate(IchExcelSpecies, ScientificName = case_when(is.na(ScientificName) 
                                                                       ~ "No Catch", TRUE ~ ScientificName))
View(IchExcelSpeciesA)

# now all Scientific name is filled out so can be matched with species code
# when joined with SpeciesUpdate2 

IchExcelSpeciesCode <- left_join(IchExcelSpeciesA, SpeciesUpdate2)
str(IchExcelSpeciesCode)

# now that excel and access is sorted out, need to combine for full Ich Data

IchAccessB <- AccessGap %>%
  mutate(YSI = as.character(YSI),
         TotalCountSpecies = as.numeric(TotalCountSpecies))
View(IchAccessB)


IchExcelSpeciesB <- IchExcelSpeciesCode %>%
  mutate(TL = as.numeric(TL),
         FL = as.numeric(FL),
         SecchiDiskDepth = as.numeric(SecchiDiskDepth),
         WaterTemperature = as.numeric(WaterTemperature),
         DO = as.numeric(DO),
         SpCnd = as.numeric(SpCnd),
         EC = as.numeric(EC),
         pH = as.numeric(pH),
         MicrocystisVisualRank = as.numeric(MicrocystisVisualRank),
         VegetationRank = as.numeric(VegetationRank),
         Turbidity = as.numeric(Turbidity),
         TotalCountSpecies = as.numeric(TotalCountSpecies))
View(IchExcelSpeciesB)

# now that columns type match...bind rows

AllIch <- bind_rows(IchAccessB, IchExcelSpeciesB)
View(AllIch)

AllIch <- bind_rows(IchAccessB, IchExcelSpeciesCode)



#############
##removed PhysicalDataID = as.numeric(PhysicalDataID) from All ich code since Excel doesn't have
## physical data ID 



SPLT <- filter(AllIch, SpeciesCode == "SPLT")
PRS <- filter(AllIch, SpeciesCode == "PRS")

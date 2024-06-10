
library(tidyverse)
library(dplyr)
library(ggplot2)
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

################# bring in qa/qc physical data #################################
PhysData <-read_csv("drift data/LT_phys_qc_20240118.csv")

######### Read in Access catch data ###########################################

CatchData <- read_csv("ichthyo data/TblLarvalCatchDataAccess.csv")
Species <- read_csv("ichthyo data/TblLarvalLookUpV2.csv")
SamplingData <- read_csv("ichthyo data/TblLarvalSampAccess.csv")
Sampling2 <- read_csv("ichthyo data/IchSampData.csv", skip = 1)
IEPFish <- read_csv("ichthyo data/IEP FISH CODE.csv")

########## Read in Excel Catch data ###########################################

IchLabData <- read_csv("ichthyo data/IchLabExcelData.csv", skip=1)


wy <- read_csv("WaterYearType_CDEC.csv") 
inundation <- read_csv("Yolo_Bypass_Inundation_1998-2022.csv")


Species <- Species %>%
  rename (SpeciesCode = "Code",
          CommonName = "Species",
          ScientificName = "Scientific Name")
View(Species)

SamplingData <- SamplingData %>%
  rename (SamplingQAQCByAccess = "QA/QC'dBy",
          SamplingEnteredByAccess = 'EnteredBy',
          FieldCommentsLarval = "FieldComments") %>%
  select(-c(SamplingEnteredByAccess, SamplingQAQCByAccess))
View(SamplingData)

IEPFish <- IEPFish %>%
  select(-c("...5":"...17"))
View(IEPFish)

PhysData <- PhysData %>%
  select(-c(MeterSetTime, FlowMeterStart, FlowMeterEnd, FlowMeterSpeed, ConditionCode))
#these flow meter values relate to drift data, not ichthyo

CatchData <- CatchData %>%
  rename(CommentsCatch = Comments)



CheckLocation <- filter(CatchData, !is.na(ChannelLocation))

#confirms that channel location/tow location included with speciated data

#combine and update catch/species data for channel location
CatchSpecies <- left_join(CatchData, Species)
View(CatchSpecies)

CatchSpecies2 <- mutate(CatchSpecies, ChannelLocation = case_when(is.na(ChannelLocation) ~ "Center", TRUE ~ ChannelLocation))

IchSample <- left_join(PhysData, SamplingData, by = "PhysicalDataID")
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
  select(-c(StartTime, StopTime, "2ndStartTime_East", "2ndStopTime_East",
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
  select(-c(LarvalCode.x, LarvalCode.y)) %>%
  rename(FieldCommentsAccess = "FieldComments") %>%
  relocate(event_id, Datetime)
View(IchAccess)

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

#lab data in excel 4-22-19 onward. Access phys data through 1/27/2020
#filter physical data file to account for this overlap
Phys2019 <- PhysData %>% 
  filter(Date > "2019-04-16" & Date < "2020-01-01")

#access data that does not overlap with excel
#this is full data - physical, sampling, catch that is in access
IchAccessB <- IchAccessA %>%
  filter(Date< "2019-04-22")

#access data that does overlap with excel
IchAccessC <- IchAccessA %>%
  filter(Date > "2019-04-16" & Date < "2020-01-01")

# testjoin <- left_join(IchAccessC, IchSampling3)

# write_csv(IchAccessB, paste("test files/AccessCatchData.csv"))
# write_csv(,paste("test files/overlapdata.csv"))

#lab data for 4/22/2019 onward is in Excel
#physical data overlaps for 01/06/2020 and 01/27/2020 - use excel phys data for this, remove from access phys data
#join phys data from 4/22/2019 to end of 2019 with catch data from Excel
#combine lab data from 01/06/2020 and 01/27/2020 with excel
#once all these separate pieces are accounted for, bind all dataframes to create complete ich catch data 1999-2022

#bring in lab data from Excel, then separate to what does and does not overlap
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
         LarvalLifeStage = '...14',) %>%
  filter(!is.na(Program)) %>%
  select(-c(SamplingNumber, SampleID, Program, OrganismType, OrganismGroup))

#format date and time columns for ability to combine with Access data
IchLabData$Date<-as.Date(IchLabData$Date,"%m/%d/%Y")
IchLabData$Year <- year(IchLabData$Date)
IchLabData$Month <- month(IchLabData$Date)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
IchLabData$MonthAbb <- mymonths[ IchLabData$Month ]
IchLabData$Datetime = paste(IchLabData$Date, IchLabData$Time)
IchLabData$Datetime <- ymd_hm(IchLabData$Datetime)
IchLabData$Time <- strptime(IchLabData$Time, format = "%H:%M", tz = "") %>%
  strftime(IchLabData$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)
IchLabData$Time <- hms::as_hms(IchLabData$Time)

IchLabData2 <- IchLabData %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)%>%
  filter(year(Date)<2023)

#combine lab data with "sampling data to ensure no missing field comments for ich tows

#create two lab data frames. one 4-22-2019 to end of 2019, one 2020 to 2022
IchLab2019 <- IchLabData2 %>%
  filter(year(Date) == 2019) %>%
  mutate(FL = as.numeric(FL))

IchLab2019$ScientificName <- str_replace_all(IchLab2019$ScientificName, "Menidia berylina", "Menidia beryllina")

IchLabExcel <- IchLabData2 %>%
  filter(year(Date) > 2019)

#correct scientific name misspelling
IchLabExcel$ScientificName <- str_replace_all(IchLabExcel$ScientificName, "Menidia berylina", "Menidia beryllina")

#important to account for changes in scientific name and species code notation from contractors here - merge these
#lab data with speciesupdate2 before binding with the data from Access

IchLab2019S <- left_join(IchLab2019, SpeciesUpdate2)

IchLabExcelS <- left_join(IchLabExcel, SpeciesUpdate2)

#note that the counts for lab are different from how counts were notated in Access - will need to update this to make sure
#all the counts match prior to publishing

IchSampling <- Sampling2 %>%
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
         FieldCommentsExcel = 'Field Comments',
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

IchSampling2$Date<-as.Date(IchSampling2$Date,"%m/%d/%Y")
IchSampling2$Year <- year(IchSampling2$Date)
IchSampling2$Month <- month(IchSampling2$Date)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
IchSampling2$MonthAbb <- mymonths[ IchSampling2$Month ]
IchSampling2$Datetime = paste(IchSampling2$Date, IchSampling2$Time)
IchSampling2$Datetime <- ymd_hm(IchSampling2$Datetime)
IchSampling2$Time <- strptime(IchSampling2$Time, format = "%H:%M", tz = "") %>%
  strftime(IchSampling2$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)
IchSampling2$Time <- hms::as_hms(IchSampling2$Time)

#filtering the rows helps to remove the demonstration data that was in the file

IchSampling2 <- filter(IchSampling2, year(Date)>2018)
View(IchSampling2)

#drop unnecessary columns and create event id column for ease of merging with phys data and catch data

IchSampling3 <- IchSampling2 %>%
  select(-c(PhysicalDataID, PhysicalDataIDx, FlowMeter50Start, FlowMeter50End,
            EnteredBy, QAQCBy, SpotCode, SpotNumber, Observation, Program, SamplingNumber,
            SubsampleNumber,DilutionVolume,SlideCount)) %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime) %>%
  filter(year(Date)<2023)
View(IchSampling3)

str(IchSampling3)


#2020 overlap between access and excel
IchOverlap <- IchSampling3 %>%
  filter(Date == "2020-01-06" | Date == "2020-01-27")

IchOverlapCatch <- left_join(IchOverlap, IchLabExcelS)

IchSampling4 <- IchSampling3 %>%
  filter(!(Date == "2020-01-06" | Date == "2020-01-27"))

IchExcelCatch <- left_join(IchSampling4, IchLabExcelS)

#test binding the two jan 2020 overlap dates with the rest of the excel catch data - success
tesbind <- bind_rows(IchOverlapCatch, IchExcelCatch)

#next step is binding these to the 2019 overlap
#phys2019, ichaccessC, and ichlab2019
#sampling2019 is for ichthyo only - phys data file has both drift and ich - ich is not done at shr and last ich tow in 2019 was july
Sampling2019 <- left_join(Phys2019, IchAccessC) %>%
  filter(!(Station == "SHR")) %>%
  filter(!(is.na(StartValue))) %>%
  select(-c(ScientificName, TL, FL, SpeciesCode, CommonName))


str(Sampling2019)
str(IchLab2019)
#now to combine that sampling data with the lab data to get full catch
IchCatch2019 <- left_join(Sampling2019, IchLab2019S)

#bind access to 2019, then bind that combined dataframe to excel
IchAccessOverlap <- bind_rows(IchAccessB, IchCatch2019)

IchExcelOverlap <- bind_rows(IchOverlapCatch, IchExcelCatch) %>%
  mutate(FL = as.numeric(FL))

IchFullData <- bind_rows(IchAccessOverlap, IchExcelOverlap)

#export bound file
write_csv(IchFullData, "test files/FullData.csv")


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

#find method to add a column for individual measurement counts instead of total counts
# first verify that "total count species" column is only 4/22/2019 onward

viewtotal <- IchFullData %>%
  filter(!is.na(TotalCountSpecies))

viewNAtotal <- IchFullData%>%
  filter(is.na(TotalCountSpecies))

view2019 <- IchFullData %>%
  filter(Date > "2019-04-15")

test2019 <- IchFullData %>%
  mutate(IndividualCount = if_else(TotalCountSpecies >= 1, 1, NA))
#this worked - is there a way to combine this with the already existing count column so it isn't excessive? besides manually?


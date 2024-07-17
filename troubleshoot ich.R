
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
  filter(Date< "2019-04-22") %>%
  rename(FlowMeterStart = StartValue,
         FlowMeterEnd = EndValue,
         MeterSetTime = SetTime) %>%
  select(-c(FieldCommentsAccess))

#access data that does overlap with excel
IchAccessC <- IchAccessA %>%
  filter(Date > "2019-04-16" & Date < "2020-01-01") %>%
  rename(FlowMeterStart = StartValue,
         FlowMeterEnd = EndValue,
         MeterSetTime = SetTime) %>%
  select(-c(FieldCommentsAccess))

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

#per contractor form Menidia sp. = common name silverside
# between lab data in excel and lab data from access, add to the 
# species lookup table to cover everything for joining 
#double check species names

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

IchOverlapCatch <- left_join(IchOverlap, IchLabExcelS) %>%
  rename(FieldComments = FieldCommentsExcel)

IchSampling4 <- IchSampling3 %>%
  filter(!(Date == "2020-01-06" | Date == "2020-01-27"))

IchExcelCatch <- left_join(IchSampling4, IchLabExcelS) %>%
  rename(FieldComments = FieldCommentsExcel) %>%
  filter(Station == "STTD")
#removes the times ich net was used at SHR for comparison - no actual tow done

#test binding the two jan 2020 overlap dates with the rest of the excel catch data - success
# tesbind <- bind_rows(IchOverlapCatch, IchExcelCatch)

#next step is binding these to the 2019 overlap
#phys2019, ichaccessC, and ichlab2019
#sampling2019 is for ichthyo only - phys data file has both drift and ich - ich is not done at shr and last ich tow in 2019 was july
Sampling2019 <- left_join(Phys2019, IchAccessC) %>%
  filter(!(Station == "SHR")) %>%
  filter(!(is.na(FlowMeterStart))) %>%
  select(-c(ScientificName, TL, FL, SpeciesCode, CommonName, FieldComments))


str(Sampling2019)
str(IchLab2019)
#now to combine that sampling data with the lab data to get full catch
IchCatch2019 <- left_join(Sampling2019, IchLab2019S)


#bind access to 2019, then bind that combined dataframe to excel
IchAccessOverlap <- bind_rows(IchAccessB, IchCatch2019)

IchAccessOverlapA <- IchAccessOverlap %>%
  rename(FieldComments = FieldCommentsLarval,
         LabComments = CommentsCatch)

PhysExcel <- PhysData %>%
  filter(year(Date) > 2019) %>%
  filter(Station == "STTD")

PhysExcelOverlap <- PhysExcel %>%
  filter(Date == "2020-01-06" | Date == "2020-01-27") %>%
  select(-c(FieldComments, MeshSize))

PhysExcelB <- PhysExcel %>%
  filter(Date >= "2020-02-10") %>%
  select(-c(FieldComments, MeshSize, SamplingAltered)) %>%
  filter(Date < "2022-07-01") %>%
  filter(Station == "STTD") %>%
  unique()

IchOverlapCatchA <- left_join(PhysExcelOverlap, IchOverlapCatch)
IchExcelCatchA <- left_join(IchExcelCatch, PhysExcelB)

IchExcelOverlap <- bind_rows(IchOverlapCatchA, IchExcelCatchA) %>%
  mutate(FL = as.numeric(FL)) %>%
  mutate(CountIndividual = if_else(TotalCountSpecies >= 1, 1, NA))%>%
  group_by(event_id, PhysicalDataID, Datetime, Date, Time, Station, YSI, WeatherCode,Tide, MicrocystisVisualRank,
           WaterTemperature, Secchi, Conductivity, SpCnd, pH, DO, Turbidity, FlowDirection, VegetationRank, ConditionCode, SamplingAltered,
           FieldComments_WQ, Flag_PQC, Comment_PQC, DataCorrectionComments, Year, Month, MonthAbb,
           MeterSetTime, FlowMeterStart, FlowMeterEnd, FlowMeterSpeed,MeshSize, FieldComments,
           SampleVolume, LabComments, SpeciesCode, ScientificName, CommonName, TL,
           FL, LifeStage, LarvalLifeStage, TotalCountSpecies, CountIndividual) %>%
  summarise(Count = sum(CountIndividual))

# #find way to summarize total counts per fork length of each species on each sampling date 
# IchExcelOverlapTT <- bind_rows(IchOverlapCatchA, IchExcelCatchA) %>%
#   mutate(FL = as.numeric(FL)) %>%
#   mutate(CountIndividual = if_else(TotalCountSpecies >= 1, 1, NA)) 
# 
# IchExelOverlapTTT <- IchExcelOverlapTT%>%
#   arrange(ymd(IchExcelOverlapTT$Date)) %>%
#   group_by(event_id, PhysicalDataID, Datetime, Date, Time, Station, YSI, WeatherCode,Tide, MicrocystisVisualRank,
#            WaterTemperature, Secchi, Conductivity, SpCnd, pH, DO, Turbidity, FlowDirection, VegetationRank, ConditionCode, SamplingAltered,
#            FieldComments_WQ, Flag_PQC, Comment_PQC, DataCorrectionComments, Year, Month, MonthAbb,
#            MeterSetTime, FlowMeterStart, FlowMeterEnd, FlowMeterSpeed,MeshSize, FieldComments,
#            SampleVolume, LabComments, SpeciesCode, ScientificName, CommonName, TL,
#            FL, LifeStage, LarvalLifeStage, TotalCountSpecies, CountIndividual) %>%
#   summarise(Count = sum(CountIndividual))

# ### the code below works, but doesn't include all columns
# IchExelOverlapTTT <- IchExcelOverlapTT%>%
#   arrange(ymd(IchExcelOverlapTT$Date)) %>%
#   group_by(event_id, Datetime, Date, Time, Station, CountIndividual) %>%
#   summarise(Count = sum(CountIndividual))
# 
# IchExelOverlapTTT <- IchExcelOverlapTT%>%
#   arrange(ymd(IchExcelOverlapTT$Date)) %>%
#   group_by(event_id, Datetime, Date, Time, Station, SpeciesCode, ScientificName, CommonName, FL, CountIndividual) %>%
#   summarise(Count = sum(CountIndividual))

#referencenotes
# Df = df %>%
# group_by(sampleID, ..... fork length)
# %>%
# summarize(Count = sum(Count))
# summarize(Count = n())
# group_by(SampleID:speceis)

# IchExcelOverlapA <- left_join(IchExcelOverlap, PhysExcel)

IchFullData <- bind_rows(IchAccessOverlapA, IchExcelOverlap)

#export bound file
write_csv(IchFullData, "test files/IchFullData.csv")



#find method to add a column for individual measurement counts instead of total counts
# first verify that "total count species" column is only 4/22/2019 onward

# viewtotal <- IchFullData %>%
#   filter(!is.na(TotalCountSpecies))
# 
# viewNAtotal <- IchFullData%>%
#   filter(is.na(TotalCountSpecies))
# 
# view2019 <- IchFullData %>%
#   filter(Date > "2019-04-15")
# 
# test2019 <- IchFullData %>%
  # mutate(IndividualCount = if_else(TotalCountSpecies >= 1, 1, NA))
#this worked - is there a way to combine this with the already existing count column so it isn't excessive? besides manually?
#maybe do this before binding rows to create full data set and call the column "Count" so it binds nicely?

IchFullData <- IchFullData%>%
  relocate(event_id, Datetime, Date, Time, PhysicalDataID, Station, YSI, WeatherCode, Tide, FlowDirection,
           ConditionCode, VegetationRank, MicrocystisVisualRank, WaterTemperature, Secchi, Conductivity, SpCnd, 
           pH, DO, Turbidity, MeterSetTime, FlowMeterSpeed, FlowMeterStart, FlowMeterEnd, TowLocation, SampleVolume)

sampUnique <- IchFullData %>%
  unique() %>%
  arrange(Datetime)

samp_catch_physMerge <- IchFullData %>%
  mutate(WY = ifelse(Month >9, Year + 1, Year)) %>%
  left_join(wy, by = "WY") %>%
  select(-c(Index, WYType)) %>%
  mutate(Flowdiff = abs(FlowMeterEnd-FlowMeterStart)) %>%
  select(-c(LarvalDataID, LarvalCatchID, CountIndividual, TotalCountSpecies)) %>%
  arrange(Datetime)


################################################################################################

SamplingQAQC <- filter(sampUnique, !is.na(FieldComments) | ConditionCode>1 | !is.na(LabComments))
SamplingQAQC$Flag_SAMP <-  ""
SamplingQAQC$Comment_SAMP <-""
SamplingQAQC$Flag_LAB <- ""
SamplingQAQC$Comment_LAB <- ""
today <- today()
write.csv(SamplingQAQC, paste("R_write/IchSamplingQAQC_", today, ".csv"))


SamplingQAQC_fill <- read.csv("R_write/IchSamplingQAQC_Notated_2024-06-20.csv")
SamplingQAQC_fill_s <- SamplingQAQC_fill %>%
  select(c(event_id, PhysicalDataID, Flag_SAMP, Comment_SAMP, Flag_LAB, Comment_LAB))

###############################################################################################
inundation <- inundation %>%
  rename(Date = Dates)
inundation$Date<-as.Date(inundation$Date,"%m/%d/%Y")

inundation2 <- inundation %>%
  mutate(Month = month(Date),
         Year = year(Date),
         WY = ifelse(Month > 9, Year + 1, Year)) %>%
  select(-c(Month, Year))

inundation2a <- inundation %>%
  mutate(Month = month(Date),
         Year = year(Date),
         WY = ifelse(Month > 9, Year + 1, Year))

# Modify sample table to include Flowdiff
samp3 <-sampUnique %>%
  mutate(Flowdiff = abs(FlowMeterEnd-FlowMeterStart),
         Flowdiff_s = Flowdiff/MeterSetTime) 

# merged inundation and sampling
inundation_flow <- left_join(samp3, inundation2, by = "Date") %>%
  filter(Flowdiff<100000) %>%
  filter(Station == "STTD") %>%
  mutate(Flowdiff_s = Flowdiff/MeterSetTime,
         Inundation_n = ifelse(Inundation == "TRUE", 40000, 0))

# all inundation
inundation3 <- inundation %>%
  mutate(Inundation_n = ifelse(Inundation == "TRUE", 40000, 0),
         Inundation_n_low = ifelse(Inundation == "TRUE", 5000,0))

# Plot - only inundation events that correspond with sampling date
inplot1 <- ggplot(inundation_flow) + 
  geom_point(aes(x = Date, y = Flowdiff)) + 
  geom_col(aes(x = Date, y = Inundation_n), fill = "blue", linewidth = 2) + 
  labs(title = "Flowdiff with Inundation events corresponding with sample dates") +
  facet_grid(Station~.) +
  theme_bw()

ggplotly(inplot1)

# Plot - All inundation events
inplot2 <- ggplot() +
  geom_point(data = inundation_flow, aes(x = Date, y = Flowdiff)) +
  labs(title = "Flowdiff with all Inundation") +
  geom_col(data = inundation3, aes(x = Date, y = Inundation_n), fill = 
             "blue", alpha = 0.6) + theme_bw()
ggplotly(inplot2)

inplot3 <- ggplot() +
  geom_point(data = inundation_flow, aes(x = Date, y = Flowdiff_s)) +
  labs(title = "Standardized Flowdiff with all Inundation") +
  geom_col(data = inundation3, aes(x = Date, y = Inundation_n_low), fill = 
             "blue", alpha = 0.6) + theme_bw()

grid.arrange(inplot1, inplot2, inplot3, nrow = 3)


# Allow 10 days after last "inundation=TRUE" to also count as inundated. Merge with sample data. 
inundation4 <- inundation2a %>%
  mutate(Inundation2 = ifelse(lag(Inundation, 10) == "TRUE", "TRUE", Inundation)) %>%
  select(c(Date, Month:Inundation2))

inundation4$Inundation2[inundation4$Date=="1998-01-01"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-02"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-03"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-04"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-05"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-06"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-07"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-08"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-09"] <- "FALSE"
inundation4$Inundation2[inundation4$Date=="1998-01-10"] <- "FALSE"

samp_catch_phys <- left_join(samp_catch_physMerge, inundation4)

#14. Look at distribution of flowmeter values

# Histogram of values
samp_catch_phys$Month <- ordered(samp_catch_phys$Month)
FlowHist <- ggplot(samp_catch_phys, aes(Flowdiff)) + geom_histogram() +
  facet_wrap(~Station) + theme_bw()
ggplotly(FlowHist)

# SHR vs STTD boxplot
FlowBox <- ggplot(samp_catch_phys) + geom_boxplot(aes(x = Station, y = Flowdiff)) + theme_bw()

# SHR vs STTD boxplot by month
FlowBoxMonth <- ggplot(samp_catch_phys) + geom_boxplot(aes(x = Month, y = Flowdiff, fill = Month)) + facet_wrap(~Station) + scale_fill_viridis(discrete = TRUE) + theme_bw() 

# SHR vs STTD boxplot by year
FlowBoxYear <- ggplot(samp_catch_phys) + geom_boxplot(aes(x = ordered(WY), y = Flowdiff, fill = ordered(WY))) + facet_wrap(~Station) + theme_bw() 

# SetTime vs Flowdiff by station and month
FlowPoint <- ggplot(samp_catch_phys, aes(x = MeterSetTime, y = Flowdiff, color = Month)) + geom_point(size = 3) + facet_wrap(~Station) + theme_bw()

# SetTime vs Flowdiff by station and flowmeter type
FlowPoint2 <- ggplot(samp_catch_phys, aes(x = MeterSetTime, y = Flowdiff, color = FlowMeterSpeed)) + geom_point(size = 3) + facet_wrap(~Station) + theme_bw() + scale_color_viridis(discrete = TRUE) 


grid.arrange(FlowBox, FlowHist)
grid.arrange(FlowBoxMonth, FlowBoxYear)
grid.arrange(FlowPoint, FlowPoint2)


checkich <- samp_catch_phys %>% filter(Flowdiff > 40000)
checkich2 <- samp_catch_phys %>% filter(Flowdiff <1000)
#checking for later outlier section
checkich3 <- samp_catch_phys %>% filter(Flowdiff <200)

#fix known flowmeter errors
#flowmeter when it reads 0, use as if it reads 1000000

samp_catch_phys$Flowdiff[samp_catch_phys$event_id == "STTD_2015-04-30 08:16:00"] <- 1000000-996009
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id == "SHR_2016-03-17 08:25:00"] <- 954818
samp_catch_phys$Flowdiff[samp_catch_phys$event_id == "SHR_2016-03-17 08:25:00"] <- 954818-941900

#comment on data sheet and confirmation with NI, 7/25/2012 STTD flowmeter value was recorded incorrectly, should match flowmeter for zoop
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id == "STTD_2012-07-25 10:11:00"] <- 899989
samp_catch_phys$Flowdiff[samp_catch_phys$event_id == "STTD_2012-07-25 10:11:00"] <- 900000-899989


# 1/24/2012 SHR appears to be correct numbers based on datasheet but flow diff very high
# 1/23/2004 SHR appears to be correct numbers based on datasheet but flow diff very high
# 2/16/1999 SHR appears to be correct numbers based on datasheet but flow diff very high


#change the same flowmeter values in samp3

samp3$Flowdiff[samp3$event_id == "STTD_2015-04-30 08:16:00"] <- 1000000-996009
samp3$FlowMeterEnd[samp3$event_id == "SHR_2016-03-17 08:25:00"] <- 954818
samp3$Flowdiff[samp3$event_id == "SHR_2016-03-17 08:25:00"] <- 954818-941900

#comment on data sheet and confirmation with NI, 7/25/2012 STTD flowmeter value was recorded incorrectly, should match flowmeter for zoop
samp3$FlowMeterEnd[samp3$event_id == "STTD_2012-07-25 10:11:00"] <- 899989
samp3$Flowdiff[samp3$event_id == "STTD_2012-07-25 10:11:00"] <- 900000-899989

#remaining outlier values will be flagged in later qa/qc of flowmeter values

#16. Merge data with lab/sampling QAQC 
#* Replace NA with blank
#* 
# FM_Samp <- left_join(samp_catch_phys, SamplingQAQC_fill_s, by = "event_id")

FM_Samp <- left_join(samp_catch_phys, SamplingQAQC_fill_s, by = "event_id") %>%
  mutate(Flag_SAMP = replace(Flag_SAMP, is.na(Flag_SAMP), "" ),
         Comment_SAMP = replace(Comment_SAMP, is.na(Comment_SAMP), ""),
         Flag_LAB = replace(Flag_LAB, is.na(Flag_LAB), ""),
         Comment_LAB = replace(Comment_LAB, is.na(Comment_LAB), "")) %>%
  select(-c(PhysicalDataID.y))%>%
  rename(PhysicalDataID = "PhysicalDataID.x")

#does the na for meterset time need to stay in? maybe so that it runs the fucntion
Flow.sum.STTD <- samp3 %>%
  left_join(inundation4) %>%
  left_join(wy)%>%
  filter(!is.na(Flowdiff), !is.na(Inundation2),!is.na(MeterSetTime), Station=="STTD") %>%
  mutate(Flow_s = Flowdiff/MeterSetTime) %>%
  group_by(Station, FlowMeterSpeed, WYClass, Inundation2) %>%
  summarize(n= n(),
            min.Flowdiff = min(Flowdiff),
            max.Flowdiff = max(Flowdiff),
            median.Flowdiff = median(Flowdiff),
            min.Flowdiff_s = min(Flow_s),
            max.Flowdiff_s = max(Flow_s),
            median.Flowdiff_s = median(Flow_s),
            Flow_Q1 = quantile(Flowdiff, probs = 0.25),
            Flow_Q3 = quantile(Flowdiff, probs = 0.75),
            Flow_UL = Flow_Q3 + 1.5 * (Flow_Q3-Flow_Q1),
            Flow_s_Q1 = quantile(Flow_s, probs = 0.25), 
            Flow_s_Q3 = quantile(Flow_s, probs = 0.75),
            Flow_s_UL = Flow_s_Q3 + 1.5 * (Flow_s_Q3-Flow_s_Q1),
            Flow_s_MAD = mad(Flow_s))

str(Flow.sum.STTD)

# Flow.sum.STTD <- samp3 %>%
#   left_join(inundation4) %>%
#   left_join(wy)%>%
#   filter(!is.na(Flowdiff), !is.na(Inundation2), Station=="STTD") %>%
#   mutate(Flow_s = Flowdiff/MeterSetTime)
# 
# isna <- Flow.sum.STTD %>%
#   filter(is.na(Flow_s))
# 
# naset <- Flow.sum.STTD %>%
#   filter(is.na(MeterSetTime))

Flow.sum.STTD %>%
  kbl() %>%
  kable_styling()


#also keep the is.na meter set time here?
Flow.sum.SHR <- samp3 %>%
  left_join(inundation4) %>%
  left_join(wy)%>%
  mutate(Flow_s = Flowdiff/MeterSetTime) %>%
  group_by(Station, FlowMeterSpeed, WYClass) %>%
  filter(!is.na(Flowdiff), !is.na(MeterSetTime), Station == "SHR") %>%
  summarize(n= n(),
            min.Flowdiff = min(Flowdiff),
            max.Flowdiff = max(Flowdiff),
            median.Flowdiff = median(Flowdiff),
            min.Flowdiff_s = min(Flow_s),
            max.Flowdiff_s = max(Flow_s),
            median.Flowdiff_s = median(Flow_s),
            Flow_Q1 = quantile(Flowdiff, probs = 0.25),
            Flow_Q3 = quantile(Flowdiff, probs = 0.75),
            Flow_UL = Flow_Q3 + 1.5 * (Flow_Q3-Flow_Q1),
            Flow_s_Q1 = quantile(Flow_s, probs = 0.25), 
            Flow_s_Q3 = quantile(Flow_s, probs = 0.75),
            Flow_s_UL = Flow_s_Q3 + 1.5 * (Flow_s_Q3-Flow_s_Q1),
            Flow_s_MAD = mad(Flow_s))

Flow.sum.SHR %>%
  kbl() %>%
  kable_styling()


#outliers for flow

Flow.outlier.STTD <- left_join(FM_Samp, Flow.sum.STTD) %>%
  mutate(Flow_s = Flowdiff/MeterSetTime) %>%
  filter(!is.na(Flowdiff), !is.na(Inundation2), Station=="STTD") %>%
  group_by(Station, FlowMeterSpeed, WYClass) %>%
  mutate(Flow_modZ = abs(0.6745*(Flow_s - median.Flowdiff_s)/Flow_s_MAD),
         Flow_Outlier = ifelse((Flow_s > Flow_UL) & Flow_modZ > 3.5, "Both", ifelse(Flow_s > Flow_UL, "Tukey", ifelse(Flow_modZ > 3.5, "MAD", 
                                                                                                                      "None"))))
str(Flow.outlier.STTD)

Flow.outlier.STTD$FlowMeterSpeed <- str_replace_all(Flow.outlier.STTD$FlowMeterSpeed, "low", "Low")

summary(factor(Flow.outlier.STTD$Flow_Outlier))

Flow.outlier.SHR <- left_join(FM_Samp, Flow.sum.SHR) %>%
  mutate(Flow_s = Flowdiff/MeterSetTime) %>%
  filter(!is.na(Flowdiff),  Station=="SHR") %>%
  group_by(Station, FlowMeterSpeed, WYClass) %>%
  mutate(Flow_modZ = abs(0.6745*(Flow_s - median.Flowdiff_s)/Flow_s_MAD),
         Flow_Outlier = ifelse((Flow_s > Flow_UL) & Flow_modZ > 3.5, "Both", ifelse(Flow_s > Flow_UL, "Tukey", ifelse(Flow_modZ > 3.5, "MAD", 
                                                                                                                      "None"))))
str(Flow.outlier.SHR)

summary(factor(Flow.outlier.SHR$Flow_Outlier))

#first do for STTD
FlowBoxMonth1a <- ggplot(Flow.outlier.STTD) + geom_boxplot(aes(x = WYClass, y = Flow_s)) +  scale_fill_viridis(discrete = TRUE) + theme_bw() 
ggplotly(FlowBoxMonth1a)


outlierWY1a <- ggplot(Flow.outlier.STTD, aes(x = WYClass, y = Flow_s, color = Flow_Outlier)) + geom_point(size = 2.5, alpha = 0.5) +
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))


outlierMonth1a <- ggplot(Flow.outlier.STTD, aes(x = Month, y = Flow_s, color = Flow_Outlier)) + geom_point(size = 2.5, alpha = 0.5) + 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

grid.arrange(FlowBoxMonth1a, outlierWY1a, outlierMonth1a)

#do the same for SHR
FlowBoxMonth1b <- ggplot(Flow.outlier.SHR) + geom_boxplot(aes(x = WYClass, y = Flow_s)) +  scale_fill_viridis(discrete = TRUE) + theme_bw() 
ggplotly(FlowBoxMonth1b)


outlierWY1b <- ggplot(Flow.outlier.SHR, aes(x = WYClass, y = Flow_s, color = Flow_Outlier)) + geom_point(size = 2.5, alpha = 0.5) +   
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))


outlierMonth1b <- ggplot(Flow.outlier.SHR, aes(x = Month, y = Flow_s, color = Flow_Outlier)) + geom_point(size = 2.5, alpha = 0.5) +   
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

grid.arrange(FlowBoxMonth1b, outlierWY1b, outlierMonth1b)

#then combine
Flow.outlier <- rbind(Flow.outlier.SHR, Flow.outlier.STTD)



summary(factor(Flow.outlier$Flow_Outlier))

# Flowmeter Flags and New Flowdiff calculated
# 
# Cutoffs based on looking at plots and Upper limits calculated above. 
# 
# Flag   | Description                | Outlier                   | Flowdiff
# Flag 3 | Highly Suspect             | Both                      | < 200 or >= 70000 or >= 50000 if inundation = FALSE
# Flag 2 | Suspect                    | Tukey or MAD but not Both | 200-999 
# Flag 1 | Acceptable                 |                           | 
#   No flag| Acceptable                 | None                      | 1000-59999
# 
# Add comment about what was flagged (FM for flowmeter-related)
# 22. Calculate replacement values where Flag_FM = 3 or Comment_SAMP = FM
# * Replacement values are median standardized flowdiff * settime based on groupings of Station, Flowmeter Speed, Water Year Class


#column type error....figure this out
FlowmeterQAQC <- Flow.outlier %>%
  filter(!is.na(Flowdiff), Flow_modZ>=0.00000001) %>%
  mutate(Flag_FM = ifelse(Flow_Outlier=="Both" | Flowdiff < 200 | (Flowdiff >= 50000 & Inundation2 ==FALSE) | Flowdiff >= 70000 , 3, 
                          ifelse(Flow_Outlier %in% c("Tukey", "MAD") | Flowdiff < 1000, 2,
                                 "")),
         Comment_FM = ifelse(Flag_FM %in% c(2,3), "FM", ""),
         FlowdiffAdj = ifelse(Flag_FM == 3 | Comment_SAMP == "FM", median.Flowdiff_s * MeterSetTime, Flowdiff))


# compare_df_cols_same(Flow.outlier.SHR, Flow.outlier.STTD, bind_method = "rbind", verbose= TRUE)


# str(Flow.outlier.SHR)
# str(Flow.outlier.STTD)
# 
# checkflow <- Flow.outlier %>% filter(Flow_outlier == "Both")
# #error for station SHR, flowmeter speed LOW, wy class A
# 
# checkflow2 <- Flow.outlier %>% filter(Flowdiff >= 50000 & Inundation2 == FALSE)
# checkflow3 <- Flow.outlier %>% filter(Flowdiff >= 70000, 3, ifelse(Flow_outlier %in% c("Tukey", "Mad")))
# checkflow4 <- Flow.outlier %>% filter(Flowdiff < 1000, 2, "")

FlowmeterQAQC$Flag_FM <- ordered(FlowmeterQAQC$Flag_FM)
print("Flowmeter Outliers")
summary(factor(FlowmeterQAQC$Flag_FM))
print("Sampling Outliers")
summary(factor(FlowmeterQAQC$Flag_SAMP))
print("Lab Outliers")
summary(factor(FlowmeterQAQC$Flag_LAB))


outlierPreFlow <- ggplot(FlowmeterQAQC, aes(x = factor(Month), y = Flowdiff,  text = paste('Date: ', Date,'<br>Inundation: ', Inundation2, '<br>Set time: ', MeterSetTime), color = factor(Flag_FM))) + geom_point(size = 2.5, alpha = 0.5) +  scale_color_viridis(discrete = TRUE) + labs(title = "Flowmeter Pre-QC") +
  facet_wrap(~Station) + 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

ggplotly(outlierPreFlow)

outlierPostFlow <- ggplot(FlowmeterQAQC, aes(x = factor(Month), y = FlowdiffAdj, color = factor(Flag_FM))) + geom_point(size = 2.5, alpha = 0.5) + scale_color_viridis(discrete = TRUE)  + labs(title = "Flowmeter Post-QC") +
  facet_wrap(~Station) +   
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

ggplotly(outlierPostFlow)

grid.arrange(outlierPreFlow, outlierPostFlow, nrow = 2)


outlierPreSamp <- ggplot(Flow.outlier, aes(x = factor(Month), y = Flowdiff, color = factor(Flag_SAMP))) + geom_point(size = 3, alpha = 0.8) + scale_color_viridis(discrete = TRUE) + labs(title = "Sampling Pre-QC") +
  facet_wrap(~Station) + 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

ggplotly(outlierPreSamp)

outlierPostSamp <- ggplot(FlowmeterQAQC, aes(x = factor(Month), y = FlowdiffAdj, color = factor(Flag_SAMP))) + geom_point(size = 3, alpha = 0.8) + scale_color_viridis(discrete = TRUE) + labs(title = "Sampling Post-QC") +
  facet_wrap(~Station) + 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

ggplotly(outlierPostSamp)

grid.arrange(outlierPreSamp, outlierPostSamp, nrow = 2)


NetArea = 0.4572 * 0.25
FlowQAQC_CPUE <- FlowmeterQAQC %>%
  mutate(Volume = ifelse(FlowMeterSpeed == "Regular",
                         Flowdiff * 26873 * NetArea/999999,
                         ifelse(FlowMeterSpeed == "Low",
                                Flowdiff * 57560 * NetArea/999999, NA))) %>%
  filter(!is.na(Flowdiff)) %>%
  mutate(CPUE= round(Count/Volume,3))%>%
  
  mutate(VolumeAdj = ifelse(FlowMeterSpeed == "Regular",
                            FlowdiffAdj * 26873 * NetArea/999999,
                            ifelse(FlowMeterSpeed == "Low",
                                   FlowdiffAdj * 57560 * NetArea/999999, NA))) %>%
  mutate(CPUEAdj = round(Count/VolumeAdj,3))%>%
  arrange(Datetime)


FlowQAQC_CPUE$Month <- factor(FlowQAQC_CPUE$Month)
FlowQAQC_low <- filter(FlowQAQC_CPUE, CPUE < 10)
FlowQAQC_high <- filter(FlowQAQC_CPUE, CPUE > 10)

# Histogram of values
CPUEHist <- ggplot(FlowQAQC_CPUE, aes(CPUEAdj)) + geom_histogram(binwidth = 1) +
  facet_wrap(~Station) + theme_bw()
ggplotly(CPUEHist)

# SHR vs STTD boxplot
CPUEBox <- ggplot(FlowQAQC_CPUE) + geom_boxplot(aes(x = Station, y = CPUEAdj)) + theme_bw()
ggplotly(CPUEBox)

# SHR vs STTD boxplot by month
CPUEBoxMonth <- ggplot(FlowQAQC_CPUE) + geom_boxplot(aes(x = Month, y = CPUEAdj, color = Month)) + facet_wrap(~Station) + scale_color_viridis(discrete = TRUE) + theme_bw() 
ggplotly(CPUEBoxMonth)


grid.arrange(CPUEHist, CPUEBox)

# SHR vs STTD boxplot by year
CPUEBoxYear <- ggplot(FlowQAQC_CPUE) + geom_boxplot(aes(x = factor(WY), y = CPUEAdj)) + facet_grid(Station~.) + theme_bw() 
ggplotly(CPUEBoxYear)

grid.arrange(CPUEBoxMonth, CPUEBoxYear)


# Species
ich_summary <-FlowQAQC_CPUE %>%
  filter(WY>1998) %>%
  group_by(Station, WYClass, ScientificName) %>%
  summarize(median.CPUE = median(CPUEAdj),
            max.CPUE = max(CPUEAdj),
            max.Count = max(Count),
            median.Count = median(Count),
            total.CPUE = sum(CPUEAdj),
            n = n()) %>%
  filter(n > 2)

# All CPUE
speciesCPUE <- ggplot(FlowQAQC_CPUE, aes(ScientificName, CPUEAdj)) + 
  geom_jitter(aes(col = WYClass)) + facet_grid(Station~.) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15)) 

ggplotly(speciesCPUE)

# Median CPUE
species2 <- ggplot(ich_summary, aes(ScientificName, median.CPUE)) + geom_jitter(aes(col = WYClass)) + facet_grid(Station~.) + 
  scale_color_viridis(discrete=TRUE) + 
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))
ggplotly(species2)

# MaxCPUE
species3 <- ggplot(ich_summary, aes(ScientificName, max.CPUE)) + geom_jitter(aes(col = WYClass)) + facet_grid(Station~.) + 
  scale_color_viridis(discrete=TRUE) + 
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))
ggplotly(species3)

# Median Count
medCount <- ggplot(ich_summary, aes(ScientificName, median.Count)) + geom_jitter(aes(col = WYClass)) + facet_grid(Station~.) + 
  scale_color_viridis(discrete=TRUE) + 
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))
ggplotly(medCount)

# Max Count
maxCount <- ggplot(ich_summary, aes(ScientificName, max.Count)) + geom_jitter(aes(col = WYClass)) + facet_grid(Station~.) + 
  scale_color_viridis(discrete=TRUE) + 
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))


grid.arrange(species2, species3)
grid.arrange(medCount, maxCount)

# Which are the outliers?
ggplotly(maxCount)

## Check
checkCounts <- filter(FlowQAQC_CPUE, Count>1000)
# Match up with datasheet 
#tfs is high - could be real?

checkCPUE <- filter(FlowQAQC_CPUE, CPUEAdj > 100)
checkCPUE2 <- filter(FlowQAQC_CPUE, is.na(CPUEAdj))



### Table of values
CPUE.adj.table <- FlowQAQC_CPUE %>%
  group_by(Station, FlowMeterSpeed, WYClass, ScientificName) %>%
  summarize(min.CPUE = min(CPUEAdj),
            median.CPUE = median(CPUEAdj),
            max.CPUE = max(CPUEAdj),
            n = n())

CPUE.adj.table %>%
  kbl() %>%
  kable_styling()


CPUEQAQC <- FlowQAQC_CPUE %>%
  mutate(Flag_CPUE = ifelse(CPUEAdj>70, 2, "" ),
         Comment_CPUE = ifelse(CPUEAdj>70, "CPUE", ""))

outlier2a <- ggplot(CPUEQAQC, aes(x = Month, y = CPUE, color = Flag_FM)) + geom_point(size = 2.5, alpha = 0.5) + scale_color_viridis(discrete = "TRUE") + facet_wrap(~Station) + labs(title = "CPUE calculated with original Flowmeter values") + 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

ggplotly(outlier2a)

outlier2b <- ggplot(CPUEQAQC, aes(x = Month, y = CPUEAdj, color = Flag_CPUE)) + geom_point(size = 2.5, alpha = 0.5) + scale_color_viridis(discrete = "TRUE") + facet_wrap(~Station) +   labs(title = "CPUE calculated with adjusted Flowmeter values + CPUE flags") + 
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size =14),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14), 
                     strip.text = element_text(size = 15))

ggplotly(outlier2b)

grid.arrange(outlier2a, outlier2b)



ich_select <- CPUEQAQC %>%
  dplyr::rename(Inundation = Inundation2,
                FlagPhys = Flag_PQC,
                FlagSamp = Flag_SAMP,
                FlagLab = Flag_LAB,
                FlagFM = Flag_FM, 
                FlagCPUE = Flag_CPUE,
                CommentPhys = Comment_PQC,
                CommentSamp = Comment_SAMP,
                CommentLab = Comment_LAB,
                CommentFM = Comment_FM,
                CommentCPUE = Comment_CPUE) %>%
  select(c(event_id, Datetime, Station,
           WY, WYClass, Inundation,
           WeatherCode, Tide, MicrocystisVisualRank,
           WaterTemperature:Turbidity,
           ConditionCode,FieldComments, 
           MeterSetTime, FlowMeterSpeed, FlowMeterStart, FlowMeterEnd, Flowdiff, FlowdiffAdj, Volume, VolumeAdj,LabComments, ScientificName, LifeStage, Count, CPUE, CPUEAdj, FlagPhys, CommentPhys, FlagSamp, CommentSamp, FlagLab, CommentLab, FlagFM, CommentFM, FlagCPUE, CommentCPUE))



empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

ich_final <- ich_select %>%
  mutate(LifeStage = replace(LifeStage, LifeStage == "N/A", NA)) %>%
  mutate(across(c(FlagSamp:CommentCPUE), list(empty_as_na)) ) %>%
  select(-c(FlagSamp:CommentCPUE)) %>%
  rename(FlagSamp = FlagSamp_1, 
         FlagLab = FlagLab_1,
         FlagFM = FlagFM_1,
         FlagCPUE = FlagCPUE_1,
         CommentSamp = CommentSamp_1, 
         CommentLab = CommentLab_1,
         CommentFM = CommentFM_1,
         CommentCPUE = CommentCPUE_1) %>%
  mutate(QCFlags = paste(FlagSamp, " ", FlagLab, " ", FlagFM, " ", FlagCPUE)) %>%
  mutate(QCComments = paste(CommentSamp, " ", CommentLab, " ",CommentFM, " ",  CommentCPUE)) %>%
  relocate(FlowMeterSpeed, .before = "FlowMeterStart")

##also create fish crosswalk like in the main fish dataset? use iep fish codes? 

## Filter rows with a 7
threes <- inv_final%>%
  filter(grepl("3", QCFlags ))

# calculate proportion flagged
## Amount replaced
print(paste0(round(nrow(threes)/nrow(inv_final)*100,3), "% highly suspicious"))



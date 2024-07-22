library(tidyverse)
library(gridExtra) # combine plots
library(stringr)
library(readr)
library(lubridate)
library(plotly)
library(viridis)
library(kableExtra)
library(tidylog)

# Load data - several tables
phys <- read_csv("drift data/LT_phys_qc_20240118.csv")
catch <- read_csv("drift data/DriftCatchDataAccess_20240119.csv")
catch2 <- read_csv("drift data/DriftLabExcelData_20240430.csv", skip=1)
samp <- read_csv("drift data/DriftInvertSampAccess_20240119.csv")
samp2 <- read_csv("drift data/DriftSampExcelData.csv", skip=1)
tax <- read_csv("drift data/DriftTaxonomy.csv")
wy <- read_csv("WaterYearType_CDEC.csv") 
inundation <- read_csv("Yolo_Bypass_Inundation_1998-2022.csv")


#rename columns and variables for consistency across dataframes, simplify for later work
#remove unnecessary columns

samp <- samp %>% 
  rename(FlowMeterStart = `DriftStartMeter`,
         FlowMeterEnd = `DriftEndMeter`) %>%
  select(-c(`EnteredBy`, `QA/QC'dBy`, StartTime, StopTime))
str(samp)

catch2 <- catch2 %>%
  filter(!(is.na(`Measuring program short name`))) %>%
  rename(Count = `Value...11`,
         LifeStage = `Value...12`,
         TaxonName = Observable,
         Date = `Sampling Event Date`,
         Time = `Sampling Event Time`,
         Station = `Sampling Area Number`,
         SAMCode = `Sampling Event Number`,
         SampleID = `Sample ID`,
         LabComments = lab_comments,
         Classification = Category) %>%
  select(-c(`Measuring program short name`, `Observation Type Short Name`,
            SAMCode))

samp2 <- samp2 %>%
  filter(!(is.na(`Measuring program short name`))) %>%
  rename(FlowMeterStart = `Flow Meter Start`,
         FlowMeterEnd = `Flow Meter End`,
         Date = `Sampling Event Date`,
         Time = `Sampling Event Time`,
         Station = `Sampling Area Number`,
         SAMCode = `Sampling Event Number`) %>%
  select(-c(`Observation Area Number`, `Spot Code (original/duplicate)`, `...27`, `...28`,
            `...29`, `...30`, `...31`, `...32`,
            `Spot Number`, `Flow Meter Start (50)`, `Flow Meter End (50)`,
            `Entered by`, `QAQC'd by`, `...6`, `Measuring program short name`,
            SAMCode))


#add and change date formats

catch2$Date<-as.Date(catch2$Date,"%m/%d/%Y")
catch2$Year <- year(catch2$Date)
catch2$Month <- month(catch2$Date)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
catch2$MonthAbb <- mymonths[catch2$Month ]
catch2$Datetime = paste(catch2$Date, catch2$Time)
catch2$Datetime <- ymd_hm(catch2$Datetime)
catch2$Time <- strptime(catch2$Time, format = "%H:%M", tz = "") %>%
  strftime(catch2$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)
catch2$Time <- hms::as_hms(catch2$Time)



# #correct the time for catch2 on dates:8/31/21 STTD 11:06, 8/31/21 SHR 8:06; 9/28/21 SHR 8:10
# just correct in csv and note instead?


samp2$Date<-as.Date(samp2$Date,"%m/%d/%Y")
samp2$Year <- year(samp2$Date)
samp2$Month <- month(samp2$Date)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
samp2$MonthAbb <- mymonths[samp2$Month ]
samp2$Datetime = paste(samp2$Date, samp2$Time)
samp2$Datetime <- ymd_hm(samp2$Datetime)
samp2$Time <- strptime(samp2$Time, format = "%H:%M", tz = "") %>%
  strftime(samp2$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)
samp2$Time <- hms::as_hms(samp2$Time)

inundation <- inundation %>%
  rename(Date = Dates)
inundation$Date<-as.Date(inundation$Date,"%m/%d/%Y")
str(phys)
str(catch)
str(inundation)
str(catch2)
str(samp2)

#creating eventID for later merging of data from excel and access

#start with data that is from excel - this does have some physical data included - watch for combining with phys data
catch2 <- catch2 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

samp2 <- samp2 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

samp_catch2 <- left_join(samp2, catch2) %>%
  filter(Date < "2023-01-01")
str(samp_catch2)

samp_catch2$Date <- as.Date(samp_catch2$Date)
samp_catch2 <- samp_catch2 %>%
  mutate(FlowMeterEnd = as.numeric(FlowMeterEnd))
str(samp_catch2)

comments1 <- samp_catch2 %>%
  filter(!is.na(`Field Comments`))

#select only up to 2022 for publishing

#25 lines in samp_catch2 have no taxon name - 2021 and 2022 data.

# Merge physical data


samp_catch_phys2 <- left_join(samp_catch2, phys, by = c("event_id","Datetime", "Station", "Date", "Time",
                                                        "Year", "Month", "MonthAbb")) %>%
  select(-c("FlowMeterStart.y", "FlowMeterEnd.y", "MeterSetTime", "FlowMeterSpeed", 
            "Observation Area Name", "Physical Data ID", "Sampling Altered", "ConditionCode")) %>%
  rename(FlowMeterStart = "FlowMeterStart.x",
         FlowMeterEnd = "FlowMeterEnd.x",
         FlowMeterSpeed = "Flow Meter Speed",
         SetTime = "Set Time",
         Field_Comments = "Field Comments",
         SampleVolume = "Sample Volume",
         SubsampleNumber = "Subsample Number",
         SlideCount = "Slide Count",
         ConditionCode = "Condition Code") %>%
  select(-c(Field_Comments))


#number of rows expanded a lot due to duplicates - find a way to manage this


catchtax <- left_join(catch,tax) %>%
  select(-c(Kingdom, Phylum, Subphylum, Class, Subclass,
            Infraclass, Superorder, Suborder, Infraorder, Superfamily,
            Genus, Species, CommonName, TaxonRank))



# Merge datasets for CPUE variables
samp_catch <- left_join(samp, catchtax)

#rename and remove columns from join

# samp_catch_phys0 <- left_join(phys, samp_catch, by = "PhysicalDataID") %>%
#   filter(!is.na(Station)) %>%
#   filter(Date < "2020-02-10") 
# notjoinedPhysDataID <- anti_join(phys, samp_catch, by = "PhysicalDataID")


samp_catch_phys0 <- left_join(phys, samp_catch, by = "PhysicalDataID") %>%
  filter(!is.na(Station)) %>%
  filter(Date < "2019-04-22")



# For second part 2019, merge phys-samp, then add catch.
# For the additional data

catch2019 <- catch2 %>%
  filter(Date > "2019-04-10" & Date < "2020-02-01")


phys2019 <- phys %>%
  filter(Date > "2018-12-31" & Date < "2020-01-01") %>%
  select(-c(FieldComments))

phys_samp <- left_join(phys2019, samp, by = "PhysicalDataID")%>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x")) %>%
  rename(FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y",
         FlowMeterSpeed = "FlowMeterSpeed.y",
         ConditionCode = "ConditionCode.y")

gap <- left_join(phys_samp, catch2019)
#seems to take care of missing 2019 catch data...next, how to combine? 

checksampphys <- samp_catch_phys0 %>%
  filter(!is.na(FieldComments.y))



samp_catch_phys0 <- samp_catch_phys0 %>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x")) %>%
  rename(FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y",
         FlowMeterSpeed = "FlowMeterSpeed.y",
         ConditionCode = "ConditionCode.y",
         FieldComments = "FieldComments.y")


samp_catch_phys <- bind_rows(samp_catch_phys0, gap) %>%
  relocate(event_id, Datetime)






#check invert taxons and counts ensure none disappeared

nacount <- samp_catch_phys0 %>%
  filter(is.na(Count))



sampcatchphysMerge <- bind_rows(samp_catch_phys0, samp_catch_phys2) %>%
  relocate(event_id, Datetime)

nacombined <- sampcatchphysMerge %>%
  filter(is.na(Count))

#to merge excel with access - use bindrows instead of
sampcatchphysMerge <- rbind(samp_catch_phys0, phys_samp_catch0) 

#check various comments columns, find solution for number of columns


# All samplings - remove catch info and find unique entries
sampUnique <- sampcatchphysMerge %>%
  select(-c(WeatherCode:Turbidity, Year:Comment_PQC,  LabComments, TaxonName:LifeStage)) %>%
  unique() %>%
  arrange(Datetime)

sampUnique <- combined %>%
  unique() %>%
  arrange(Datetime)

naunique <- sampUnique %>%
  filter(is.na(Count))


samp_catch_physMerge <- combined %>%
  mutate(WY = ifelse(Month >9, Year + 1, Year)) %>%
  left_join(wy, by = "WY") %>%
  select(-c(Index, WYType)) %>%
  mutate(Flowdiff = FlowMeterEnd-FlowMeterStart)


sampA <- combined %>%
  select(-c(WeatherCode:Turbidity, Year:Comment_PQC,  TaxonName:LifeStage)) %>%
  unique() %>%
  arrange(Datetime)

lab <- sampA %>%
  filter(!is.na(lab_comments))
#only 15 lab comments for all data

SamplingQAQC <- filter(sampUnique, !is.na(FieldComments) | ConditionCode>1 | !is.na(lab_comments) )
SamplingQAQC$Flag_SAMP <-  ""
SamplingQAQC$Comment_SAMP <-""
SamplingQAQC$Flag_LAB <- ""
SamplingQAQC$Comment_LAB <- ""
today <- today()

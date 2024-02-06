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
catch2 <- read_csv("drift data/DriftLabExcelData.csv", skip=1)
samp <- read_csv("drift data/DriftInvertSampAccess_20240119.csv")
samp2 <- read_csv("drift data/DriftSampExcelData.csv", skip=1)
tax <- read_csv("drift data/DriftTaxonomy.csv")
wy <- read_csv("WaterYearType_CDEC.csv") 
inundation <- read_csv("Yolo_Bypass_Inundation_1998-2022.csv")


# Add and change date formats (does this even need to be done for phys??)
# phys$Date<-as.Date(phys$Date,"%m/%d/%Y")
# phys$Year <- year(phys$Date)
# phys$Month <- month(phys$Date)
# phys$MonthAbb <- mymonths[phys$Month ]
# phys$MonthAbb <-ordered(phys$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# phys$Datetime = paste(phys$Date, phys$Time)
# phys$Datetime <- as.POSIXct(phys$Datetime, 
#                             format = "%Y-%m-%d %H:%M:%S")

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
         SampleID = `Sample ID`) %>%
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
samp_catch2$Time <- strptime(samp_catch2$Time, format = "%H:%M", tz = "") %>%
  strftime(samp_catch2$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)
str(samp_catch2)
samp_catch2$Time <- hms::as_hms(samp_catch2$Time)
samp_catch2 <- samp_catch2 %>%
  mutate(FlowMeterEnd = as.numeric(FlowMeterEnd))
str(samp_catch2)

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

# comments3 <- samp_catch_phys2 %>%
#   filter(!is.na(Field_Comments))
# 
# Comments4 <- samp_catch_phys2 %>%
#   filter(!is.na(FieldComments))
# 
# duplicate <- left_join(comments3, Comments4) %>%
#   select("event_id", "Station", "Date", "Field_Comments", "FieldComments")
#this confirms these two comments columns contain exactly the same comments


# Flow <- samp_catch_phys2 %>%
#   select(c("event_id", "Datetime", "Station", "FlowMeterStart.x", "FlowMeterEnd.x",
#            "FlowMeterStart.y", "FlowMeterEnd.y", "Set Time", "MeterSetTime"))
# 
# altered <- samp_catch_phys2 %>%
#   filter(!is.na("SamplingAltered"))
# 
# Condcode <- samp_catch_phys2 %>%
#   select(c("event_id", "Station", "Condition Code", "ConditionCode"))

# Merge datasets for CPUE variables
samp_catch <- left_join(samp, catch)

#rename and remove columns from join

samp_catch_phys0 <- left_join(phys, samp_catch, by = "PhysicalDataID") %>%
  filter(!is.na(Station)) %>%
  filter(Date < "2020-02-10") 
notjoinedPhysDataID <- anti_join(phys, samp_catch, by = "PhysicalDataID")

# check <- samp_catch_phys0 %>%
#   filter(Date > "2019-12-31" & Date < "2021-01-01") %>%
#   filter(is.na(PhysicalDataID))
# 
# flow <- samp_catch_phys0 %>%
#   filter(!is.na(FlowMeterSpeed.y))

samp_catch_phys0 <- samp_catch_phys0 %>%
  select(-c(FlowMeterStart.x, FlowMeterEnd.x, FlowMeterSpeed.x, MeterSetTime,
            ConditionCode.x, FieldComments.x)) %>%
  rename(FlowMeterStart = FlowMeterStart.y,
         FlowMeterEnd = FlowMeterEnd.y,
         FlowMeterSpeed = FlowMeterSpeed.y,
         ConditionCode = ConditionCode.y,
         FieldComments = FieldComments.y,
         Category = Classification)

#check invert taxons and counts ensure none disappeared

nacount <- samp_catch_phys0 %>%
  filter(is.na(Count))

# condcode <- samp_catch_phys0 %>%
#   filter(!is.na(ConditionCode.y))
# 
# comments <- samp_catch_phys0 %>%
#   filter(!is.na(FieldComments.y))
# 
# comments2 <- samp_catch_phys0 %>%
#   filter(is.na(FieldComments.x))
#need to figure out field comments column

combined <- bind_rows(samp_catch_phys0, samp_catch_phys2)

nacombined <- combined %>%
  filter(is.na(Count))

#to merge excel with access - use bindrows instead of
sampcatchphysMerge <- rbind(samp_catch_phys0, phys_samp_catch0) 

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

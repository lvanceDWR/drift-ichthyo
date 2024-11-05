#utilize the data from the 2021 publication for up to 2019, then use excel moving forward

library(tidyverse)
library(gridExtra) # combine plots
library(stringr)
library(readr)
library(lubridate)
library(plotly)
library(viridis)
library(kableExtra)
library(tidylog)


phys <- read_csv("drift data/LT_phys_qc_20240118.csv")
catch <- read_csv("drift data/Drift_Catch_20200918.csv")
catch2 <- read_csv("drift data/Drift_Catch_Contractor_20210330.csv")
samp <- read_csv("drift data/Drift_Samplings_20200918.csv")
samp2 <- read_csv("drift data/Drift_Sampling_Access_20210330.csv")
catch3 <- read_csv("drift data/DriftLabExcelData_20240430.csv", skip=1)
samp3 <- read_csv("drift data/DriftSampExcelData.csv", skip=1)
tax <- read_csv("drift data/DriftTaxonomy.csv")
wy <- read_csv("WaterYearType_CDEC.csv") 
inundation <- read_csv("Yolo_Bypass_Inundation_1998-2022.csv")



######################### ACCESS ##############################################################################

phys$Date<-as.Date(phys$Date,"%m/%d/%Y")
phys$Year <- year(phys$Date)
phys$Month <- month(phys$Date)
phys$MonthAbb <-ordered(phys$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

phys$Datetime = paste(phys$Date, phys$Time)
phys$Datetime <- as.POSIXct(phys$Datetime, 
                            format = "%Y-%m-%d %H:%M:%S")
catch$Time <- as.POSIXct(catch$Time,
                         format = "%m/%d/%Y %H:%M")
samp$Date<-as.Date(samp$Date,"%m/%d/%Y")
samp$`Date/Time` = as.POSIXct(samp$`Date/Time`, 
                              format = "%m/%d/%Y %H:%M:%S")
catch2$Date <- as.Date(catch2$Date, "%m/%d/%Y")
catch2$Time <- strptime(catch2$Time, format = "%H:%M:%S") %>%
  strftime(catch2$Time, format = "%H:%M:%S",tz = "", usetz = FALSE)
catch2$Datetime = paste(catch2$Date, catch2$Time)
catch2$Datetime <- as.POSIXct(catch2$Datetime, 
                              format = "%Y-%m-%d %H:%M:%S")


catch <- catch %>%
  mutate(SamplingID = str_extract(`Sampling number`, "[^/]+")) %>%
  rename(Datetime = Time,
         TaxonName = Observable,
         LifeStage = `Life Stage`) %>%
  select(-c(Value, `Sampling number`)) %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)
catch$SamplingID = as.numeric(catch$SamplingID)

samp <- samp %>% 
  rename(SamplingID = `Sampling number`,
         Datetime = `Date/Time`,
         InvertDataID = `Invertebrate Data ID`,
         ConditionCode = `Condition Code`,
         SetTime = `Set Time`,
         FlowMeterSpeed = `Flow Meter Speed`,
         FlowMeterStart = `Flow Meter Start`,
         FlowMeterEnd = `Flow Meter End`,
         LabComments = `Lab Comments`,
         FieldComments = `Field Comments`) %>%
  select(-`Invertebrate Catch ID`) %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)
samp$SamplingID <- as.numeric(samp$SamplingID)
summary(samp)

catch2 <- catch2 %>%
  rename(Count = Individuals,
         Category = `Insect/Non-Insect`,
         LifeStage = Stage,
         TaxonName = Name) %>%
  select(-c("Number", "Time")) %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

samp2 <- samp2 %>%
  rename(FlowMeterStart = DriftStartMeter,
         FlowMeterEnd = DriftEndMeter) %>%
  select(-c("StartTime", "StopTime"))

samp_catch <- left_join(samp, catch, by = c("event_id", "SamplingID", "Station")) %>%
  rename(Datetime = Datetime.x) %>%
  select(-c(Datetime.y))

# Merge physical data
samp_catch_phys0 <- left_join(phys, samp_catch, by = c("event_id", "PhysicalDataID")) %>%
  rename(Station = Station.x,
         Date = Date.x,
         Time = Time.x,
         Datetime = Datetime.x) %>%
  select(-c(Station.y, Date.y, Time.y, Datetime.y)) %>%
  filter(!is.na(Station)) %>%
  filter(Date < "2019-04-22") %>%
  arrange(Datetime)
notjoinedPhysDataID <- anti_join(phys, samp_catch, by = "PhysicalDataID")

# For second part 2019, merge phys-samp, then add catch.
# For the additional data
phys_samp <- left_join(phys, samp2, by = c("PhysicalDataID")) %>%
  filter(Date > "2019-04-16" & Date < "2020-01-01") %>%
  mutate(SamplingID = "N/A") 

phys_samp_catch0 <- left_join(phys_samp, catch2, by = c("Datetime", "Date", "Station")) %>%
  select(c(PhysicalDataID:Comment_PQC, SamplingID, InvertDataID:SetTime, FlowMeterSpeed, FlowMeterStart, FlowMeterEnd, LabComments, FieldComments, TaxonName, Count, Category, LifeStage))

sampcatchphysMergeA <- rbind(samp_catch_phys0, phys_samp_catch0) 

##########################################################################################


############################ EXCEL #########################################

samp3 <- samp3 %>%
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

catch3 <- catch3 %>%
  filter(!(is.na(`Measuring program short name`))) %>%
  rename(Count = `Value...11`,
         LifeStage = `Value...12`,
         TaxonName = Observable,
         Date = `Sampling Event Date`,
         Time = `Sampling Event Time`,
         Station = `Sampling Area Number`,
         SAMCode = `Sampling Event Number`,
         SampleID = `Sample ID`,
         LabComments = lab_comments) %>%
  select(-c(`Measuring program short name`, `Observation Type Short Name`,
            SAMCode))


catch3$Date<-as.Date(catch3$Date,"%m/%d/%Y")
catch3$Year <- year(catch3$Date)
catch3$Month <- month(catch3$Date)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
catch3$MonthAbb <- mymonths[catch3$Month ]
catch3$Datetime = paste(catch3$Date, catch3$Time)
catch3$Datetime <- ymd_hm(catch3$Datetime)
catch3$Time <- strptime(catch3$Time, format = "%H:%M", tz = "") %>%
  strftime(catch3$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)
catch3$Time <- hms::as_hms(catch3$Time)

samp3$Date<-as.Date(samp3$Date,"%m/%d/%Y")
samp3$Year <- year(samp3$Date)
samp3$Month <- month(samp3$Date)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
samp3$MonthAbb <- mymonths[samp3$Month ]
samp3$Datetime = paste(samp3$Date, samp3$Time)
samp3$Datetime <- ymd_hm(samp3$Datetime)
samp3$Time <- strptime(samp3$Time, format = "%H:%M", tz = "") %>%
  strftime(samp3$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)
samp3$Time <- hms::as_hms(samp3$Time)


#first create event_id
catch3 <- catch3 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

samp3 <- samp3 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

#join data frames and select for data through end of 2022
samp_catch3 <- left_join(samp3, catch3) %>%
  filter(Date < "2023-01-01")
str(samp_catch3)

#ensure column types match for joining with Access data
samp_catch3$Date <- as.Date(samp_catch3$Date)
samp_catch3 <- samp_catch3 %>%
  mutate(FlowMeterEnd = as.numeric(FlowMeterEnd))

#combine with phys data in prep to merge with access data before flowmeter values
#rename some columns, remove duplicate columns

samp_catch_phys3 <- left_join(samp_catch3, phys, by = c("event_id","Datetime", "Station", "Date", "Time",
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
  select(-c(Field_Comments, SampleID, Attribute)) %>%
  unique() %>%
  arrange(Datetime)



####################################################################################################



inundation <- inundation %>%
  rename(Date = Dates)
inundation$Date<-as.Date(inundation$Date,"%m/%d/%Y")









































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
catch <- read_csv("drift data/DriftCatchDataAccess_20240119.csv")
catch2 <- read_csv("drift data/DriftLabExcelData_20240430.csv", skip=1)
samp <- read_csv("drift data/DriftInvertSampAccess_20240119.csv")
samp2 <- read_csv("drift data/DriftSampExcelData.csv", skip=1)
tax <- read_csv("drift data/DriftTaxonomy.csv")
wy <- read_csv("WaterYearType_CDEC.csv") 
inundation <- read_csv("Yolo_Bypass_Inundation_1998-2022.csv")

samp <- samp %>% 
  rename(FlowMeterStart = `DriftStartMeter`,
         FlowMeterEnd = `DriftEndMeter`) %>%
  select(-c(`EnteredBy`, `QA/QC'dBy`, StartTime, StopTime))


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
         LabComments = lab_comments) %>%
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

########################### EXCEL DATA ######################################################################
#first create event_id
catch2 <- catch2 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

samp2 <- samp2 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

#join data frames and select for data through end of 2022
samp_catch2 <- left_join(samp2, catch2) %>%
  filter(Date < "2023-01-01")
str(samp_catch2)

#ensure column types match for joining with Access data
samp_catch2$Date <- as.Date(samp_catch2$Date)
samp_catch2 <- samp_catch2 %>%
  mutate(FlowMeterEnd = as.numeric(FlowMeterEnd))

#combine with phys data in prep to merge with access data before flowmeter values
#rename some columns, remove duplicate columns

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
  select(-c(Field_Comments, SampleID, Attribute)) %>%
  unique() %>%
  arrange(Datetime)


###############################################################################################################
inv_tax_2 <- read_csv("drift data/TblInvertsLookUpV2.csv")

# Merge datasets for CPUE variables
inv_tax <- full_join(tax, inv_tax_2, by = "OrganismID") %>%
  relocate(Kingdom, "Phylum.x", "Phylum.y", Subphylum, "Class.x", "Class.y",
           "Order.x", "Order.y", "Family.x", "Family.y")

taxaccess <- inv_tax %>%
  select(c(Genus, "Species.x", CommonName, OrganismID, TaxonName, TaxonRank, Category)) %>%
  rename(Species = "Species.x") %>%
  relocate(OrganismID, Category, TaxonName, TaxonRank, CommonName, Genus, Species)

# catchtaxa <- left_join(catch, taxaccess)

catchA <- catch %>%
  select(-c(InvertCode, Classification, Order, Family))

catchtaxa <- left_join(catchA, taxaccess, by = "OrganismID")

catchpiv <- pivot_longer(catchtaxa, 
                                cols = c(Larvae, Pupae, Nymphs, Emergents,
                                         Adults, "NA"),
                                names_to = c("LifeStage"),
                                values_to = c("CountStage"),
                                values_drop_na = TRUE)

# catchptax <- left_join(catchpiv, taxaccess)

phys_samp1 <- left_join(phys, samp, by = "PhysicalDataID") %>%
  filter(!is.na(PhysicalDataID)) %>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x", "InvertCode")) %>%
  rename(ConditionCode = "ConditionCode.y",
         FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y", 
         FieldComments = "FieldComments.y",
         FlowMeterSpeed = "FlowMeterSpeed.y") %>%
  filter(Date < "2019-04-22")

samp_catch <- left_join(phys_samp1, catchpiv, by = "InvertDataID") %>%
  arrange(Datetime)
#issue is that it doesn't look like there's any catch data until 2001 - doesn't seem correct.

#rename and remove columns from join
# 04-22-2019 onward was entered into Excel, not Access
samp_catch_phys0 <- full_join(phys, samp_catch, by = "PhysicalDataID") %>%
  filter(!is.na(Station)) %>%
  filter(Date < "2019-04-22") 
notjoinedPhysDataID <- anti_join(phys, samp_catch, by = "PhysicalDataID")

#clean up and rename some columns
samp_catch_phys0 <- samp_catch_phys0 %>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x")) %>%
  rename(FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y",
         FlowMeterSpeed = "FlowMeterSpeed.y",
         ConditionCode = "ConditionCode.y",
         FieldComments = "FieldComments.y") %>%
  arrange(Datetime)


samp_catch_physA <- left_join(samp_catch_phys0, taxaccess) 

samp_catch_phys00 <- pivot_longer(samp_catch_physA,
                                  cols = c(Larvae, Pupae, Nymphs, Emergents,
                                           Adults, "NA"),
                                  names_to = c("LifeStage"),
                                  values_to = c("CountStage"),
                                  values_drop_na = TRUE) 

#########################################################################################################
# For second part 2019, merge phys-samp, then add catch.
# For the additional data

catch2019 <- catch2 %>%
  filter(Date > "2019-04-10" & Date < "2020-02-01")

phys2019 <- phys %>%
  filter(Date > "2018-12-31" & Date < "2020-01-01") %>%
  select(-c(FieldComments))
#fieldcomments removed for 2019 since all NA and the comments are in a diff comments column

phys_samp <- left_join(phys2019, samp, by = "PhysicalDataID")%>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x")) %>%
  rename(FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y",
         FlowMeterSpeed = "FlowMeterSpeed.y",
         ConditionCode = "ConditionCode.y")

gap <- left_join(phys_samp, catch2019)

#######################################################################################################


#combine the 2019 data with the rest of the access data
samp_catch_phys <- bind_rows(samp_catch_phys0, gap)

##########################################################################################################

#Now combine these two files to then continue with qa/qc
sampcatchphysMerge <- bind_rows(samp_catch_phys, samp_catch_phys2) %>%
  relocate(event_id, Datetime)





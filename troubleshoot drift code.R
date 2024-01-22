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


# Add and change date formats
phys$Date<-as.Date(phys$Date,"%m/%d/%Y")
phys$Year <- year(phys$Date)
phys$Month <- month(phys$Date)
phys$MonthAbb <-ordered(phys$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

phys$Datetime = paste(phys$Date, phys$Time)
phys$Datetime <- as.POSIXct(phys$Datetime, 
                            format = "%Y-%m-%d %H:%M:%S")

phys <- phys %>%
  rename(Station = StationCode)

catch <- catch %>%
  mutate(SamplingID = str_extract(`Sampling number`, "[^/]+")) %>%
  rename(Datetime = Time,
         TaxonName = Observable,
         LifeStage = `Life Stage`) %>%
  select(-c(Value, `Sampling number`))
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
  select(-`Invertebrate Catch ID`)
samp$SamplingID <- as.numeric(samp$SamplingID)
summary(samp)

catch2 <- catch2 %>%
  rename(Count = Individuals,
         Category = `Insect/Non-Insect`,
         LifeStage = Stage,
         TaxonName = Name) %>%
  select(-c("Number", "Time"))

samp2 <- samp2 %>%
  rename(FlowMeterStart = DriftStartMeter,
         FlowMeterEnd = DriftEndMeter) %>%
  select(-c("StartTime", "StopTime"))


#add and change date formats
catch$Time <- as.POSIXct(catch$Time,
                         format = "%m/%d/%Y %H:%M")
samp$Date<-as.Date(samp$Date,"%m/%d/%Y")
samp$`Date/Time` = as.POSIXct(samp$`Date/Time`, 
                              format = "%m/%d/%Y %H:%M:%S")
catch2$Date <- as.Date(catch2$Date, "%m/%d/%Y")
catch2$Time <- strptime(catch2$Time, format = "%H:%M:%S") %>%
  strftime(catch2$Time, format = "%H:%M:%S")
catch2$Datetime = paste(catch2$Date, catch2$Time)
catch2$Datetime <- as.POSIXct(catch2$Datetime, 
                              format = "%Y-%m-%d %H:%M:%S")

inundation$Date<-as.Date(inundation$Dates,"%m/%d/%Y")
str(phys)
str(catch)
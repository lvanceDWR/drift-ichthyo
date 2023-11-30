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


phys <- read_csv("drift data/TblPhysicalDataAccess.csv")
phys2 <- read_csv("drift data/DriftSampExcelData.csv", skip=1)
WQ <- read_csv("drift data/LTWQ2020on.csv", skip=1)

phys3 <- phys2 %>%
  filter(!is.na(`Measuring program short name`)) %>%
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
         SampleVolume = 'Sample Volume',
         SubsampleNumber = 'Subsample Number',
         DilutionVolume = 'Dilution Volume',
         SlideCount = 'Slide Count',
         MeshSize = 'Observation Area Name',
         Observation = 'Observation Area Number',
         SpotNumber = 'Spot Number',
         SpotCode = 'Spot Code (original/duplicate)') %>%
  select(-c(PhysicalDataID, PhysicalDataIDx,
            FlowMeter50Start,FlowMeter50End,
            EnteredBy, QAQCBy, SpotCode, SpotNumber, Observation,
            '...27', '...28', '...29', '...30', '...31', '...32', SamplingNumber))

WQ <- WQ %>%
  filter(!is.na(`Measuring Program Name`)) %>%
  rename(Program = 'Measuring Program Name',
         Date = 'WDL SAM_COLLECTION_DATE',
         Time = 'Collection Time',
         Station = 'Station Name',
         StationNumber = 'Station Number',
         SamplingNumber = 'Sampling Number (event)',
         WaterTemperature = 'water.temp',
         SpCnd = 'sp.cond',
         DO = 'DO.probe',
         Turbidity = 'turb',
         Secchi = secchi,
         Conductivity = EC,
         SampleDate = `Sample Date`,
         SampleTime = `Sample Time`,
         SampleNumber = `Sample Number`,
         MicrocystisVisualRank = microcyst,
         VegetationRank = VegRank) %>%
  select(-c('Run Number', 'Run Name'))

phys <- phys %>% mutate(Datetime = paste(Date, Time, sep = " "))
phys3 <- phys3 %>% mutate(Datetime = paste(Date, Time, sep = " "))
WQ <- WQ %>% mutate(Datetime = paste(Date, Time, sep = ""))

str(phys3)
str(WQ)

phys$Datetime <- mdy_hms(phys$Datetime)
phys$Date<- mdy(phys$Date)
phys$Time <- strptime(phys$Time, format = "%H:%M", tz = "") %>%
  strftime(phys$Time, format = "%H:%M:%S")  #is this needed anymore?
phys$Year <- ordered(year(phys$Date))
phys$Month <- ordered(month(phys$Date))
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
phys$Tide <- as.factor(phys$Tide)
#add abbreviated month name
phys$MonthAbb <- mymonths[ phys$Month ]
#set order of months
phys$MonthAbb <-ordered(phys$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


phys3$Datetime <- mdy_hm(phys3$Datetime)
phys3$Date<- mdy(phys3$Date)
phys3$Year <- ordered(year(phys3$Date))
phys3$Month <- ordered(month(phys3$Date))
#add abbreviated month name
phys3$MonthAbb <- mymonths[ phys3$Month ]
#set order of months
phys3$MonthAbb <-ordered(phys3$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


WQ2 <- WQ %>%
  filter(Program == "YBFMP" | Program == "Shared") %>%
  select(-c(LabOrField, Recorder, `Field Check`, FieldChecker, Crew, 
            LightData, DriftData, LarvalData, `150_ZoopsData`, `50_ZoopsData`,
            PhytoData, ChlData, NutrData, EnteredBy, QAQCBy, SurfaceIrr,
            Depth1Irr, Depth2Irr, Depth3Irr, Depth4Irr, SubIrr1, SubIrr2, SubIrr3,
            SubIrr4, SampleNumber, SampleDate, SampleTime, SamplingNumber, StationNumber))

str(phys3)
str(WQ2)
str(phys)

WQ2$Datetime <- mdy_hm(WQ2$Datetime)
WQ2$Time <- strptime(WQ2$Time, format = "%H:%M", tz = "") %>%
  strftime(WQ2$Time, format = "%H:%M:%S")
WQ2$Date<- mdy(WQ2$Date)
WQ2$Year <- ordered(year(WQ2$Date))
WQ2$Month <- ordered(month(WQ2$Date))
#add abbreviated month name
WQ2$MonthAbb <- mymonths[ WQ2$Month ]
#set order of months
WQ2$MonthAbb <-ordered(WQ2$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))



combine <- full_join(WQ2, phys3)

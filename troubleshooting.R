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
  select(-c('Run Number', 'Run Name', LabOrField, Recorder, `Field Check`, FieldChecker, Crew, 
            LightData, DriftData, LarvalData, `150_ZoopsData`, `50_ZoopsData`,
            PhytoData, ChlData, NutrData, EnteredBy, QAQCBy, SurfaceIrr,
            Depth1Irr, Depth2Irr, Depth3Irr, Depth4Irr, SubIrr1, SubIrr2, SubIrr3,
            SubIrr4, SampleNumber, SampleDate, SampleTime, SamplingNumber, StationNumber))

phys <- phys %>% mutate(Datetime = paste(Date, Time, sep = " "))
phys3 <- phys3 %>% mutate(Datetime = paste(Date, Time, sep = " "))
WQ <- WQ %>% mutate(Datetime = paste(Date, Time, sep = ""))

WQ2 <- WQ %>%
  filter(Program == "YBFMP" | Program == "Shared") %>%
  filter(Station == "STTD" | Station == "SHR")

str(phys3)
str(WQ2)

phys3$Datetime <- mdy_hm(phys3$Datetime)
phys3$Date<- mdy(phys3$Date)
phys3$Time <- strptime(phys3$Time, format = "%H:%M", tz = "") %>%
  strftime(phys3$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)  

WQ2$Datetime <- mdy_hm(WQ2$Datetime)
WQ2$Date<- mdy(WQ2$Date)
WQ2$Time <- strptime(WQ2$Time, format = "%H:%M", tz = "") %>%
  strftime(WQ2$Time, format = "%H:%M:%S", tz = "", usetz = FALSE) 


combine <- left_join(WQ2, phys3)

#troubleshooting merging Access with Excel -testing out Nicole's suggestion of 
# combining wq with phys from excel, combine with Access *before* parsing out time, date
# year, Month

str(phys)
str(combine)



phys$Datetime <- mdy_hms(phys$Datetime)
phys$Date<- mdy(phys$Date)
phys$Time <- strptime(phys$Time, format = "%H:%M", tz = "") %>%
  strftime(phys$Time, format = "%H:%M:%S", tz = "", usetz = FALSE) #add usetz = false to match phys3 and WQ


phys.s <- phys %>%
  rename(Secchi = SecchiDiskDepth,
         Station = `Station Code`,
         Conductivity = EC,
         YSI = `YSI #`) %>%
  select(-c(Recorder, `Field Check`, Crew, EnteredBy, "QA/QC'dBy",  
            LightData, DriftData, LarvalData, ZoopsData, `50_ZoopsData`, ChlData, PhytoData, NutrData,
            Comments, DataCorrectionComments, StartMeter, EndMeter, MeterSetTime)) %>%
  filter(Station == "SHR" | Station == "STTD")


combine2 <- combine %>%
  mutate(Secchi = as.numeric(Secchi),
         WaterTemperature = as.numeric(WaterTemperature),
         DO = as.numeric(DO),
         SpCnd = as.numeric(SpCnd),
         Conductivity = as.numeric(Conductivity),
         pH = as.numeric(pH),
         MicrocystisVisualRank = as.numeric(MicrocystisVisualRank),
         VegetationRank = as.numeric(VegetationRank),
         Turbidity = as.numeric(Turbidity),
         YSI = as.numeric(YSI),
         FlowMeterEnd = as.numeric(FlowMeterEnd))

str(phys.s)
str(combine2)


full <- bind_rows(phys.s, combine2)

NATide <- filter(phys.s, is.na(Tide))
NAYSI <- filter(phys.s, is.na(YSI))

#now add in the ordered factors for month, year, tide, and abbreviated months

full$Year <- ordered(year(full$Date))
full$Month <- ordered(month(full$Date))
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
full$Tide <- as.factor(full$Tide)
#add abbreviated month name
full$MonthAbb <- mymonths[ full$Month ]
#set order of months
full$MonthAbb <-ordered(full$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#this seems to fix the issue that came up

##############################################################################################################################

phys.s$Year <- ordered(year(phys.s$Date))
phys.s$Month <- ordered(month(phys.s$Date))
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
phys.s$Tide <- as.factor(phys.s$Tide)
#add abbreviated month name
phys.s$MonthAbb <- mymonths[ phys.s$Month ]
#set order of months
phys.s$MonthAbb <-ordered(phys.s$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

str(phys)

str(phys3)

phys3$Datetime <- mdy_hm(phys3$Datetime)
phys3$Date<- mdy(phys3$Date)
phys3$Time <- strptime(phys3$Time, format = "%H:%M", tz = "") %>%
  strftime(phys3$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)  
#invalid usetz argument if usetz isn't used in strftime for this
# #phys3$Time <- as.POSIXlt(phys3$Time, format = "%H:%M") - adds todays date
# phys3 <- phys3 %>%
#   mutate(Time = as.numeric(hm(Time))) - creates wrong format of time...
phys3$Year <- ordered(year(phys3$Date))
phys3$Month <- ordered(month(phys3$Date))
#add abbreviated month name
phys3$MonthAbb <- mymonths[ phys3$Month ]
#set order of months
phys3$MonthAbb <-ordered(phys3$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


WQ2 <- WQ %>%
  filter(Program == "YBFMP" | Program == "Shared") 


##################################################################################

#cdec data 

# Read files (came from cdec4gov)
LIS_WaterTemperature <- read_csv("Data/STA/LIS_25.csv")
LIS_DO <- read_csv("Data/STA/LIS_61.csv")
LIS_pH <- read_csv("Data/STA/LIS_62.csv")
LIS_Turb <- read_csv("Data/STA/LIS_27.csv")
LIS_EC <- read_csv("Data/STA/LIS_100.csv")
LIS_EC$`DATE TIME` <- mdy_hm(LIS_EC$`DATE TIME`)

str(phys3)
str(WQ2)
str(phys)

WQ2$Datetime <- mdy_hm(WQ2$Datetime)
WQ2$Date<- mdy(WQ2$Date)
WQ2$Time <- strptime(WQ2$Time, format = "%H:%M", tz = "") %>%
  strftime(WQ2$Time, format = "%H:%M:%S", tz = "", usetz = FALSE) 
#invalid usetz argument, must add tz= FALSE in order to not receive error

WQ2$Year <- ordered(year(WQ2$Date))
WQ2$Month <- ordered(month(WQ2$Date))
#add abbreviated month name
WQ2$MonthAbb <- mymonths[ WQ2$Month ]
#set order of months
WQ2$MonthAbb <-ordered(WQ2$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))



combine <- left_join(WQ2, phys3)
#this join now seems to maintain all data points correctly for each sampling and station
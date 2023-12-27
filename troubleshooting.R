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
WQ <- read_csv("drift data/LTWQ2020onward.csv", skip=1)

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
            '...27', '...28', '...29', '...30', '...31', '...32', SamplingNumber, Program,
            SampleVolume, SubsampleNumber, DilutionVolume, SlideCount))

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
            SubIrr4, SampleNumber, SampleDate, SampleTime, SamplingNumber, StationNumber, Program))

#removing program because it is not necessary for publishing - different entries for WQ and drift/ich can cause join errors

phys <- phys %>% mutate(Datetime = paste(Date, Time, sep = " "))
phys3 <- phys3 %>% mutate(Datetime = paste(Date, Time, sep = " "))
WQ <- WQ %>% mutate(Datetime = paste(Date, Time, sep = ""))

WQ2 <- WQ %>%
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

#combine the physical data and water quality  from excel before joining to Access data
combine <- left_join(WQ2, phys3)

#check column formats before joining access data and excel data
str(phys)
str(combine)

#configure date time columns before combining
phys$Datetime <- mdy_hms(phys$Datetime)
phys$Date<- mdy(phys$Date)
phys$Time <- strptime(phys$Time, format = "%H:%M", tz = "") %>%
  strftime(phys$Time, format = "%H:%M:%S", tz = "", usetz = FALSE) #add usetz = false to match phys3 and WQ


#rename some columns,filter to stations drift is collected 
phys.s <- phys %>%
  rename(Secchi = SecchiDiskDepth,
         Station = `Station Code`,
         Conductivity = EC,
         YSI = `YSI #`) %>%
  select(-c(Recorder, `Field Check`, Crew, EnteredBy, "QA/QC'dBy",  
            LightData, DriftData, LarvalData, ZoopsData, `50_ZoopsData`, ChlData, PhytoData, NutrData,
            Comments, DataCorrectionComments, StartMeter, EndMeter, MeterSetTime)) %>%
  filter(Station == "SHR" | Station == "STTD")

#check column formats again before joining access data and excel data
str(phys.s)
str(combine)

#match column types for combining access and excel data together
#filter out dates that were for NDFS only and no drift were collected
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
         FlowMeterEnd = as.numeric(FlowMeterEnd)) %>%
  filter(!(Date == "2020-07-13" | Date == "2020-07-27" | Date == "2020-08-10"))

#ensure column types match, then create full file
str(phys.s)
str(combine2)

yolo_phys <-bind_rows(phys.s, combine2)
#will it be necessary to separate out the flowmeter values with stations/datetimes, 
# to merge with the catch data/lab data instead of being in the physical data file?

Flowmeter <- yolo_phys %>%
  select(c(Station, Date, Time, YSI, WeatherCode, Tide, ConditionCode,
           SamplingAltered, MeterSetTime, FlowMeterStart, FlowMeterEnd,
           FlowMeterSpeed,MeshSize)) %>%
  filter(year(Date) >= 2020) %>%
  filter(!(Date == "2020-01-06" | Date == "2020-01-07" |
             Date == "2020-01-27" | Date == "2020-01-28"))

write_csv(Flowmeter, "data drift/Flowmeterdata.csv")


################## Troubleshoot strftime problem with time format

LIS_WaterTemperature <- read_csv("drift data/STA/LIS_25.csv")
LIS_DO <- read_csv("drift data/STA/LIS_61.csv")
LIS_pH <- read_csv("drift data/STA/LIS_62.csv")
LIS_Turb <- read_csv("drift data/STA/LIS_221.csv")
LIS_EC <- read_csv("drift data/STA/LIS_100.csv")
LIS_EC$`DATE TIME` <- mdy_hm(LIS_EC$`DATE TIME`)


LIS_WaterTemperature_ed <- LIS_WaterTemperature %>%
  select(3,5) %>%
  filter(parameter_value<99998) %>%
  mutate(Temp_C = round((parameter_value-32)*5/9,1)) %>%
  rename(Temp_F = parameter_value)
LIS_DO_ed <- LIS_DO %>%
  select(3,5) %>%
  rename(DO = parameter_value)
LIS_pH_ed <- LIS_pH %>%
  select(3,5) %>%
  rename(pH = parameter_value)
LIS_Turb_ed <- LIS_Turb %>%
  select(3,5) %>%
  rename(Turbidity = parameter_value)
LIS_EC_ed <- LIS_EC%>%
  select(3,5) %>%
  rename(Conductivity = parameter_value)

LIS_WaterTemperature_ed$Time <- hms::as_hms(LIS_WaterTemperature_ed$datetime)

OlsonNames


# Merge variables together
a <- merge(LIS_DO_ed, LIS_Turb_ed, by = "datetime")
b <- merge(a, LIS_EC_ed, by = "datetime")
c <- merge(b, LIS_pH_ed, by = "datetime")
LIS_WQ <- left_join(LIS_WaterTemperature_ed, c, by = "datetime")

# str(LIS_WQ)
LIS_WQ$Time <- hms::as_hms(LIS_WQ$datetime)
LIS_WQ$Date <- as_date(LIS_WQ$datetime)
LIS_WQ$Hour <- as.numeric(format(strptime(LIS_WQ$Time, "%H:%M:%S"), "%H"))

LIS_WQ_f <- LIS_WQ %>%
  rename(WaterTemperature = Temp_C) %>%
  dplyr::filter(Hour>06 & Hour<15) %>%
  dplyr::filter(WaterTemperature>0 & WaterTemperature<40) %>%
  dplyr::filter(Conductivity>0)%>%
  filter(DO>0)%>%
  filter(pH>0 & pH<14)%>%
  filter(Turbidity >0) %>%
  mutate(StationCode="LIS_RT") %>%
  select(1,3:7,9,11)





















# combine <- full_join(WQ2, phys3) #flowmeter values for 2021 are lost....figure out how to avoid
# #12 rows "only in x" "only in y" not matched - these are the missing values when joined
# # the 12 rows that have a problem are differing in "program"
# SHR Oct 2021 - sampling day was 10/12/2021 according to datasheet, not 10/13/2021

#use case when to solve that sampling date?
#make sure to specify that the date desired is a date variable - how to not add an extra column?
#fixed by correcting date to correct date in database and re-reading in new file

# str(combine)
# 
# fix <- WQ2 %>%
#   if_else(WQ$Date == "2021-10-13", Date == "2021-10-12")
# 
# fix <- WQ2 %>%
#   case_when(Date == "2021-10-13" ~ as_date("2021-10-12"),
#             TRUE ~ Date)

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

# phys.s$Year <- ordered(year(phys.s$Date))
# phys.s$Month <- ordered(month(phys.s$Date))
# mymonths <- c("Jan","Feb","Mar",
#               "Apr","May","Jun",
#               "Jul","Aug","Sep",
#               "Oct","Nov","Dec")
# phys.s$Tide <- as.factor(phys.s$Tide)
# #add abbreviated month name
# phys.s$MonthAbb <- mymonths[ phys.s$Month ]
# #set order of months
# phys.s$MonthAbb <-ordered(phys.s$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# str(phys)
# 
# str(phys3)
# 
# phys3$Datetime <- mdy_hm(phys3$Datetime)
# phys3$Date<- mdy(phys3$Date)
# phys3$Time <- strptime(phys3$Time, format = "%H:%M", tz = "") %>%
#   strftime(phys3$Time, format = "%H:%M:%S", tz = "", usetz = FALSE)  
# #invalid usetz argument if usetz isn't used in strftime for this
# # #phys3$Time <- as.POSIXlt(phys3$Time, format = "%H:%M") - adds todays date
# # phys3 <- phys3 %>%
# #   mutate(Time = as.numeric(hm(Time))) - creates wrong format of time...
# phys3$Year <- ordered(year(phys3$Date))
# phys3$Month <- ordered(month(phys3$Date))
# #add abbreviated month name
# phys3$MonthAbb <- mymonths[ phys3$Month ]
# #set order of months
# phys3$MonthAbb <-ordered(phys3$MonthAbb,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# 
# WQ2 <- WQ %>%
#   filter(Program == "YBFMP" | Program == "Shared") 


##################################################################################

#cdec data 

#2. Download CDEC Retrieve package

#```{r}
devtools::install_github("flowwest/CDECRetrieve", force=TRUE)

remotes::install_github("flowwest/CDECRetrieve")

# Alt method:
# install roxygen2 (normal install method)
# install.packages("https://cran.r-project.org/src/contrib/Archive/CDECRetrieve/CDECRetrieve_0.1.4.tar.gz", repos=NULL, method = "libcurl")
## Or use winzip to unzip the files, copy folder into the .libPaths() specified (or go to Tools Install to check where lib is)
#```

library(tidyverse)
library(data.table) #rbindlist()
library(lubridate) #today()
library(CDECRetrieve) #cdec_datasets
library(readr)
library(tidylog)

stations <- "LIS"

# Define start and end dates - these will remain the same throughout
starttemp <- "2008-07-16"
end <- "2022-12-31"

# Define sensor number, time interval  temperature 7/16/2008-present
sensortemp <- c("25") # water temp F 
interval <- "E" # Event = every 15 minutes


### Download data, bind, write --------------------------------------------
Data_temp <- lapply(sensortemp, 
                       function(x){
                         cdec_query(station = stations,
                                    sensor_num = x,
                                    dur_code = interval,
                                    start_date = starttemp,
                                    end_date = end)
                       })

sensors_df_temp <- bind_rows(Data_temp) # bind rows into data frame

# repeat for turb 8/1/2013-present

stations <- "LIS"

# Define start and end dates - these will remain the same throughout
startturb <- "2013-08-01"
end <- "2022-12-31"

# Define sensor number, time interval 
sensorturb <- c("221") # turbidity
interval <- "E" # Event = every 15 minutes


### Download data, bind, write --------------------------------------------
Data_turb <- lapply(sensorturb, 
                       function(x){
                         cdec_query(station = stations,
                                    sensor_num = x,
                                    dur_code = interval,
                                    start_date = startturb,
                                    end_date = end)
                       })

sensors_df_turb <- bind_rows(Data_turb) # bind rows into data frame

#repeat for DO (sensor 61) 8/1/2013-present
stations <- "LIS"

# Define start and end dates - these will remain the same throughout
startDO <- "2013-08-01"
end <- "2022-12-31"

# Define sensor number, time interval 
sensorDO <- c("61") # DO
interval <- "E" # Event = every 15 minutes


### Download data, bind, write --------------------------------------------
Data_DO <- lapply(sensorDO, 
                       function(x){
                         cdec_query(station = stations,
                                    sensor_num = x,
                                    dur_code = interval,
                                    start_date = startDO,
                                    end_date = end)
                       })

sensors_df_DO <- bind_rows(Data_DO) # bind rows into data frame

#repeat for pH (sensor 62) 8-1-2013 - present
stations <- "LIS"

# Define start and end dates - these will remain the same throughout
startpH <- "2013-08-01"
end <- "2022-12-31"

# Define sensor number, time interval 
sensorpH <- c("62") # pH
interval <- "E" # Event = every 15 minutes


### Download data, bind, write --------------------------------------------
Data_pH <- lapply(sensorpH, 
                       function(x){
                         cdec_query(station = stations,
                                    sensor_num = x,
                                    dur_code = interval,
                                    start_date = startpH,
                                    end_date = end)
                       })

sensors_df_pH <- bind_rows(Data_pH) # bind rows into data frame

#repeat for EC (sensor 100) 8/1/2013 - present
stations <- "LIS"

# Define start and end dates - these will remain the same throughout
startEC <- "2013-08-01"
end <- "2022-12-31"

# Define sensor number, time interval 
sensorEC <- c("100") # EC
interval <- "E" # Event = every 15 minutes


### Download data, bind, write --------------------------------------------
Data_EC <- lapply(sensorEC, 
                       function(x){
                         cdec_query(station = stations,
                                    sensor_num = x,
                                    dur_code = interval,
                                    start_date = startEC,
                                    end_date = end)
                       })

sensors_df_EC <- bind_rows(Data_EC) # bind rows into data frame

#cdec4gov seems outdated
#cdec 
#water temp = 25 is now deg F
# DO = 61
# pH = 62
# Turb = 221
# EC = 100

# Read files (came from cdec4gov)
LIS_WaterTemperature <- read_csv("Data/STA/LIS_25.csv")
LIS_DO <- read_csv("Data/STA/LIS_61.csv")
LIS_pH <- read_csv("Data/STA/LIS_62.csv")
LIS_Turb <- read_csv("Data/STA/LIS_27.csv")
LIS_EC <- read_csv("Data/STA/LIS_100.csv")
LIS_EC$`DATE TIME` <- mdy_hm(LIS_EC$`DATE TIME`)









#############################################################################
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
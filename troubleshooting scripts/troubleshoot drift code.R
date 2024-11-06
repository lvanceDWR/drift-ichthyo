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
phys <- phys %>%
  arrange(Datetime)


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
  filter(Date < "2023-01-01") %>%
  unique()
str(samp_catch2)

samp_catch2$Date <- as.Date(samp_catch2$Date)
samp_catch2 <- samp_catch2 %>%
  mutate(FlowMeterEnd = as.numeric(FlowMeterEnd))
str(samp_catch2)

#check how many values exist without jan 2020 in catch2
# catch3 <- catch2 %>%
#   filter(Date > "2020-02-09") %>%
#   filter(Date < "2023-01-01")
# #total is 1498 which matches sampcatchphys2 with distinct. solved.
# 
# #check how many distinct values exist
# samp_catch3 <- samp_catch2 %>%
#   distinct()
# 
# write_csv(samp_catch2, paste("R_write/sampcatch2.csv"))
# write_csv(samp_catch3, paste("R_write/sampcatch3.csv"))
# 
# 
# samp_catch_phys3 <- left_join(samp_catch3, phys, by = c("event_id","Datetime", "Station", "Date", "Time",
#                                                         "Year", "Month", "MonthAbb")) %>%
#   select(-c("FlowMeterStart.y", "FlowMeterEnd.y", "MeterSetTime", "FlowMeterSpeed", 
#             "Observation Area Name", "Physical Data ID", "Sampling Altered", "ConditionCode")) %>%
#   rename(FlowMeterStart = "FlowMeterStart.x",
#          FlowMeterEnd = "FlowMeterEnd.x",
#          FlowMeterSpeed = "Flow Meter Speed",
#          SetTime = "Set Time",
#          Field_Comments = "Field Comments",
#          SampleVolume = "Sample Volume",
#          SubsampleNumber = "Subsample Number",
#          SlideCount = "Slide Count",
#          ConditionCode = "Condition Code") %>%
#   distinct()



# merge phys data and make sure early 2020 sampling data doesnt get lost because of overlap in excel and access

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
  distinct() %>%
  arrange(Datetime)
#find where the early 2020 sampling data is - Jan 2020 - make sure it doesn't get lost.

#need to add taxon name to the data from access and 2019/2020 data overlap
tax2 <- tax %>%
  select(c(Class, Order, Family, Genus, Species, CommonName, OrganismID, TaxonName))

catchtaxa <- left_join(catch, tax2, by = "OrganismID") %>%
  select(-c("Order.x", "Family.y")) %>%
  rename(Order = "Order.y",
         Family = "Family.x")

catchtaxapiv <- pivot_longer(catchtaxa,
                             cols = c(Larvae, Pupae, Nymphs, Emergents,
                                      Adults, "NA"),
                             names_to = c("LifeStage"),
                             values_to = c("CountStage"),
                             values_drop_na = TRUE)
#3257 longer?

any <- catchtaxa %>%
  filter(!is.na(TaxonName))
#272 rows without taxon name- note for later

#find a way to not lose taxon name, family, order, count, 
# catchtax <- left_join(tax, catchpivot) %>%
#   select(-c(Kingdom, Phylum, Subphylum, Class, Subclass,
#             Infraclass, Superorder, Suborder, Infraorder, Superfamily,
#             Genus, Species, TaxonRank))

#select only up to 2022 for publishing

#25 lines in samp_catch2 have no taxon name - 2021 and 2022 data., 272 for access data, 297 total






################### Physsamp1 is 1998 through 4/16/2019 - all catch and sample data entered into ACCESS
### phys samp combo for making sure we don't lost jan 2020 dates and late 2019 data
phys_samp1 <- left_join(phys, samp, by = "PhysicalDataID") %>%
  filter(!is.na(PhysicalDataID)) %>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x")) %>%
  rename(ConditionCode = "ConditionCode.y",
         FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y", 
         FieldComments = "FieldComments.y",
         FlowMeterSpeed = "FlowMeterSpeed.y") %>%
  filter(Date < "2019-04-22")
## this removes data that was entered into excel - the last physical data entered into access was 1/28/20

#################### Physsampleoverlap is sample data that was entered into ACCESS and catch entered into EXCEL
############ 4/22/2019 through 01/28/2020 ############################################
# this is the data from april 2019 to jan 2020
physsampoverlap <- left_join(phys, samp, by = "PhysicalDataID") %>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x")) %>%
  rename(ConditionCode = "ConditionCode.y",
         FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y", 
         FieldComments = "FieldComments.y",
         FlowMeterSpeed = "FlowMeterSpeed.y") %>%
  filter(Date > "2019-04-16" & Date < "2020-02-10")

overlapcatch <- left_join(physsampoverlap, catch2) %>%
  distinct()
########################################################################################

## part 1 adding catch data

# data1 <- left_join(phys_samp1, catchtaxa, by = "InvertDataID", "InvertCode") %>%
#   unique()
# 
# AccessData1 <- left_join(phys_samp1, catchtaxapiv, by = c("InvertDataID", "InvertCode")) %>%
#   unique()
# ###2001 and earlier no taxa information?? find out why expanding so much - life stage pivot not helping
# 
# abc <- AccessData1 %>%
#   filter(is.na(TaxonName))

# # Merge physical data
# check2 <- data1 %>%
#   filter(is.na(TaxonName))


samp_catch1 <- left_join(samp, catchtaxapiv, by = "InvertDataID") %>%
  select(-c(InvertCode.y)) %>%
  rename(InvertCode = "InvertCode.x")

testjoin <- left_join(phys_samp1, samp_catch1, by = "PhysicalDataID")%>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x")) %>%
  rename(FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y",
         FlowMeterSpeed = "FlowMeterSpeed.y",
         ConditionCode = "ConditionCode.y",
         FieldComments = "FieldComments.y")
naa <- testjoin %>%
  filter(is.na(TaxonName))

# taxadata <- left_join(samp_catch_phys0, tax2, by = c("OrganismID", "Order", "Family"))
# checkdata <- taxadata %>%
#   filter(is.na(TaxonName))
#this creates 6791 rows with no taxon name



#figure out how to navigate life stage column for access data
# catchpivot <- pivot_longer(catch,
#                            cols = c(Larvae, Pupae, Nymphs, Emergents,
#                                     Adults, "NA"),
#                            names_to = c("LifeStage"),
#                            values_to = c("CountStage"),
#                            values_drop_na = TRUE)
# 
# checkcount <- catchpivot %>%
#   summarise(CountCheck = sum(CountStage))

samp_catch <- left_join(samp, catchpivot, by = "InvertDataID") %>%
  select(-c(InvertCode.y)) %>%
  rename(InvertCode = "InvertCode.x")

trial <- samp_catch_phys0 %>%
  select(-c(Classification, Order, Family))
#getting rid of columns with na prevents issue of a bunch of NA when joining. This helps prevent loss of info.
trial2 <- left_join(trial, tax) %>%
  select(-c(Kingdom, Phylum, Subphylum, Class, Subclass,
            Infraclass, Superorder, Suborder, Infraorder, Superfamily,
            Genus, Species, TaxonRank))

trial3 <- pivot_longer(trial2,
                       cols = c(Larvae, Pupae, Nymphs, Emergents,
                                Adults, "NA"),
                       names_to = c("LifeStage"),
                       values_to = c("CountStage"),
                       values_drop_na = TRUE) %>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x")) %>%
  rename(ConditionCode = "ConditionCode.y",
         FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y", 
         FieldComments = "FieldComments.y") %>%
  arrange(Datetime)

#number of rows expanded a lot due to duplicates - find a way to manage this






# Merge datasets for CPUE variables
samp_catcht <- left_join(samp, catchtax)

samp_catch <- left_join(samp, catch, by = "InvertDataID") %>%
  select(-c(InvertCode.y)) %>%
  rename(InvertCode = "InvertCode.x")

#rename and remove columns from join

samp_catch_phys00 <-left_join(phys, samp_catch, by = "PhysicalDataID") %>%
  filter(!is.na(Station)) %>%
  filter(Date < "2019-04-22") %>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x", "FieldComments.x", Order, Family)) %>%
  rename(ConditionCode = "ConditionCode.y",
         FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y", 
         FieldComments = "FieldComments.y") %>%
  arrange(Datetime)


samp_catch_phys01 <- left_join(samp_catch_phys00, tax) %>%
  select(-c(Kingdom, Phylum, Subphylum, Class, Subclass,
            Infraclass, Superorder, Suborder, Infraorder, Superfamily,
            Genus, Species, TaxonRank))


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

#invert code has no data here. 

gap <- left_join(phys_samp, catch2019)
#seems to take care of missing 2019 catch data...next, how to combine? 

#jan thru 4/23/2019 is in access - the rest is in excel
accessoverlap <- gap %>%
  filter(Date < "2019-04-22") %>%
  select(-c(InvertCode, SampleVolume, SampleID, Attribute, TaxonName, Classification, Count, 
            LifeStage, wet_mass, dry_mass, LabComments, condition))

accessoverlapcatch <- left_join(accessoverlap, catchtax)

checksampphys <- samp_catch_phys0 %>%
  filter(!is.na(FieldComments.y))



samp_catch_phys0 <- samp_catch_phys00 %>%
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

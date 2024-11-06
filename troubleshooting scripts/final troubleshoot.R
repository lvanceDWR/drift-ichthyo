#another test

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
inv_tax_2 <- read_csv("drift data/TblInvertsLookUpV2.csv")


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

## taxa for access
inv_tax <- full_join(tax, inv_tax_2, by = "OrganismID") %>%
  relocate(Kingdom, "Phylum.x", "Phylum.y", Subphylum, "Class.x", "Class.y",
           "Order.x", "Order.y", "Family.x", "Family.y")

taxaccess <- inv_tax %>%
  select(c(Genus, "Species.x", CommonName, OrganismID, TaxonName, TaxonRank, Category)) %>%
  rename(Species = "Species.x") %>%
  relocate(OrganismID, Category, TaxonName, TaxonRank, CommonName, Genus, Species) %>%
  select(-c(Genus, Species))

# change date time variables and format

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

############################ EXCEL Data #######################################################
#first create event_id
catch2 <- catch2 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

samp2 <- samp2 %>%
  mutate(event_id = paste0(Station, "_", Datetime)) %>%
  relocate(event_id, Datetime)

#join data frames and select for data through end of 2022
samp_catch2 <- left_join(samp2, catch2) %>%
  filter(Date < "2023-01-01") %>%
  arrange(Datetime)
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
########################################################################################################

####################################### ACCESS DATA #####################################################
samp_catch <- left_join(samp, catch)

#rename and remove columns from join
# 04-22-2019 onward was entered into Excel, not Access
samp_catch_phys0 <- left_join(phys, samp_catch, by = "PhysicalDataID") %>%
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
         FieldComments = "FieldComments.y")

# For second part 2019, merge phys-samp, then add catch.
# For the additional data

catch2019 <- catch2 %>%
  filter(Date > "2019-04-10" & Date < "2020-02-01")

phys2019 <- phys %>%
  filter(Date > "2019-04-20" & Date < "2020-02-10") %>%
  select(-c(FieldComments))
#fieldcomments removed for 2019 since all NA and the comments are in a diff comments column

phys_samp <- left_join(phys2019, samp, by = "PhysicalDataID")%>%
  select(-c("ConditionCode.x", "MeterSetTime", "FlowMeterStart.x", "FlowMeterEnd.x",
            "FlowMeterSpeed.x")) %>%
  rename(FlowMeterStart = "FlowMeterStart.y",
         FlowMeterEnd = "FlowMeterEnd.y",
         FlowMeterSpeed = "FlowMeterSpeed.y",
         ConditionCode = "ConditionCode.y") %>%
  arrange(Datetime)

gap <- left_join(phys_samp, catch2019) %>%
  select(-(SampleID))

#combine the 2019 and Jan 2020 data with the rest of the Excel data
samp_catch_physA <- bind_rows(samp_catch_phys2, gap) %>%
  select(-c(Attribute)) %>%
  arrange(Datetime)

#### add taxon data to access data before combining with excel info
samp_catch_physC <- samp_catch_phys0 %>%
  select(-c(Classification, Order, Family))

samp_catch_physD <- left_join(samp_catch_physC, taxaccess, by = "OrganismID") %>%
  rename(NotDetermined = "NA",
         TotalCount = "Count") %>%
  arrange(Datetime)

Nolifestage <- samp_catch_physD %>%
  filter(is.na(Larvae) | is.na(Pupae) | is.na(Nymphs) |
           is.na(Emergents) | is.na(Adults) | is.na(NotDetermined)) %>%
  filter(Date < "2005-02-01") %>%
  select(-c(Larvae, Pupae, Nymphs, Emergents, 
            Adults, NotDetermined)) %>%
  rename(Count = "TotalCount") %>%
  arrange(Datetime)

samp_catch_physB <- pivot_longer(samp_catch_physD,
                                 cols = c(Larvae, Pupae, Nymphs, Emergents,
                                          Adults, NotDetermined),
                                 names_to = c("LifeStage"),
                                 values_to = c("Count"),
                                 values_drop_na = TRUE) %>%
  arrange(Datetime) %>%
  unique()
#################################################################################


#recombine Access data
samp_catch_physAB <- bind_rows(Nolifestage, samp_catch_physB) %>%
  unique()


#Now combine  with access data and then continue with qa/qc
sampcatchphysMerge <- bind_rows(samp_catch_physAB, samp_catch_phys2) %>%
  relocate(event_id, Datetime)



#####################################################################################################
#create exported joined data file for ease of publishing data in future
write_csv(sampcatchphysMerge, paste("R_write/Sampling_Drift1998-2022.csv"))

# All samplings - remove catch info and find unique entries
sampUnique <- sampcatchphysMerge %>%
  unique() %>%
  arrange(Datetime)
##################################################################################################


sampA <- sampcatchphysMerge %>%
  select(-c(WeatherCode:Turbidity, Year:Comment_PQC,  TaxonName:LifeStage)) %>%
  unique() %>%
  arrange(Datetime)


samp_catch_physMerge <- sampcatchphysMerge %>%
  mutate(WY = ifelse(Month >9, Year + 1, Year)) %>%
  left_join(wy, by = "WY") %>%
  select(-c(Index, WYType)) %>%
  mutate(Flowdiff = FlowMeterEnd-FlowMeterStart)


SamplingQAQC <- filter(sampUnique, !is.na(FieldComments) | ConditionCode>1 | !is.na(LabComments))
SamplingQAQC$Flag_SAMP <-  ""
SamplingQAQC$Comment_SAMP <-""
SamplingQAQC$Flag_LAB <- ""
SamplingQAQC$Comment_LAB <- ""
today <- today()
write.csv(SamplingQAQC, paste("R_write/SamplingQAQC_", today, ".csv"))

SamplingQAQC_fill <- read.csv("R_write/SamplingQAQC_ Notated_20240430 .csv")
SamplingQAQC_fill_s <- SamplingQAQC_fill %>%
  select(c(event_id, PhysicalDataID, Flag_SAMP, Comment_SAMP, Flag_LAB, Comment_LAB))



inundation <- inundation %>%
  mutate(Month = month(Date),
         Year = year(Date),
         WY = ifelse(Month > 9, Year + 1, Year))

# Modify sample table to include Flowdiff
samp3 <-sampUnique %>%
  mutate(Flowdiff = FlowMeterEnd-FlowMeterStart,
         Flowdiff_s = Flowdiff/SetTime) 

# merged inundation and sampling
inundation_flow <- left_join(samp3, inundation, by = "Date") %>%
  filter(Flowdiff<100000) %>%
  filter(Station == "STTD") %>%
  mutate(Flowdiff_s = Flowdiff/SetTime,
         Inundation_n = ifelse(Inundation == "TRUE", 40000, 0))

# all inundation
inundation2 <- inundation %>%
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
  geom_col(data = inundation2, aes(x = Date, y = Inundation_n), fill = 
             "blue", alpha = 0.6) + theme_bw()
ggplotly(inplot2)

inplot3 <- ggplot() +
  geom_point(data = inundation_flow, aes(x = Date, y = Flowdiff_s)) +
  labs(title = "Standardized Flowdiff with all Inundation") +
  geom_col(data = inundation2, aes(x = Date, y = Inundation_n_low), fill = 
             "blue", alpha = 0.6) + theme_bw()

grid.arrange(inplot1, inplot2, inplot3, nrow = 3)


# Allow 10 days after last "inundation=TRUE" to also count as inundated. Merge with sample data. 
inundation4 <- inundation %>%
  mutate(Inundation2 = ifelse(lead(Inundation, 10) == "TRUE", "TRUE", Inundation)) %>%
  select(c(Date, Month:Inundation2))

samp_catch_phys <- left_join(samp_catch_physMerge, inundation4)


#############

samp_catch_phys$Month <- ordered(samp_catch_phys$Month)
FlowHist <- ggplot(samp_catch_phys, aes(Flowdiff)) + geom_histogram() +
  facet_wrap(~Station) + theme_bw()

# SHR vs STTD boxplot
FlowBox <- ggplot(samp_catch_phys) + geom_boxplot(aes(x = Station, y = Flowdiff)) + theme_bw()

# SHR vs STTD boxplot by month
FlowBoxMonth <- ggplot(samp_catch_phys) + geom_boxplot(aes(x = Month, y = Flowdiff, fill = Month)) + facet_wrap(~Station) + scale_fill_viridis(discrete = TRUE) + theme_bw() 

# SHR vs STTD boxplot by year
FlowBoxYear <- ggplot(samp_catch_phys) + geom_boxplot(aes(x = ordered(WY), y = Flowdiff, fill = ordered(WY))) + facet_wrap(~Station) + theme_bw() 

# SetTime vs Flowdiff by station and month
FlowPoint <- ggplot(samp_catch_phys, aes(x = SetTime, y = Flowdiff, color = Month)) + geom_point(size = 3) + facet_wrap(~Station) + theme_bw()

# SetTime vs Flowdiff by station and flowmeter type
FlowPoint2 <- ggplot(samp_catch_phys, aes(x = SetTime, y = Flowdiff, color = FlowMeterSpeed)) + geom_point(size = 3) + facet_wrap(~Station) + theme_bw() + scale_color_viridis(discrete = TRUE) 


grid.arrange(FlowBox, FlowHist)
grid.arrange(FlowBoxMonth, FlowBoxYear)
grid.arrange(FlowPoint, FlowPoint2)
###########################################





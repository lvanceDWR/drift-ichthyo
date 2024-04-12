



field_dat_50$flowdiff <- abs(field_dat_50$x50_zoop_start_meter- field_dat_50$x50_zoop_end_meter)





samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2020-11-30 10:13:00"] <- 598353
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2020-11-30 10:13:00"] <- 603427
samp_catch_phys$Flowdiff[samp_catch_phys$event_id== "SHR_2020-11-30 10:13:00"] <- 603427-598353

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-04-27 09:33:00"]<- 987030
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-04-27 09:33:00"]<- 992157
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-04-27 09:33:00"]<- 992157-987030

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-05-24 09:44:00"]<-962445
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-05-24 09:44:00"]<-974411
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-05-24 09:44:00"]<-974411-962445

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-05-25 09:16:00"]<-957174
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-05-25 09:16:00"]<-961755
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-05-25 09:16:00"]<-961755-957174

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-06-07 09:02:00"]<-946267
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-06-07 09:02:00"]<-957000
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-06-07 09:02:00"]<-957000-946267

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-06-09 09:37:00"]<-945753
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-06-09 09:37:00"]<-946108
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-06-09 09:37:00"]<-946108-945753

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-06-21 08:58:00"]<-934553
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-06-21 08:58:00"]<-945578
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-06-21 08:58:00"]<-945578-934553

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-06-22 09:17:00"]<-930003
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-06-22 09:17:00"]<-934500
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-06-22 09:17:00"]<-934500-930003

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-07-06 09:03:00"]<-925636
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-07-06 09:03:00"]<-929896
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-07-06 09:03:00"]<-929896-925636

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-08-31 08:06:00"]<-839104
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-08-31 08:06:00"]<-849068
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-08-31 08:06:00"]<-849068-839104

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-08-31 11:06:00"]<-834952
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-08-31 11:06:00"]<-838900
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-08-31 11:06:00"]<-838900-834952

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-09-14 07:51:00"]<-825385
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-09-14 07:51:00"]<-834847
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-09-14 07:51:00"]<-834847-825385

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-09-14 11:34:00"]<-818234
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-09-14 11:34:00"]<-825276
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-09-14 11:34:00"]<-825276-818234

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-09-28 08:10:00"]<-808333
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-09-28 08:10:00"]<-818171
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-09-28 08:10:00"]<-818171-808333

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-09-28 11:25:00"]<-805353
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-09-28 11:25:00"]<-808080
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-09-28 11:25:00"]<-808080-805353

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-10-12 08:24:00"]<-795151
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-10-12 08:24:00"]<-805190
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-10-12 08:24:00"]<-805190-795151  

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-10-13 11:07:00"]<-788937
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-10-13 11:07:00"]<-794960
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-10-13 11:07:00"]<- 794960-788937 

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-10-18 08:23:00"]<-779507
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-10-18 08:23:00"]<-788877
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-10-18 08:23:00"]<-788877-779507

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-10-19 08:13:00"]<-775811
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-10-19 08:13:00"]<-779403
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-10-19 08:13:00"]<-779403-775811

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-11-15 10:05:00"]<-757646
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-11-15 10:05:00"]<-764877
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-11-15 10:05:00"]<-764877-757646

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-11-16 09:48:00"]<-754627
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-11-16 09:48:00"]<-757538
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-11-16 09:48:00"]<-757538-754627

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-11-29 09:19:00"]<-749206
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-11-29 09:19:00"]<-754562
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-11-29 09:19:00"]<-754562-749206

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-11-30 09:30:00"]<-745847
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-11-30 09:30:00"]<-749000
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-11-30 09:30:00"]<-749000-745847

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-12-13 09:21:00"]<-741150
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-12-13 09:21:00"]<-745711
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-12-13 09:21:00"]<-745711-741150

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-12-14 10:25:00"]<-737932
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-12-14 10:25:00"]<-741067
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-12-14 10:25:00"]<-741067-737932

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2021-12-27 10:18:00"]<-734515
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2021-12-27 10:18:00"]<-736997
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2021-12-27 10:18:00"]<-736997-734515

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2021-12-28 10:48:00"]<-731561
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2021-12-28 10:48:00"]<-734447
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2021-12-28 10:48:00"]<-734447-731561

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2022-01-11 09:14:00"]<-724454
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2022-01-11 09:14:00"]<-729502
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2022-01-11 09:14:00"]<-729502-724454 

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2022-01-10 10:54:00"]<-729767
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2022-01-10 10:54:00"]<-731057
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2022-01-10 10:54:00"]<-731057-729767   

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2022-01-25 09:28:00"]<-713547
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2022-01-25 09:28:00"]<-718350
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2022-01-25 09:28:00"]<-718350-713547

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="STTD_2022-01-24 12:33:00"]<-720225
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="STTD_2022-01-24 12:33:00"]<-724020
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="STTD_2022-01-24 12:33:00"]<-724020-720225  

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id=="SHR_2022-02-08 09:38:00"]<-703484
samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id=="SHR_2022-02-08 09:38:00"]<-708021
samp_catch_phys$Flowdiff[samp_catch_phys$event_id=="SHR_2022-02-08 09:38:00"]<-708021-703484

samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id==""]<-
  samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id==""]<-
  samp_catch_phys$Flowdiff[samp_catch_phys$event_id==""]<-   
  
  samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id==""]<-
  samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id==""]<-
  samp_catch_phys$Flowdiff[samp_catch_phys$event_id==""]<-   
  
  samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id==""]<-
  samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id==""]<-
  samp_catch_phys$Flowdiff[samp_catch_phys$event_id==""]<-   
  
  samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id==""]<-
  samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id==""]<-
  samp_catch_phys$Flowdiff[samp_catch_phys$event_id==""]<-   
  
  samp_catch_phys$FlowMeterStart[samp_catch_phys$event_id==""]<-
  samp_catch_phys$FlowMeterEnd[samp_catch_phys$event_id==""]<-
  samp_catch_phys$Flowdiff[samp_catch_phys$event_id==""]<-   
  
  
  
  
  
  
  
  
  
  #fix the same flow meter values in samp3
  
samp3$FlowMeterStart[samp3$PhysicalDataID==1727] <- 573926
samp3$FlowMeterEnd[samp3$PhysicalDataID==1727] <- 581200
samp3$Flowdiff[samp3$PhysicalDataID==1727] <- 581200-573926

samp3$FlowMeterStart[samp3$InvertDataID==978] <- 788767
samp3$FlowMeterEnd[samp3$InvertDataID==978] <- 	795100
samp3$Flowdiff[samp3$InvertDataID==978] <- 795100-788767

samp3$FlowMeterStart[samp3$event_id=="SHR_2020-11-30 10:13:00"] <- 598353
samp3$FlowMeterEnd[samp3$event_id=="SHR_2020-11-30 10:13:00"] <- 603427
samp3$Flowdiff[samp3$event_id== "SHR_2020-11-30 10:13:00"] <- 603427-598353

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-04-27 09:33:00"]<- 987030
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-04-27 09:33:00"]<- 992157
samp3$Flowdiff[samp3$event_id=="STTD_2021-04-27 09:33:00"]<- 992157-987030

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-05-24 09:44:00"]<-962445
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-05-24 09:44:00"]<-974411
samp3$Flowdiff[samp3$event_id=="SHR_2021-05-24 09:44:00"]<-974411-962445

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-05-25 09:16:00"]<-957174
samp3$FlowMeterEnd[samp3s$event_id=="STTD_2021-05-25 09:16:00"]<-961755
samp3$Flowdiff[samp3$event_id=="STTD_2021-05-25 09:16:00"]<-961755-957174

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-06-07 09:02:00"]<-946267
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-06-07 09:02:00"]<-957000
samp3$Flowdiff[samp3$event_id=="SHR_2021-06-07 09:02:00"]<-957000-946267

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-06-09 09:37:00"]<-945753
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-06-09 09:37:00"]<-946108
samp3$Flowdiff[samp3$event_id=="STTD_2021-06-09 09:37:00"]<-946108-945753

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-06-21 08:58:00"]<-934553
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-06-21 08:58:00"]<-945578
samp3$Flowdiff[samp3$event_id=="SHR_2021-06-21 08:58:00"]<-945578-934553

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-06-22 09:17:00"]<-930003
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-06-22 09:17:00"]<-934500
samp3$Flowdiff[samp3$event_id=="STTD_2021-06-22 09:17:00"]<-934500-930003

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-07-06 09:03:00"]<-925636
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-07-06 09:03:00"]<-929896
samp3$Flowdiff[samp3$event_id=="STTD_2021-07-06 09:03:00"]<-929896-925636

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-08-31 08:06:00"]<-839104
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-08-31 08:06:00"]<-849068
samp3$Flowdiff[samp3$event_id=="SHR_2021-08-31 08:06:00"]<-849068-839104

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-08-31 11:06:00"]<-834952
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-08-31 11:06:00"]<-838900
samp3$Flowdiff[samp3$event_id=="STTD_2021-08-31 11:06:00"]<-838900-834952

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-09-14 07:51:00"]<-825385
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-09-14 07:51:00"]<-834847
samp3$Flowdiff[samp3$event_id=="SHR_2021-09-14 07:51:00"]<-834847-825385

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-09-14 11:34:00"]<-818234
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-09-14 11:34:00"]<-825276
samp3$Flowdiff[samp3$event_id=="STTD_2021-09-14 11:34:00"]<-825276-818234

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-09-28 08:10:00"]<-808333
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-09-28 08:10:00"]<-818171
samp3$Flowdiff[samp3$event_id=="SHR_2021-09-28 08:10:00"]<-818171-808333

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-10-12 08:24:00"]<-795151
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-10-12 08:24:00"]<-805190
samp3$Flowdiff[samp3$event_id=="SHR_2021-10-12 08:24:00"]<-805190-795151

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-10-13 11:07:00"]<-788937
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-10-13 11:07:00"]<-794960
samp3$Flowdiff[samp3$event_id=="STTD_2021-10-13 11:07:00"]<- 794960-788937

samp3$FlowMeterStart[samp3ys$event_id=="SHR_2021-10-18 08:23:00"]<-779507
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-10-18 08:23:00"]<-788877
samp3$Flowdiff[samp3$event_id=="SHR_2021-10-18 08:23:00"]<-788877-779507

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-10-19 08:13:00"]<-775811
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-10-19 08:13:00"]<-779403
samp3$Flowdiff[samp3$event_id=="STTD_2021-10-19 08:13:00"]<-779403-775811

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-11-15 10:05:00"]<-757646
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-11-15 10:05:00"]<-764877
samp3$Flowdiff[samp3$event_id=="SHR_2021-11-15 10:05:00"]<-764877-757646

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-11-16 09:48:00"]<-754627
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-11-16 09:48:00"]<-757538
samp3$Flowdiff[samp3$event_id=="STTD_2021-11-16 09:48:00"]<-757538-754627

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-11-29 09:19:00"]<-749206
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-11-29 09:19:00"]<-754562
samp3$Flowdiff[samp3$event_id=="SHR_2021-11-29 09:19:00"]<-754562-749206

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-12-13 09:21:00"]<-741150
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-12-13 09:21:00"]<-745711
samp3$Flowdiff[samp3$event_id=="SHR_2021-12-13 09:21:00"]<-745711-741150

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-12-14 10:25:00"]<-737932
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-12-14 10:25:00"]<-741067
samp3$Flowdiff[samp3$event_id=="STTD_2021-12-14 10:25:00"]<-741067-737932

samp3$FlowMeterStart[samp3$event_id=="SHR_2021-12-27 10:18:00"]<-734515
samp3$FlowMeterEnd[samp3$event_id=="SHR_2021-12-27 10:18:00"]<-736997
samp3$Flowdiff[samp3$event_id=="SHR_2021-12-27 10:18:00"]<-736997-734515

samp3$FlowMeterStart[samp3$event_id=="STTD_2021-12-28 10:48:00"]<-731561
samp3$FlowMeterEnd[samp3$event_id=="STTD_2021-12-28 10:48:00"]<-734447
samp3$Flowdiff[samp3$event_id=="STTD_2021-12-28 10:48:00"]<-734447-731561

samp3$FlowMeterStart[samp3$event_id=="SHR_2022-01-11 09:14:00"]<-724454
samp3$FlowMeterEnd[samp3$event_id=="SHR_2022-01-11 09:14:00"]<-729502
samp3$Flowdiff[samp3$event_id=="SHR_2022-01-11 09:14:00"]<-729502-724454 

samp3$FlowMeterStart[samp3$event_id=="STTD_2022-01-10 10:54:00"]<-729767
samp3$FlowMeterEnd[samp3$event_id=="STTD_2022-01-10 10:54:00"]<-731057
samp3$Flowdiff[samp3$event_id=="STTD_2022-01-10 10:54:00"]<-731057-729767 

samp3$FlowMeterStart[samp3$event_id=="SHR_2022-01-25 09:28:00"]<-713547
samp3$FlowMeterEnd[samp3$event_id=="SHR_2022-01-25 09:28:00"]<-718350
samp3$Flowdiff[samp3$event_id=="SHR_2022-01-25 09:28:00"]<-718350-713547

samp3$FlowMeterStart[samp3$event_id=="STTD_2022-01-24 12:33:00"]<-720225
samp3$FlowMeterEnd[samp3$event_id=="STTD_2022-01-24 12:33:00"]<-724020
samp3$Flowdiff[samp3$event_id=="STTD_2022-01-24 12:33:00"]<-724020-720225

samp3$FlowMeterStart[samp3$event_id=="SHR_2022-02-08 09:38:00"]<-703484
samp3$FlowMeterEnd[samp3$event_id=="SHR_2022-02-08 09:38:00"]<-708021
samp3$Flowdiff[samp3$event_id=="SHR_2022-02-08 09:38:00"]<-708021-703484
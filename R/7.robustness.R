##################################################################
# Read data 
##################################################################

# 1. Update the FEWS IPC data to reflect the corrected values for the zones
# (My hunch is this will make the IPC perform worse, and variation in the IPC fall).
rm(list=ls())
require(tidyverse)
library(caret)


# T-1 price 

mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

# fs.year.month = mw.hh %>% distinct(FS_year,FS_month,ea_id)
# nrow(fs.year.month)

# Define train and test based on years 
mw.cluster$FS_month = as.integer(mw.cluster$FS_month)
mw.cluster.ipc1 = mw.cluster %>% 
  dplyr::filter(!is.na(IPC1))


mw.2010.cluster= mw.cluster.ipc1 %>% 
  dplyr::filter(FS_year==2010|FS_year==2011) 

mw.2013.cluster= mw.cluster.ipc1 %>% 
  dplyr::filter(FS_year==2013)  


# 3. Estimate quarter fixed effects. (R1)
lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +   floodmax + maxdaysnorain+
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2+quarter1+quarter2+quarter3 , data = mw.2010.cluster, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.2013.cluster, se.fit = TRUE)


# cor(predicted.logFCS, mw.2013.cluster$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = mw.2013.cluster$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+ raincytot + day1rain +     floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3, data = mw.2010.cluster, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.2013.cluster, se.fit = TRUE)

cor(predicted.HDDS, mw.2013.cluster$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3, data = mw.2010.cluster, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.2013.cluster, se.fit = TRUE)

cor(predicted.rCSI, mw.2013.cluster$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = mw.2013.cluster$rCSI)

 

################################################################################
# 4. Relatedly, perhaps estimate month fixed effects without prices, 
################################################################################

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +   floodmax + maxdaysnorain+
                   # clust_maize_price +  clust_maize_mktthin +
                   percent_ag + elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2+
                   # quarter1+quarter2+quarter3 +
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11, data = mw.2010.cluster, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.2013.cluster, se.fit = TRUE)


# cor(predicted.logFCS, mw.2013.cluster$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = mw.2013.cluster$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+ raincytot + day1rain +     floodmax + maxdaysnorain+
                 #clust_maize_price +  clust_maize_mktthin +
                 percent_ag + elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+
                 date +
                 # quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11, data = mw.2010.cluster, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.2013.cluster, se.fit = TRUE)

cor(predicted.HDDS, mw.2013.cluster$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 # clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+
                 # quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11, data = mw.2010.cluster, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.2013.cluster, se.fit = TRUE)

cor(predicted.rCSI, mw.2013.cluster$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = mw.2013.cluster$rCSI)




#####################################################################################
# 5. swap in GIEWS prices for 4 markets instead of our good price data (R2)
#####################################################################################
# Read in GIEWS 

library(readr)
GIEW_malawi <- read_csv("data/raw/price/GIEW_malawi.csv")


colnames(GIEW_malawi)


market.address =  unique(GIEW_malawi$Market)[1:5]
market.address[6] = unique(GIEW_malawi$Market)[7]

source("R/functions/GoogleMapApi.R")
# map.key = "YOUR KEY HERE"

market.coord = coordFind(market.address)
write.csv("data/")

source("R/functions/MktNearCluster.R")

market.coord = market.coord %>% dplyr::mutate(mkt = search) %>% dplyr::select(mkt,lat,lon)

Malawi_coord <- read_csv("data/clean/concordance/Malawi_coord.csv")

near.mkt = MktNearCluster(Malawi_coord,market.coord) 

near.mkt  = near.mkt[!duplicated(near.mkt$ea_id),] %>% 
  distinct(ea_id,near_mkt)
  
  
cluster.yearmon = mw.cluster%>% dplyr::distinct(ea_id,FS_month,FS_year) 

# Join cluster and nearby market 
cluster.yearmon.mkt = left_join(cluster.yearmon,near.mkt,by="ea_id")




source("R/functions/Yearmon.R")

cluster.yearmon.date  = yearmon(df = cluster.yearmon.mkt,year_var = "FS_year",month_var = "FS_month")

cluster.yearmon.date = cluster.yearmon.date %>% distinct(ea_id,date,near_mkt)


# Join price based on date and market 
colnames(GIEW_malawi)[9]="real_price"

library(imputeTS)
GIEW.price = GIEW_malawi %>% 
  dplyr::filter(Commodity == "Maize") %>%
  dplyr::select(Market,Date,real_price) %>%
  dplyr::filter(Market!="National Average") %>% 
  group_by(Market) %>% 
  dplyr::mutate( real_price = na.kalman(real_price))

GIEW.price.join = GIEW.price %>% 
  ungroup() %>%
  dplyr::mutate(mkt = Market) %>% 
  dplyr::mutate(date = as.Date(Date, "%m/%d/%Y")) %>%
  dplyr::distinct(mkt,date,real_price)


 
GIEW.cluster.joined= left_join (cluster.yearmon.date,GIEW.price.join,by = c("near_mkt"="mkt","date"="date"))

GIEW.cluster.joined = GIEW.cluster.joined %>% 
  mutate( GIEW_price= real_price ) %>%
  distinct(ea_id,date,GIEW_price)
  
  

mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

mw.cluster.date =  yearmon(df = mw.cluster,year_var = "FS_year",month_var = "FS_month")

mw.cluster.date.GIEW = left_join(mw.cluster.date, GIEW.cluster.joined, by=c("date"="date","ea_id"="ea_id"))
################################################################################
# 6. Estimate (back of the envelope?) 

# spatial temporal variation between our good price data and GIEWS prices
################################################################################

cor(mw.cluster.date.GIEW$clust_maize_price,mw.cluster.date.GIEW$GIEW_price)

# change figure s1 to add the GIEWS MARKETS 

mw.1011 =mw.cluster.date.GIEW %>% dplyr::filter(FS_year<2013) 
mw.13 =mw.cluster.date.GIEW %>% dplyr::filter(FS_year==2013) 


ggplot(mw.1011,aes( x=date, y = clust_maize_price)) + 
  geom_line()

ggplot(mw.1011,aes( x=date, y = GIEW_price)) + 
  geom_line()

ggplot(mw.13,aes( x=date, y = clust_maize_price)) + 
  geom_line()

ggplot(mw.13,aes( x=date, y = GIEW_price)) + 
  geom_line()


mw.2010.cluster = mw.cluster.date.GIEW %>% dplyr::filter(FS_year<2013) %>%  dplyr::filter(!is.na(IPC1))
mw.2013.cluster = mw.cluster.date.GIEW %>% dplyr::filter(FS_year==2013) %>%  dplyr::filter(!is.na(IPC1))


lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +   floodmax + maxdaysnorain+
                   # clust_maize_price +  clust_maize_mktthin +
                   percent_ag + elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2+
                   # quarter1+quarter2+quarter3 +
                   # month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+month10+month11
                   GIEW_price,data = mw.2010.cluster, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.2013.cluster, se.fit = TRUE)


# cor(predicted.logFCS, mw.2013.cluster$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = mw.2013.cluster$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+ raincytot + day1rain +     floodmax + maxdaysnorain+
                 #clust_maize_price +  clust_maize_mktthin +
                 percent_ag + elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+
                 # quarter1+quarter2+quarter3+
                 # month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+month10+month11
                 GIEW_price,data = mw.2010.cluster, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.2013.cluster, se.fit = TRUE)

cor(predicted.HDDS, mw.2013.cluster$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 # clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+
                 FNID_MW2012C3030306+ FNID_MW2012C3030613 +
                 # quarter1+quarter2+quarter3+
                 # month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+month10+month11
                 GIEW_price,data = mw.2010.cluster, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.2013.cluster, se.fit = TRUE)

cor(predicted.rCSI, mw.2013.cluster$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = mw.2013.cluster$rCSI)





################################################################################
# 6. Lasso (R1)
################################################################################


##################################################################
# Split by year, LASSO
##################################################################


lambda <- 10^seq(-3, 3, length = 100)

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                   clust_maize_price +  clust_maize_mktthin + 
                   percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender + asset_index2+quarter1+quarter2+quarter3+
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11+ month12, data = mw.2010.cluster.ipc1, method = "glmnet",trControl = trainControl("cv", number = 10),
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.logFCS = predict(lm.logFCS, mw.2013.cluster, se.fit = TRUE)


# cor(predicted.logFCS, mw.2013.cluster$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = mw.2013.cluster$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.2010.cluster, method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.HDDS = predict(lm.HDDS, mw.2013.cluster, se.fit = TRUE)

cor(predicted.HDDS, mw.2013.cluster$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.2010.cluster,method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.rCSI = predict(lm.rCSI, mw.2013.cluster, se.fit = TRUE)


cor(predicted.rCSI, mw.2013.cluster$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = mw.2013.cluster$rCSI)





###########################################
##  Split randomly,OLS
###########################################

# 2. Split the data geographically (R1) rather than temporally for model building.

mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

# fs.year.month = mw.hh %>% distinct(FS_year,FS_month,ea_id)
# nrow(fs.year.month)


mw.cluster$FS_month = as.integer(mw.cluster$FS_month)

mw.cluster.ipc1 = mw.cluster %>% 
  dplyr::filter(!is.na(IPC1))

mw.cluster.ipc1 = mw.cluster.ipc1 %>% 
  mutate( quarter1 = if_else( FS_month<4,1,0 ) ) %>%
  mutate( quarter2 = if_else( FS_month>4 & FS_month<7,1,0 ) ) %>%
  mutate( quarter3 = if_else( FS_month>6 & FS_month<10,1,0 ) ) %>%
  mutate( quarter4 = if_else( FS_month>9,1,0 ) ) %>%
  mutate( month1 = if_else( FS_month==1,1,0 ) ) %>%
  mutate( month2 = if_else( FS_month==2,1,0 ) ) %>%
  mutate( month3 = if_else( FS_month==3,1,0 ) ) %>%
  mutate( month4 = if_else( FS_month==4,1,0 ) ) %>%
  mutate( month5 = if_else( FS_month==5,1,0 ) ) %>%
  mutate( month6 = if_else( FS_month==6,1,0 ) ) %>%
  mutate( month7 = if_else( FS_month==7,1,0 ) ) %>%
  mutate( month8 = if_else( FS_month==8,1,0 ) ) %>%
  mutate( month9 = if_else( FS_month==9,1,0 ) ) %>%
  mutate( month10 = if_else( FS_month==10,1,0 ) ) %>%
  mutate( month11 = if_else( FS_month==11,1,0 ) ) %>%
  mutate( month12 = if_else( FS_month==12,1,0 ) )  

trainIndex <- createDataPartition(mw.cluster.ipc1$FCS, p = .7, 
                                  list = FALSE, 
                                  times = 1)

mw.cluster_Train <- mw.cluster.ipc1[ trainIndex,]
mw.cluster_Test  <- mw.cluster.ipc1[-trainIndex,]

######################################################
# random OLS
####################################################
lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)



lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)



######################################################
# random OLS + quarter
####################################################

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2+quarter1+quarter2+quarter3, data = mw.cluster_Train, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2+quarter1+quarter2+quarter3, data = mw.cluster_Train, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)



lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2+quarter1+quarter2+quarter3, data = mw.cluster_Train, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)




######################################################
# random OLS + month
####################################################

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                   #clust_maize_price +  clust_maize_mktthin + 
                   percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender + asset_index2+quarter1+quarter2+quarter3+
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11+ month12, data = mw.cluster_Train, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                # clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)



lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                # clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)









###########################################
##  Split randomly,lasso
###########################################

lambda <- 10^seq(-3, 3, length = 100)

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                   clust_maize_price +  clust_maize_mktthin + 
                   percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender + asset_index2+quarter1+quarter2+quarter3+
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11+ month12, data = mw.cluster_Train,  method = "glmnet",trControl = trainControl("cv", number = 10),
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train,  method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)


lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train, method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)


# regularization penalty.

lm.rCSI$bestTune$lambda

coef(lm.HDDS$finalModel,lm.HDDS$bestTune$lambda)

coef(lm.rCSI$finalModel,lm.rCSI$bestTune$lambda)








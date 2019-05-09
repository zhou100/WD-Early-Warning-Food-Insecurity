####################################################################################################################################
# Goal : This script aims to put together the houhsehold, price and weather data. 

# purpose: 
# 1. generate variables at the household level to be used in the regression 
# 2. generate cluster geovariables used to extract weather and match with prices 

# Input : 
# 1. csv file of cleaned lsms household survey   dataset

# Output: 
# 0. cluster geovariables 

# Yujun Zhou -  04/18/18
###################################################################


require(tidyverse)
library(zoo)
library(imputeTS)

####################################################
### Merge Malawi Data 
####################################################
rm(list = ls())

# read household data 
mw.lsms = read.csv("data/clean/MW_household.csv",stringsAsFactors = FALSE)

concoord_lhz_ea = mw.lsms %>% distinct(FNID,ea_id)


# Locate ipczones where we don't have IPC values 
# library(readxl)
# FEWS_IPC <- read_excel("data/raw/IPC_value/FEWS NET_MW_IPC_data_merge.xlsx",sheet = "Common Unit", skip = 2)
# FEWS_IPC = FEWS_IPC %>% dplyr::filter(!is.na(FNID_OLD))
# # We have only 39 livelihood zones that have IPC 
# length(unique(FEWS_IPC$FNID_OLD))
#  
# # 18 livelihood zones don't have IPC value
# unique(concoord_lhz_ea$FNID[!(concoord_lhz_ea$FNID  %in% FEWS_IPC$FNID_OLD)])
# 
# # 2 IPC zones we don't have LSMS data (one on the island, one don't have observation)
# FEWS_IPC$FNID_OLD[! (FEWS_IPC$FNID_OLD %in% concoord_lhz_ea$FNID)]

mw.lsms = mw.lsms %>%
  mutate(ea_id = as.character(ea_id)) %>%
  mutate(yearmon = as.yearmon(yearmon)) %>%
  mutate(rural = ifelse(reside=="rural",1,0)) %>%
  mutate(hh_gender = ifelse(hh_gender=="Male",1,2)) 

 
# remove columns that can not be used in the prediction anlaysis 
mw.lsms = mw.lsms %>%
          mutate(ea_id = as.character(ea_id) ) %>%
          dplyr::select(-cellphone_cost,-Reason1,-Reason2,-Reason3,-MAHFP,-hh_a01,-slope,-reside) %>%
          filter(!is.na(FS_year) & !is.na(FCS) & !is.na(rCSI)) 


# check for missing values 
colSums(is.na(mw.lsms))

 

# fill in missing values by the ones in the same year/month and same cluster 
mw.lsms.fill = mw.lsms %>% 
         group_by(ea_id,FS_year,FS_month) %>% 
          mutate(number_celphones= ifelse(is.na(number_celphones), mean(number_celphones, na.rm=TRUE), number_celphones)) %>% 
  mutate(floor_cement= ifelse(is.na(floor_cement), mean(floor_cement, na.rm=TRUE), floor_cement)) %>% 
  mutate(floor_tile= ifelse(is.na(floor_tile), mean(floor_tile, na.rm=TRUE), floor_tile)) %>% 
  mutate(dist_road= ifelse(is.na(dist_road), mean(dist_road, na.rm=TRUE), dist_road)) %>% 
  mutate(dist_popcenter= ifelse(is.na(dist_popcenter), mean(dist_popcenter, na.rm=TRUE), dist_popcenter)) %>% 
  mutate(dist_admarc= ifelse(is.na(dist_admarc), mean(dist_admarc, na.rm=TRUE), dist_admarc)) %>% 
  mutate(percent_ag= ifelse(is.na(percent_ag), mean(percent_ag, na.rm=TRUE), percent_ag)) %>% 
  mutate(elevation= ifelse(is.na(elevation), mean(elevation, na.rm=TRUE), elevation)) %>% 
  mutate(hh_age= ifelse(is.na(hh_age), mean(hh_age, na.rm=TRUE), hh_age)) 

# check if the missing still exist 

colSums(is.na(mw.lsms.fill))

####################################################
# read price data 
####################################################

load("data/clean/market/mw_price_final.RData")

colnames(mw_price_merge_final) 

# mw_price_merge_final

mw_price_merge_final$yearmon = as.yearmon(mw_price_merge_final$yearmon)
# class(mw_price_merge_final$ea_id)

mw.current.price = mw_price_merge_final %>% 
  dplyr::select(-mkt,-dist_km,-weights,-FNID) %>% 
  distinct() %>% 
  group_by(ea_id,yearmon) %>% 
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))

library(imputeTS)


mw.current.price.impute = mw.current.price %>% 
                          mutate(year=format(date,"%Y")) %>% 
                          group_by(ea_id,year) 


for ( index in  4:ncol(mw.current.price.impute)-1) {
  col.name.temp = colnames(mw.current.price.impute)[index]
  mw.current.price.impute[col.name.temp] = na.interpolation(unlist(mw.current.price[col.name.temp]),option = "stine")
}

mw.lsms.fill  = mw.lsms.fill %>% mutate(yearmon = as.character(yearmon))


# select only maize price variables 
mw.current.price.impute.join  = mw.current.price.impute %>% 
  mutate(yearmon = as.character(yearmon)) %>%
  mutate(clust_maize_current = clust_maize_price) %>%
  mutate(lhz_maize_current = lhz_maize_price) %>%
  mutate(clust_maize_mktthin_current = clust_maize_mktthin) %>%
  mutate(lhz_maize_mktthin_current = lhz_maize_mktthin) %>%
  ungroup() %>%
  dplyr::select(ea_id,yearmon,clust_maize_current,lhz_maize_current, clust_maize_mktthin_current,lhz_maize_mktthin_current)
  

colnames(mw.current.price.impute.join)
# Join the current maize price 
mw.master.hh = left_join(mw.lsms.fill,mw.current.price.impute.join, by = c("ea_id","yearmon"))





# create a one month lag for each price 
mw.current.price.impute["yearmon_lag1"]= mw.current.price.impute$yearmon + 0.1
mw.lag.price.impute = mw.current.price.impute

mw.lag.price.join = mw.lag.price.impute %>% 
  mutate(yearmon = yearmon_lag1) %>%
  mutate(yearmon = as.character(yearmon)) %>%
  mutate(clust_maize_lag = clust_maize_price) %>%
  mutate(lhz_maize_lag = lhz_maize_price) %>%
  mutate(clust_maize_mktthin_lag = clust_maize_mktthin) %>%
  mutate(lhz_maize_mktthin_lag = lhz_maize_mktthin) %>%
  ungroup() %>%
  dplyr::select(ea_id,yearmon,clust_maize_lag,lhz_maize_lag, clust_maize_mktthin_lag,lhz_maize_mktthin_lag)



# Join the one month lag  maize price 
mw.master.hh = left_join(mw.master.hh,mw.lag.price.join, by = c("ea_id","yearmon"))



#######################################################################
# read weather data 
#######################################################################

load("data/clean/weather/mw_weather_final.RData")

mw.weather.final = mw.weather.final %>% dplyr::filter(!is.na(VID) & !is.na(tmean))

mw.weather.final["lhz_floodmax"][is.na(mw.weather.final["lhz_floodmax"])] = 0

colSums(is.na(mw.weather.final))


mw.master.hh = left_join(mw.master.hh,mw.weather.final, by = c("ea_id","FS_year","FNID"))


mw.master.hh = mw.master.hh %>% dplyr::filter(!is.na(VID) & !is.na(date))

colSums(is.na(mw.master.hh))

lapply(mw.master.hh, class)



#################################################################
# Keep the data set (both cluster and household level )
#################################################################

require(tidyverse)
 
                
 
# Collapse to cluster level  
mw.master.clust = mw.master.hh %>% 
  group_by(ea_id,FS_year) %>%   
  dplyr::select(-case_id,-FNID,-TA_names,-area_string,-region_string,-urban_string,-Month,-FS_year,-VID,-yearmon,-cropyear) %>%
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))  

colSums(is.na(mw.master.clust))

lapply(mw.master.clust, class)

# rescale data 
mw.master.clust = mw.master.clust %>% 
  mutate(logFCS= log(FCS)) %>%
  mutate(raincytot = raincytot/1000) %>% 
  mutate(elevation = elevation/1000) 


TA_concordance = mw.master.hh %>% ungroup() %>% 
  dplyr::select(ea_id,TA_names,FNID) %>% distinct() 
TA_concordance=TA_concordance[!duplicated(TA_concordance$ea_id),] 

sum(duplicated(TA_concordance$ea_id))

# Join TA/FNID names 
mw.master.clust =left_join(mw.master.clust,TA_concordance,by="ea_id")

# Create TA level averages  

mw.master.clust= mw.master.clust %>%
  group_by(TA_names,FS_year) %>%
  mutate(TA_maize_current  = mean(clust_maize_current,na.rm=TRUE)) %>%
  mutate(TA_maize_lag  = mean(clust_maize_lag,na.rm=TRUE)) %>%
  mutate(TA_maize_mktthin  = mean(clust_maize_mktthin_current,na.rm=TRUE)) %>%
  mutate(TA_maize_mktthin_lag  = mean(clust_maize_mktthin_lag,na.rm=TRUE)) %>%
  mutate(TA_raincytot  = mean(raincytot,na.rm=TRUE)) %>%
  mutate(TA_day1rain  = mean(day1rain,na.rm=TRUE)) %>%
  mutate(TA_maxdaysnorain = mean(maxdaysnorain,na.rm=TRUE)) %>%
  mutate(TA_floodmax  = mean(floodmax,na.rm=TRUE)) %>%
  mutate(TA_heatdays  = mean(heatdays,na.rm=TRUE)) %>%
  mutate(TA_gdd  = mean(gdd,na.rm=TRUE)) %>%
  mutate(TA_tmean  = mean(tmean,na.rm=TRUE))  %>%
  distinct()


# generate quarter and month fixed effect 

mw.master.clust = mw.master.clust %>% 
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


# generate dummies for each FNID 

library(fastDummies)
mw.master.clust = fastDummies::dummy_cols(mw.master.clust, select_columns = "FNID")

# generate dummies for each region 

mw.lsms = read.csv("data/clean/MW_household.csv",stringsAsFactors = FALSE)

# 


######################################################
# Read in GIEWS price data (both current and one month lag)
######################################################
library(readr)
GIEW_malawi <- read_csv("data/raw/price/GIEW_malawi.csv")


colnames(GIEW_malawi)


market.address =  unique(GIEW_malawi$Market)[1:5]
market.address[6] = unique(GIEW_malawi$Market)[7]

source("R/functions/GoogleMapApi.R")
map.key = "YOUR GOOGLE MAP KEY HERE"

# market.coord = coordFind(market.address)
# write.csv(market.coord,"data/clean/concordance/GIEWS_mkt_coord.csv",row.names = FALSE)

market.coord = read_csv("data/clean/concordance/GIEWS_mkt_coord.csv")

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




# colnames(mw.master.hh)
write.csv(mw.master.hh, file= "data/mw_dataset_hh.csv",row.names = FALSE)
write.csv(mw.master.clust, file= "data/mw_dataset_cluster.csv",row.names = FALSE)


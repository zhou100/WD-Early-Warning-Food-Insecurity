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


# read price data 
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

# create a one month lag for each price 
mw.current.price.impute["yearmon_lag1"]= mw.current.price.impute$yearmon + 0.1

mw.current.price.impute["yearmon"] = mw.current.price.impute["yearmon_lag1"]

mw.current.price.impute = mw.current.price.impute %>% dplyr::select(-yearmon_lag1)

mw.lsms.fill  = mw.lsms.fill %>% mutate(yearmon = as.character(yearmon))
mw.current.price.impute  = mw.current.price.impute %>% mutate(yearmon = as.character(yearmon))


mw.master.hh = left_join(mw.lsms.fill,mw.current.price.impute, by = c("ea_id","yearmon"))


# read weather data 
load("data/clean/weather/mw_weather_final.RData")

mw.weather.final = mw.weather.final %>% dplyr::filter(!is.na(VID) & !is.na(tmean))

mw.weather.final["lhz_floodmax"][is.na(mw.weather.final["lhz_floodmax"])] = 0

colSums(is.na(mw.weather.final))


mw.master.hh = left_join(mw.master.hh,mw.weather.final, by = c("ea_id","FS_year","FNID"))


mw.master.hh = mw.master.hh %>% dplyr::filter(!is.na(VID) & !is.na(date))

colSums(is.na(mw.master.hh))

lapply(mw.master.hh, class)



#########################################
# Keep the data set (both cluster and household level )
#########################################


mw.master.hh = mw.master.hh %>% 
  dplyr::select (-case_id,-ea_id,-VID,-cropyear,-year,-Month,-yearmon,-date)

# Collapse to cluster level  
mw.master.clust = mw.master.hh %>% 
  group_by(ea_id,FS_year) %>%   
  dplyr::select(-FNID,-TA_names) %>%
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
  mutate(TA_maize_price  = mean(clust_maize_price,na.rm=TRUE)) %>%
  mutate(TA_rice_price  = mean(clust_rice_price,na.rm=TRUE)) %>%
  mutate(TA_nuts_price  = mean(clust_nuts_price,na.rm=TRUE)) %>%
  mutate(TA_beans_price  = mean(clust_beans_price,na.rm=TRUE)) %>%
  mutate(TA_maize_mktthin  = mean(clust_maize_mktthin,na.rm=TRUE)) %>%
  mutate(TA_rice_mktthin  = mean(clust_rice_mktthin,na.rm=TRUE)) %>%
  mutate(TA_nuts_mktthin  = mean(clust_nuts_mktthin,na.rm=TRUE)) %>%
  mutate(TA_beans_mktthin  = mean(clust_beans_mktthin,na.rm=TRUE)) %>%
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






# colnames(mw.master.hh)
write.csv(mw.master.hh, file= "data/mw_dataset_hh.csv",row.names = FALSE)
write.csv(mw.master.clust, file= "data/mw_dataset_cluster.csv",row.names = FALSE)


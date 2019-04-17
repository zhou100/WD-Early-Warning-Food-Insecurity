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

mw.lsms$ea_id = as.character(mw.lsms$ea_id)
mw.lsms$yearmon = as.yearmon(mw.lsms$yearmon)
mw.lsms$rural =  ifelse(mw.lsms$reside=="rural",1,0)

# remove columns that can not be used in the prediction anlaysis 
mw.lsms = mw.lsms %>% 
          dplyr::select(-cellphone_cost,-Reason1,-Reason2,-Reason3,-MAHFP,-hh_a01,-head_age,-slope,-reside) %>%
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
  mutate(ag_percent= ifelse(is.na(ag_percent), mean(ag_percent, na.rm=TRUE), ag_percent)) %>% 
  mutate(elevation= ifelse(is.na(elevation), mean(elevation, na.rm=TRUE), elevation)) 
  
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





mw.master.hh = mw.master.hh %>% dplyr::select (-case_id,-ea_id,-TA_names,-VID,-cropyear,-year,-Month,-yearmon,-date)


mw.master.clust = mw.master.hh %>% 
  group_by(ea_id,FS_year,FNID) %>%   
  dplyr::select(-FNID,-head_gender,-head_edlevel) %>%
  dplyr::summarise_all(funs(mean(.,na.rm=TRUE)))  

colSums(is.na(mw.master.clust))

lapply(mw.master.clust, class)



# colnames(mw.master.hh)
write.csv(mw.master.hh, file= "data/mw_dataset_hh.csv",row.names = FALSE)
write.csv(mw.master.clust, file= "data/mw_dataset_cluster.csv",row.names = FALSE)


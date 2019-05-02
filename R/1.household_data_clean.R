####################################################################################################################################
# Goal : This script aims to clean up household lsms data, generate geocoordinates,  generate variables such as the asset index etc.
# purpose: 
# 1. generate variables at the household level to be used in the regression 
# 2. generate cluster geovariables used to extract weather and match with prices 

# Input : 
# 1. csv file of cleaned lsms household survey   dataset

# Output: 
# 0. cluster geovariables 
# 1. csv file of lsms merged with lhz FNID and small variables changes 
# 
# Yujun Zhou -  03/20/18
###################################################################
rm(list=ls())
library(zoo)
require(tidyverse)
library(readr)

source("R/functions/Yearmon.R")

#############################################
# collect the lat and lon from the household data 
#############################################
##################################################################
# Goal : retrieve geoordinates information for each 
# input : csv: cleaned aggreagate data for each country  
# output:  a list of cluster geoordinates for the given countries for each given yaer 
###################################################################

Malawi_aggregate <- read_csv("data/clean/LSMS/Malawi_aggregate.csv")

#colnames(Malawi_aggregate)
mw.concord = Malawi_aggregate %>% dplyr::select(ea_id,lat_modified,lon_modified) %>% distinct() %>% na.omit()

write.csv(mw.concord,file="data/clean/concordance/Malawi_coord.csv",row.names = FALSE)
 

length(unique(Malawi_aggregate$ea_id)) 

 

##########################################################################################
# Spatially join the LHZ and get a concordance table 
# this concordance is directely obtained using ArcGIS/QGIS spatial joining function
# lhz Shapefile :data/shapefiles/livelihood_zone/malawi/livelihood zone 2012/MW_Admin1_LHZ_2012.3
##########################################################################################
mw.cluster.lhz = read.csv("data/clean/concordance/mw_ea_lhz_concordance.csv",stringsAsFactors = FALSE)
# colnames(mw.cluster.lhz)
mw.cluster.lhz = mw.cluster.lhz %>% dplyr::select(ea_id,FNID) %>% distinct()


# remove the duplicated joins ( one ea_id mapped to different livelihood zones)
nrow(mw.cluster.lhz[!duplicated(mw.cluster.lhz$ea_id),])


mw.cluster.lhz.matched = mw.cluster.lhz[!duplicated(mw.cluster.lhz$ea_id),]

write.csv(mw.cluster.lhz.matched,file="data/clean/concordance/mw_cluster_lhz.csv",row.names = FALSE)

##################################################################################################
##### Malawi hh data cleaning 
##################################################################################################
Malawi_aggregate = read.csv("data/clean/LSMS/Malawi_aggregate.csv",stringsAsFactors = FALSE)

##### Make nutrition variables from strings to dummy variables 

Malawi_aggregate = Malawi_aggregate %>%
 mutate(Month=month.name[FS_month] ) %>% 
 mutate( roof_natural_inverse = 1- roof_natural ) %>% 
 mutate(nutri_avail = if_else(nutri_avail != "Severe Constraint" & nutri_avail != "Moderate Constraint" ,"No Constraint",nutri_avail) )  %>%
 mutate(nutri_rentention = if_else(nutri_rentention != "Severe Constraint" & nutri_rentention != "Moderate Constraint","No Constraint",nutri_rentention))  %>%
 mutate(dummy_terrain_rough = if_else(terrain_rough=="Mid altitude mountains" & terrain_rough=="Rugged lowlands" & terrain_rough== "High-altitude plains", 1,0 )) 
   

Malawi_aggregate = Malawi_aggregate %>%
  mutate(nutri_severe_constraint=if_else(nutri_avail=="Severe Constraint",1,0) ) %>%
  mutate(nutri_moderate_constraint=if_else(nutri_avail=="Moderate Constraint",1,0) ) %>% 
  mutate(nutri_rent_severe_constraint=if_else(nutri_rentention=="Severe Constraint",1,0) ) %>%
  mutate(nutri_rent_moderate_constraint=if_else(nutri_rentention=="Moderate Constraint",1,0) ) 
  

yearmon= paste(Malawi_aggregate["FS_year"][,1],Malawi_aggregate["FS_month"][,1], sep = "-")         
Malawi_aggregate["yearmon"]= as.yearmon(yearmon,"%Y-%m")

# Check the column names 
# colnames(Malawi_aggregate)
##################################################################
# DID the asset index in Stata instead 
##################################################################
# # Create an asset index based on a number of assets 
# mw.asset = Malawi_aggregate[3:18]
# mw.asset[is.na(mw.asset)]=0
# mw.asset.pca <- prcomp(na.omit(mw.asset),
#                        center = TRUE,
#                        scale. = TRUE) 
# 
# #summary(mw.asset.pca)
# 
# mw.asset.pca.df= as.data.frame(mw.asset.pca$x)
# # Use the first direction 
# Malawi_aggregate["asset_index"] = mw.asset.pca.df$PC1

Malawi_aggregate = Malawi_aggregate %>% 
  dplyr::select(-lat_modified,-hh_wgt,-lon_modified,-region,-survey_round,-terrain_rough, -country,-nutri_avail,-nutri_rentention)  %>% distinct()


Malawi_lsms_ea = Malawi_aggregate
 
mw_concordance <-  read.csv("data/clean/concordance/mw_cluster_lhz.csv")
mw_concordance =  mw_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))

Malawi_lsms_ea = Malawi_lsms_ea  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
Malawi_lsms_ea = dplyr::left_join(Malawi_lsms_ea,mw_concordance,by = "ea_id")

# length(unique(Malawi_lsms_ea$ea_id))


##################################################################################################
# Join the IPC value 
##################################################################################################

library(readxl)
FEWS_IPC <- read_excel("data/raw/IPC_value/FEWS NET_MW_IPC_data_merge.xlsx",sheet = "Common Unit", skip = 2)


require(tidyverse)

length(unique(FEWS_IPC$FNID_OLD))
length(unique(FEWS_IPC$FNID))


colnames(FEWS_IPC)
# We mostly need 2009-2011 and 2012-2013 values.

# Remove extra columns  
FEWS_IPC = FEWS_IPC %>% dplyr::select(-FNID,-CN,-ADMIN1,-ADMIN2,-LZCODE,-LZNAMEE)

 FEWS_IPC = FEWS_IPC %>% dplyr::select(FNID_OLD,CS200907,CS200910,CS201001,CS201004,
  CS201007,CS201010,CS201101,CS201104,CS201107,CS201110,CS201201,CS201204,
  CS201210,CS201301,CS201304,CS201307,CS201310)

# Expand the value from quaterly to monthly 
 FEWS_IPC= FEWS_IPC  %>% 
  mutate(CS200908 = CS200907) %>%
  mutate(CS200909 = CS200907) %>%
  mutate(CS200911 = CS200910) %>%
  mutate(CS200912 = CS200910) %>%
  # year 2010
  mutate(CS201002 = CS201001) %>%
  mutate(CS201003 = CS201001) %>%
  mutate(CS201005 = CS201004) %>%
  mutate(CS201006 = CS201004) %>%
  mutate(CS201008 = CS201007) %>%
  mutate(CS201009 = CS201007) %>%
  mutate(CS201011 = CS201010) %>%
  mutate(CS201012 = CS201010) %>%
  # year 2011
  mutate(CS201102 = CS201101) %>%
  mutate(CS201103 = CS201101) %>%
  mutate(CS201105 = CS201104) %>%
  mutate(CS201106 = CS201104) %>%
  mutate(CS201108 = CS201107) %>%
  mutate(CS201109 = CS201107) %>%
  mutate(CS201111 = CS201110) %>%
  mutate(CS201112 = CS201110) %>%
  # year 2012
  mutate(CS201202 = CS201201) %>%
  mutate(CS201203 = CS201201) %>%
  mutate(CS201205 = CS201204) %>%
  mutate(CS201206 = CS201204) %>%
  mutate(CS201207 = CS201204) %>% 
  mutate(CS201208 = CS201204) %>%
  mutate(CS201209 = CS201204) %>%
  mutate(CS201211 = CS201210) %>%
  mutate(CS201212 = CS201210) %>%
  # year 2013 
  mutate(CS201302 = CS201301) %>%
  mutate(CS201303 = CS201301) %>%
  mutate(CS201305 = CS201304) %>%
  mutate(CS201306 = CS201304) %>%
  mutate(CS201308 = CS201307) %>%
  mutate(CS201309 = CS201307) %>%
  mutate(CS201311 = CS201310) %>%
  mutate(CS201312 = CS201310)  


# wide to long 

FEWS_IPC_long = FEWS_IPC %>% 
  gather (-FNID_OLD,value= "IPC_value",key="Date") %>% 
  mutate(year = substr(Date, 3, 6)) %>%
  mutate(month = substr(Date, 7, 8)) %>%
  mutate(year=as.numeric(year)) %>%
  mutate(month = as.numeric(month)) %>%
  dplyr::filter(!is.na(FNID_OLD)) %>% 
  dplyr::filter(!IPC_value==99) %>% 
  distinct()


# generate year mon 
source("R/functions/Yearmon.R")

FEWS_IPC_long = yearmon(FEWS_IPC_long,year_var = "year",month_var = "month")

FEWS_IPC_long2009 = FEWS_IPC_long %>% filter (year <2010)

table(FEWS_IPC_long2009$IPC_value)

MW2012C3030306 Oct 2009


# generate 12 month lag 

FEWS_IPC_long_lag=  FEWS_IPC_long %>%
  group_by(FNID_OLD) %>%
  arrange(date) %>%
  mutate( IPC_value= ifelse(IPC_value==88|IPC_value==99,NA,IPC_value) ) %>%
  mutate(IPC1  = dplyr::lag(x=IPC_value,n=1,order_by = date)) %>%
  mutate(IPC12 = dplyr::lag(x=IPC_value,n=12 ,order_by = date)) %>%
  mutate(FNID= FNID_OLD) %>%
  ungroup()
  

IPC12_2 = FEWS_IPC_long_lag %>% dplyr::filter(FNID =="MW2012C3030306")

FEWS_IPC_long_lag = FEWS_IPC_long_lag %>% dplyr::select(FNID,yearmon,IPC1,IPC12)

FEWS_IPC_duplicate = FEWS_IPC_long_lag %>%
  dplyr::filter(!is.na(FNID)) %>% 
  distinct() %>%
  arrange(FNID,yearmon)


 

Malawi_lsms_ea = 
  dplyr::left_join(Malawi_lsms_ea,FEWS_IPC_duplicate,by = c("FNID"="FNID","yearmon"="yearmon"))


Malawi_lsms_ea %>% dplyr::filter(FNID =="MW2012C3030306") %>% select(yearmon,IPC12)

 length(unique(Malawi_lsms_ea$ea_id))

write.csv(Malawi_lsms_ea,"data/clean/MW_household.csv",row.names = FALSE)


S = Malawi_lsms_ea %>% filter(FS_year==2013) 
length(unique(S$ea_id))








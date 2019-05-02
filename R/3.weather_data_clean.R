##################################################################
# Goal : This script aims to clean up the rainfall and temperature data，
# 
# purpose: generate variables day1rain,  total rainfall in growing season , rainfall in areas that are prone to flood for Tanzania and Malawi 
# haven't generated the ones for Uganda, because it has two rainy seasons 
# link the weather variable to livelihood zone id and cluster id .

# Input : 
# 1. daily rainfall extracted from CHIRPS (using script rainfall_daily_cluster.R and cluster buffer shapefiles)
# 2. daily temperture data from African Drought Monitor
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 

# Output: 
# 0. day1rain, when the first day of rain comes (both lhz and cluster level) 
# 1. total rainfalls in growing seasons  （Growing season total precipitation）
# 2. Number of max no rain days in a crop year
# 3.
#  GDD - number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 

################################
# data to get in the future 
#########################################
# 4. Schlenker & Roberts (2009), number and percentage of days in a set number of quantile bins (per-bin deviations from long-run in z-scores)

# 5.5 temperatue bins  Deschenes and Greenstone (2011) (<10, 20-25, etc. )
# 6. soil quality：Soil characteristics: K factor, slope lenghth, fraction irrigated , moisture capacity, salinity 
# 7. NDVI 
# 
# Yujun Zhou -  04/11/18
###################################################################


rm(list=ls())

package = c("dplyr","zoo","rgeos", "rgdal", "raster","lubridate")
lapply(package, require, character.only = TRUE)

source("R/functions/CropYearTZMW.R") 
source("R/functions/CropYearUG.R") 
source("R/functions/RemoveX.R") 
source("R/functions/WeatherTranspose.R") 


##################################################################################
# load the lhz weather variables 
##################################################################################

precip_lhz_mw <- read.csv("data/raw/rain/chirps_daily_0716.csv")

# colnames(precip_lhz_tz)
# colnames(precip_lhz_ug)
 #colnames(precip_lhz_mw)


# nrow(precip_lhz_tz)
# nrow(precip_lhz_ug)



precip_lhz_mw = precip_lhz_mw %>% mutate(Date =as.Date(Date) ) %>% dplyr::select(-X,-month)

# the mw lhz colnames  are faulty, load the ones from the temperature data
mw_tmin <- read.csv("data/raw/temperature/mw_daily_tmin.csv")
colnames(precip_lhz_mw)[2:61] = colnames(mw_tmin)[2:(ncol(mw_tmin)-1)]


##################################################################################
# load the cluster weather variables 
##################################################################################

load("data/raw/rain/CHIRPS_malawi_cluster.rda")

precip_clust_mw[["Date"]] = as.Date(precip_clust_mw$Date,"%m/%d/%Y")
#colnames(precip_clust_mw)
precip_clust_mw = precip_clust_mw %>% dplyr::select(-X)


rain.MW.list = list(precip_lhz_mw,precip_clust_mw)

# generate the year and month variable from date 


# 1. generate crop year, so that it's summing up by crop year 
source("R/functions/CropYearTZMW.R") 

#lapply(rainlist,function(x){colnames(x)})

# generate cropyear 
rain.MW.cropyear = lapply(rain.MW.list, function(x){CropYearTZMW(x)})


save(rain.MW.cropyear, file = "data/raw/rain/MW_rain_cropyear.rda")


############################################################################
### first day of rain for the rainy season (since October or month >=10) for each livelihood zones 
### should there be a threshold other than 0 ?
############################################################################

########################################################################  
# generate the first date of rain after October for Tanzania and After April for Uganda 
# day1rain "the number of days after Oct 1 where five-day rainfall > 10 and it rained at least 3/5 days"
######################################################################## 

rm(list=ls())
load("data/raw/rain/MW_rain_cropyear.rda")

# moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)

 

####livelihood level day1rain for malawi ###########
precip.lhz.mw.rain = rain.MW.cropyear[[1]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "left", fill = NA))) %>%
  na.omit()

lhz.mw.day1rain=
  precip.lhz.mw.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)

save(lhz.mw.day1rain,file="data/clean/weather/lhz_day1rain.RData" )

 
####cluster level day1rain for tanzania ###########
 


####cluster level day1rain for malawi ###########
precip.clust.mw.rain = rain.MW.cropyear[[2]]%>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(rollmedian(., 5, align = "left", fill = NA))) %>%
  na.omit()

clust.mw.day1rain=
  precip.clust.mw.rain %>%
  dplyr::select(-Date,everything()) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(first(which(.>2.2))+2)) %>% 
  dplyr::select(-Date)

save( clust.mw.day1rain,file="data/clean/weather/clust_day1rain.RData" )



#################################################################################
#### generate maxdaysno rain 
########### longest consequetive days without rain
#### during the rainy season (Oct-Mar) per crop year (May-Apr)"
#################################################################################

library(runner)

maxdaysnorain.lhz.mw = 
  rain.MW.cropyear[[1]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,NA))) %>% # 1 indicates that the day has 0 rainfall
  dplyr::select(-Date) %>%
  dplyr::mutate_all(funs(streak_run(.,na_rm=FALSE))) %>% # count the number of consecutive days of 0 rain
  group_by(cropyear) %>%
  dplyr::summarise_all(funs(max(.,na.rm = TRUE)))   # count the days with 0 rain
  

maxdaysnorain.clust.mw = 
  rain.MW.cropyear[[2]] %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-cropyear,-year,-month) %>%
  dplyr::mutate_all(funs(ifelse(.==0,1,NA))) %>% # 1 indicates that the day has 0 rainfall,otherwise remove the value
  dplyr::select(-Date) %>%
  dplyr::mutate_all(funs(streak_run(.,na_rm=FALSE))) %>% # count the number of consecutive days of 0 rain
  group_by(cropyear) %>%
  dplyr::summarise_all(funs(max(.,na.rm = TRUE)))   # count the max days with 0 rain

 
 
save(maxdaysnorain.lhz.mw,file="data/clean/weather/lhz_maxdaysnorain.RData" )
save(maxdaysnorain.clust.mw,file="data/clean/weather/clust_maxdaysnorain.RData" )


#################################################################################
#### generate rain_cytot
###########  "total rainfall from Oct to Apr （or march to july） by ipczone and cropyear" 
#################################################################################


# rain.TZMW.list = list(precip_lhz_tz,precip_lhz_mw,precip_clust_tz,precip_clust_mw)
# rain.UG.list = list(precip_lhz_ug,precip_clust_ug)

 rain.cytot.lhz.mw = 
   rain.MW.cropyear[[1]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month) %>%
   dplyr::summarise_all(funs(sum)) %>%  # count the total rain 
   dplyr::select(-Date)
 
 #colnames(rain.cytot.lhz.mw)
 
 rain.cytot.clust.mw = 
   rain.MW.cropyear[[2]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-Date,cropyear,-year,-month) %>%
   dplyr::summarise_all(funs(sum))  # count the total rain 

# colnames(rain.cytot.clust.mw)
 

 
 save(rain.cytot.lhz.mw,file="data/clean/weather/lhz_rain_cytot.RData" )
 save(rain.cytot.clust.mw,file="data/clean/weather/clust_rain_cytot.RData" )
 
 
 #################################################################################
 #### generate max rain 
 ###########  "maximum of rainfall from Oct to Apr in one month by ipczone/cluster and cropyear" 
 #################################################################################
 
 
 # rain.TZMW.list = list(precip_lhz_tz,precip_lhz_mw,precip_clust_tz,precip_clust_mw)
 # rain.UG.list = list(precip_lhz_ug,precip_clust_ug)
 
 maxrain.lhz.mw = 
   rain.MW.cropyear[[1]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month) %>%
   dplyr::select(-Date) %>%
    dplyr::summarise_all(funs(max(.,na.rm = TRUE))) # count the max rain amount 
 
 #colnames(rain.cytot.lhz.mw)
 
 maxrain.clust.mw = 
   rain.MW.cropyear[[2]] %>%
   dplyr::group_by(cropyear) %>%
   dplyr::arrange(Date) %>%
   dplyr::select(-cropyear,-year,-month) %>%
   dplyr::select(-Date) %>%
   dplyr::summarise_all(funs(max(.,na.rm = TRUE))) # count the max rain amount 
 
 # colnames(rain.cytot.clust.mw)
 
 
 save(maxrain.lhz.mw,file="data/clean/weather/lhz_maxrain.RData" )
 save(maxrain.clust.mw,file="data/clean/weather/clust_maxrain.RData" )
 
 
#################################################################################
#### generate mean temperature in the growing season   
#################################################################################

mw_tmin <- read.csv("data/raw/temperature/mw_daily_tmin.csv")
mw_tmax <- read.csv("data/raw/temperature/mw_daily_tmax.csv")

source("R/functions/CropYearTZMW.R") 

temp.MW.list= list(mw_tmax,mw_tmin)


formatTempDF= function(df){
  formatted.DF = df %>% dplyr::mutate(Date= as.Date(date1,"%m/%d/%Y")) %>% dplyr::select(-date1)   
    
  tryCatch( {formatted.DF = formatted.DF %>% dplyr::select(-X)},error=function(e){} )
  return(formatted.DF)
}

temp.MW.list.format = lapply(temp.MW.list, function(x){formatTempDF(x)})


temp.MW.cropyear = lapply(temp.MW.list.format, function(x){CropYearTZMW(x)})


mw.date = temp.MW.cropyear[[1]]$Date
mw.cropyear = temp.MW.cropyear[[2]]$cropyear


temp.MW.cropyear[[1]] = temp.MW.cropyear[[1]] %>% dplyr::select(-Date,-month,-year,-cropyear)
temp.MW.cropyear[[2]] = temp.MW.cropyear[[2]] %>% dplyr::select(-Date,-month,-year,-cropyear)

mw.temp.mean.full = (temp.MW.cropyear[[1]] + temp.MW.cropyear[[2]])/2 -273.15
mw.temp.mean.full["Date"] = mw.date
mw.temp.mean.full["cropyear"] = mw.cropyear


mw.tmean = 
  mw.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::select(-Date) %>%
  dplyr::summarise_all(funs(mean))   # generate the mean temperature by year by ipczone




save(mw.tmean,file="data/clean/weather/tmean.RData" )



#################################################################################
#### generate growing degree days  
###########  number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 
#################################################################################


mw.gdd = mw.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>8 & .<30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))   %>% # count the days with 0 rain
  dplyr::select(-Date)

 

save(mw.gdd, file="data/clean/weather/gdd.RData")


#################################################################################
#### generate heat days  (>30 c )
###########  number of days where temp was between 8 to 32 C (Tmax + Tmin)/2 . Deschênes and Greenstone (2007) yield  on weather 
#################################################################################


mw.heatday = mw.temp.mean.full %>%
  dplyr::group_by(cropyear) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate_all(funs(ifelse(.>=30,1,0))) %>%
  dplyr::group_by(cropyear) %>%
  dplyr::summarise_all(funs(length(Date[.==1])))   %>% # count the days with 0 rain
  dplyr::select(-Date)

 

save(mw.heatday, file="data/clean/weather/heatday.RData")
#################################################################################
## Transpose the rainfall variable with cluster/ipc zone id and year 
#############################################################################


library(dplyr)
rm(list=ls())

load("data/clean/weather/clust_day1rain.RData")
load("data/clean/weather/clust_maxdaysnorain.RData")
load("data/clean/weather/clust_rain_cytot.RData")

load("data/clean/weather/lhz_day1rain.RData")
load("data/clean/weather/lhz_maxdaysnorain.RData")
load("data/clean/weather/lhz_rain_cytot.RData")

load("data/clean/weather/gdd.RData")
load("data/clean/weather/tmean.RData")
load("data/clean/weather/heatday.RData")

load("data/clean/weather/lhz_maxrain.RData")
load("data/clean/weather/clust_maxrain.RData")


source("R/functions/WeatherTranspose.R") 

mw.clust.list = list(rain.cytot.clust.mw,clust.mw.day1rain,maxdaysnorain.clust.mw,maxrain.clust.mw)
 
 
mw.lhz.list = list(lhz.mw.day1rain, mw.gdd,mw.tmean,rain.cytot.lhz.mw,maxdaysnorain.lhz.mw,mw.heatday,maxrain.lhz.mw)
 
mw.clust.list.transpose= lapply(mw.clust.list, WeatherTranspose)
 
mw.lhz.list.transpose= lapply(mw.lhz.list, WeatherTranspose)
 

source("R/functions/RemoveX.R") 
mw.clust.list.transpose= lapply(mw.clust.list.transpose, RemoveX)
 

mw.lhz.list.transpose= lapply(mw.lhz.list.transpose, RemoveX)
 
 


#################################################################################
##generate dummy for flood susceptible areas
#################################################################################

# need a concordance table of  cluster id and ipczone from coord and 
library(dplyr)
mw_concordance = read.csv("data/clean/concordance/mw_cluster_lhz.csv",stringsAsFactors = FALSE)
mw_concordance = mw_concordance %>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))
colnames(mw_concordance) = c("id","FNID")


fnid_to_v = read.csv("data/clean/concordance/FNID to V .csv",stringsAsFactors=FALSE)
colnames(fnid_to_v) = c("id","VID")

source("R/functions/FnidV.R") 
mw.lhz.list.transpose = lapply(mw.lhz.list.transpose,function(x){FnidV(x,fnid_to_v)})


#mw.lhz.list = list(lhz.mw.day1rain, mw.gdd,mw.tmean,rain.cytot.lhz.mw,
# maxdaysnorain.lhz.mw,mw.heatday,maxrain.lhz.mw)


# MW2012C3020515 MW2012C3031005  MW2012C3031206

flood_mw_fnid = c("MW2012C3020515","MW2012C3031005","MW2012C3031206")


# Generate max daily rain in flood region 
length(mw.lhz.list)
floodmax.lhz.mw.noflood = mw.lhz.list.transpose[[7]] %>% 
  dplyr::filter(!(id %in% flood_mw_fnid))%>% mutate(value =0 )


floodmax.lhz.mw.flood = mw.lhz.list.transpose[[7]] %>% 
  dplyr::filter(id %in% flood_mw_fnid)

floodmax_lhz_mw = dplyr::bind_rows(floodmax.lhz.mw.noflood,floodmax.lhz.mw.flood)
# flood max should be in the same year 
floodmax_lhz_mw["FS_year"] =floodmax_lhz_mw["cropyear"] 



# select the clusters that are in the flood prone region 
mw_flood_clust = 
  mw_concordance %>% dplyr::filter(FNID %in% flood_mw_fnid)  %>% dplyr::select(id)
# mw_flood_clust 

# create flood max at mw clust level 
# the maximum amount of rainfall in the current month 

# mw.clust.list = list(rain.cytot.clust.mw,clust.mw.day1rain,maxdaysnorain.clust.mw)
# the first 


mw.clust.list.transpose[[1]]$id = as.character(mw.clust.list.transpose[[1]]$id)



#mw.clust.list = list(rain.cytot.clust.mw,clust.mw.day1rain,maxdaysnorain.clust.mw,
# maxrain.clust.mw)


floodmax_clust_mw_flood = mw.clust.list.transpose[[4]] %>% 
  dplyr::filter(!id %in% mw_flood_clust$id)%>% mutate(value =0 )

floodmax_clust_mw_noflood = mw.clust.list.transpose[[4]] %>% 
  dplyr::filter(id %in% mw_flood_clust$id)


floodmax_clust_mw = dplyr::bind_rows(floodmax_clust_mw_noflood,floodmax_clust_mw_flood)

# flood max should be in the same year , so change the FS_year to the current year, instead of one year before
floodmax_clust_mw["FS_year"] =floodmax_clust_mw["cropyear"] 


 

save(floodmax_lhz_mw,floodmax_clust_mw,file = "data/clean/weather/floodmax.RData")



save(mw.lhz.list.transpose,
     mw.clust.list.transpose,file="data/clean/weather/weather_transpose.RData")

 



#################################################################################
## merge the rainfall variable with cluster/ipc zone id and year 
#############################################################################


# raincytot  day1rain  maxdays floodmax
rm(list=ls())
load("data/clean/weather/weather_transpose.RData")
load("data/clean/weather/floodmax.RData")



# mw.clust.list = list(rain.cytot.clust.mw,clust.mw.day1rain,maxdaysnorain.clust.mw)

 
# mw.lhz.list = list(lhz.mw.day1rain, mw.gdd,mw.tmean,rain.cytot.lhz.mw,maxdaysnorain.lhz.mw)

clust.names = c("raincytot","day1rain","maxdaysnorain")

for (i in 1:length(mw.clust.list.transpose)){
  colnames(mw.clust.list.transpose[[i]])[3] = clust.names[i]
 
  colnames(mw.clust.list.transpose[[i]])[1] = "ea_id"
 
}


lhz.names = c("lhz_day1rain","gdd","tmean","lhz_raincytot","lhz_maxdaysnorain","heatdays")

for (i in 1:length(mw.lhz.list.transpose)){
  colnames(mw.lhz.list.transpose[[i]])[3] = lhz.names[i]
  
  colnames(mw.lhz.list.transpose[[i]])[1] = "FNID"
 
}

colnames(mw.lhz.list.transpose[[1]])
colnames(mw.clust.list.transpose[[1]])

 
colnames(floodmax_clust_mw)[1] = "ea_id"
colnames(floodmax_clust_mw)[3] = "floodmax"

colnames(floodmax_lhz_mw)[1] = "FNID"
colnames(floodmax_lhz_mw)[3] = "lhz_floodmax"


floodmax_clust_mw = floodmax_clust_mw %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year) )

floodmax_lhz_mw = floodmax_lhz_mw %>% dplyr::select(-cropyear) %>% dplyr::mutate(FS_year = as.numeric(FS_year),FNID  = as.character(FNID) )
 

# need a concordance table of  cluster id and ipczone from coord and 
library(dplyr)
mw_concordance = read.csv("data/clean/concordance/mw_cluster_lhz.csv")
mw_concordance = mw_concordance %>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))


#################################################################################
## merge Malawi data
#############################################################################

mw.weather.clust = full_join(mw.clust.list.transpose[[1]],mw.clust.list.transpose[[2]])
mw.weather.clust = full_join(mw.weather.clust,mw.clust.list.transpose[[3]])
mw.weather.clust = full_join(mw.weather.clust,mw_concordance,by=c("ea_id"))


mw.weather.lhz= full_join(mw.lhz.list.transpose[[1]],mw.lhz.list.transpose[[2]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[3]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[4]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[5]])
mw.weather.lhz= full_join(mw.weather.lhz,mw.lhz.list.transpose[[6]])


mw.weather.final = full_join(mw.weather.clust,mw.weather.lhz)
mw.weather.final = full_join(mw.weather.final,floodmax_clust_mw,by=c("FS_year","ea_id"))
mw.weather.final = full_join(mw.weather.final,floodmax_lhz_mw,by=c("FS_year","FNID"))


mw.weather.final = mw.weather.final %>% dplyr::arrange(FS_year) %>% dplyr::filter(!is.na(ea_id)) %>% dplyr::filter(!is.na(cropyear)) %>% dplyr::select(-VID.y) 

colnames(mw.weather.final)[colnames(mw.weather.final)=="VID.x"]="VID"

mw.weather.final$floodmax[is.na(mw.weather.final$floodmax)] =0
mw.weather.final$floodmax[is.na(mw.weather.final$lhz_floodmax)] =0
save(mw.weather.final,file="data/clean/weather/mw_weather_final.RData")



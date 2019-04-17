##################################################################
# Goal : This script aims to clean up wfp price data, impute missing prices, generate mkt_thiness measures and then match them to different geospatial levels 
# purpose: use the clean market price and thinness measure to generate the most relevant price at the cluster level, TA level and IPC zone 

# Input : 
# 1. raw csv files downloaded from wfp price 
# 2. population density raster data 
# 3. coordinates of clusters. 
# 4. shapefile of livelihood zones. 

# Output: 
# 0. market coordinates generated from market_coordinates.R 
# 1. a df of price by product by mkt by yearmon, with missing imputed by nearest market or interpolated by recent months
# 2. a df of market thinness measures by product by mkt by yearmon   
# 3. matching the price and mkt_thinness to the cluster and ipczone level 
# 
# Yujun Zhou -  03/20/18
###################################################################
rm(list=ls())


package = c("plyr","dplyr","maptools","rgeos", "rgdal", "raster","FastKNN","geosphere")

lapply(package, require, character.only = TRUE)


source("R/functions/Yearmon.R") 
source("R/functions/market_transpose.R") 
source("R/functions/NearMkt.R") 
source("R/functions/spatial_price_impute.R") 
source("R/functions/PopuWeight.R") 
source("R/functions/NameToPrice.R") 
source("R/functions/WeightedPrice.R") 
source("R/functions/MktReshape.R") 

##################################################################
#  1. get market coordinates 
##################################################################

##################################################################
# Goal : retrieve market geoordinates information 
# input : raw csv files downloaded from wfp price 
# output:  a list of market geoordinates for the given countries. 
###################################################################
source("R/functions/GoogleMapApi.r") 
map.key = ""
malawi.maize.price = read.csv("data/raw/price/malawi/maize_joined_0817.csv",stringsAsFactors = FALSE)

# find the geo-coordinates of the markets in Malawi using google map api
mkt.names.mw = colnames(malawi.maize.price)
mkt.names.mw = mkt.names.mw[6:length(mkt.names.mw)]
mkt.names.mw[42] = "BEMBEKE"
mkt.names.mw[45]= "TSANGANO"
mkt.names.mw[51]= "MONKEY BAY"

mkt.names.mw.lower = unlist(lapply(mkt.names.mw,tolower))

mkt.list.mw <- lapply(mkt.names.mw.lower, function(x){paste("grocery",x,sep=" ")})
mkt.list.mw = unlist (mkt.list.mw)


address.mw<- lapply(mkt.list.mw, function(x){paste(x,"Malawi",sep=",")})
address.mw = unlist (address.mw)
address.mw

coord.mw = coordFind(address.mw)
coord.mw$mkt = mkt.names.mw
coord.mw

# HEWE	-11.19237,	33.46928
# CHATOLOMA	-12.81557795,	33.43434
# KASIYA	-13.76667,	33.38333
# CHIMBIYA	-14.59522,	35.7987
# BEMBEKE_TURNOFF	-14.403275,	34.36941
# SHARPEVALEY	-14.60627,	34.73305
# MAYAKA	-15.58018384,	35.3545
# EMBANGWENI	-12.166667,	33.46667


mkt.coord.mw = coord.mw %>% dplyr::select(lat,lon,mkt)

coord.mw[coord.mw$mkt =="HEWE",]$lat = -11.19237
coord.mw[coord.mw$mkt =="HEWE",]$lon = 33.46928

coord.mw[coord.mw$mkt =="CHATOLOMA",]$lat = -12.81557795
coord.mw[coord.mw$mkt =="CHATOLOMA",]$lon = 33.43434

coord.mw[coord.mw$mkt =="KASIYA",]$lat = -13.76667
coord.mw[coord.mw$mkt =="KASIYA",]$lon = 33.38333

coord.mw[coord.mw$mkt =="CHIMBIYA",]$lat = -14.59522
coord.mw[coord.mw$mkt =="CHIMBIYA",]$lon = 35.7987

coord.mw[coord.mw$mkt =="BEMBEKE",]$lat = -14.403275
coord.mw[coord.mw$mkt =="BEMBEKE",]$lon =34.36941


coord.mw[coord.mw$mkt =="SHARPEVALEY",]$lat = 	-14.60627
coord.mw[coord.mw$mkt =="SHARPEVALEY",]$lon =34.73305


coord.mw[coord.mw$mkt =="MAYAKA",]$lat = -15.58018384
coord.mw[coord.mw$mkt =="MAYAKA",]$lon =35.3545


coord.mw[coord.mw$mkt =="EMBANGWENI",]$lat = -12.166667
coord.mw[coord.mw$mkt =="EMBANGWENI",]$lon =33.46667

write.csv(x= coord.mw,file = "data/clean/market/mkt_coord_mw.csv",row.names = FALSE)


#############################################################################################################
# 2. general cleaning (separete the raw data into by country and by commodity)
#############################################################################################################

source("R/functions/market_transpose.R") 


malawi.maize.price = read.csv("data/raw/price/malawi/maize_joined_0817.csv",stringsAsFactors = FALSE)
malawi.rice.price = read.csv("data/raw/price/malawi/rice_joined_0817.csv",stringsAsFactors = FALSE)
malawi.nuts.price = read.csv("data/raw/price/malawi/nuts_joined_0817.csv",stringsAsFactors = FALSE)
malawi.beans.price = read.csv("data/raw/price/malawi/beans_joined_0817.csv",stringsAsFactors = FALSE)

mw.prices.list<-list(malawi.maize.price,malawi.rice.price,malawi.nuts.price,malawi.beans.price)

mw_mkt_transpose = function(df){
    df = df %>% dplyr::select(-X)
    df.trans = as.data.frame(t(df))
    colnames(df.trans)  =   as.character(unlist(df.trans[1,]))
    df.trans = df.trans[-(1:4),]
    df.trans = df.trans %>% tibble::rownames_to_column()
    colnames(df.trans)[1] = "mkt"    
    df.trans[["mkt"]][which(df.trans[["mkt"]]=="BEMBEKE.TURN.OFF")]="BEMBEKE"
    df.trans[["mkt"]][which(df.trans[["mkt"]]=="TSANGANO.TURN.OFF")]="TSANGANO"
    df.trans[["mkt"]][which(df.trans[["mkt"]]=="MONKEY.BAY")]="MONKEY BAY"
    return(df.trans)
}


mw_prices_trans <- lapply(mw.prices.list, function(x){
  mw_mkt_transpose(x)
})

########################################################################
# 3.  impute price by the nearest market 
########################################################################

# read in the mkt coordinates 

mkt_coord_mw<-read.csv("data/clean/market/mkt_coord_mw.csv",stringsAsFactors = FALSE)

# find the nearest mkt for each market using NearMKt function
source("R/functions/NearMkt.R") 
near_mw = NearMkt(mkt_coord_mw)

 
# impute missing price by the price of the nearest mkt using SpatialPriceImpu function
source("R/functions/spatial_price_impute.R") 
mw_prices_imputed <- lapply(mw_prices_trans, function(x){
  SpatialPriceImpu(x,near_mw)
})
################################################################################
# 4. generate mkt_thinness measure for each market 
###############################################################################
# mkt thinness for each market 
# number of missings for each yearmon for each mkt 
 

mw_mktthin<- lapply(mw_prices_trans,function(x){ifelse(is.na(x), 1, 0)})
for (i in 1:length(mw_prices_trans)) {
  mw_mktthin[[i]][,1]<-mw_prices_trans[[i]][,1]
}



mw_names = c("maize","rice","nuts","beans")

path = "data/clean/market/impute_thin/"

###########################################################################################
####  write imputed prices
###########################################################################################

for (i in 1:length(mw_names)){
  write.csv(mw_prices_imputed[[i]], paste(path,paste(mw_names[i],"_price_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}


###########################################################################################
####  write market thinness variable 
###########################################################################################
for (i in 1:length(mw_names)){
  write.csv(mw_mktthin[[i]], paste(path,paste(mw_names[i],"_mktthin_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}

################################################################################
# 5.  livelihood zones with population weights for each market shed
###############################################################################


#landscan_pop <- raster("shapefiles/LandScandata/Population/lspop2011") # land scan data 

# library(curl);
# id <- "0B-wuZ2XMFIBUd09Ob0pKVkRzQTA";
# sURL <- sprintf("https://docs.google.com/uc?id=%s&export=download", id);
# con <- curl(sURL);
# read.csv(con)
 
 
landscan_pop <- raster("D:/work related/LandScandata/Population/lspop2011") # land scan data (not uploaded)
 
 
lhz_mw <- readOGR("data/shapefiles/livelihood_zone/malawi/livelihood zone 2012/MW_Admin1_LHZ_2012.3/MW_Admin1_LHZ_2012.3.shp")                  # Tanzania livelihood zones
lhz_mw_intersect <- readOGR("data/shapefiles/livelihood_zone/malawi/mw_intersect.shp")  # intersection of lhz and market_thinness


 

source("R/functions/PopuWeight.R")

mw_popweight = PopuWeight(landscan_pop,lhz_mw,lhz_mw_intersect)

write.csv(mw_popweight,"data/clean/market/mw_popweight.csv",row.names = FALSE)

###############################################################################
# 6. link price and mkt_thinness measure for livelihood zones based on the pop weight computed above
###############################################################################
 
 
mw_popweight <- read.csv("data/clean/market/mw_popweight.csv",stringsAsFactors = FALSE)

path = "data/clean/market/impute_thin/"

file_list <- list.files(path=path,
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)),
         function(i){read.csv(paste(path,i,sep=""),stringsAsFactors = FALSE)}), envir = .GlobalEnv)


# save the prices in a list to make the loop easy

 mw_prices_impu<-list(maize_price_mw,rice_price_mw,nuts_price_mw,beans_price_mw)

 mw_mktthin<-list(maize_mktthin_mw,rice_mktthin_mw,nuts_mktthin_mw,beans_mktthin_mw)



source("R/functions/NameToPrice.R")

 

mw_lhz_price_unweight <- lapply(mw_prices_impu, function(x){
  NameToPrice(mw_popweight,x)
})



 

mw_lhz_mktthin_unweight <- lapply(mw_mktthin, function(x){
  NameToPrice(mw_popweight,x)
})

source("R/functions/WeightedPrice.R")

 

mw_lhz_price <- lapply(mw_lhz_price_unweight, function(x){
  WeightedPrice(x)
})


mw_lhz_mktthin <- lapply(mw_lhz_price_unweight, function(x){
  WeightedPrice(x)
}) 


mw_names = c("maize","rice","nuts","beans")

path = "data/clean/market/lhz_prices/"


for (i in 1:length(mw_names)){
  write.csv(mw_lhz_price[[i]], paste(path,paste(mw_names[i],"_lhz_price_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}


 
for (i in 1:length(mw_names)){
  write.csv(mw_lhz_mktthin[[i]], paste(path,paste(mw_names[i],"_lhz_mktthin_mw.csv",sep = ""),sep = "" ),row.names=FALSE )
}

#


###############################################################################
# 7. link price and mkt_thinness measure at the Cluster level 
###############################################################################
# need a concordance table with cluster and its nearest mkt using MktNearCluster function 

# Market geo-coordinates 
 
mkt_coord_mw<-read.csv("data/clean/market/mkt_coord_mw.csv",stringsAsFactors = FALSE)

# cluster geo-coordinates 
 
clust_coord_mw<-read.csv("data/clean/concordance/Malawi_coord.csv",stringsAsFactors = FALSE)

clust_coord_mw = na.omit(clust_coord_mw)


source("R/functions/MktNearCluster.R") 

# cluster geo-coordinates 
cluster_mkt_concord_mw = MktNearCluster(clust_coord_mw,mkt_coord_mw)


 colnames(cluster_mkt_concord_mw)[2] = "mkt"

# read in the imputed price data 
path = "data/clean/market/impute_thin/"

file_list <- list.files(path=path, 
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)), 
         function(i){read.csv(paste(path,i,sep=""),stringsAsFactors = FALSE)}), envir = .GlobalEnv)




# save the prices in a list to make the loop easy 
 mw_prices_impu<-list(maize_price_mw,rice_price_mw,nuts_price_mw,beans_price_mw)

 mw_mktthin<-list(maize_mktthin_mw,rice_mktthin_mw,nuts_mktthin_mw,beans_mktthin_mw)


source("R/functions/NameToPrice.R") 

 

mw_cluster_price <- lapply(mw_prices_impu, function(x){
  NameToPrice(cluster_mkt_concord_mw,x)
})

 
mw_cluster_mktthin <- lapply(mw_mktthin, function(x){
  NameToPrice(cluster_mkt_concord_mw,x)
})

 
mw_names = c("maize","rice","nuts","beans")

dir.create("data/clean/market/cluster_prices")
path = "data/clean/market/cluster_prices/"

 
for (i in 1:length(mw_names)){
  write.csv(mw_cluster_price[[i]], paste(path,paste(mw_names[i],"_clust_price_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}


 
 
for (i in 1:length(mw_names)){
  write.csv(mw_cluster_mktthin[[i]], paste(path,paste(mw_names[i],"_clust_mktthin_mw.csv",sep = ""),sep = "" ),row.names=FALSE)
}

  


###############################################################################
# 8. Transpose all the prices 
###############################################################################

rm(list=ls())

path = "data/clean/market/cluster_prices/"

file_list <- list.files(path=path, 
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)), 
         function(i){read.csv(paste(path,i,sep=""),stringsAsFactors = FALSE)}), envir = .GlobalEnv)




path = "data/clean/market/lhz_prices/"

file_list <- list.files(path=path, 
                        pattern = "csv$",
                        full.names=FALSE)
dfnames<-file_list
dfnames <- gsub(".csv","", dfnames)


list2env(
  lapply(setNames(file_list, make.names(dfnames)), 
         function(i){read.csv(paste(path,i,sep=""),stringsAsFactors = FALSE)}), envir = .GlobalEnv)


 
mw_names = c("maize","rice","nuts","beans")

 mw_clust_price<-list(maize_clust_price_mw,rice_clust_price_mw,nuts_clust_price_mw,beans_clust_price_mw)

 mw_clust_mktthin<-list(maize_clust_mktthin_mw,rice_clust_mktthin_mw,nuts_clust_mktthin_mw,beans_clust_mktthin_mw)

 mw_lhz_price<-list(maize_lhz_price_mw,rice_lhz_price_mw,nuts_lhz_price_mw,beans_lhz_price_mw)

 mw_lhz_mktthin<-list(maize_lhz_mktthin_mw,rice_lhz_mktthin_mw,nuts_lhz_mktthin_mw,beans_lhz_mktthin_mw)


source("R/functions/MktReshape.R") 
 
 
clust_price_mw_long = lapply(mw_clust_price,MktReshape)
clust_mktthin_mw_long = lapply(mw_clust_mktthin,MktReshape)

lhz_price_mw_long = lapply(mw_lhz_price,MktReshape)
lhz_mktthin_mw_long = lapply(mw_lhz_mktthin,MktReshape)

 
 
# change column names 
 
mw_clust_price_names= paste("clust",mw_names,"price",sep = "_")
mw_lhz_price_names= paste("lhz",mw_names,"price",sep = "_")
mw_clust_thin_names= paste("clust",mw_names,"mktthin",sep = "_")
mw_lhz_thin_names= paste("lhz",mw_names,"mktthin",sep = "_")

for (i in 1:length(mw_clust_price_names)){
  colnames(clust_price_mw_long[[i]])[colnames(clust_price_mw_long[[i]])=="value"] =  mw_clust_price_names[i]
  colnames(clust_mktthin_mw_long[[i]])[colnames(clust_mktthin_mw_long[[i]])=="value"] =  mw_clust_thin_names[i]
  colnames(lhz_price_mw_long[[i]])[colnames(lhz_price_mw_long[[i]])=="value"] =  mw_lhz_price_names[i]
  colnames(lhz_mktthin_mw_long[[i]])[colnames(lhz_mktthin_mw_long[[i]])=="value"] =  mw_lhz_thin_names[i]
}

 
###############################################################################
# 9. Merge all the prices  and save the data 
###############################################################################

# merge different prices in Malawi 

mw_cluster_prices = dplyr::left_join(clust_price_mw_long[[1]],clust_price_mw_long[[2]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_price_mw_long[[3]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_price_mw_long[[4]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[1]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[2]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[3]])
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,clust_mktthin_mw_long[[4]])

mw_cluster_prices$clust_maize_mktthin= ifelse(is.na(mw_cluster_prices$clust_maize_price), 1, 0)
mw_cluster_prices$clust_rice_mktthin= ifelse(is.na(mw_cluster_prices$clust_rice_price), 1, 0)
mw_cluster_prices$clust_nuts_mktthin= ifelse(is.na(mw_cluster_prices$clust_nuts_price), 1, 0)
mw_cluster_prices$clust_beans_mktthin= ifelse(is.na(mw_cluster_prices$clust_beans_price), 1, 0)


mw_lhz_prices = left_join(lhz_price_mw_long[[1]],lhz_price_mw_long[[2]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_price_mw_long[[3]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_price_mw_long[[4]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[1]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[2]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[3]])
mw_lhz_prices = left_join(mw_lhz_prices,lhz_mktthin_mw_long[[4]])

mw_lhz_prices$lhz_maize_mktthin= ifelse(is.na(mw_lhz_prices$lhz_maize_price), 1, 0)
mw_lhz_prices$lhz_rice_mktthin= ifelse(is.na(mw_lhz_prices$lhz_rice_price), 1, 0)
mw_lhz_prices$lhz_nuts_mktthin= ifelse(is.na(mw_lhz_prices$lhz_nuts_price), 1, 0)
mw_lhz_prices$lhz_beans_mktthin= ifelse(is.na(mw_lhz_prices$lhz_beans_price), 1, 0)


mw_concordance <-  read.csv("data/clean/concordance/mw_cluster_lhz.csv",stringsAsFactors = FALSE)
mw_concordance =  mw_concordance %>% dplyr::select(ea_id,FNID)%>% na.omit() %>% dplyr::distinct()%>% mutate_all(funs(as.character))
mw_cluster_prices = mw_cluster_prices  %>% dplyr::distinct()%>% mutate( ea_id = as.character(ea_id) ) 
mw_cluster_prices = dplyr::left_join(mw_cluster_prices,mw_concordance)
mw_price_merge_final = dplyr::left_join(mw_cluster_prices,mw_lhz_prices)  %>% arrange(ea_id,yearmon)

save(mw_price_merge_final,file = "data/clean/market/mw_price_final.Rdata")


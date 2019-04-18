####################################################################################################################################
# Goal : This script aims to replicate the tables and figures used in this paper 
# Yujun Zhou -  04/17/19
###################################################################
rm(list = ls())
require(tidyverse)

mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

mw.cluster = mw.cluster %>% 
  mutate(clust_maize_price  = log(clust_maize_price))
colSums(is.na(mw.cluster))


# Split by year 
unique(mw.cluster$FS_year)

mw.2010.cluster= mw.cluster %>% 
  dplyr::filter(FS_year==2010|FS_year==2011) 

mw.2013.cluster= mw.cluster %>% 
  dplyr::filter(FS_year==2013)  



 ###########################################
## Replicate Table 1: summary statistics   
###########################################




###########################################
## Replicate Table 2: food security regression results  
###########################################

logFCS.ols <- lm(logFCS ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                   clust_maize_price +  clust_maize_mktthin + ag_percent + 
                   elevation  + nutri_rent_moderate_constraint   + 
                   dist_road + dist_admarc + roof_natural + number_celphones +hhsize + 
                   head_age + head_gender + asset_index, data=mw.2010.cluster)  # build linear regression model on logFCS


hdds.ols <- lm(HDDS ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                 clust_maize_price +  clust_maize_mktthin + ag_percent + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural + number_celphones +hhsize + 
                 head_age + head_gender + asset_index, data=mw.2010.cluster)  # build linear regression model on HDDS

rcsi.ols <- lm(rCSI  ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                 clust_maize_price +  clust_maize_mktthin + ag_percent + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural + number_celphones +hhsize + 
                 head_age + head_gender + asset_index, data=mw.2010.cluster)  # build linear regression model on rCSI

stargazer::stargazer(logFCS.ols,hdds.ols,rcsi.ols,type = "text")


###########################################
## Replicate Table 3: IPC regression results  
###########################################

IPC.rcsi  <- lm(IPC1 ~rCSI, data=mw.2010.cluster) 
IPC.logfcs  <- lm(IPC1 ~logFCS, data=mw.2010.cluster) 
IPC.hdds  <- lm(IPC1 ~ HDDS , data=mw.2010.cluster) 

stargazer::stargazer(IPC.rcsi,IPC.logfcs,IPC.hdds,type = "text")



###########################################
## Cluster level OLS prediction   
###########################################

library(caret) 

# Remove any missings, before during the prediction
mw.2010.cluster = mw.2010.cluster %>% 
  dplyr::filter(!is.na(IPC12))

#colSums(is.na(mw.2010.cluster))

length(predicted.logFCS)
length(test2013$logFCS)


mw.2013.cluster = mw.2013.cluster %>% 
  dplyr::filter(!is.na(IPC12))
# colSums(is.na(mw.2013.cluster))


# Define Train and Test based on years 
train2010 = mw.2010.cluster %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,IPC12,raincytot,day1rain,maxdaysnorain,floodmax,
                clust_maize_price,clust_maize_mktthin,ag_percent,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural,number_celphones,hhsize,
                head_gender,asset_index) %>%  na.omit()

test2013 = mw.2013.cluster %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,IPC12,raincytot,day1rain,maxdaysnorain,floodmax,
                clust_maize_price,clust_maize_mktthin,ag_percent,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural,number_celphones,hhsize,
                head_gender,asset_index)  


# logFCS 
lm.logFCS<-train(logFCS ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
               clust_maize_price +  clust_maize_mktthin + ag_percent + 
               elevation  + nutri_rent_moderate_constraint + 
               dist_road + dist_admarc + roof_natural + number_celphones +hhsize + 
               head_gender +asset_index, data = train2010, method = "lm")

predicted.logFCS = predict(lm.logFCS, test2013, se.fit = TRUE)

cor(predicted.logFCS, test2013$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = test2013$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                 clust_maize_price +  clust_maize_mktthin + ag_percent + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural + number_celphones +hhsize + 
                 head_gender + asset_index, data = train2010, method = "lm")

predicted.HDDS = predict(lm.HDDS, test2013, se.fit = TRUE)

cor(predicted.HDDS, test2013$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                 clust_maize_price +  clust_maize_mktthin + ag_percent + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural + number_celphones +hhsize + 
                 head_gender + asset_inde, data = train2010, method = "lm")

predicted.rCSI = predict(lm.rCSI, test2013, se.fit = TRUE)

cor(predicted.rCSI, test2013$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = test2013$rCSI)


class(predicted.logFCS)
class(test2013$logFCS)

length(predicted.logFCS)
length(test2013$logFCS)









###########################################
## Replicate Table 4: IPC regression results  
###########################################




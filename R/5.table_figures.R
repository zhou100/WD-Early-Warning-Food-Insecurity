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
# read hh level data 
mw.hh = read.csv("data/mw_dataset_hh.csv",stringsAsFactors = FALSE)

# Summary at the household level 
table1.summary.hh = mw.hh %>% 
mutate(logFCS = log(FCS)) %>%
mutate(FS_year = if_else(FS_year!=2013,2010,2013)) %>%
group_by(FS_year) %>% 
dplyr::select(logFCS,rCSI,HDDS) %>%
gather(-FS_year,key="var",value ="value") %>%
group_by(FS_year,var) %>%
summarise(mean=mean(value), median=median(value),sd=sd(value),min=min(value),max=max(value) )  
 
print(table1.summary.hh)


# read cluster level data 
mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

nrow(mw.cluster %>% dplyr::filter(FS_year!=2013))
nrow(mw.cluster %>% dplyr::filter(FS_year==2013))

table1.summary.cluster = mw.cluster %>% 
mutate(log_price = log(clust_maize_price) ) %>%  
mutate(FS_year = if_else(FS_year!=2013,2010,2013)) %>%
dplyr::select(FS_year,logFCS,rCSI,HDDS,
IPC1,IPC12,raincytot,day1rain,maxdaysnorain,floodmax,
log_price,clust_maize_mktthin,percent_ag,
elevation,nutri_rent_moderate_constraint,
dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,
hh_age,hh_gender,asset_index2) %>%
group_by(FS_year) %>%
gather(-FS_year,key="var",value ="value") %>%
group_by(FS_year,var) %>%
summarise(mean=mean(value,na.rm = TRUE), median=median(value,na.rm = TRUE),sd=sd(value,na.rm = TRUE),min=min(value,na.rm = TRUE),max=max(value,na.rm = TRUE) )  


print(table1.summary.cluster)

mw.cluster$IPC12


###########################################
## Replicate Table 2: food security regression results  
###########################################

logFCS.ols <- lm(logFCS ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint   + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                   hh_age + hh_gender + asset_index2, data=mw.2010.cluster)  # build linear regression model on logFCS


hdds.ols <- lm(HDDS ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + hh_gender + asset_index2, data=mw.2010.cluster)  # build linear regression model on HDDS

rcsi.ols <- lm(rCSI  ~ IPC1+IPC12+raincytot + day1rain + maxdaysnorain + floodmax + 
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + hh_gender + asset_index2, data=mw.2010.cluster)  # build linear regression model on rCSI

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
 

mw.2013.cluster = mw.2013.cluster %>% 
  dplyr::filter(!is.na(IPC12))
# colSums(is.na(mw.2013.cluster))


# Define Train and Test based on years 
train2010 = mw.2010.cluster %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,IPC12,raincytot,day1rain,lhz_maxdaysnorain,lhz_floodmax,
                clust_maize_price,clust_maize_mktthin,percent_ag,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,hh_age,
                hh_gender,asset_index2) %>%  na.omit()

test2013 = mw.2013.cluster %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,IPC12,raincytot,day1rain,lhz_maxdaysnorain,lhz_floodmax,
                clust_maize_price,clust_maize_mktthin,percent_ag,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,hh_age,
                hh_gender,asset_index2)  


# logFCS 
lm.logFCS<-train(logFCS ~ IPC1+IPC12+raincytot + day1rain + 
               clust_maize_price +  clust_maize_mktthin + percent_ag + 
               elevation  + nutri_rent_moderate_constraint + 
               dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
               hh_gender +asset_index2, data = train2010, method = "lm")

predicted.logFCS = predict(lm.logFCS, test2013, se.fit = TRUE)




cor(predicted.logFCS, test2013$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = test2013$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+IPC12+raincytot + day1rain +  
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2, data = train2010, method = "lm")

predicted.HDDS = predict(lm.HDDS, test2013, se.fit = TRUE)

cor(predicted.HDDS, test2013$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1+IPC12+raincytot + day1rain +  
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2, data = train2010, method = "lm")

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




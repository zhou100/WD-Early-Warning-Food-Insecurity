####################################################################################################################################
# Goal : This script aims to replicate the tables and figures used in this paper 
# Yujun Zhou -  04/17/19
###################################################################
rm(list = ls())
require(tidyverse)


mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

#mw.cluster2010= mw.cluster[mw.cluster$FS_year<2013 & mw.cluster$FNID=="MW2012C3010102" ,]
#"MW2012C3010102" %in% unique(mw.cluster2010$FNID)

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

# mw.reg= mw.2010.cluster %>% na.omit(IPC12)

logFCS.ols <- lm(logFCS ~ IPC1+raincytot + day1rain + maxdaysnorain + floodmax + 
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint   + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                   hh_age + hh_gender + asset_index2, data=mw.2010.cluster)  # build linear regression model on logFCS


hdds.ols <- lm(HDDS ~ IPC1+raincytot + day1rain + maxdaysnorain + floodmax + 
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + hh_gender + asset_index2, data=mw.2010.cluster)  # build linear regression model on HDDS

rcsi.ols <- lm(rCSI  ~ IPC1+raincytot + day1rain + maxdaysnorain + floodmax + 
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
mw.2010.cluster.ipc12 = mw.2010.cluster %>% 
  dplyr::filter(!is.na(IPC12))


mw.2010.cluster.ipc1 = mw.2010.cluster %>% 
  dplyr::filter(!is.na(IPC1))

#colSums(is.na(mw.2010.cluster))

mw.2013.cluster.ipc1 = mw.2013.cluster %>% 
  dplyr::filter(!is.na(IPC1))

mw.2013.cluster.ipc12 = mw.2013.cluster %>% 
  dplyr::filter(!is.na(IPC12))
# colSums(is.na(mw.2013.cluster))


# Define Train and Test based on years 
train2010 = mw.2010.cluster.ipc1 %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,IPC12,raincytot,day1rain,lhz_maxdaysnorain,lhz_floodmax,
                clust_maize_price,clust_maize_mktthin,percent_ag,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,hh_age,
                hh_gender,asset_index2) %>%  na.omit()

test2013 = mw.2013.cluster.ipc12 %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,IPC12,raincytot,day1rain,lhz_maxdaysnorain,lhz_floodmax,
                clust_maize_price,clust_maize_mktthin,percent_ag,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,hh_age,
                hh_gender,asset_index2)  


# logFCS 
lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + 
               clust_maize_price +  clust_maize_mktthin + percent_ag + 
               elevation  + nutri_rent_moderate_constraint + 
               dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
               hh_gender +asset_index2, data = mw.2010.cluster.ipc1, method = "lm")

predicted.logFCS = predict(lm.logFCS, test2013, se.fit = TRUE)


# cor(predicted.logFCS, test2013$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = test2013$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+ raincytot + day1rain +  
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2, data = train2010, method = "lm")

predicted.HDDS = predict(lm.HDDS, test2013, se.fit = TRUE)

cor(predicted.HDDS, test2013$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2, data = train2010, method = "lm")

predicted.rCSI = predict(lm.rCSI, test2013, se.fit = TRUE)


cor(predicted.rCSI, test2013$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = test2013$rCSI)





##################################################################
# Split by year, LASSO
##################################################################


lambda <- 10^seq(-3, 3, length = 100)

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + 
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2, data = mw.2010.cluster.ipc1, method = "glmnet",trControl = trainControl("cv", number = 10),
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.logFCS = predict(lm.logFCS, test2013, se.fit = TRUE)


# cor(predicted.logFCS, test2013$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = test2013$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1 + raincytot + day1rain +  
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2, data = train2010, method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.HDDS = predict(lm.HDDS, test2013, se.fit = TRUE)

cor(predicted.HDDS, test2013$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2, data = train2010,method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.rCSI = predict(lm.rCSI, test2013, se.fit = TRUE)


cor(predicted.rCSI, test2013$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = test2013$rCSI)


################################################################################################
# Table 4: The percentage of food insecure clusters correctly predicted to be food insecure. 
################################################################################################
logFCS  = bind_cols(as.data.frame(predicted.logFCS), as.data.frame(test2013$logFCS))
names() = c("logFCS","logFCS_predict")

logFCS$cat_logFCS<-cut(logFCS$logFCS, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
logFCS$cat_logFCS_predict<-cut(logFCS$logFCS_predict, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))

confusionMatrix(logFCS$cat_logFCS_predict,logFCS$cat_logFCS)



logFCS_hh$cat_logFCS<-cut(logFCS_hh$logFCS, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
logFCS_hh$cat_logFCS_predict<-cut(logFCS_hh$logFCS_predict, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))

confusionMatrix(logFCS_hh$cat_logFCS_predict,logFCS_hh$cat_logFCS)

plot(logFCS$logFCS,logFCS$logFCS_predict)
abline(0,1)
rmse_logFCS = sqrt( sum( (logFCS$logFCS_predict - logFCS$logFCS)^2 , na.rm = TRUE ) / nrow(logFCS) )



# HDDS 3 6 
HDDS$cat_HDDS<-cut(HDDS$HDDS, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
HDDS$cat_HDDS_predict<-cut(HDDS$HDDS_predict, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))

confusionMatrix(HDDS$cat_HDDS_predict,HDDS$cat_HDDS)

plot(HDDS$HDDS,HDDS$HDDS_predict)
abline(0,1)
rmse_HDDS = sqrt( sum( (HDDS$HDDS_predict - HDDS$HDDS)^2 , na.rm = TRUE ) / nrow(HDDS) )




# rcsi P1 < 4
#P2 = 4-17
#P3 = 17-42
#P4/5 > 42



RCSI$cat_RCSI<-cut(RCSI$RCSI, c(-Inf,4,17,42,Inf),labels=c("Food Secure", "Mild","Moderate","Severe"))
RCSI$cat_RCSI_predict<-cut(RCSI$RCSI_predict, c(-Inf,4,17,42,Inf),labels=c(c("Food Secure", "Mild","Moderate","Severe")))

confusionMatrix(RCSI$cat_RCSI_predict,RCSI$cat_RCSI)

plot(RCSI$RCSI,RCSI$RCSI_predict)
abline(0,1)

RCSI$RCSI_predict[RCSI$RCSI_predict<0] <-0

plot(RCSI$RCSI,RCSI$RCSI_predict)
abline(0,1)


###########################################
##  Split randomly,OLS
###########################################


# 1. Update the FEWS IPC data to reflect the corrected values for the zones
# (My hunch is this will make the IPC perform worse, and variation in the IPC fall).


# 2. Split the data geographically (R1) rather than temporally for model building.

mw.cluster.ipc1 = mw.cluster %>% 
  dplyr::filter(!is.na(IPC1))


trainIndex <- createDataPartition(mw.cluster.ipc1$FCS, p = .7, 
                                  list = FALSE, 
                                  times = 1)

mw.cluster_Train <- mw.cluster.ipc1[ trainIndex,]
mw.cluster_Test  <- mw.cluster.ipc1[-trainIndex,]

 
lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + 
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain + 
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)



lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain + 
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)



# 3. Estimate quarter fixed effects. (R1)



# 4. Relatedly, perhaps estimate month fixed effects without prices, 
# swap in GIEWS prices for 4 markets instead of our good price data (R2)



# 5. Estimate (back of the envelope?) 

# spatial temporal variation between our good price data and GIEWS prices
# Lasso (R1)


###########################################
##  Split randomly,lasso
###########################################

lambda <- 10^seq(-3, 3, length = 100)

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + 
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2, data = mw.cluster_Train,  method = "glmnet",trControl = trainControl("cv", number = 10),
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain + 
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2, data = mw.cluster_Train,  method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)


lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain + 
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2, data = mw.cluster_Train, method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)


# regularization penalty.

lm.rCSI$bestTune$lambda

coef(lm.HDDS$finalModel,lm.HDDS$bestTune$lambda)

coef(lm.rCSI$finalModel,lm.rCSI$bestTune$lambda)


###########################################
## Replicate Table 4: IPC regression results  
###########################################




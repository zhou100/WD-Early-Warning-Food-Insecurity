####################################################################################################################################
# Goal : This script aims to replicate the tables used in this paper 
# Yujun Zhou -  04/17/19

# Make the IPC value missing map 
# 
# Quarter 
# 
# Fixed effect in the regression table 
# 
# Region/ ipczone fixed effect 
# 
# 
# Or quarter by region fixed effect 
# 
# Report the level of fixed effects 
# 
# Variables ( Lasso choose varibale, drop some )



###################################################################
rm(list = ls())
require(tidyverse)


mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

mw.cluster = mw.cluster %>% 
  mutate(clust_maize_price  = log(clust_maize_price))
colSums(is.na(mw.cluster))

# Check where IPC1 is missing 
mw.cluster[is.na(mw.cluster$IPC1),]$FNID

# Split by year 
unique(mw.cluster$FS_year)

mw.2010.cluster= mw.cluster %>% 
  dplyr::filter(FS_year==2010|FS_year==2011) 


length(unique(mw.2010.cluster$FNID))

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
mutate(log_price = log(clust_maize_current) ) %>%  
mutate(FS_year = if_else(FS_year!=2013,2010,2013)) %>%
dplyr::select(FS_year,logFCS,rCSI,HDDS,
IPC1,IPC12,raincytot,day1rain,maxdaysnorain,floodmax,
clust_maize_current,clust_maize_mktthin_current,percent_ag,
elevation,nutri_rent_moderate_constraint,
dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,
hh_age,hh_gender,asset_index2) %>%
group_by(FS_year) %>%
gather(-FS_year,key="var",value ="value") %>%
group_by(FS_year,var) %>%
summarise(mean=mean(value,na.rm = TRUE), median=median(value,na.rm = TRUE),sd=sd(value,na.rm = TRUE),min=min(value,na.rm = TRUE),max=max(value,na.rm = TRUE) )  


print(table1.summary.cluster)



###########################################
## Replicate Table 2: food security regression results  
###########################################

# mw.reg= mw.2010.cluster %>% na.omit(IPC12)

logFCS.ols <- lm(logFCS ~ IPC1 +IPC12+raincytot + day1rain + maxdaysnorain  + 
                   clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint   + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                   hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.2010.cluster)  # build linear regression model on logFCS


hdds.ols <- lm(HDDS ~ IPC1 +IPC12+ raincytot + day1rain + maxdaysnorain  + 
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.2010.cluster)  # build linear regression model on HDDS

rcsi.ols <- lm(rCSI  ~ IPC1 +IPC12+ raincytot  + maxdaysnorain + 
                   hh_gender + percent_ag+ dist_admarc + day1rain + elevation +dist_road +
                 clust_maize_current +  clust_maize_mktthin_current  + 
                   + nutri_rent_moderate_constraint   + 
                    roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + asset_index2 + quarter1 + quarter2 +quarter3, data=mw.2010.cluster)  # build linear regression model on rCSI
 
 
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

mw.2010.cluster.ipc1 = mw.2010.cluster %>% 
  dplyr::filter(!is.na(IPC1))

#colSums(is.na(mw.2010.cluster))

mw.2013.cluster.ipc1 = mw.2013.cluster %>% 
  dplyr::filter(!is.na(IPC1))

write.csv(mw.2010.cluster.ipc1,"data/clean/mw.2010.cluster.csv",row.names = FALSE)

write.csv(mw.2013.cluster.ipc1,"data/clean/mw.2013.cluster.csv",row.names = FALSE)


###################################################################
## Full model cluster level predictions 
####################################################################
# logFCS 
lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +   floodmax + maxdaysnorain+
               clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
               elevation  + nutri_rent_moderate_constraint + 
               dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
               #month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+month10+month11+ 
               quarter1+quarter2+quarter3 +
                             hh_gender +asset_index2, data = mw.2010.cluster.ipc1, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.2013.cluster.ipc1, se.fit = TRUE)


# cor(predicted.logFCS, mw.2013.cluster.ipc1$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = mw.2013.cluster.ipc1$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+raincytot + day1rain +     floodmax + maxdaysnorain+
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 quarter1+quarter2+quarter3 +
                 hh_gender + asset_index2, data = mw.2010.cluster.ipc1, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.2013.cluster.ipc1, se.fit = TRUE)

cor(predicted.HDDS, mw.2013.cluster.ipc1$HDDS, method = c("pearson"))^2
postResample(pred = predicted.HDDS, obs = mw.2013.cluster.ipc1$HDDS)


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 +raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 quarter1+quarter2+quarter3 +
                 hh_gender + asset_index2, data = mw.2010.cluster.ipc1, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.2013.cluster.ipc1, se.fit = TRUE)

cor(predicted.rCSI, mw.2013.cluster.ipc1$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = mw.2013.cluster.ipc1$rCSI)

###################################################################
## IPC value model cluster level predictions 
####################################################################
# logFCS  IPC value model
lm.logFCS.ipc<-train(logFCS ~IPC1, data = mw.2010.cluster.ipc1, method = "lm")

predicted.logFCS.ipc = predict(lm.logFCS.ipc, mw.2013.cluster.ipc1, se.fit = TRUE)
postResample(pred = predicted.logFCS.ipc, obs = mw.2013.cluster.ipc1$logFCS)


# cor(predicted.logFCS, mw.2013.cluster.ipc1$logFCS, method = c("pearson"))^2



# HDDS IPC value model
lm.HDDS.ipc<-train(HDDS ~ IPC1, data = mw.2010.cluster.ipc1, method = "lm")

predicted.HDDS.ipc = predict(lm.HDDS.ipc, mw.2013.cluster.ipc1, se.fit = TRUE)

cor(predicted.HDDS.ipc, mw.2013.cluster.ipc1$HDDS, method = c("pearson"))^2


# rCSI IPC value model
lm.rCSI.ipc<-train(rCSI ~ IPC1, data = mw.2010.cluster.ipc1, method = "lm")

predicted.rCSI.ipc = predict(lm.rCSI.ipc, mw.2013.cluster.ipc1, se.fit = TRUE)

cor(predicted.rCSI.ipc, mw.2013.cluster.ipc1$rCSI, method = c("pearson"))^2



################################################################################################
# Table 4: The percentage of food insecure clusters correctly predicted to be food insecure. 
################################################################################################

##########################################################
# categorical prediction using full model
#########################################################

# FCS 28 42  

logFCS  = bind_cols(as.data.frame(predicted.logFCS), as.data.frame(mw.2013.cluster.ipc1$logFCS))
names(logFCS) = c("logFCS_predict","logFCS")

logFCS$cat_logFCS<-cut(logFCS$logFCS, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
logFCS$cat_logFCS_predict<-cut(logFCS$logFCS_predict, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
# category prediction 
confusionMatrix(data=logFCS$cat_logFCS_predict,reference=logFCS$cat_logFCS)

logFCS.matrix.full = as.matrix(confusionMatrix(logFCS$cat_logFCS_predict,logFCS$cat_logFCS))

logFCS.matrix.full
# Percentage of food insecure clusters correctly predicted to be food insecure
table4.logFCS.full = logFCS.matrix.full[2,2]/(logFCS.matrix.full[2,2] + logFCS.matrix.full[3,2])
table4.logFCS.full

# HDDS 3 6 
HDDS  = bind_cols(as.data.frame(predicted.HDDS), as.data.frame(mw.2013.cluster.ipc1$HDDS))
names(HDDS) = c("HDDS_predict","HDDS")
HDDS$cat_HDDS<-cut(HDDS$HDDS, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
HDDS$cat_HDDS_predict<-cut(HDDS$HDDS_predict, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))

confusionMatrix(HDDS$cat_HDDS_predict,HDDS$cat_HDDS)
HDDS.matrix.full = as.matrix(confusionMatrix(HDDS$cat_HDDS_predict,HDDS$cat_HDDS))


# Percentage of food insecure clusters correctly predicted to be food insecure
table4.HDDS.full = HDDS.matrix.full[2,2]/(HDDS.matrix.full[2,2] + HDDS.matrix.full[3,2])
table4.HDDS.full

# RCSI 4 17 42
RCSI  = bind_cols(as.data.frame(predicted.rCSI), as.data.frame(mw.2013.cluster.ipc1$rCSI))
names(RCSI) = c("RCSI_predict","RCSI")
RCSI$cat_RCSI<-cut(RCSI$RCSI, c(-Inf,4,17,42,Inf),labels=c("Food Secure", "Mild","Moderate","Severe"))
RCSI$cat_RCSI_predict<-cut(RCSI$RCSI_predict, c(-Inf,4,17,42,Inf),labels=c(c("Food Secure", "Mild","Moderate","Severe")))

confusionMatrix(RCSI$cat_RCSI_predict,RCSI$cat_RCSI)

RCSI.matrix.full = as.matrix(confusionMatrix(RCSI$cat_RCSI_predict,RCSI$cat_RCSI))
# Percentage of food insecure clusters correctly predicted to be food insecure
table4.RCSI.full = RCSI.matrix.full[2,2]/(RCSI.matrix.full[1,2] + RCSI.matrix.full[2,2])
table4.RCSI.full


#######################################################################################
# categorical prediction using IPC only
######################################################################################
logFCS  = bind_cols(as.data.frame(predicted.logFCS.ipc), as.data.frame(mw.2013.cluster.ipc1$logFCS))
names(logFCS) = c("logFCS_predict","logFCS")

logFCS$cat_logFCS<-cut(logFCS$logFCS, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
logFCS$cat_logFCS_predict<-cut(logFCS$logFCS_predict, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
# category prediction 
confusionMatrix(logFCS$cat_logFCS_predict,logFCS$cat_logFCS)

logFCS.matrix.ipc = as.matrix(confusionMatrix(logFCS$cat_logFCS_predict,logFCS$cat_logFCS))

# Percentage of food insecure clusters correctly predicted to be food insecure
table4.logFCS.ipc = logFCS.matrix.ipc[2,2]/(logFCS.matrix.ipc[2,2] + logFCS.matrix.ipc[3,2])


# HDDS 3 6 
HDDS  = bind_cols(as.data.frame(predicted.HDDS.ipc), as.data.frame(mw.2013.cluster.ipc1$HDDS))
names(HDDS) = c("HDDS_predict","HDDS")
HDDS$cat_HDDS<-cut(HDDS$HDDS, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
HDDS$cat_HDDS_predict<-cut(HDDS$HDDS_predict, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))

confusionMatrix(HDDS$cat_HDDS_predict,HDDS$cat_HDDS)
HDDS.matrix.ipc = as.matrix(confusionMatrix(HDDS$cat_HDDS_predict,HDDS$cat_HDDS))


# Percentage of food insecure clusters correctly predicted to be food insecure
table4.HDDS.ipc = HDDS.matrix.ipc[2,2]/(HDDS.matrix.ipc[2,2] + HDDS.matrix.ipc[3,2])

# RCSI 4 17 42
RCSI  = bind_cols(as.data.frame(predicted.rCSI.ipc), as.data.frame(mw.2013.cluster.ipc1$rCSI))
names(RCSI) = c("RCSI_predict","RCSI")
RCSI$cat_RCSI<-cut(RCSI$RCSI, c(-Inf,4,17,42,Inf),labels=c("Food Secure", "Mild","Moderate","Severe"))
RCSI$cat_RCSI_predict<-cut(RCSI$RCSI_predict, c(-Inf,4,17,42,Inf),labels=c(c("Food Secure", "Mild","Moderate","Severe")))

confusionMatrix(RCSI$cat_RCSI_predict,RCSI$cat_RCSI)

RCSI.matrix.ipc= as.matrix(confusionMatrix(RCSI$cat_RCSI_predict,RCSI$cat_RCSI))
# Percentage of food insecure clusters correctly predicted to be food insecure
table4.RCSI.ipc = RCSI.matrix.ipc[2,2]/(RCSI.matrix.ipc[1,2] + RCSI.matrix.ipc[2,2])




# Table 4 

table4.matrix = matrix(nrow=2,ncol=3)

table4.matrix[1,1] = table4.HDDS.ipc
table4.matrix[1,2] = table4.logFCS.ipc
table4.matrix[1,3] = table4.RCSI.ipc
table4.matrix[2,1] = table4.HDDS.full
table4.matrix[2,2] = table4.logFCS.full
table4.matrix[2,3] = table4.RCSI.full



table4.matrix



################################################################################################
# Table s1: Regression of the tail  
################################################################################################

mw.tail.fcs =  mw.2010.cluster %>%  
  dplyr::filter(logFCS<log(42))

mw.tail.hdds =  mw.2010.cluster %>%  
  dplyr::filter(HDDS<6)

mw.tail.rcsi =  mw.2010.cluster %>%  
  dplyr::filter(rCSI>4)

logFCS.ols <- lm(logFCS ~ IPC1 +IPC12+raincytot + day1rain + maxdaysnorain  + 
                   clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint   + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                   hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.tail.fcs)  # build linear regression model on logFCS


hdds.ols <- lm(HDDS ~ IPC1 +IPC12+ raincytot + day1rain + maxdaysnorain  + 
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.tail.hdds)  # build linear regression model on HDDS

rcsi.ols <- lm(rCSI  ~ IPC1 +IPC12+ raincytot  + maxdaysnorain + 
                 hh_gender + percent_ag+ dist_admarc + day1rain + elevation +dist_road +
                 clust_maize_current +  clust_maize_mktthin_current  + 
                 + nutri_rent_moderate_constraint   + 
                 roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + asset_index2 + quarter1 + quarter2 +quarter3, data=mw.tail.rcsi)  # build linear regression model on rCSI


stargazer::stargazer(logFCS.ols,hdds.ols,rcsi.ols,type = "text")




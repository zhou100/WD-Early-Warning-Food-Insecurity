##################################################################
# Read data 
##################################################################

# 1. Update the FEWS IPC data to reflect the corrected values for the zones
# (My hunch is this will make the IPC perform worse, and variation in the IPC fall).

# 3. Estimate quarter fixed effects. (R1)
mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

# fs.year.month = mw.hh %>% distinct(FS_year,FS_month,ea_id)
# nrow(fs.year.month)


mw.cluster$FS_month = as.integer(mw.cluster$FS_month)
mw.cluster.ipc1 = mw.cluster %>% 
  dplyr::filter(!is.na(IPC1))



# generate quarter and month fixed effect 
mw.cluster.ipc1 = mw.cluster.ipc1 %>% 
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
  
  

mw.2010.cluster= mw.cluster.ipc1 %>% 
  dplyr::filter(FS_year==2010|FS_year==2011) 

mw.2013.cluster= mw.cluster.ipc1 %>% 
  dplyr::filter(FS_year==2013)  


train2010 = mw.2010.cluster %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,raincytot,day1rain,maxdaysnorain,floodmax,
                lhz_maxdaysnorain,lhz_floodmax,
                clust_maize_price,clust_maize_mktthin,percent_ag,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,hh_age,
                hh_gender,asset_index2,starts_with("month"),starts_with("quarter")) %>%  na.omit()

test2013 = mw.2013.cluster %>% 
  dplyr::select(logFCS,HDDS,rCSI,IPC1,raincytot,day1rain,maxdaysnorain,floodmax,
                lhz_maxdaysnorain,lhz_floodmax,
                clust_maize_price,clust_maize_mktthin,percent_ag,
                elevation,  nutri_rent_moderate_constraint,
                dist_road,dist_admarc,roof_natural_inverse,number_celphones,hhsize,hh_age,
                hh_gender,asset_index2,starts_with("month"),starts_with("quarter")) 


lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +   floodmax + maxdaysnorain+
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2+quarter1+quarter2+quarter3+quarter4 , data = train2010, method = "lm")

predicted.logFCS = predict(lm.logFCS, test2013, se.fit = TRUE)


# cor(predicted.logFCS, test2013$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = test2013$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+ raincytot + day1rain +     floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4, data = train2010, method = "lm")

predicted.HDDS = predict(lm.HDDS, test2013, se.fit = TRUE)

cor(predicted.HDDS, test2013$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4, data = train2010, method = "lm")

predicted.rCSI = predict(lm.rCSI, test2013, se.fit = TRUE)

cor(predicted.rCSI, test2013$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = test2013$rCSI)

 

################################################################################
# 4. Relatedly, perhaps estimate month fixed effects without prices, 
# 5. swap in GIEWS prices for 4 markets instead of our good price data (R2)
################################################################################

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +   floodmax + maxdaysnorain+
                   # clust_maize_price +  clust_maize_mktthin +
                   percent_ag + elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2+quarter1+quarter2+quarter3+quarter4 +
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11+ month12, data = train2010, method = "lm")

predicted.logFCS = predict(lm.logFCS, test2013, se.fit = TRUE)


# cor(predicted.logFCS, test2013$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = test2013$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1+ raincytot + day1rain +     floodmax + maxdaysnorain+
                 #clust_maize_price +  clust_maize_mktthin +
                 percent_ag + elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = train2010, method = "lm")

predicted.HDDS = predict(lm.HDDS, test2013, se.fit = TRUE)

cor(predicted.HDDS, test2013$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 # clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = train2010, method = "lm")

predicted.rCSI = predict(lm.rCSI, test2013, se.fit = TRUE)

cor(predicted.rCSI, test2013$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = test2013$rCSI)






################################################################################
# 6. Estimate (back of the envelope?) 

# spatial temporal variation between our good price data and GIEWS prices
################################################################################







################################################################################
# 6. Lasso (R1)
################################################################################


##################################################################
# Split by year, LASSO
##################################################################


lambda <- 10^seq(-3, 3, length = 100)

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                   clust_maize_price +  clust_maize_mktthin + 
                   percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11+ month12, data = mw.2010.cluster.ipc1, method = "glmnet",trControl = trainControl("cv", number = 10),
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.logFCS = predict(lm.logFCS, test2013, se.fit = TRUE)


# cor(predicted.logFCS, test2013$logFCS, method = c("pearson"))^2

postResample(pred = predicted.logFCS, obs = test2013$logFCS)


# HDDS
lm.HDDS<-train(HDDS ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = train2010, method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.HDDS = predict(lm.HDDS, test2013, se.fit = TRUE)

cor(predicted.HDDS, test2013$HDDS, method = c("pearson"))^2


# rCSI
lm.rCSI<-train(rCSI ~ IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = train2010,method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.rCSI = predict(lm.rCSI, test2013, se.fit = TRUE)


cor(predicted.rCSI, test2013$rCSI, method = c("pearson"))^2

postResample(pred = predicted.rCSI, obs = test2013$rCSI)





###########################################
##  Split randomly,OLS
###########################################

# 2. Split the data geographically (R1) rather than temporally for model building.

mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)

# fs.year.month = mw.hh %>% distinct(FS_year,FS_month,ea_id)
# nrow(fs.year.month)


mw.cluster$FS_month = as.integer(mw.cluster$FS_month)

mw.cluster.ipc1 = mw.cluster %>% 
  dplyr::filter(!is.na(IPC1))

mw.cluster.ipc1 = mw.cluster.ipc1 %>% 
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

trainIndex <- createDataPartition(mw.cluster.ipc1$FCS, p = .7, 
                                  list = FALSE, 
                                  times = 1)

mw.cluster_Train <- mw.cluster.ipc1[ trainIndex,]
mw.cluster_Test  <- mw.cluster.ipc1[-trainIndex,]

######################################################
# random OLS
####################################################
lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)



lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2, data = mw.cluster_Train, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)



######################################################
# random OLS + quarter
####################################################

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                   clust_maize_price +  clust_maize_mktthin + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender +asset_index2+quarter1+quarter2+quarter3+quarter4, data = mw.cluster_Train, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2+quarter1+quarter2+quarter3+quarter4, data = mw.cluster_Train, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)



lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain + maxdaysnorain + floodmax +
                 clust_maize_price +  clust_maize_mktthin + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender +asset_index2+quarter1+quarter2+quarter3+quarter4, data = mw.cluster_Train, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)




######################################################
# random OLS + month
####################################################

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                   #clust_maize_price +  clust_maize_mktthin + 
                   percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11+ month12, data = mw.cluster_Train, method = "lm")

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                # clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train, method = "lm")

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)



lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                # clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train, method = "lm")

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)









###########################################
##  Split randomly,lasso
###########################################

lambda <- 10^seq(-3, 3, length = 100)

lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                   clust_maize_price +  clust_maize_mktthin + 
                   percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                   month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                   month10+month11+ month12, data = mw.cluster_Train,  method = "glmnet",trControl = trainControl("cv", number = 10),
                 tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.logFCS = predict(lm.logFCS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.logFCS, obs = mw.cluster_Test$logFCS)


lm.HDDS<-train(HDDS ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train,  method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.HDDS = predict(lm.HDDS, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.HDDS, obs = mw.cluster_Test$HDDS)


lm.rCSI<-train(rCSI ~IPC1 + raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_price +  clust_maize_mktthin + 
                 percent_ag +   elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 hh_gender + asset_index2+quarter1+quarter2+quarter3+quarter4+
                 month1 + month2+ month3+month4 + month5+month6 + month7 + month8 + month9+
                 month10+month11+ month12, data = mw.cluster_Train, method = "glmnet",trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))

predicted.rCSI = predict(lm.rCSI, mw.cluster_Test, se.fit = TRUE)
postResample(pred = predicted.rCSI, obs = mw.cluster_Test$HDDS)


# regularization penalty.

lm.rCSI$bestTune$lambda

coef(lm.HDDS$finalModel,lm.HDDS$bestTune$lambda)

coef(lm.rCSI$finalModel,lm.rCSI$bestTune$lambda)








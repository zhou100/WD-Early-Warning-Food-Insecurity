############################################################################
# testing different specifications in terms of R squared and categorical accuracyy
#######################################################################

rm(list=ls())
require(tidyverse)
library(caret)
library(readr)

source("R/functions/TrainModel.R")
source("R/functions/TestModel.R")
source("R/functions/R2Compute.R")
source("R/functions/PredictMalawi.R")
source("R/functions/FormulaComposer.R")
source("R/functions/CategoryRecall.R")
source("R/functions/CategoryAccuracy.R")
source("R/functions/ModelPerformance.R")

########################################################################
# read data 
##############################################################################
malawi.2010 = read_csv("data/clean/mw.2010.cluster.csv" )
malawi.2013 = read_csv("data/clean/mw.2013.cluster.csv" )


####################################################
# Group variables by category for computing formula
####################################################
rain.vars = paste("raincytot","day1rain","floodmax","maxdaysnorain",sep = "+")
temp.vars = paste("gdd","tmean","heatdays",sep = "+")
price.current= paste("clust_maize_current","clust_maize_mktthin_current",sep = "+")
price.lag = paste("clust_maize_lag","clust_maize_mktthin_lag",sep = "+")
GIEW.current = "GIEW_price_current"
GIEW.lag = "GIEW_price_lag"
geo.vars = paste("percent_ag","elevation","nutri_rent_moderate_constraint","dist_road","dist_admarc",sep = "+")
asset.vars = paste("roof_natural_inverse","number_celphones","hhsize","hh_age",sep = "+") 
trend.vars = paste("trend",sep = "+")  
quarter.vars = paste("quarter1","quarter2","quarter3",sep="+")
month.vars = paste("month1","month2","month3","month4","month5","month6","month7","month8","month9","month10","month11",sep=
                     "+")
region.vars= paste("region_Central","region_North",sep="+")
fnid.vars= paste("FNID_MW2012C3010102","FNID_MW2012C3010107","FNID_MW2012C3010210","FNID_MW2012C3010201","FNID_MW2012C3010209","FNID_MW2012C3010309","FNID_MW2012C3010508","FNID_MW2012C3010311","FNID_MW2012C3010417","FNID_MW2012C3010409","FNID_MW2012C3010517","FNID_MW2012C3010503","FNID_MW2012C3020103","FNID_MW2012C3020209","FNID_MW2012C3020211","FNID_MW2012C3020213","FNID_MW2012C3020203","FNID_MW2012C3020303","FNID_MW2012C3020403","FNID_MW2012C3020515","FNID_MW2012C3020513","FNID_MW2012C3020603","FNID_MW2012C3020619","FNID_MW2012C3020703","FNID_MW2012C3020803","FNID_MW2012C3020813","FNID_MW2012C3020913","FNID_MW2012C3020815","FNID_MW2012C3030115","FNID_MW2012C3030112","FNID_MW2012C3030114","FNID_MW2012C3999918","FNID_MW2012C3030204","FNID_MW2012C3030214","FNID_MW2012C3030206","FNID_MW2012C3030304","FNID_MW2012C3030314","FNID_MW2012C3030306","FNID_MW2012C3030414","FNID_MW2012C3030404","FNID_MW2012C3030514","FNID_MW2012C3030519","FNID_MW2012C3030506","FNID_MW2012C3030613","FNID_MW2012C3031313","FNID_MW2012C3030716","FNID_MW2012C3030714","FNID_MW2012C3030804","FNID_MW2012C3030816","FNID_MW2012C3030904","FNID_MW2012C3031005","FNID_MW2012C3031105","FNID_MW2012C3031213","FNID_MW2012C3031206","FNID_MW2012C3030606",sep="+"
)
quarter.region = paste("quarter1_region_south","quarter1_region_central","quarter1_region_north","quarter2_region_south","quarter2_region_central","quarter2_region_north","quarter3_region_south","quarter3_region_central","quarter3_region_north","quarter4_region_south","quarter4_region_central","quarter4_region_north",sep = "+"
)  


##############################################################
# Testing differnt model specifications
# Split by year
##############################################################

# old regresssion results
x_old = paste(rain.vars,price.current,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_old",x_vars=x_old,train_df=malawi.2010,test_df=malawi.2013)

# temperture data 
x_temp = paste(rain.vars,price.current,geo.vars,asset.vars,temp.vars,sep = "+" )
ModelPerformance(model_name= "ols_temp",x_vars=x_temp,train_df=malawi.2010,test_df=malawi.2013)

# lag price
x_lag_price = paste(rain.vars,price.lag,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_lag_price",x_vars=x_lag_price,train_df=malawi.2010,test_df=malawi.2013)

# quarter
x_quarter = paste(rain.vars,price.current,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter",x_vars=x_quarter,train_df=malawi.2010,test_df=malawi.2013)

# quarter lag price
x_quarter_lagprice = paste(rain.vars,price.lag,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_lagprice",x_vars=x_quarter_lagprice,train_df=malawi.2010,test_df=malawi.2013)

# quarter no price 
x_quarter_noprice = paste(rain.vars, geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_noprice",x_vars=x_quarter_noprice,train_df=malawi.2010,test_df=malawi.2013)

# region
x_region = paste(rain.vars,price.current,geo.vars,asset.vars,region.vars,sep = "+" )
ModelPerformance(model_name= "ols_region",x_vars=x_region,train_df=malawi.2010,test_df=malawi.2013)


# quarter region
x_quarter_region = paste(rain.vars,price.current,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region",x_vars=x_quarter_region,train_df=malawi.2010,test_df=malawi.2013)

# quarter region noprice
x_quarter_region_noprice = paste(rain.vars,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region_noprice",x_vars=x_quarter_region_noprice,train_df=malawi.2010,test_df=malawi.2013)

 
# 4. Relatedly, perhaps estimate month fixed effects without prices, 
# month 
x_month = paste(rain.vars,geo.vars,asset.vars,month.vars,sep = "+" )
ModelPerformance(model_name= "ols_month",x_vars=x_month,train_df=malawi.2010,test_df=malawi.2013)

# month + region
x_month_region = paste(rain.vars,geo.vars,asset.vars,month.vars,region.vars,sep = "+" )
ModelPerformance(model_name= "ols_month_region",x_vars=x_month_region,train_df=malawi.2010,test_df=malawi.2013)

# 5. swap in GIEWS prices for 4 markets instead of our good price data (R2)
# GIEWS price
x_GIEWS_price = paste(rain.vars,GIEW.current,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_giews_price",x_vars=x_GIEWS_price,train_df=malawi.2010,test_df=malawi.2013)

# GIEWS lag price
x_lag_GIEWS_price = paste(rain.vars,GIEW.lag,geo.vars,asset.vars,temp.vars,sep = "+" )
ModelPerformance(model_name= "ols_giews_lag_price",x_vars=x_lag_GIEWS_price,train_df=malawi.2010,test_df=malawi.2013)

 
# month + price
x_month_price = paste(rain.vars,geo.vars,asset.vars,month.vars,price.lag,sep = "+" )
ModelPerformance(model_name= "ols_month_price",x_vars=x_month_price,train_df=malawi.2010,test_df=malawi.2013)

# trend 
x_trend = paste(rain.vars,geo.vars,asset.vars,trend.vars,price.lag,sep = "+" )
ModelPerformance(model_name= "ols_trend",x_vars=x_trend,train_df=malawi.2010,test_df=malawi.2013)

#####################################################
# LASSO REGRESSIONS (year split )
#####################################################
 
library(caret)
 
# Lasso logFCS
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)


lasso.logFCS = train(logFCS~IPC1+raincytot+day1rain+floodmax+maxdaysnorain+gdd+tmean+heatdays+clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+dist_road+dist_admarc+roof_natural_inverse+number_celphones+hhsize+hh_age+quarter1+quarter2+quarter3+region_Central+region_North,data = malawi.2010, method = "glmnet",trControl = trainControl("cv", number = 10),
                      tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.logFCS = predict(lasso.logFCS, malawi.2013, se.fit = TRUE)

# logFCS results
# R squared 
# postResample(pred = predicted.logFCS, obs = malawi.2013$logFCS)
R2Compute(predicted.logFCS,malawi.2013$logFCS)

# Recall 
CategoryRecall(yvar="logFCS", predicted=predicted.logFCS,test.df=malawi.2013)

# categorical accuracy 
CategoryAccuracy(yvar="logFCS", predicted=predicted.logFCS,test.df=malawi.2013)
lasso.logFCS$bestTune$alpha
lasso.logFCS$bestTune$lambda


coef(lasso.logFCS$finalModel,lasso.logFCS$bestTune$lambda)



# HDDS Lasso results
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)

lasso.HDDS = train(HDDS~IPC1+raincytot+day1rain+floodmax+maxdaysnorain+gdd+tmean+heatdays+clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+dist_road+dist_admarc+roof_natural_inverse+number_celphones+hhsize+hh_age+quarter1+quarter2+quarter3+region_Central+region_North,data = malawi.2010, method = "glmnet",trControl = trainControl("cv", number = 10),
                   tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.HDDS = predict(lasso.HDDS, malawi.2013, se.fit = TRUE)
# R squared 
# postResample(pred = predicted.HDDS, obs = malawi.2013$HDDS)
R2Compute(predicted.HDDS,malawi.2013$HDDS)

# Recall 
CategoryRecall(yvar="HDDS", predicted=predicted.HDDS,test.df=malawi.2013)

# categorical accuracy 
CategoryAccuracy(yvar="HDDS", predicted=predicted.HDDS,test.df=malawi.2013)

lasso.HDDS$bestTune$alpha
lasso.HDDS$bestTune$lambda


coef(lasso.HDDS$finalModel,lasso.HDDS$bestTune$lambda)


# rCSI LASSO results
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0, 5, 0.1)


lasso.rCSI = train(rCSI~IPC1+raincytot+day1rain+floodmax+maxdaysnorain+gdd+tmean+heatdays+clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+dist_road+dist_admarc+roof_natural_inverse+number_celphones+hhsize+hh_age+quarter1+quarter2+quarter3+region_Central+region_North,data = malawi.2010, method = "glmnet",trControl = trainControl("cv", number = 10),
                   tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.rCSI = predict(lasso.rCSI, malawi.2013, se.fit = TRUE)

# R squared 
# postResample(pred = predicted.rCSI, obs = malawi.2013$rCSI)
R2Compute(predicted.rCSI,malawi.2013$rCSI)

# Recall 
CategoryRecall(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013)

# categorical accuracy 
CategoryAccuracy(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013)


lasso.rCSI$bestTune$lambda

lasso.rCSI$bestTune$alpha

coef(lasso.rCSI$finalModel,lasso.rCSI$bestTune$lambda)





################################################################### 
# 2. Split the data geographically (R1) rather than temporally for model building.
############################################################ 

mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)
mw.cluster = mw.cluster %>% 
  mutate(clust_maize_price  = log(clust_maize_price))
colSums(is.na(mw.cluster))

mw.cluster  = mw.cluster %>% 
  dplyr::filter(!is.na(IPC1))
table(mw.cluster$region_string)

# Suppose we train it on central and SOUTH to predict North
mw.cluster.North= mw.cluster %>% dplyr::filter(region_string=="North") 
mw.cluster.SouthCentral= mw.cluster %>% dplyr::filter(region_string!="North") 

x_old = paste(rain.vars,price.current,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_old",x_vars=x_old,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# temperture data 
x_temp = paste(rain.vars,price.current,geo.vars,asset.vars,temp.vars,sep = "+" )
ModelPerformance(model_name= "ols_temp",x_vars=x_temp,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# lag price
x_lag_price = paste(rain.vars,price.lag,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_lag_price",x_vars=x_lag_price,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# quarter
x_quarter = paste(rain.vars,price.current,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter",x_vars=x_quarter,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# quarter lag price
x_quarter_lagprice = paste(rain.vars,price.lag,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_lagprice",x_vars=x_quarter_lagprice,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# quarter no price 
x_quarter_noprice = paste(rain.vars, geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_noprice",x_vars=x_quarter_noprice,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# region
x_region = paste(rain.vars,price.current,geo.vars,asset.vars,region.vars,sep = "+" )
ModelPerformance(model_name= "ols_region",x_vars=x_region,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)


# quarter region
x_quarter_region = paste(rain.vars,price.current,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region",x_vars=x_quarter_region,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# quarter region noprice
x_quarter_region_noprice = paste(rain.vars,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region_noprice",x_vars=x_quarter_region_noprice,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)


# 4. Relatedly, perhaps estimate month fixed effects without prices, 
# month 
x_month = paste(rain.vars,geo.vars,asset.vars,month.vars,sep = "+" )
ModelPerformance(model_name= "ols_month",x_vars=x_month,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# month + region
x_month_region = paste(rain.vars,geo.vars,asset.vars,month.vars,region.vars,sep = "+" )
ModelPerformance(model_name= "ols_month_region",x_vars=x_month_region,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# 5. swap in GIEWS prices for 4 markets instead of our good price data (R2)
# GIEWS price
x_GIEWS_price = paste(rain.vars,GIEW.current,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_giews_price",x_vars=x_GIEWS_price,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# GIEWS lag price
x_lag_GIEWS_price = paste(rain.vars,GIEW.lag,geo.vars,asset.vars,temp.vars,sep = "+" )
ModelPerformance(model_name= "ols_giews_lag_price",x_vars=x_lag_GIEWS_price,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)


# month + price
x_month_price = paste(rain.vars,geo.vars,asset.vars,month.vars,price.lag,sep = "+" )
ModelPerformance(model_name= "ols_month_price",x_vars=x_month_price,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# trend 
x_trend = paste(rain.vars,geo.vars,asset.vars,trend.vars,price.lag,sep = "+" )
ModelPerformance(model_name= "ols_trend",x_vars=x_trend,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)


#####################################################
# LASSO REGRESSIONS (region split )
#####################################################

library(caret)

# Lasso logFCS
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)


lasso.logFCS = train(logFCS~IPC1+raincytot+day1rain+floodmax+maxdaysnorain+gdd+tmean+heatdays+clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+dist_road+dist_admarc+roof_natural_inverse+number_celphones+hhsize+hh_age+quarter1+quarter2+quarter3+region_Central+region_North,data = mw.cluster.SouthCentral, method = "glmnet",trControl = trainControl("cv", number = 10),
                     tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.logFCS = predict(lasso.logFCS, mw.cluster.North, se.fit = TRUE)

# logFCS results
# R squared 
# postResample(pred = predicted.logFCS, obs = mw.cluster.North$logFCS)
R2Compute(predicted.logFCS,mw.cluster.North$logFCS)

# Recall 
CategoryRecall(yvar="logFCS", predicted=predicted.logFCS,test.df=mw.cluster.North)

# categorical accuracy 
CategoryAccuracy(yvar="logFCS", predicted=predicted.logFCS,test.df=mw.cluster.North)
lasso.logFCS$bestTune$alpha
lasso.logFCS$bestTune$lambda


coef(lasso.logFCS$finalModel,lasso.logFCS$bestTune$lambda)



# HDDS Lasso results
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)

lasso.HDDS = train(HDDS~IPC1+raincytot+day1rain+floodmax+maxdaysnorain+gdd+tmean+heatdays+clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+dist_road+dist_admarc+roof_natural_inverse+number_celphones+hhsize+hh_age+quarter1+quarter2+quarter3+region_Central+region_North,data = mw.cluster.SouthCentral, method = "glmnet",trControl = trainControl("cv", number = 10),
                   tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.HDDS = predict(lasso.HDDS, mw.cluster.North, se.fit = TRUE)
# R squared 
# postResample(pred = predicted.HDDS, obs = mw.cluster.North$HDDS)
R2Compute(predicted.HDDS,mw.cluster.North$HDDS)

# Recall 
CategoryRecall(yvar="HDDS", predicted=predicted.HDDS,test.df=mw.cluster.North)

# categorical accuracy 
CategoryAccuracy(yvar="HDDS", predicted=predicted.HDDS,test.df=mw.cluster.North)

lasso.HDDS$bestTune$alpha
lasso.HDDS$bestTune$lambda


coef(lasso.HDDS$finalModel,lasso.HDDS$bestTune$lambda)


# rCSI LASSO results
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0, 1, 0.1)


lasso.rCSI = train( rCSI~IPC1+raincytot+day1rain+floodmax+maxdaysnorain+gdd+tmean+heatdays+clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+dist_road+dist_admarc+roof_natural_inverse+number_celphones+hhsize+hh_age+quarter1+quarter2+quarter3+region_Central+region_North,data = mw.cluster.SouthCentral, method = "glmnet",trControl = trainControl("cv", number = 10),
                   tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.rCSI = predict(lasso.rCSI, mw.cluster.North, se.fit = TRUE)

# R squared 
# postResample(pred = predicted.rCSI, obs = mw.cluster.North$rCSI)
R2Compute(predicted.rCSI,mw.cluster.North$rCSI)

# Recall 
CategoryRecall(yvar="rCSI", predicted=predicted.rCSI,test.df=mw.cluster.North)

# categorical accuracy 
CategoryAccuracy(yvar="rCSI", predicted=predicted.rCSI,test.df=mw.cluster.North)


lasso.rCSI$bestTune$lambda

lasso.rCSI$bestTune$alpha

coef(lasso.rCSI$finalModel,lasso.rCSI$bestTune$lambda)

################################################################### 
##  Split randomly 
############################################################ 

mw.cluster = read.csv("data/mw_dataset_cluster.csv",stringsAsFactors = FALSE)
mw.cluster = mw.cluster %>% 
  mutate(clust_maize_price  = log(clust_maize_price))
colSums(is.na(mw.cluster))


mw.cluster = mw.cluster %>% 
  mutate(quarter1_region_south = quarter1 * region_South   ) %>% 
  mutate(quarter1_region_central = quarter1 * region_Central   ) %>% 
  mutate(quarter1_region_north = quarter1 * region_North   ) %>% 
  mutate(quarter2_region_south = quarter2 * region_South   ) %>% 
  mutate(quarter2_region_central = quarter2 * region_Central   ) %>% 
  mutate(quarter2_region_north = quarter2 * region_North   ) %>% 
  mutate(quarter3_region_south = quarter3 * region_South   ) %>% 
  mutate(quarter3_region_central = quarter3 * region_Central   ) %>% 
  mutate(quarter3_region_north = quarter3 * region_North   ) %>% 
  mutate(quarter4_region_south = quarter4 * region_South   ) %>% 
  mutate(quarter4_region_central = quarter4 * region_Central   ) %>% 
  mutate(quarter4_region_north = quarter4 * region_North   )

mw.cluster  = mw.cluster %>% 
  dplyr::filter(!is.na(IPC1)) 

trainIndex <- createDataPartition(mw.cluster$FCS, p = .7, 
                                  list = FALSE, 
                                  times = 1)

mw.cluster_Train <- mw.cluster[ trainIndex,]
mw.cluster_Test  <- mw.cluster[-trainIndex,]

x_old = paste(rain.vars,price.current,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_old",x_vars=x_old,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# temperture data 
x_temp = paste(rain.vars,price.current,geo.vars,asset.vars,temp.vars,sep = "+" )
ModelPerformance(model_name= "ols_temp",x_vars=x_temp,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# lag price
x_lag_price = paste(rain.vars,price.lag,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_lag_price",x_vars=x_lag_price,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# quarter
x_quarter = paste(rain.vars,price.current,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter",x_vars=x_quarter,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# quarter lag price
x_quarter_lagprice = paste(rain.vars,price.lag,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_lagprice",x_vars=x_quarter_lagprice,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# quarter no price 
x_quarter_noprice = paste(rain.vars, geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_noprice",x_vars=x_quarter_noprice,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# region
x_region = paste(rain.vars,price.current,geo.vars,asset.vars,region.vars,sep = "+" )
ModelPerformance(model_name= "ols_region",x_vars=x_region,train_df=mw.cluster_Train,test_df=mw.cluster_Test)


# quarter region
x_quarter_region = paste(rain.vars,price.current,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region",x_vars=x_quarter_region,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# quarter region noprice
x_quarter_region_noprice = paste(rain.vars,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region_noprice",x_vars=x_quarter_region_noprice,train_df=mw.cluster_Train,test_df=mw.cluster_Test)


# 4. Relatedly, perhaps estimate month fixed effects without prices, 
# month 
x_month = paste(rain.vars,geo.vars,asset.vars,month.vars,sep = "+" )
ModelPerformance(model_name= "ols_month",x_vars=x_month,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# month + region
x_month_region = paste(rain.vars,geo.vars,asset.vars,month.vars,region.vars,sep = "+" )
ModelPerformance(model_name= "ols_month_region",x_vars=x_month_region,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# 5. swap in GIEWS prices for 4 markets instead of our good price data (R2)
# GIEWS price
x_GIEWS_price = paste(rain.vars,GIEW.current,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_giews_price",x_vars=x_GIEWS_price,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# GIEWS lag price
x_lag_GIEWS_price = paste(rain.vars,GIEW.lag,geo.vars,asset.vars,temp.vars,sep = "+" )
ModelPerformance(model_name= "ols_giews_lag_price",x_vars=x_lag_GIEWS_price,train_df=mw.cluster_Train,test_df=mw.cluster_Test)


# month + price
x_month_price = paste(rain.vars,geo.vars,asset.vars,month.vars,price.lag,sep = "+" )
ModelPerformance(model_name= "ols_month_price",x_vars=x_month_price,train_df=mw.cluster_Train,test_df=mw.cluster_Test)

# trend 
x_trend = paste(rain.vars,geo.vars,asset.vars,trend.vars,price.lag,sep = "+" )
ModelPerformance(model_name= "ols_trend",x_vars=x_trend,train_df=mw.cluster_Train,test_df=mw.cluster_Test)



################################################################################
# 6. Estimate (back of the envelope?) 
# spatial temporal variation between our good price data and GIEWS prices
################################################################################
# change figure s1 to add the GIEWS MARKETS 
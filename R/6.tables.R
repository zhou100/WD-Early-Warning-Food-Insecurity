####################################################################################################################################
# Goal : This script aims to replicate the tables used in this paper 
# Yujun Zhou -  04/17/19
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

logFCS.ols <- lm(logFCS ~ IPC1 +raincytot + day1rain + maxdaysnorain  + 
                   clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint   + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                   hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.2010.cluster)  # build linear regression model on logFCS


hdds.ols <- lm(HDDS ~ IPC1 + raincytot + day1rain + maxdaysnorain  + 
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.2010.cluster)  # build linear regression model on HDDS

rcsi.ols <- lm(rCSI  ~ IPC1 + raincytot  + maxdaysnorain + 
                   hh_gender + percent_ag+ dist_admarc + day1rain + elevation +dist_road +
                 clust_maize_current +  clust_maize_mktthin_current  + 
                   + nutri_rent_moderate_constraint   + 
                    roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + asset_index2 + quarter1 + quarter2 +quarter3, data=mw.2010.cluster)  # build linear regression model on rCSI
 
 
stargazer::stargazer(logFCS.ols,hdds.ols,rcsi.ols,type = "text")


#####################################################
# Replicate Table 3 
#####################################################

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

##############################################################
# Testing differnt model specifications
# Split by year
##############################################################


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
asset.vars2 = paste("roof_natural_inverse","number_celphones","hhsize","hh_age","hh_gender",sep = "+") 
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

# Table 3 Row 1 (Demonstration with detail)
# Main model： quarter fixed effects

# logFCS 
lm.logFCS<-train(logFCS ~IPC1 + raincytot + day1rain +   floodmax + maxdaysnorain+
                   clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                   quarter1+quarter2+quarter3 +
                   hh_gender +asset_index2, data = malawi.2010, method = "lm")

predicted.logFCS = predict(lm.logFCS, malawi.2013 , se.fit = TRUE)



# HDDS
lm.HDDS<-train(HDDS ~ IPC1+raincytot + day1rain +     floodmax + maxdaysnorain+
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint +  
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 quarter1+quarter2+quarter3 +
                 hh_gender + asset_index2, data = malawi.2010 , method = "lm")

predicted.HDDS = predict(lm.HDDS, malawi.2013 , se.fit = TRUE)



# rCSI
lm.rCSI<-train(rCSI ~ IPC1 +raincytot + day1rain +  floodmax + maxdaysnorain+
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + hh_age+
                 quarter1+quarter2+quarter3 +
                 hh_gender + asset_index2, data = malawi.2010 , method = "lm")

predicted.rCSI = predict(lm.rCSI, malawi.2013, se.fit = TRUE)


# Column 1 through 3  R squared
format(cor(predicted.logFCS, malawi.2013$logFCS, method = c("pearson"))^2,digits = 3)
format(cor(predicted.HDDS, malawi.2013$HDDS, method = c("pearson"))^2,digits = 3)
format(cor(predicted.rCSI, malawi.2013$rCSI, method = c("pearson"))^2,digits = 3)

# Column 4-6 Categorical recall
format(CategoryRecall(yvar="logFCS", predicted=predicted.logFCS,test.df=malawi.2013),digits = 3)
format(CategoryRecall(yvar="HDDS", predicted=predicted.HDDS,test.df=malawi.2013),digits = 3)
format(CategoryRecall(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013),digits = 3)


# Column 7-9 # categorical accuracy 
format(CategoryAccuracy(yvar="logFCS", predicted=predicted.logFCS,test.df=malawi.2013),digits = 3)
format(CategoryAccuracy(yvar="HDDS", predicted=predicted.HDDS,test.df=malawi.2013),digits = 3)
format(CategoryAccuracy(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013),digits = 3)

# ( Same approach for the other results but simplified code with the ModelPerformance function )
# Table 3 Row 2
# Main model without cluster price variables 
x_quarter_noprice = paste(rain.vars, geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_noprice",x_vars=x_quarter_noprice,train_df=malawi.2010,test_df=malawi.2013)

# Table 3 Row 3 
# Main Model with region fixed effect and without cluster price variables
x_quarter_region_noprice = paste(rain.vars,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region_noprice",x_vars=x_quarter_region_noprice,train_df=malawi.2010,test_df=malawi.2013)

# Table 3 Row 4 
# Main Model with GIEWS price and without cluster price variables
x_GIEWS_price = paste(rain.vars,GIEW.current,geo.vars,asset.vars,sep = "+" )
ModelPerformance(model_name= "ols_giews_price",x_vars=x_GIEWS_price,train_df=malawi.2010,test_df=malawi.2013)

####################################
# Table 3 LASSO Regression results (Row 5 )
# LASSO REGRESSIONS (year split )
##############################################
malawi.2010 = read_csv("data/clean/mw.2010.cluster.csv" )
malawi.2013 = read_csv("data/clean/mw.2013.cluster.csv" )

# Lasso logFCS
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)


lasso.logFCS = train(logFCS~IPC1+raincytot+day1rain+
                       floodmax+maxdaysnorain+gdd+tmean+heatdays+
                       clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+
                       GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+
                       dist_road+dist_admarc+roof_natural_inverse+number_celphones+
                       hhsize+hh_age+hh_gender+asset_index2 +
                       quarter1+quarter2+quarter3+region_Central+region_South,
                     data = malawi.2010, method = "glmnet",trControl = trainControl("cv", number = 10),
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




# HDDS Lasso results
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)

lasso.HDDS = train(HDDS~IPC1+raincytot+day1rain+
                     floodmax+maxdaysnorain+gdd+tmean+heatdays+
                     clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+
                     GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+
                     dist_road+dist_admarc+roof_natural_inverse+number_celphones+
                     hhsize+hh_age+hh_gender+asset_index2 +
                     quarter1+quarter2+quarter3+region_Central+region_South,
                   data = malawi.2010, method = "glmnet",trControl = trainControl("cv", number = 10),
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


lasso.rCSI = train(rCSI~IPC1+raincytot+day1rain+
                     floodmax+maxdaysnorain+gdd+tmean+heatdays+
                     clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+
                     GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+
                     dist_road+dist_admarc+roof_natural_inverse+number_celphones+
                     hhsize+hh_age+hh_gender+asset_index2 +
                     quarter1+quarter2+quarter3+region_Central+region_South,
                   data = malawi.2010, method = "glmnet",trControl = trainControl("cv", number = 10),
                   tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.rCSI = predict(lasso.rCSI, malawi.2013, se.fit = TRUE)

# R squared 
# postResample(pred = predicted.rCSI, obs = malawi.2013$rCSI)
R2Compute(predicted.rCSI,malawi.2013$rCSI)

# Recall 
CategoryRecall(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013)

# categorical accuracy 
CategoryAccuracy(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013)





###########################################
## Replicate Table 4: IPC regression results  
###########################################

malawi.2010 = read_csv("data/clean/mw.2010.cluster.csv" )
malawi.2013 = read_csv("data/clean/mw.2013.cluster.csv" )

IPC.rcsi  <- lm(IPC1 ~rCSI, data=malawi.2010) 
IPC.logfcs  <- lm(IPC1 ~logFCS, data=malawi.2010) 
IPC.hdds  <- lm(IPC1 ~ HDDS , data=malawi.2010) 

stargazer::stargazer(IPC.rcsi,IPC.logfcs,IPC.hdds,type = "text")



################################################################################################
# Table 5: The percentage of food insecure clusters correctly predicted to be food insecure. 
################################################################################################
###########################################
## Cluster level OLS prediction   
###########################################
table5.logFCS.full = format(CategoryRecall(yvar="logFCS", predicted=predicted.logFCS,test.df=malawi.2013),digits = 3)
table5.HDDS.full = format(CategoryRecall(yvar="HDDS", predicted=predicted.HDDS,test.df=malawi.2013),digits = 3)
table5.rCSI.full = format(CategoryRecall(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013),digits = 3)


###################################################################
## IPC value model cluster level predictions 
####################################################################

malawi.2010 = read_csv("data/clean/mw.2010.cluster.csv" )
malawi.2013 = read_csv("data/clean/mw.2013.cluster.csv" )

# logFCS  IPC value model
lm.logFCS.ipc<-train(logFCS ~IPC1, data = malawi.2010, method = "lm")
predicted.logFCS.ipc = predict(lm.logFCS.ipc, malawi.2013, se.fit = TRUE)
cor(predicted.logFCS.ipc, malawi.2013$logFCS, method = c("pearson"))^2

# HDDS IPC value model
lm.HDDS.ipc<-train(HDDS ~ IPC1, data = malawi.2010, method = "lm")
predicted.HDDS.ipc = predict(lm.logFCS.ipc, malawi.2013, se.fit = TRUE)
cor(predicted.HDDS.ipc, malawi.2013$HDDS, method = c("pearson"))^2


# rCSI IPC value model
lm.rCSI.ipc<-train(rCSI ~ IPC1, data = malawi.2010, method = "lm")
predicted.rCSI.ipc = predict(lm.logFCS.ipc, malawi.2013, se.fit = TRUE)
cor(predicted.rCSI.ipc, malawi.2013$rCSI, method = c("pearson"))^2

table5.HDDS.ipc = format(CategoryRecall(yvar="HDDS", predicted=predicted.HDDS.ipc,test.df=malawi.2013),digits = 3)
table5.logFCS.ipc =format(CategoryRecall(yvar="logFCS", predicted=predicted.logFCS.ipc,test.df=malawi.2013),digits = 3)
table5.rCSI.ipc =format(CategoryRecall(yvar="rCSI", predicted=predicted.rCSI.ipc,test.df=malawi.2013),digits = 3)


# Table 5 

table5.matrix = matrix(nrow=2,ncol=3)

table5.matrix[1,1] = table5.HDDS.ipc
table5.matrix[1,2] = table5.logFCS.ipc
table5.matrix[1,3] = table5.rCSI.ipc
table5.matrix[2,1] = table5.HDDS.full
table5.matrix[2,2] = table5.logFCS.full
table5.matrix[2,3] = table5.rCSI.full



table5.matrix





#####################################################
# Tabel S2
###################################################
# Main Results (Tabel S2 Row 1)
# Column 1 through 3  R squared
format(cor(predicted.logFCS, malawi.2013$logFCS, method = c("pearson"))^2,digits = 3)
format(cor(predicted.HDDS, malawi.2013$HDDS, method = c("pearson"))^2,digits = 3)
format(cor(predicted.rCSI, malawi.2013$rCSI, method = c("pearson"))^2,digits = 3)

# Column 4-6 Categorical recall
format(CategoryRecall(yvar="logFCS", predicted=predicted.logFCS,test.df=malawi.2013),digits = 3)
format(CategoryRecall(yvar="HDDS", predicted=predicted.HDDS,test.df=malawi.2013),digits = 3)
format(CategoryRecall(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013),digits = 3)


# Column 7-9 # categorical accuracy 
format(CategoryAccuracy(yvar="logFCS", predicted=predicted.logFCS,test.df=malawi.2013),digits = 3)
format(CategoryAccuracy(yvar="HDDS", predicted=predicted.HDDS,test.df=malawi.2013),digits = 3)
format(CategoryAccuracy(yvar="rCSI", predicted=predicted.rCSI,test.df=malawi.2013),digits = 3)


# ( Same approach for the other results but simplified code with the ModelPerformance function )
# Table S2 Row 2
# Main model without MSD maize price
# with MSD maize price iprevious month
x_quarter_lagprice = paste(rain.vars,price.lag,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_lagprice",x_vars=x_quarter_lagprice,train_df=malawi.2010,test_df=malawi.2013)

# Table S2 Row 3 
# Main Model without MSD maize price with quarter by region fixed effect 
x_quarter_region = paste(rain.vars,price.current,geo.vars,asset.vars,region.vars,quarter.vars,quarter.region,sep = "+" )
ModelPerformance(model_name= "ols_quarter_region",x_vars=x_quarter_region,train_df=malawi.2010,test_df=malawi.2013)

# Table S2 Row 4 
# Main Model without MSD maize price with month fixed effect with region fixed effect
x_month_region = paste(rain.vars,geo.vars,asset.vars,month.vars,region.vars,sep = "+" )
ModelPerformance(model_name= "ols_month_region",x_vars=x_month_region,train_df=malawi.2010,test_df=malawi.2013)

# Table S2 row 5 
# Main model with month fixed effec
x_month_price = paste(rain.vars,geo.vars,asset.vars,month.vars,price.lag,sep = "+" )
ModelPerformance(model_name= "ols_month_price",x_vars=x_month_price,train_df=malawi.2010,test_df=malawi.2013)


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

# Table S2, Split region row 1 
# Main model 
x_quarter = paste(rain.vars,price.current,geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter",x_vars=x_quarter,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

# Table S2, Split region row 1 
# Main model without MSD maize price 
x_quarter_noprice = paste(rain.vars, geo.vars,asset.vars,quarter.vars,sep = "+" )
ModelPerformance(model_name= "ols_quarter_noprice",x_vars=x_quarter_noprice,train_df=mw.cluster.SouthCentral,test_df=mw.cluster.North)

#####################################################
# Tabel S2 
# LASSO REGRESSIONS (region split ROW 3 )
#####################################################

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
library(caret)

# Lasso logFCS
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)


lasso.logFCS.region = train(logFCS~IPC1+raincytot+day1rain+
                       floodmax+maxdaysnorain+gdd+tmean+heatdays+
                       clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+
                       GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+
                       dist_road+dist_admarc+roof_natural_inverse+number_celphones+
                       hhsize+hh_age+hh_gender+asset_index2 +
                       quarter1+quarter2+quarter3+region_Central+region_South,
                     data = mw.cluster.SouthCentral, method = "glmnet",trControl = trainControl("cv", number = 10),
                     tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.logFCS.region = predict(lasso.logFCS, mw.cluster.North, se.fit = TRUE)

# logFCS results
# R squared 
# postResample(pred = predicted.logFCS, obs = mw.cluster.North$logFCS)
R2Compute(predicted.logFCS.region,mw.cluster.North$logFCS)

# Recall 
CategoryRecall(yvar="logFCS", predicted=predicted.logFCS.region,test.df=mw.cluster.North)

# categorical accuracy 
CategoryAccuracy(yvar="logFCS", predicted=predicted.logFCS.region,test.df=mw.cluster.North)



# HDDS Lasso results
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0 , 5, 0.1)

lasso.HDDS.region = train(HDDS~IPC1+raincytot+day1rain+
                     floodmax+maxdaysnorain+gdd+tmean+heatdays+
                     clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+
                     GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+
                     dist_road+dist_admarc+roof_natural_inverse+number_celphones+
                     hhsize+hh_age+hh_gender+asset_index2 +
                     quarter1+quarter2+quarter3+region_Central+region_South,
                   data = mw.cluster.SouthCentral, method = "glmnet",trControl = trainControl("cv", number = 10),
                   tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.HDDS.region = predict(lasso.HDDS.region, mw.cluster.North, se.fit = TRUE)
# R squared 
# postResample(pred = predicted.HDDS, obs = mw.cluster.North$HDDS)
R2Compute(predicted.HDDS.region,mw.cluster.North$HDDS)

# Recall 
CategoryRecall(yvar="HDDS", predicted=predicted.HDDS.region,test.df=mw.cluster.North)

# categorical accuracy 
CategoryAccuracy(yvar="HDDS", predicted=predicted.HDDS.region,test.df=mw.cluster.North)


# rCSI LASSO results
lambda <- 10^seq(-3, 3, length = 100)
alpha_grid <- seq(0, 1, 0.1)


lasso.rCSI.region = train( rCSI~IPC1+raincytot+day1rain+
                      floodmax+maxdaysnorain+gdd+tmean+heatdays+
                      clust_maize_current+clust_maize_mktthin_current+clust_maize_lag+clust_maize_mktthin_lag+
                      GIEW_price_current+percent_ag+elevation+nutri_rent_moderate_constraint+
                      dist_road+dist_admarc+roof_natural_inverse+number_celphones+
                      hhsize+hh_age+hh_gender+asset_index2 +
                      quarter1+quarter2+quarter3+region_Central+region_South,
                    data = mw.cluster.SouthCentral, method = "glmnet",trControl = trainControl("cv", number = 10),
                    tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda))
predicted.rCSI.region = predict(lasso.rCSI, mw.cluster.North, se.fit = TRUE)

# R squared 
# postResample(pred = predicted.rCSI, obs = mw.cluster.North$rCSI)
R2Compute(predicted.rCSI.region,mw.cluster.North$rCSI)

# Recall 
CategoryRecall(yvar="rCSI", predicted=predicted.rCSI.region,test.df=mw.cluster.North)

# categorical accuracy 
CategoryAccuracy(yvar="rCSI", predicted=predicted.rCSI.region,test.df=mw.cluster.North)





###################################
# Table S3: LASSO regression coefficients
################################
# Table S3 Split by year, column 1 (logFCS)
lasso.logFCS$bestTune$alpha
lasso.logFCS$bestTune$lambda
coef(lasso.logFCS$finalModel,lasso.logFCS$bestTune$lambda)

# Table S3 Split by year, column 1 (HDDS)
lasso.rCSI$bestTune$lambda
lasso.rCSI$bestTune$alpha
coef(lasso.rCSI$finalModel,lasso.rCSI$bestTune$lambda)

# Table S3 Split by year, column 1 (HDDS)
lasso.rCSI$bestTune$lambda
lasso.rCSI$bestTune$alpha
coef(lasso.rCSI$finalModel,lasso.rCSI$bestTune$lambda)


# Table S3 Split by region, column 1 (logFCS)
lasso.logFCS.region$bestTune$alpha
lasso.logFCS.region$bestTune$lambda
coef(lasso.logFCS.region$finalModel,lasso.logFCS.region$bestTune$lambda)

# Table S3 Split by region, column 1 (HDDS)
lasso.HDDS.region$bestTune$alpha
lasso.HDDS.region$bestTune$lambda
coef(lasso.HDDS.region$finalModel,lasso.HDDS.region$bestTune$lambda)

# Table S3 Split by region, column 1 (rCSI)
lasso.rCSI.region$bestTune$lambda
lasso.rCSI.region$bestTune$alpha
coef(lasso.rCSI.region$finalModel,lasso.rCSI.region$bestTune$lambda)

################################################################################################
# Table s3: Regression of the tail  
################################################################################################
mw.tail.fcs =  mw.2010.cluster %>%  
  dplyr::filter(logFCS<log(42))

mw.tail.hdds =  mw.2010.cluster %>%  
  dplyr::filter(HDDS<6)

mw.tail.rcsi =  mw.2010.cluster %>%  
  dplyr::filter(rCSI>4)

logFCS.ols <- lm(logFCS ~ IPC1 +raincytot + day1rain + maxdaysnorain  + 
                   clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                   elevation  + nutri_rent_moderate_constraint   + 
                   dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                   hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.tail.fcs)  # build linear regression model on logFCS


hdds.ols <- lm(HDDS ~ IPC1 + raincytot + day1rain + maxdaysnorain  + 
                 clust_maize_current +  clust_maize_mktthin_current + percent_ag + 
                 elevation  + nutri_rent_moderate_constraint   + 
                 dist_road + dist_admarc + roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + hh_gender + asset_index2+ quarter1 + quarter2 +quarter3, data=mw.tail.hdds)  # build linear regression model on HDDS

rcsi.ols <- lm(rCSI  ~ IPC1 + raincytot  + maxdaysnorain + 
                 hh_gender + percent_ag+ dist_admarc + day1rain + elevation +dist_road +
                 clust_maize_current +  clust_maize_mktthin_current  + 
                 + nutri_rent_moderate_constraint   + 
                 roof_natural_inverse + number_celphones +hhsize + 
                 hh_age + asset_index2 + quarter1 + quarter2 +quarter3, data=mw.tail.rcsi)  # build linear regression model on rCSI


stargazer::stargazer(logFCS.ols,hdds.ols,rcsi.ols,type = "text")


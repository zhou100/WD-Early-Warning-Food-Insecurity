############################################################################
# testing different specifications in terms of R squared and categorical accuracy

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


################################################################################
# Cluster level model specification comparisons (used in Table 5)
################################################################################

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
## Prediction results using variables at different levels
##
################################################################################


####################################################
# create variables at the lhz / TA level for computing formula
####################################################
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
source("R/functions/formula.R")
source("R/functions/linear_fit.R")



 mw.2010 <- read_csv("data/clean/mw.2010.cluster.csv")
sum(is.na(mw.2010$IPC1))
mw.2013 <- read_csv("data/clean/mw.2013.cluster.csv")


lhz.vars = mw.2010 %>% select(starts_with("lhz"))
colnames(lhz.vars)

TA.vars = mw.2010 %>% select(starts_with("TA"))
colnames(TA.vars)

mw.2010 = mw.2010 %>% 
  mutate(maize_current = clust_maize_current) %>%
  mutate(maize_mktthin_current = clust_maize_mktthin_current) %>%
  mutate(asset = asset_index2) %>%
  mutate(TA_maize_mktthin_current = TA_maize_mktthin)
  

mw.2010.lhz = mw.2010 %>% 
  dplyr::select(FNID,roof_natural_inverse,
                number_celphones,percent_ag,elevation,
                nutri_rent_moderate_constraint,
                dist_road,dist_admarc,hhsize,
                hh_age,hh_gender,asset
  ) %>%
  group_by(FNID) %>%
  summarise_all(funs(mean(.))) %>%
  mutate(lhz_roof_natural_inverse = roof_natural_inverse) %>%
  mutate(lhz_number_celphones = number_celphones) %>%
  mutate(lhz_percent_ag= percent_ag) %>%
  mutate(lhz_elevation = elevation) %>%
  mutate(lhz_nutri_rent_moderate_constraint = nutri_rent_moderate_constraint) %>%
  mutate(lhz_dist_road = dist_road) %>%
  mutate(lhz_dist_admarc = dist_admarc) %>%  
  mutate(lhz_hhsize = hhsize) %>%  
  mutate(lhz_hh_age = hh_age) %>%  
  mutate(lhz_hh_gender = hh_gender) %>%  
  mutate(lhz_asset = asset) %>% 
  dplyr::select(FNID,starts_with("lhz"))


mw.2010.TA = mw.2010 %>% 
  dplyr::select(TA_names,roof_natural_inverse,
                number_celphones,percent_ag,elevation,
                nutri_rent_moderate_constraint,
                dist_road,dist_admarc,hhsize,
                hh_age,hh_gender,asset
  ) %>%
  group_by(TA_names) %>%
  summarise_all(funs(mean(.))) %>%
  mutate(TA_roof_natural_inverse = roof_natural_inverse) %>%
  mutate(TA_number_celphones = number_celphones) %>%
  mutate(TA_percent_ag= percent_ag) %>%
  mutate(TA_elevation = elevation) %>%
  mutate(TA_nutri_rent_moderate_constraint = nutri_rent_moderate_constraint) %>%
  mutate(TA_dist_road = dist_road) %>%
  mutate(TA_dist_admarc = dist_admarc) %>%  
  mutate(TA_hhsize = hhsize) %>%  
  mutate(TA_hh_age = hh_age) %>%  
  mutate(TA_hh_gender = hh_gender) %>%  
  mutate(TA_asset = asset) %>% 
  dplyr::select( starts_with("TA"))

mw.2010 = left_join(mw.2010,mw.2010.lhz,by="FNID" )  
mw.2010 = left_join(mw.2010,mw.2010.TA,by="TA_names" )  

mw.2013 = mw.2013 %>% 
  mutate(maize_current = clust_maize_current) %>%
  mutate(maize_mktthin_current = clust_maize_mktthin_current) %>%
  mutate(asset = asset_index2)  %>%
  mutate(TA_maize_mktthin_current = TA_maize_mktthin)

mw.2013.lhz = mw.2013 %>% 
  dplyr::select(FNID,roof_natural_inverse,
                number_celphones,percent_ag,elevation,
                nutri_rent_moderate_constraint,
                dist_road,dist_admarc,hhsize,
                hh_age,hh_gender,asset
  ) %>%
  group_by(FNID) %>%
  summarise_all(funs(mean(.))) %>%
  mutate(lhz_roof_natural_inverse = roof_natural_inverse) %>%
  mutate(lhz_number_celphones = number_celphones) %>%
  mutate(lhz_percent_ag= percent_ag) %>%
  mutate(lhz_elevation = elevation) %>%
  mutate(lhz_nutri_rent_moderate_constraint = nutri_rent_moderate_constraint) %>%
  mutate(lhz_dist_road = dist_road) %>%
  mutate(lhz_dist_admarc = dist_admarc) %>%  
  mutate(lhz_hhsize = hhsize) %>%  
  mutate(lhz_hh_age = hh_age) %>%  
  mutate(lhz_hh_gender = hh_gender) %>%  
  mutate(lhz_asset = asset) %>% 
  dplyr::select(FNID,starts_with("lhz"))


mw.2013.TA = mw.2013 %>% 
  dplyr::select(TA_names,roof_natural_inverse,
                number_celphones,percent_ag,elevation,
                nutri_rent_moderate_constraint,
                dist_road,dist_admarc,hhsize,
                hh_age,hh_gender,asset
  ) %>%
  group_by(TA_names) %>%
  summarise_all(funs(mean(.))) %>%
  mutate(TA_roof_natural_inverse = roof_natural_inverse) %>%
  mutate(TA_number_celphones = number_celphones) %>%
  mutate(TA_percent_ag= percent_ag) %>%
  mutate(TA_elevation = elevation) %>%
  mutate(TA_nutri_rent_moderate_constraint = nutri_rent_moderate_constraint) %>%
  mutate(TA_dist_road = dist_road) %>%
  mutate(TA_dist_admarc = dist_admarc) %>%  
  mutate(TA_hhsize = hhsize) %>%  
  mutate(TA_hh_age = hh_age) %>%  
  mutate(TA_hh_gender = hh_gender) %>%  
  mutate(TA_asset = asset) %>% 
  dplyr::select( starts_with("TA"))

mw.2013 = left_join(mw.2013,mw.2013.lhz,by="FNID" )  
mw.2013 = left_join(mw.2013,mw.2013.TA,by="TA_names" )  

# levels 
levels<-c("lhz","TA","clust")

# Y variables  
yvars=c("logFCS","HDDS","rCSI")


# variables 
weather = c("raincytot","day1rain","floodmax","maxdaysnorain")
access<-c("maize_current","maize_mktthin_current")
land<-c("percent_ag","elevation","nutri_rent_moderate_constraint")
distance<-c("dist_road","dist_admarc")
quarter = c("quarter1","quarter2","quarter3") 
asset1 <-c("roof_natural_inverse","number_celphones")
demo<-c("hhsize","hh_age","hh_gender","asset")

model3_variables<-c(weather,access,asset1,land,distance,demo)
model2_variables<-c(weather,access,asset1,land,distance)
model1_variables<-c(weather,access,land,distance)
model0_variables = quarter

models = c("model1","model2","model3")
# goal : # iterate over models and levels using pastes 
# to have all the variables for each model and level


for (level in levels){
  for(model in models){
    
  # assign levels of variables groups 
  group_var_name<-paste(level,model,"vars",sep="_")
  assign(group_var_name,c())
  
  
  varlist = get(paste(model,"_variables",sep=""))
  
  level.var.list =c()
  for(var in varlist){
    level.var <-paste(level,var,sep = "_")
    level.var.list<-append(get(group_var_name),level.var)
    assign(group_var_name,level.var.list)
  }
  
  level.var.list= append(get(group_var_name),quarter) 
  assign(group_var_name,level.var.list)
  
  }
  }
  

  
model0_vars=quarter

clust_model1_vars = gsub(clust_model1_vars,pattern="clust_",replacement = "")
clust_model2_vars = gsub(clust_model2_vars,pattern="clust_",replacement = "")
clust_model3_vars = gsub(clust_model3_vars,pattern="clust_",replacement = "")

######################################
# create pairs of predicted vs actual 
# For each measure and for each level
####################################
source("R/functions/formula.R")
source("R/functions/linear_fit.R")

# Define function for creating the pairs for a given measure at a specific geospatial level and a model specification
CreatePairs = function( yvar=c("logFCS","HDDS","rCSI") , level=c("clust","TA","lhz"),model = c("model0","model1","model2","model3")  ){
  formula.temp =NULL
  vars.temp =NULL
  pair.temp = NULL 
  
  # get the X variables for the given level and model 
  
  if (model=="model0") {
    vars.temp = model0_vars
  } else {
    vars.temp = get(paste(level,model,"vars",sep="_") )
  }
  
  # 1. Create the formulas using the formula_compose function
   formula.temp <-formula_compose(yvar,vars.temp)
  # 2. generate prediction using linear fit function
  prediction.temp <-linear_fit(formula.temp,mw.2010,mw.2013)
  
  # 3. combind into pairs
  pair.temp <- cbind(mw.2013[[yvar]], prediction.temp)
  colnames(pair.temp) <- c("actual","predict")
  
  return(pair.temp)
}

##########################################################
# run regression on different levels,store in list and write as csv files
#########################################################
# 3 vars and 3 levels and 4 models 
#pair.list = vector("list", 36)

# create folders for storing the results
dir.create("output/results", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction/lhz", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction/TA", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction/clust", showWarnings = TRUE, recursive = FALSE, mode = "0777")



# yvars:"logFCS" "HDDS"   "rCSI"
# levels :    "lhz"  "TA"    "clust" 
# models: "model0" "model1" "model2" "model3"

  
source("R/functions/R2ComputePair.R")


models_to_run = c("model0",models)
models_to_run



for (level in levels){
  # define a r2 matrix for storing results
  r2matrix = matrix( nrow=4,ncol=3,NA)
  colnames(r2matrix) = yvars
  rownames(r2matrix) = models_to_run
  r2matrix  
    
    for(yvar in yvars){
 
      for (model in models_to_run){
        directory = paste("output/results/prediction/",level,"",sep = "/")
        # Create predicted and actual 
        pair.save  = CreatePairs(yvar,level,model)
        
        # save the results(pairs) in  csv files 
        file.name = paste(yvar,level,model,"pair.csv",sep="_")
        directory.file.name = paste(directory, file.name,sep = '')
        write.csv(pair.save,
                  directory.file.name,
                  row.names = FALSE)
        
        # compute r2 square using r2compute and save in the r2matrix
        
        r2 = R2ComputePair(pair.save)
        r2matrix[model,yvar] = r2
        
       }
    }
    
    # write the r2 matrix in a csv file
    r2.file.name = paste(level,"r2matrix.csv",sep="_")
    r2.directory.file.name = paste(directory, r2.file.name,sep = '')
    write.csv(r2matrix,
              r2.directory.file.name,
              row.names = TRUE)
    
}



##########################################################
# run regression on different levels,store in list and write as csv files
# tail regressions 
#########################################################
# 3 vars and 3 levels and 4 models 
#pair.list = vector("list", 36)

# create folders for storing the results
dir.create("output/results", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction_tail", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction_tail/lhz", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction_tail/TA", showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create("output/results/prediction_tail/clust", showWarnings = TRUE, recursive = FALSE, mode = "0777")


CreatePairsTail = function( yvar=c("logFCS","HDDS","rCSI") , level=c("clust","TA","lhz"),model = c("model0","model1","model2","model3")  ){
  formula.temp =NULL
  vars.temp =NULL
  pair.temp = NULL 
  
  
  if (yvar == "logFCS"){
      mw.2010.tail = mw.2010 %>%  
      dplyr::filter(logFCS<log(42))
  } else if (yvar == "HDDS") {
    mw.2010.tail = mw.2010 %>%  
      dplyr::filter(HDDS<6)
  } else if (yvar == "rCSI") {
    mw.2010.tail = mw.2010 %>%  
      dplyr::filter(rCSI>4)
  }
  
  # get the X variables for the given level and model 
  
  if (model=="model0") {
    vars.temp = model0_vars
  } else {
    vars.temp = get(paste(level,model,"vars",sep="_") )
  }
  
  # 1. Create the formulas using the formula_compose function
  formula.temp <-formula_compose(yvar,vars.temp)
  # 2. generate prediction using linear fit function
  prediction.temp <-linear_fit(formula.temp,mw.2010.tail,mw.2013)
  
  # 3. combind into pairs
  pair.temp <- cbind(mw.2013[[yvar]], prediction.temp)
  colnames(pair.temp) <- c("actual","predict")
  
  return(pair.temp)
}

# yvars:"logFCS" "HDDS"   "rCSI"
# levels :    "lhz"  "TA"    "clust" 
# models: "model0" "model1" "model2" "model3"


source("R/functions/R2ComputePair.R")
source("R/functions/formula.R")
source("R/functions/linear_fit.R")


models_to_run = c("model0",models)
models_to_run



for (level in levels){
  # define a r2 matrix for storing results
  r2matrix = matrix( nrow=4,ncol=3,NA)
  colnames(r2matrix) = yvars
  rownames(r2matrix) = models_to_run
  r2matrix  
  
  for(yvar in yvars){
    
    for (model in models_to_run){
      directory = paste("output/results/prediction_tail/",level,"",sep = "/")
      # Create predicted and actual 
      pair.save  = CreatePairsTail(yvar,level,model)
      
      # save the results(pairs) in  csv files 
      file.name = paste(yvar,level,model,"tail_pair.csv",sep="_")
      directory.file.name = paste(directory, file.name,sep = '')
      write.csv(pair.save,
                directory.file.name,
                row.names = FALSE)
      
      # compute r2 square using r2compute and save in the r2matrix
      
      r2 = R2ComputePair(pair.save)
      r2matrix[model,yvar] = r2
      
    }
  }
  
  # write the r2 matrix in a csv file
  r2.file.name = paste(level,"tail_r2matrix.csv",sep="_")
  r2.directory.file.name = paste(directory, r2.file.name,sep = '')
  write.csv(r2matrix,
            r2.directory.file.name,
            row.names = TRUE)
  
}




################################################ 
# Measure model performance accuracy, type I and type II errors.
# prepare for figure S4
#####################################################


 
#   collapse model performance into a couple of tables by having the following columns:
# % correct, % type I, % type II and a different row for each model.


################################################ 
# generate the categorized actual 
#####################################################


logFCS <- read.csv("output/results/prediction/clust/logFCS_clust_model3_pair.csv")
HDDS <- read.csv("output/results/prediction/clust/HDDS_clust_model3_pair.csv")
rCSI <- read.csv("output/results/prediction/clust/rCSI_clust_model3_pair.csv")

colnames(logFCS)<-c("logFCS","logFCS_predict")
colnames(HDDS)<-c("HDDS","HDDS_predict")
colnames(rCSI)<-c("rCSI","rCSI_predict")

actual_logFCS = logFCS$logFCS
actual_HDDS = HDDS$HDDS
actual_rCSI = rCSI$rCSI

cat_logFCS<-cut(actual_logFCS, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
cat_HDDS<-cut(actual_HDDS, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
cat_rCSI<-cut(actual_rCSI, c(-Inf,4,17,42,Inf),labels=c("Food Secure","Mild","Moderate","Severe"))


#"Poor_predict","Borderline_predict","Acceptable_predict"
# Poor_actual","Borderline_actual","Acceptable_actual"

################################################################################################  
# generate information matrix for each measure * model * level (accuracy, type1, type2 )
##################################################################################################### 

# information matrix for logFCS (accuracy, type1, type2 )

logFCS_full_list= list.files("output/results/all_predict/logFCS",full.names = TRUE)
HDDS_full_list= list.files("output/results/all_predict/HDDS",full.names = TRUE)
rCSI_full_list= list.files("output/results/all_predict/rCSI",full.names = TRUE)


model_name = gsub("output/results/all_predict/logFCS/","",logFCS_full_list)
model_name = gsub("_pair.csv","",model_name)
model_name = gsub("model","m",model_name)


info_master = data.frame(model=model_name[1],accuracy=0,type1=0,type2=0)
info_master

for (i in 1:length(logFCS_full_list)){
  temp_file  = read.csv(logFCS_full_list[i])
  cat_logFCS_pred<-cut(temp_file[,2], c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
  confu_mat= as.matrix(confusionMatrix(cat_logFCS_pred,cat_logFCS))
  accuracy = sum(diag(confu_mat))/sum(rowSums(confu_mat))
  # lower =  predict to be better than actual, over prediction 
  type2 = sum(confu_mat[lower.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  type1 = sum(confu_mat[upper.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  model = model_name[i]
  info_table = data.frame(model,accuracy,type1,type2)
  info_master = bind_rows(info_master,info_table)
}

info_master

# information matrix for HDDS (accuracy, type1, type2 )

model_name = gsub("output/results/all_predict/HDDS/","",HDDS_full_list)
model_name = gsub("_pair.csv","",model_name)
model_name = gsub("model","m",model_name)



for (i in 1:length(HDDS_full_list)){
  temp_file  = read.csv(HDDS_full_list[i])
  cat_HDDS_pred<-cut(temp_file[,2], c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
  confu_mat= as.matrix(confusionMatrix(cat_HDDS_pred,cat_HDDS))
  accuracy = sum(diag(confu_mat))/sum(rowSums(confu_mat))
  # lower =  predict to be better than actual, over prediction 
  type2 = sum(confu_mat[lower.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  type1 = sum(confu_mat[upper.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  model = model_name[i]
  info_table = data.frame(model,accuracy,type1,type2)
  info_master = bind_rows(info_master,info_table)
}

info_master


# information matrix for rCSI (accuracy, type1, type2 )

model_name = gsub("output/results/all_predict/rCSI/","",rCSI_full_list)
model_name = gsub("_pair.csv","",model_name)
model_name = gsub("model","m",model_name)


for (i in 1:length(rCSI_full_list)){
  temp_file  = read.csv(rCSI_full_list[i])
  cat_rCSI_pred<-cut(temp_file[,2], c(-Inf,4,17,42,Inf),labels=c("Food Secure","Mild","Moderate","Severe"))
  confu_mat= as.matrix(confusionMatrix(cat_rCSI_pred,cat_rCSI))
  accuracy = sum(diag(confu_mat))/sum(rowSums(confu_mat))
  # lower =  predict to be better than actual, over prediction 
  type2 = sum(confu_mat[upper.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  type1 = sum(confu_mat[lower.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  model = model_name[i]
  info_table = data.frame(model,accuracy,type1,type2)
  info_master = bind_rows(info_master,info_table)
}
confu_mat

info_master



##################################################################
## Do the same for tail Measures
##################################################################
logFCS_tail_list= list.files("output/results/all_tail_predictions/logFCS",full.names = TRUE)
HDDS_tail_list= list.files("output/results/all_tail_predictions/HDDS",full.names = TRUE)
rCSI_tail_list= list.files("output/results/all_tail_predictions/rCSI",full.names = TRUE)


model_name = gsub("output/results/all_tail_predictions/logFCS/","",logFCS_tail_list)
model_name = gsub("_pair.csv","",model_name)
model_name = gsub("model","m",model_name)
model_name

info_tail = data.frame(model=model_name[1],accuracy=0,type1=0,type2=0)
info_tail

for (i in 1:length(logFCS_tail_list)){
  temp_file  = read.csv(logFCS_tail_list[i])
  cat_logFCS_pred<-cut(temp_file[,2], c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
  confu_mat= as.matrix(confusionMatrix(cat_logFCS_pred,cat_logFCS))
  accuracy = sum(diag(confu_mat))/sum(rowSums(confu_mat))
  # lower =  predict to be better than actual, over prediction 
  type2 = sum(confu_mat[lower.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  type1 = sum(confu_mat[upper.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  model = model_name[i]
  info_table = data.frame(model,accuracy,type1,type2)
  info_tail = bind_rows(info_tail,info_table)
}

info_tail


model_name = gsub("output/results/all_tail_predictions/HDDS/","",HDDS_tail_list)
model_name = gsub("_pair.csv","",model_name)
model_name = gsub("model","m",model_name)
model_name


for (i in 1:length(HDDS_tail_list)){
  temp_file  = read.csv(HDDS_tail_list[i])
  cat_HDDS_pred<-cut(temp_file[,2], c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
  confu_mat= as.matrix(confusionMatrix(cat_HDDS_pred,cat_HDDS))
  accuracy = sum(diag(confu_mat))/sum(rowSums(confu_mat))
  # lower =  predict to be better than actual, over prediction 
  type2 = sum(confu_mat[lower.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  type1 = sum(confu_mat[upper.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  model = model_name[i]
  info_table = data.frame(model,accuracy,type1,type2)
  info_tail = bind_rows(info_tail,info_table)
}
info_tail


model_name = gsub("output/results/all_tail_predictions/rCSI/","",rCSI_tail_list)
model_name = gsub("_pair.csv","",model_name)
model_name = gsub("model","m",model_name)
model_name



for (i in 1:length(rCSI_tail_list)){
  temp_file  = read.csv(rCSI_tail_list[i])
  cat_rCSI_pred<-cut(temp_file[,2], c(-Inf,4,17,42,Inf),labels=c("Food Secure","Mild","Moderate","Severe"))
  confu_mat= as.matrix(confusionMatrix(cat_rCSI_pred,cat_rCSI))
  accuracy = sum(diag(confu_mat))/sum(rowSums(confu_mat))
  # lower =  predict to be better than actual, over prediction 
  type2 = sum(confu_mat[upper.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  type1 = sum(confu_mat[lower.tri(confu_mat, diag = FALSE)])/sum(rowSums(confu_mat))
  model = model_name[i]
  info_table = data.frame(model,accuracy,type1,type2)
  info_tail = bind_rows(info_tail,info_table)
}


info_tail

# remove row 1 
info_tail = info_tail[-1,]
info_master = info_master[-1,]



write.csv(info_tail,"output/results/figureS4_matrix_tail.csv",row.names = FALSE)
write.csv(info_master,"output/results/figureS4_matrix_full.csv",row.names = FALSE)





 
 






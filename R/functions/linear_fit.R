####################################### #############  
# Goals
#   (1) fit a linear function based on a training set and a testing set 

# Inputs
#   (1) formula: formula of y on the x variables 
#   (2) train_data: dataframe containing both label and features 
#   (3) test_data: dataframe containing both label and features

# Outputs
#   (1) a dataframe containing the predicated value of y variable in the training and testing set. 
############# ############# ############# ############# ############# ############# ############# 

linear_fit <- function(lm_formula, train_data,test_data){
  
  
  # fit the model on the training data 
  lm_fit <- lm(formula = lm_formula, data = train_data)
  
  # predict training values
  # pred_train <- predict(lm_fit)
  
  ### compute the TEST predication 
  pred_test <- predict(lm_fit, newdata = test_data)
  # pred_list <- list("pred_test" = pred_test, "pred_train" = pred_train)
  
  
  return(pred_test)
}

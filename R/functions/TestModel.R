
####################################################################################
## Prediction on test data set 
####################################################################################

##################################################################
# Goal : test on testing data set 
# input: trained model , test data frame  
# output: trained model
####################################################################  

library(caret)
TestModel = function( model, test.df){
  
  predicted  = predict(model, test.df, se.fit = TRUE)
  
  return(predicted)
}
 




 
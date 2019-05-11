
####################################################################################
## Evaluate model performance (R squared)  on test data set 
####################################################################################

##################################################################
# Goal : evalute performance on testing data set 
# input: predict, actual 
# output: r2
####################################################################  

library(caret)
R2Compute = function( predict, actual){
  
  r2  = cor(predict,actual , method = c("pearson"))^2

  return(r2)
}


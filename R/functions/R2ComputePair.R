
####################################################################################
## Evaluate model performance (R squared)  on test data set 
####################################################################################

##################################################################
# Goal : evalute performance on testing data set 
# input: predict, actual 
# output: r2
####################################################################  

library(caret)
R2ComputePair = function(pair){
  r2 = cor(pair[,1],pair[,2], method = c("pearson"))^2
  r2 = format(round(r2,3),nsmall = 3)
  return(as.numeric(r2))
}


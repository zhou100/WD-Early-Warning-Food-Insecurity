####################################################################################
## train model
####################################################################################

##################################################################
# Goal : Train model on training set 
# input: Y and X (in formula format) , data frame, method 
# output: trained model
################################## 

library(caret)
TrainModel = function(train_df, formula,method= "lm"){

if (method=="lm"){
  trained.model <- train(formula, data = train_df, method = method)  
} else {
  lambda <- 10^seq(-3, 3, length = 100)
  trained.model = train(formula, data = train_df, method = "glmnet",trControl = trainControl("cv", number = 10),
        tuneGrid = expand.grid(alpha = 1, lambda = lambda))
}
    

return(trained.model)

}
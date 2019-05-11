# Define prediction function 
PredictMalawi = function(y_var,formula,method="lm",train_df, test_df){
  # Train on training set 
  model = TrainModel(train_df = train_df, formula=formula,method=method)
  # predict 
  predict = TestModel(model,test.df = test_df)
  return(predict)
}
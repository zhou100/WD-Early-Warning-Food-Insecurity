# Define result matrix function 
ModelPerformance =function(model_name,x_vars,method="lm",train_df,test_df){
  # Three outcome variable
  y.vector = c("logFCS","HDDS","rCSI")
  # Three measurements (r2,recall,accuracy): 9 results
  result.matrix = matrix(nrow=1,ncol=9,data = NA)
  for (i in 1:3){
    yvar= y.vector[i]
    predicted = PredictMalawi(y_var=yvar,formula=FormulaComposer(y_var=yvar, x_vars=x_vars),method=method,train_df=train_df,test_df=test_df)
    
    # Compute R squared 
    r2 = R2Compute(predict =predicted,actual = as.numeric(test_df[[yvar]]) )
    # Compute recall rate 
    
    recall = CategoryRecall(yvar=yvar,predicted=predicted,test.df = test_df  )  
    # Compute category accuracy  
    accuracy = CategoryAccuracy(yvar=yvar, predicted = predicted,test.df = test_df )
    
    result.matrix[1,i] = r2
    result.matrix[1,i+3] = recall
    result.matrix[1,i+6] = accuracy
  }
  return(cat(model_name,result.matrix) )
}
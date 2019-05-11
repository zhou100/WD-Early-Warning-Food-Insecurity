##########################################################
# categorical prediction  
# return overall categorical accuracy 
#########################################################

# FCS 28 42  
# HDDS 3 6 
# RCSI 4 17 42
library(caret)

CategoryAccuracy = function(yvar= c("logFCS","HDDS","rCSI"), predicted,test.df){
  predict = predicted
  actual= as.numeric(test.df[[yvar]])
  
  if ( yvar == "logFCS") {
    
    actual.category <-cut(actual, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
    predict.category <-cut(predict, c(0,log(28), log(42),Inf),labels=c("Poor","Borderline","Acceptable"))
    
  } else if ( yvar == "HDDS") {
    actual.category <-cut(actual, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
    predict.category <-cut(predict, c(0,3, 6,Inf),labels=c("Low Diversity","Medium Diversity","Good Diversity"))
    
  } else if ( yvar == "rCSI") {
    actual.category<-cut(actual, c(-Inf,4,17,42,Inf),labels=c("Food Secure", "Mild","Moderate","Severe"))
    predict.category<-cut(predict, c(-Inf,4,17,42,Inf),labels=c(c("Food Secure", "Mild","Moderate","Severe")))
    
  }
  
  # confusionMatrix of category prediction 
  accuracy =  confusionMatrix(data=predict.category,reference=actual.category )$overall['Accuracy']
  

  return (accuracy)
}




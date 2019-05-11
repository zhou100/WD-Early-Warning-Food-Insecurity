##########################################################
# categorical prediction using full model
# return recall rate of the insecure category 
#########################################################

# FCS 28 42  
# HDDS 3 6 
# RCSI 4 17 42
library(caret)

CategoryRecall = function(yvar= c("logFCS","HDDS","rCSI"), predicted,test.df){
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

    # category prediction 
    category.matrix =  as.matrix(confusionMatrix(data=predict.category,reference=actual.category ))

    # Percentage of food insecure clusters correctly predicted to be food insecure

    if ( yvar == "logFCS") {
      recall.rate = category.matrix[2,2]/(category.matrix[2,2] + category.matrix[3,2])
      
    } else if ( yvar == "HDDS") {
      recall.rate= category.matrix[2,2]/(category.matrix[2,2] + category.matrix[3,2])
    } else if ( yvar == "rCSI") {
      recall.rate= category.matrix[2,2]/(category.matrix[1,2] + category.matrix[2,2])
      
    } 
  
  return (recall.rate)
}
  
  
   
  
formula_compose <- function(y_name,groupvars){
  
  # Goals
  #   (1) compose a formula for implementing models in R.
  
  # Inputs
  #   (1) y_name: string, name of the y variable 
  #   (2) groupvars: dataframe containing all the x variables 
  
  # Outputs
  #   (1) a formula of y regressed on the x's
  
  x_variables<- paste(groupvars,collapse = "+")
  x_variables<- paste(x_variables,"1",sep = "+")
  x_variables<- paste(x_variables,"IPC1",sep = "+")
  formula_name<-paste(y_name,x_variables,sep="~")
  return(as.formula(formula_name))
}

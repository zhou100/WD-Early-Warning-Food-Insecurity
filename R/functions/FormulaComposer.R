# Define formula composer function 
FormulaComposer = function(y_var, x_vars){
  dependent = paste(y_var,"~IPC1",sep="")  
  formula = as.formula(   paste(dependent,x_vars, sep="+")  ) 
  return(formula)
}
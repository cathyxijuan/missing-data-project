### Usage: making RMSEA table. It is a matrix containing fmin and RMSEA. 
# Argument: fit.matrix : a matrix containing fmin and rmsea

rmsea_table <- function(fit.matrix) {
  fmin <- (fit.matrix[,"fmin"])*2 
  rmsea <- fit.matrix[,"rmsea"]
  final_table <- cbind(fmin, rmsea)
  row.names(final_table) <- rep("",nrow(final_table))
  round(final_table,4)
}





### Usage: making CFI table. It is a matrix containing fmin, fmin for baseline model and RMSEA. 
# Argument: fit.matrix : a matrix containing fit indices 

cfi_table <- function(fit.matrix) {
  fmin <- (fit.matrix[,"fmin"])*2 
  cfi <- fit.matrix[,"cfi"]
  fminB <- fit.matrix[,"baseline.chisq"]/1000000
  final_table <- cbind(fmin, fminB, cfi)
  row.names(final_table) <- rep("",nrow(final_table))
  round(final_table,4)
}

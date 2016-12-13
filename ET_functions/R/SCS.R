#' SCS Model
#' 
#' This function calculate the SCS method for Runoff, in atempt to use physical parameters rather the curve number of #' the original method.
#' Here the usage is for estimation of daily runoff
#' 
#'@param P (mm) daily rain.
#'@param I (mm) daily initial abstraction (we try to use the daily interception from the interception model)
#'@author Elad Dente
#'@references http://www.professorpatel.com/uploads/7/6/5/6/7656897/tr55.pdf
#'@return I, S, CN and Q (all in mm)

SCS=function(P,I){
  S=I/0.2
  Q=((P-I)*(P-I))/(P-I+S)
  
  #IS is the I acording to I=0.2*S
  IS=0.2*S
  #CN number for SI units
  CN=25400/(S+254)
  #results
  return(list(IS=IS,I=I,runoff=Q,CN=CN))
}
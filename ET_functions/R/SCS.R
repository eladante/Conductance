#' SCS Model
#' 
#' This function calculate the SCS method for Runoff. This function atempt to use physical parameters rather the curve number of the original method.
#' Here the usage is for estimation of daily runoff
#' 
#'@param P (mm) daily rain.
#'@param I (mm) daily initial abstraction (we try to use the daily interception from the interception model)
#'@param S (-)  (from the CN number. default is 130 for CN 66)
#'@author Elad Dente
#'@references http://www.professorpatel.com/uploads/7/6/5/6/7656897/tr55.pdf
#'@return Q (mm) runoff

SCS=function(P,I,S){
  Q=((P-I)^2)/(P-I+S)
  
  #IS is the I acordin to I=0.2*S
  IS=0.2*S
  
  return(list(IS=IS,I=I,runoff=Q))
}
  

#' Canopy and Stem Interception Model
#' 
#' This model calculates the precipitation necessery to fill canopy and stem storage, based on Gash et al. 1995, and Valente et al. 1977.
#' The defults represent values of mediterranean Quercus ilex (evergreen oak) ecosystem acording to the reference below.
#'
#' @usage calculate Interception for a day
#' @param P.rate (mm/hr) is the mean precipitation rate.
#' @param  Evap (mm) total evporation (Here we try to aplly the Penman-montieth ET.)
#' @param P (mm) total precipitation
#' @param c (-) canopy cover per unit area
#' @param S (mm) canopy storage capacity  
#' @param St (mm) trunks storage capacity
#' @param Pt (-) the propotion of rain goes to stems
#' @author Elad Dente
#' @return P.canopy (mm) canopy interception, P.stem (mm) stem interception, tot_inter (mm) total interception, and prc.inter (%) precentage of interception in precipitation. .
#' @references Limousin, J. M., Rambal, S., Ourcival, J. M., & Joffre, R. (2008). Modelling rainfall interception in a Mediterranean Quercus ilex ecosystem: lesson from a throughfall exclusion experiment. Journal of Hydrology, 357(1), 57-66.

interception<-
  function(P.rate,Evap,P,c=0.59,S=1.7,St=0.64,Pt=0.1,dayl=11){
  
  #       Internal Variables
  #       Ec (mm/hr) is the mean evaporation rate. 
  #       Sc (mm) canopy storage capcity per cover unit (S/c)
  #       Stc (mm) stem storage capcity per cover unit (St/c)
  #       Ptc (mm) propotion of rain goes to stemss per cover unit (Pt/c)
  
  #calculating Sc
  Ec= Evap/dayl
  #calculating Sc
  Sc= S/c
  #calculating Stc
  Stc= St/c
  #calculating Ptc
  Ptc= Pt/c
  
  #calculating P.canopy
  P.canopy=-(P.rate/Ec)*Sc*log(1-(Ec/P.rate))
  
  #calculating P.stem
  P.stem=(P.rate/(P.rate-Ec))*(Stc/Ptc)
  
  #total interception
  tot_inter=P.canopy+P.stem
  
  # % total interception from precipitation
  prc.inter=100*tot_inter/P
  
  return (list(canopy_interception=P.canopy,stem_interception=P.stem,total_interception=tot_inter,inter.precentage=prc.inter))
}
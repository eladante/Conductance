#' Canopy Conductance Model
#' 
#' The R in Jarvis parameters is for "relative". the valus can be taken from the plots and equations in "Physical Hydrology: Third Edition" by S. Lawrence Dingman (p. 282).
#Default values for Quercus ilex (oaks) and Mediterranian sclerophyllous where taken from
#Schulze, E. D., Kelliher, F. M., Korner, C., Lloyd, J., & Leuning, R. (1994). Relationships among maximum stomatal #conductance, ecosystem surface conductance, carbon assimilation rate, and plant nitrogen nutrition: a global ecology #scaling exercise. Annual Review of Ecology and Systematics, 629-660.
#and
#Limousin, J. M., Rambal, S., Ourcival, J. M., & Joffre, R. (2008). Modelling rainfall interception in a Mediterranean #Quercus ilex ecosystem: lesson from a throughfall exclusion experiment. Journal of Hydrology, 357(1), 57-66. 



#' @param T (deg c) air temperature at measured height above ground (0>T<40 deg C).
#' @param Cleaf_max (mm/s) maximum stomatal contuctance .
#' @param Rnrad (-) Relative solar radiation.
#' @param Rvpd (-) Relative absolute-humidity deficit.
#' @param Rteta (-) Relative soli moisture deficit.
#' @param LAI (-) Leaf area index.
#' @param Fs (-) Shelter factor.
#' @usage calculate canopy conductance, based on the Jarvis model. To be used in Penman-Montieth ET model.
#' @author Elad Dente
#' @return Jarvis model coefficient (Jarvis_c) and canopy conductance (Kc, mm/s)

canopy_conductance<-
  function(Cleaf_max=4.8, Rnrad, Rvpd=0.85,T, Rteta, LAI=1.61,Fs=0.2) {
    
    #       Internal Variables
    #       RT  (-) from  T, based on Stewart (1988) relationsip). 
    
    #RT from T
    RT=(T*(40-T)^1.18)/691
    
    #claculating Jc
    Jarvis_c = (Cleaf_max*Rnrad*Rvpd*RT*Rteta)
    
    #claculating Kc
    Kc = (Jarvis_c*LAI*Fs)
    
    #return Kc
    return(list(Jarvis_coefficient=Jarvis_c, Relative_temperature=RT, Canopy_conductance_mm.s=Kc))
  }

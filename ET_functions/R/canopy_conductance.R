#' Canopy Conductance Model
#' 
#' The R in Jarvis parameters is for "relative". the valus can be taken from the plots in "Physical Hydrology: Third Edition" by S. Lawrence Dingman
#' (p. 282)
#' Defult values for the relative parameters are set to Rnrad=1, Rvpd=1, Rteta=1.
#' Cleaf_max was set to be 5 mm/s as default, according the etimation in "Physical Hydrology: Third Edition" to "mixed covrer".
#' LAI was set to 3 for "mixed cover". The range of values for this kind of vegetation is 3-6.
#' Kc (canopy conductance)= Kleaf (Jarvis model. function of cleaf_max,Rnrad,Rvpd,RT and RTeta) * LAI


#' @param T (deg c) air temperature at measured height above ground (0>T<40 deg C).
#' @usage calculate canopy conductance, based on the Jarvis model. To be used in Penman-Montieth ET model.
#' @usage will be added
#' @author Elad Dente
#' @return Jarvis model coefficient (Jarvis_c) and canopy conductance (Kc, mm/s)

canopy_conductance<-
  function(Cleaf_max=4.8, Rnrad, Rvpd=0.85,T, Rteta, LAI=1.61) {
    
    #       Internal Variables
    #       RT  (-) from  T, based on Stewart (1988) relationsip). 
    
    #RT from T
    RT=(T*(40-T)^1.18)/691
    
    #claculating Jc
    Jarvis_c = (Cleaf_max*Rnrad*Rvpd*RT*Rteta)
    
    #claculating Kc
    Kc = (Jarvis_c*LAI)
    
    #return Kc
    return(list(Jarvis_coefficient=Jarvis_c, Temperature_effect=RT, Canopy_conductance_mm.s=Kc))
  }


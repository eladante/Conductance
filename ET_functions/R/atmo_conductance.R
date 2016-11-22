#' Atmospheric conductance model
#' 
#' Based on eq.6.48 (p. 278) in "Physical Hydrology: Third Edition" by S. Lawrence Dingman
#' This model gives estimation of the atmospheric conductance (Ca) based on the 
#' wind velocity (at 2m above ground, 'windspeed') and vegetation height ('z_veg')
#' 
#' @param   windspeed (cm/s) at 2m above ground 
#' @param   z_veg (cm), height of vegetation above ground
#' @usage calculate atmospheric conductance to be used in Penman-Montieth ET model.
#' @author Elad Dente
#' @return zd, z0, zm (all in cm), and atmospheric conductance (cm/s)

atmo_conductance <-
function(windspeed, z_veg) {
    #       Internal Variables
    #
    #       Zd     (cm)    zero-plan displacement height
    #       z0    (cm)     roughness height
    #       zm      (cm)      measurement_height
    
    #zd from Z_veg
    zd = 0.7*z_veg
    
    #z0 from z_veg
    z0 = 0.1*z_veg
    
    #zm from z_veg
    zm = (z_veg +200)
     
    #claculating Ca
    Ca = (windspeed/(6.25*(log((zm-zd)/z0))^2))
    
    #return Ca
    return(list(displacement_height=zd, roughness_height=z0, measurement_height=zm, atmospheric_conductance_cm.s=Ca))
  }

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atmo_conductance.R
\name{atmo_conductance}
\alias{atmo_conductance}
\title{Atmospheric conductance model}
\usage{
calculate atmospheric conductance to be used in Penman-Montieth ET model.
}
\arguments{
\item{windspeed}{(cm/s) at 2m above ground}

\item{z_veg}{(cm), height of vegetation above ground}
}
\value{
zd, z0, zm (all in cm), and atmospheric conductance (cm/s)
}
\description{
Based on eq.6.48 (p. 278) in "Physical Hydrology: Third Edition" by S. Lawrence Dingman
This model gives estimation of the atmospheric conductance (Ca) based on the 
wind velocity (at 2m above vegetation, 'windspeed') and vegetation height ('z_veg')
}
\author{
Elad Dente
}


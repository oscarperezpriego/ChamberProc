# functions to compute volume and surface area of common chamber geometries

calcChamberGeometryCuboid <- function (
		###Calculate the inner volume of the chamber and the enclosed respiring surface area for a cuboid shaped chamber.
		width			##<< the inner length of one basal edge in m
		,depth			##<< the inner length of the other basal edge in m
		,height			##<< the inner height of the chamber in m
		,taper = 1.0	##<< A form factor describing a linear taper of the chamber. 1.0 is equal to no taper. 
){
	##details<< the respiring surface is assumed to be the basal area of the chamber
	##details<<
	## There are functions for other geometries
	## \itemize{
	## \item Cylinder: \code{\link{calcChamberGeometryCylinder}}
	## } 
	respArea      <- width * depth
	##value<< a vector with components
	c (   chamberVolume = respArea * height * taper	##<< the volume inside the chamber in cubic meters
		, respArea = respArea)				    	##<< the basal aera of the cylinder in square meters
}
attr (calcChamberGeometryCuboid,"ex") <- function(){
	calcChamberGeometryCuboid(0.6,0.6,0.6)
}


calcChamberGeometryCylinder <- function (
		### Calculate the inner volume of the chamber and the enclosed respiring surface area for a cylinder-shaped chamber.
		radius			##<< the inner radius of the cylinder in m
		,height			##<< the inner height of the cyclinder in m
		,taper = 1.0	##<< A form factor describing a linear taper of the chamber. 1.0 is equal to no taper. 
){
	##details<< the respiring surface is assumed to be the basal area of the chamber
	##value<< a vector with components
	respArea = pi * radius^2.0
	c (chamberVolume = respArea * height * taper	##<< the volume inside the chamber in cubic meters
	   , respArea = respArea)				##<< the basal aera of the cylinder in square meters
} 
attr (calcChamberGeometryCylinder,"ex") <- function(){
  innerRadius <- 0.1016
  innerHeight <- 0.0762
  calcChamberGeometryCylinder(innerRadius, innerHeight)
}

convert_mumolPers_to_gPerday <- function(
	### convert from mumol CO2 / second to g C / day
	flux	                ##<< numeric vector or array: flux
	, molarMass = 12.011	##<< numeric scalar: the molar mass of the element 
	  ##of interest (default carbon) in g/mol
){
  ##seealso<< \code{\link{convert_gPerday_to_mumolPers}}
  ##seealso<< \code{\link{RespChamberProc}}
  ##value<< numeric vector of flux in other units
	##details<<
	## Concentration measures are usually given by micromol CO2 across several 
	## seconds, and
	## the flux, i.e. its slope hence given in micromol CO2 per second.
	## To compare carbon balances, the units of gC per day are more convenient.  
	##
	## mumol are converted to mol by /1e6
	## mol are converted to gC by *12.011
	## per second are converted to per day by *3600*24
	##
	## For Carbon the conversion factor is 1.038, i.e. very close to one.
	##
	## In order to convert masses of other elements, specify the molarMass argument, 
	## e.g. for converting mol H2O per second to H2O per day, 
	## specify \code{molarMass=2*1.008+15.999}.
  # flux * 1e-6 * molarMass * 3600*24
  flux * molarMass * 0.0864
}
attr(convert_mumolPers_to_gPerday, "ex") <- function(){
	convert_mumolPers_to_gPerday(c(1,10))
}

#' @export
convert_gPerday_to_mumolPers <- function(
  ### convert from g C / day to mumol CO2 / second 
  flux	                ##<< numeric vector or array: flux
  , molarMass = 12.011	##<< numeric scalar: the molar mass of the element 
  ##of interest (default carbon) in g/mol
){
  ##seealso<< \code{\link{convert_mumolPers_to_gPerday}}
  ##seealso<< \code{\link{RespChamberProc}}
  ##value<< numeric vector of flux in other units
  flux / (molarMass * 0.0864)
}
attr(convert_gPerday_to_mumolPers, "ex") <- function(){
  molFlux = c(1,10)
  massFlux = convert_mumolPers_to_gPerday(molFlux)
  convert_gPerday_to_mumolPers(massFlux)
}

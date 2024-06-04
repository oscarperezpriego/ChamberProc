\name{convert_gPerday_to_mumolPers}
\alias{convert_gPerday_to_mumolPers}
\title{convert gPerday to mumolPers}
\description{convert from g C / day to mumol CO2 / second }
\usage{convert_gPerday_to_mumolPers(flux, molarMass = 12.011)}
\arguments{
  \item{flux}{numeric vector or array: flux}
  \item{molarMass}{numeric scalar: the molar mass of the element
of interest (default carbon) in g/mol}
}

\value{numeric vector of flux in other units}

\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{convert_mumolPers_to_gPerday}}

\code{\link{RespChamberProc}}}
\examples{
molFlux = c(1,10)
massFlux = convert_mumolPers_to_gPerday(molFlux)
convert_gPerday_to_mumolPers(massFlux)
}

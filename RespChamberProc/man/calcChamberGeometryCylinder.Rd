\name{calcChamberGeometryCylinder}
\alias{calcChamberGeometryCylinder}
\title{calcChamberGeometryCylinder}
\description{Calculate the inner volume of the chamber and the enclosed respiring surface area for a cylinder-shaped chamber.}
\usage{calcChamberGeometryCylinder(radius, height, 
    taper = 1)}
\arguments{
  \item{radius}{the inner radius of the cylinder in m}
  \item{height}{the inner height of the cyclinder in m}
  \item{taper}{A form factor describing a linear taper of the chamber. 1.0 is equal to no taper.}
}
\details{the respiring surface is assumed to be the basal area of the chamber}
\value{a vector with components
\item{chamberVolume}{the volume inside the chamber in cubic meters}
\item{respArea}{the basal aera of the cylinder in square meters
}}

\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
innerRadius <- 0.1016
innerHeight <- 0.0762
calcChamberGeometryCylinder(innerRadius, innerHeight)
}

\name{calcChamberGeometryCuboid}
\alias{calcChamberGeometryCuboid}
\title{calcChamberGeometryCuboid}
\description{Calculate the inner volume of the chamber and the enclosed respiring surface area for a cuboid shaped chamber.}
\usage{calcChamberGeometryCuboid(width, depth, height, 
    taper = 1)}
\arguments{
  \item{width}{the inner length of one basal edge in m}
  \item{depth}{the inner length of the other basal edge in m}
  \item{height}{the inner height of the chamber in m}
  \item{taper}{A form factor describing a linear taper of the chamber. 1.0 is equal to no taper.}
}
\details{the respiring surface is assumed to be the basal area of the chamber

There are functions for other geometries
\itemize{
\item Cylinder: \code{\link{calcChamberGeometryCylinder}}
} }
\value{a vector with components
\item{chamberVolume}{the volume inside the chamber in cubic meters}
\item{respArea}{the basal aera of the cylinder in square meters
}}

\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
calcChamberGeometryCuboid(0.6,0.6,0.6)
}

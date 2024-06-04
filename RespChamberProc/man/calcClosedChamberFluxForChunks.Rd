\name{calcClosedChamberFluxForChunks}
\alias{calcClosedChamberFluxForChunks}
\title{calcClosedChamberFluxForChunks}
\description{apply \code{\link{calcClosedChamberFlux}} for each chunk in data}
\usage{calcClosedChamberFluxForChunks(ds, colChunk = "iChunk", 
    ..., volume, volumesByChunk, isVerbose = TRUE)}
\arguments{
  \item{ds}{tibble or data.frame}
  \item{colChunk}{column name (scalar string) holding a factor
unique to each chunk}
  \item{\dots}{further arguments to
\code{\link{calcClosedChamberFlux}}}
  \item{volume}{volume inside the chamber im [m3]}
  \item{volumesByChunk}{tibble or data.frame with column <colChunk> and
column  \code{volume} giving a volume for each chunk identifier
By default, value of argument volume is used for eah chunkg 
Allows for adjusting the chamber volume across different chunks 
(argument \code{volumne} in \code{\link{calcClosedChamberFlux}})}
  \item{isVerbose}{set to FALSE to avoid messages}
}

\value{a tibble with a row for each measurement cycle and additional 
column <colChunk> identifying the measurement cycle}

\author{Thomas Wutzler, Oscar Perez Priego}






\name{read81xVar}
\alias{read81xVar}
\title{read81xVar}
\description{Read a Licor generated .x81 file into a data-frame with guessing initial rows		}
\usage{read81xVar(fName, sep = "\\t", ..., colsTimeStamp = 3, 
    formatTS = NULL, tz = "UTC", na.strings = c("", 
        "NA", "NAN", "\\"NAN\\""), labelID = "Label:")}
\arguments{
  \item{fName}{scalar string: file name
file information}
  \item{sep}{column separator}
  \item{\dots}{further arguments to \code{\link{read.table}}}
  \item{colsTimeStamp}{integer vector: colums with time stamp column
(will be set to POSIXct)}
  \item{formatTS}{format of the timestamp columns,
see \code{\link{strptime}}}
  \item{tz}{specify a time zone when converting to POSIXct,
default: UTC}
  \item{na.strings}{see \code{\link{read.table}}}
  \item{labelID}{string at the start of lines indicating the label}
}



\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
#fName <- "inst/genData/Flux2_140929_1700.81x"
#fName <- "inst/genData/Flux2_140929_1700.81x"
fName <- system.file(
  "genData/Flux2_140929_1700.81x", package = "RespChamberProc")
if (nzchar(fName)) {
  ds <- read81xVar(fName)
  #plot( CO2 ~ Date, ds )
  #plot( CO2 ~ Date, ds[ds$iChunk == 9,] )
}
}

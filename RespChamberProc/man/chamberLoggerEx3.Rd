     \name{chamberLoggerEx3}
     \docType{data}
     \alias{chamberLoggerEx3}
     \title{chamberLoggerEx3}
     \description{Respiraction chamber measurement example of another format.
     	Exemplifying several chunks within one measurement cycle.
     }
     \usage{chamberLoggerEx3}
     \format{data.frame with columns \describe{
		\item{Time.H.M.S.}{POSIXct time of measurement}
		\item{CO2.ppm.}{molar CO2 concentration in ppm}
		\item{CellTemp.c}{air temperature inside chamber in degC}
		\item{CellPres.kPa.}{air pressure in kPa}
		}
     }
     \keyword{datasets}
     \examples{
     	data(chamberLoggerEx3)
     	plot( CO2.ppm. ~ Time.H.M.S., data=chamberLoggerEx3)
     }
      

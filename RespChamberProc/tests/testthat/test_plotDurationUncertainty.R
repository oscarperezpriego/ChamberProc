#require(testthat)
context("plotDurationUncertainty")

test_that("run plotDurationUncertainty",{
  # very strong (and therefore precise) uptake
  ds <- subset(chamberLoggerEx2, iChunk == 99)	
  #plot( CO2_dry ~ TIMESTAMP, ds )
  resDur <- plotDurationUncertainty( 
    ds, colTemp = "AirTemp", volume = 0.6*0.6*0.6
    , maxSdFlux = 0.8
    , nDur = 10
    , durations = c(100,120,150)
  )
  resDur$duration
})


			
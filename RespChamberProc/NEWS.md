
* detecting special cases in calcClosedChamberFlux
  * all concentrations numerically equal
  * no finite concentrations

# RespChamberProc 0.6.4

* checking function arguments in calcClosedChamberFluxForChunks
* moved tests to testthat
* depends on rlang instead of interp now
* fixed issue: explicitely closing file readDat

# RespChamberProc 0.6.2

* added geometry functions
* fixed support of tibbles in plotResp
* argument useFixedTLag to specify fixed lag-Time instead of estimating 
  it from the data
* specification of different volumes by chunk more robust

# RespChamberProc 0.6.1

adapted to dplyr, changed return type of calcChamberFlux to nested data.frame


# RespChamberProc 0.5.231

## Further changes

### Migration to github

The hosting of the development moved from r-forge to github. Releases will still 
be put to r-forge, because of its good package-checking setup for several platforms, 
and the help for submission to CRAN, but versioning and development of the code 
will be done on github. 

### Documentation

A package vignette, a README.Rmd and this NEWS.md file have been added.
 

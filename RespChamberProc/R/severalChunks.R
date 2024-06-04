#' @export
calcClosedChamberFluxForChunks <- function(
  ### apply \code{\link{calcClosedChamberFlux}} for each chunk in data
  ds                    ##<< tibble or data.frame
  , colChunk = "iChunk"  ##<< column name (scalar string) holding a factor 
    ## unique to each chunk
  , ...                 ##<< further arguments to 
    ## \code{\link{calcClosedChamberFlux}}
  , volume              ##<< volume inside the chamber im [m3]
  , volumesByChunk      ##<< tibble or data.frame with column <colChunk> and 
    ## column  \code{volume} giving a volume for each chunk identifier
    ## By default, value of argument volume is used for eah chunkg 
    ## Allows for adjusting the chamber volume across different chunks 
    ## (argument \code{volumne} in \code{\link{calcClosedChamberFlux}})
  , isVerbose = TRUE    ##<< set to FALSE to avoid messages
){
  if (missing(volumesByChunk)) {
    dsVol <- ds
    dsVol$volume <- volume
  } else {
    if (!is.data.frame(volumesByChunk)) stop(
      "argument volumesByChunk must be a data.frame")
    requiredColumnNames <- c(colChunk,"volume")
    iMissingCol <- which(!(requiredColumnNames %in% names(volumesByChunk)))
    if (length(iMissingCol) ) stop(
      "Missing columns in dataframe of argument volumesByChunk: "
      , paste(requiredColumnNames[iMissingCol], collapse = ","))
    if (max(table(volumesByChunk[[colChunk]])) > 1L) stop(
      "values in column ", colChunk, " in volumesByChunk must be unique.")
    dsVol <- suppressWarnings(
      left_join(
        ds
        , select(volumesByChunk, !!!syms(c(colChunk, "volume")))
        , colChunk))
    if (nrow(dsVol) != nrow(ds)) stop(
      "could not assign unique volumes to chunks.")
    if (!all(is.finite(dsVol$volume)) ) {
      chunksMissing <- unique(dsVol[[colChunk]][ !is.finite(dsVol$volume)])
      stop("need to provide a finite volume for each chunk. Check chunks "
           , paste(chunksMissing, collapse = ","))
    }
  }
  #. <- filter_(dsVol, paste0(colChunk,"==",colChunk,"[1]"))
  ans <- dsVol %>% group_by( !!sym(colChunk) ) %>% 
      do({
            iChunk <- as.character(.[[colChunk]][1])
            vol <- .$volume[1] 
            calcClosedChamberFlux(.,...,volume = vol)
          })
  names(ans)[names(ans) == "iChunk"] <- colChunk
  ##value<< a tibble with a row for each measurement cycle and additional 
  ## column <colChunk> identifying the measurement cycle
  ans
}

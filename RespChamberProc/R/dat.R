readDat <- function(
	### Read a data logger .dat file into a data-frame		
	fName					        ##<< scalar string: file name or a connection, 
	  ## e.g. returned by \code{\link{unz}}
	,nRowsFileInfo = 1		##<< integer scalar: number of lines 
	  ## before column information
	, nRowsColInfo = 2		##<< integer vector: number of lines 
	  ## with column description
	, sep = ","				    ##<< column separator
	, ...					        ##<< further arguments to \code{link{read.table}}
	, colClasses = rep(NA, ncol(colInfo))	##<< see \code{link{read.table}}
	, colsTimeStamp = 1		##<< integer vector: colums with time stamp column 
	  ## (will be set to POSIXct
	, formatTS = NULL		  ##<< format string of the timestamp columns, 
	  ## see \code{\link{strptime}}, e.g. 
	, tz = "UTC"				  ##<< specify a time zone when converting to POSIXct, 
	  ## default: current local e.g CET, UTC
	, na.strings = c('','NA','NAN','"NAN"')  ##<< see \code{link{read.table}}
){
  ##seealso<< \code{\link{read81xVar}}
  # cannot put "%" in declaration with inlinedocs
	if (!length(formatTS) ) formatTS <- "%Y-%m-%d %H:%M:%S"	
	setClass("myDate", where = globalenv())
	setAs("character","myDate", function(from) 
	  as.POSIXct(from, format = formatTS, tz = tz), where = globalenv() )
	isConnection <- inherits(fName, "connection")
	if (isConnection) {
	  con <- fName
	  ##details<< 
	  ## When providing an open connection, caller is responsible for closing it.
	  ## Closed connections are closed on exit of the function.
	  if (!isOpen(con)) {
	    on.exit(close(con))
	    open(con)
	  } 
	} else {# fName is not a connection
	  con <- file(fName,"r")
	  on.exit(close(con))
	}
	fileInfo <- readLines(con, n = nRowsFileInfo )	
	colInfo <- read.table(
	  con, header = TRUE
		#, skip = nRowsFileInfo
		, nrows = max(1,nRowsColInfo), sep = sep, na.strings = na.strings
		, stringsAsFactors = FALSE)
	colClasses[colsTimeStamp] <- "myDate"
	rawData <- read.table(con, header = FALSE
		#, skip = nRowsFileInfo+1+nRowsColInfo
		, sep = sep, na.strings = na.strings, ...
					,colClasses = colClasses)
	colnames(rawData) <- colnames(colInfo)	
	#plot( CO2_Avg ~ TIMESTAMP, data = rawData )
	attr(rawData,"fileInfo") <- fileInfo
	attr(rawData,"colInfo") <- colInfo
	as_tibble(rawData)
}
attr(readDat,"ex") <- function(){
	fName <- system.file(
	  "genData/chamberLoggerEx1_short.dat", package = "RespChamberProc")
	if (nzchar(fName)) {
		ds <- readDat(fName)
	}
	# reading first from zipped file
	fName <- system.file(
	  "genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
	if (nzchar(fName)) {
		ds <- readDat( unz(fName, filename = unzip(fName, list = TRUE)[1,"Name"] )
				,tz = "UTC")
	}
}

read81x <- function(
		### Read a Licor generated .x81 file into a data-frame		
		fName					          ##<< scalar string: file name
		, nRowsFileInfo = 23		##<< scalar integer: number of lines of initial 
		  ## file information
		, sep = "\t"				    ##<< column separator
		, ...					          ##<< further arguments to \code{\link{readDat}}
		, colsTimeStamp = 3		  ##<< integer vector: colums with time stamp column 
		  ## (will be set to POSIXct)
		, formatTS = NULL			  ##<< format of the timestamp columns, 
		  ## see \code{\link{strptime}}
		, tz = "UTC"				    ##<< specify a time zone when converting to POSIXct, 
		  ## default: UTC
		, na.strings = c('','NA','NAN','"NAN"') ##<< see \code{link{read.table}}
		, labelRowOffset = -16	##<< the row offset, usually before concentration 
		  ## measurements to generate column \code{label}
){
	##details<< 
	## version of \code{\link{readDat}} with adjusted defaults.
	## 
	## This function is deprecated because its superseeded by the 
	## more versatile \code{\link{read81xVar}}.
	#
	# find the beginning of data blocks and of summary information
	message("read81x is deprecated. Please use function read81xVar")
	if (!length(formatTS) ) formatTS <- "%Y-%m-%d %H:%M:%S"
	lines <- readLines(fName)
	blockStarts <- grep("^Type", lines)				# starts of data blocks
	# starts of summary blocks (after each data block) 
	summaryStarts <- grep("^CrvFitStatus", lines)	
	setClass("myDate", where = globalenv())
	setAs("character","myDate", function(from) 
				as.POSIXct(from, format = formatTS, tz = tz), where = globalenv() )
	fileInfo <- readLines(fName, n = nRowsFileInfo )
	colNamesFile <- unlist(read.table(fName, header = FALSE, skip = nRowsFileInfo
		, nrows = 1, sep = sep, na.strings = na.strings))
	#colNamesFile <- colNamesFile[1:(length(colNamesFile)-1)]	# skip annotation col
	colClasses = rep(NA, length(colNamesFile))	##<< see \code{link{read.table}}	
	colClasses[colsTimeStamp] <- "myDate"
	iChunk <- 1
	resBlocks <- lapply( seq_along(summaryStarts), function(iChunk){
				# read the label from above the chunk
				tmp <- scan(
				  fName, "character"
				  , skip = blockStarts[iChunk] + labelRowOffset
				  , nlines = 1, quiet = TRUE)
				label <- if (length(tmp) >= 2) tmp[2] else as.character(iChunk)
				rawData <- read.table(fName, header = FALSE
						, sep = sep, na.strings = na.strings
						, skip = blockStarts[iChunk] + 1
						, col.names = colNamesFile, fill = TRUE
						, ...
						, nrows =  summaryStarts[iChunk] - blockStarts[iChunk] - 2
						,colClasses = colClasses
				)
				cbind( iChunk = iChunk, rawData[ rawData$Type == 1,], label = label )
			})
	# warning on unequal factor levels of id and label
	res <- suppressWarnings(bind_rows( resBlocks ))		
	attr(res,"fileInfo") <- fileInfo
	as_tibble(res)
}
attr(read81x,"ex") <- function(){
	#fName <- "inst/genData/Flux2_140929_1700.81x"
	#fName <- "inst/genData/Flux2_140929_1700.81x"
	fName <- system.file(
	  "genData/Flux2_140929_1700.81x", package = "RespChamberProc")
	if (nzchar(fName)) {
		ds <- read81x(fName)
		#plot( CO2 ~ Date, ds )
		#plot( CO2 ~ Date, ds[ds$iChunk == 9,] )
	}
}

read81xVar <- function(
  ### Read a Licor generated .x81 file into a data-frame with guessing initial rows		
  fName					          ##<< scalar string: file name
  ## file information
  , sep = "\t"				    ##<< column separator
  , ...					          ##<< further arguments to \code{\link{read.table}}
  , colsTimeStamp = 3		  ##<< integer vector: colums with time stamp column 
  ## (will be set to POSIXct)
  , formatTS = NULL			  ##<< format of the timestamp columns, 
  ## see \code{\link{strptime}}
  , tz = "UTC"				    ##<< specify a time zone when converting to POSIXct, 
  ## default: UTC
  , na.strings = c('','NA','NAN','"NAN"') ##<< see \code{\link{read.table}}
  , labelID = "Label:"    ##<< string at the start of lines indicating the label
){
  ##seealso<< \code{\link{readDat}} 
  ##details<< 
  ## CAUTION: This parses a proprietary format as it was reasonably well working
  ## at the time of development of the function. The function may skip important 
  ## meta-information. Further, it may use data-rows that are not meant to be 
  ## part of the concentration fitting. 
  ## If you are more familiar with the format, please suggest improvements
  ## as an issue at the RespChamberProc repository.
  # find the beginning of data blocks and of summary information
  if (!length(formatTS) ) formatTS <- "%Y-%m-%d %H:%M:%S"
  lines <- readLines(fName)
  blockStarts0 <- grep("^Type", lines)				# starts of data blocks
  # in order to select chunks until next block, set end of the file as blockstart
  blockStarts <- c(blockStarts0, length(lines) + 1L) 
  # starts of summary blocks (after each data block) 
  #summaryStarts <- grep("^CrvFitStatus", lines)	
  setClass("myDate", where = globalenv())
  setAs("character","myDate", function(from) 
    as.POSIXct(from, format = formatTS, tz = tz), where = globalenv() )
  iChunk <- 94
  # The format may change between blocks, so
  # read the column names of each chunk
  # find the label by "Label:"
  resBlocks <- lapply( seq_along(blockStarts0), function(iChunk){
    blockStart <- blockStarts[iChunk]
    # read the label from above the info lines above the chunk
    fileInfoLines <- lines[blockStart - min(blockStart,50L):1]
    labelLines <- grep(paste0("^",labelID),fileInfoLines,value = TRUE)
    label <- if (length(labelLines)) {
      trimws(substr(labelLines[length(labelLines)], nchar(labelID) + 1, nchar(labelLines[1])))
    } else {
      as.character(iChunk)
    }
    # read the colum names
    namesLine <- lines[blockStarts[iChunk]]
    colNamesChunk0 <- unlist(read.table(
      textConnection(namesLine)
      , header = FALSE, nrows = 1, sep = sep, na.strings = na.strings
      , stringsAsFactors = FALSE))
    # workaround add some dummy columns, because there may be more data columns than column names
    colNamesChunk <- c(colNamesChunk0, paste0("dummy",1:5))
    colClasses = rep(NA, length(colNamesChunk))	##<< see \code{link{read.table}}	
    colClasses[colsTimeStamp] <- "myDate"
    #
    # determine the end of the block
    # search for the first line that starts with <ID>: with id having no tab char
    # in lines of before the next blockstart
    sumStarts <- grep("^[^\t]*:", lines[blockStart:(blockStarts[iChunk + 1L] - 1L)])
    sumStart <- if (length(sumStarts)) sumStarts[1] else blockStarts[iChunk + 1L]
    # remove a row that was not finished because of restart
    if (length(grep("The measurement was restarted", lines[blockStarts[iChunk] + sumStart - 2])))
      sumStart <- sumStart - 1
    if (sumStart <= 2) return(NULL) # only header row
    # Start by only reading the first column. If row != 1 omit row from parsing
    lines_chunk <- lines[blockStarts[iChunk] + 1L:(sumStart - 2L)]
    colClasses1 <- colClasses; colClasses1[2:length(colNamesChunk0)] <- "NULL"
    col1_data <- suppressWarnings(read.table(
      textConnection(lines_chunk)
      , header = FALSE, sep = sep, na.strings = na.strings
      , fill = TRUE
      , colClasses = colClasses1
    ))
    error_rows = col1_data[1] == -1
    rawData <- read.table(
      textConnection(lines_chunk[!error_rows])
      , header = FALSE, sep = sep, na.strings = na.strings
      , col.names = colNamesChunk
      , fill = TRUE
      , ...
      , colClasses = colClasses
    )
    cbind( iChunk = iChunk
           , rawData[rawData$Type == 1,seq_along(colNamesChunk0)]
           , label = label )
  })
  res <- suppressWarnings(bind_rows( resBlocks ))		
  as_tibble(res)
}
attr(read81xVar,"ex") <- function(){
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

.tmp.f <- function(){
  headLines <- c("something: a,  b ak","Label: myLabel","other: stuff","Type")
}


subsetContiguous <- function(
	### Get contiguous subsets 
	ds						          ##<< data.frame or tibble of measurements 
	, colTime = "TIMESTAMP"	##<< column name that of time (POSIXct)
	, colIndex = "Collar"		##<< column name of index variable (factor or integer)
	, colMeasure = "CO2_dry"##<< column name of the concentration measurement
	, gapLength = 12			  ##<< minimal length of a gap between subsets (seconds)
	, minNRec = 20				  ##<< minimum number of records within one contiguous subset
	, minTime = 60				  ##<< minimum length of time (in seconds) that 
	  ## a contiguous subsets covers
	, indexNA = 0				    ##<< value of the index column, that signifies 
	  ## records not to use
	, fIsBadChunk = function(dsi) FALSE	 ##<<
	   ## additional function taking a subset of contiguous data and returning 
	   ## a boolean value whether it shoudl be omitted
){
	##details<< 
	## The time series in logger data consists of several chunks of 
	## concentration measurments.
	## In order to determine these chunks, either a change in an index variable 
	## (input by between the measurements) or a gap in time is used.
	ds <- as_tibble(ds)
	reqCols <- c(colTime,colIndex, colMeasure)
	iMissingCols <- which(!(reqCols %in% names(ds)))
	if (length(iMissingCols) ) stop(
	  "subsetContiguous: missing columns: "
	  , paste(reqCols[iMissingCols], collapse = ","))
	timeDiffInSeconds <- diff(as.numeric(ds[[colTime]]))
	# records with missing records in time before (start a new chunk) 
	iGaps <-  which( timeDiffInSeconds > gapLength)	
	iCollarChanges <- which( diff(as.numeric(ds[[colIndex]])) != 0 )
	# start, breaks, end 
	iChunks <- c( 1, sort(union(iCollarChanges, iGaps)), nrow(ds) ) 
	##details<<
	## Between the actual series of measurements, the logger may record sparse data.
	## These chunks are indicated by value \code{indexNA} in the index column or
	## by shortness of the series. Only chunks with at least \code{minNRec} records 
	## and at least  \code{minTime} seconds are reported. Others are neglected.
	dsChunks <- as_tibble(map_df( 2:length(iChunks), function(i){
				dsia <- cbind( 
				  iChunk = i - 1
				  , filter( ds, row_number() %in% (iChunks[i - 1] + 1):(iChunks[i]) ))
				index <- dsia[[colIndex]][1] 
				dsi <- dsia[is.finite(dsia[[colMeasure]]),]
				timeSec <- as.numeric(dsi[[colTime]]) - as.numeric( dsi[[colTime]][1] )
				if (index == indexNA || nrow(dsi) < minNRec || max(timeSec) < minTime || 
				    fIsBadChunk(dsi)
				) return(NULL) else return(dsi)
			}))
	dsChunks$iChunk <- as.factor(dsChunks$iChunk)		# convert to factor
	##value<< Argument \code{ds} with between-Chunk rows omitted and an additional 
	## integer column \code{iChunk} that designates the chunk number.
	dsChunks
}

#require(testthat)
context("readDat")

test_that("x81 example",{
			fName <- system.file("genData/Flux2_140929_1700.81x", package = "RespChamberProc")
			if (nzchar(fName)) {
				#ds <- read81x(fName)
				ds <- read81xVar(fName)
				expect_true( max(ds$iChunk) > 1 )
				expect_true( table(ds$iChunk)[1] > 1 )
				expect_true( ds$label[1] == "Flux2_140929_1600" )
				#plot( CO2 ~ Date, ds )
				#plot( CO2 ~ Date, ds[ds$iChunk==2,] )
			}
		})

test_that("x81 annotation",{
			fName <- system.file("genData/testVaryingColNumber.81x", package = "RespChamberProc")
			if (nzchar(fName)) {
				#ds <- read81x(fName)
				ds <- read81xVar(fName)
				expect_true( max(ds$iChunk) > 1 )
				expect_true( table(ds$iChunk)[1] > 1 )
				expect_true( "some Comment" %in% ds$Annotation )
				#table( ds$label )
				expect_true( all( filter(ds, iChunk==1)$label == "ChunkLabel1" ))
				expect_true( all( filter(ds, iChunk==2)$label == "ChunkLabel2" ))
				#plot( CO2 ~ Date, ds )
				#plot( CO2 ~ Date, ds[ds$iChunk==9,] )
			}
		})

test_that("x81 bad cases",{
  # due to instrument errors, x81 files have sometimes been screwed up
  # collect those cases in develop directory and read them
  # to be executed from package directory
  testDir <- "develop/x81Cases"
  if (dir.exists(testDir)) {
    fNames <- file.path(testDir, dir(testDir))
    fName <- fNames[1]
    for (fName in fNames) {
      message(fName)
      ds <- read81xVar(fName)
    }
  }
})


test_that("zipped file",{
  fName <- system.file(
  "genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  nConn <- nrow(showConnections())
  con <- unz(fName, filename = unzip(fName, list = TRUE)[1,"Name"] )
  ds0 <- readDat(con, tz = "UTC")
  expect_true( nrow(ds0) > 0 )
  expect_error( isOpen(con) ) # invalid connection, because closed
  # number of open connections did not change
  #expect_equal( nConn, nrow(showConnections())) # can fail because parallel
})

test_that("zipped file with explicit opening connection",{
  fName <- system.file(
    "genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  con <- unz(fName, filename = unzip(fName, list = TRUE)[1,"Name"] )
  open(con) # when explicitly opening, then also need to close connection
  ds0 <- readDat(con, tz = "UTC")
  expect_true( nrow(ds0) > 0 )
  expect_true( isOpen(con) ) 
  close(con)
})

test_that("reading dat file",{
  fName <- system.file(
    "genData/chamberLoggerEx1_short.dat", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  nConn <- nrow(showConnections())
  ds0 <- readDat(fName, tz = "UTC")
  expect_true( nrow(ds0) > 0 )
  # number of open connections did not change
  expect_equal( nConn, nrow(showConnections())) 
})

test_that("issue #5",{
  #fName <- file.path("develop","x81Cases","2312_issue005_small.81x")
  fName <- system.file(
    "genData/2312_issue005_small.81x", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  ds <- read81xVar(fName)
  expect_true( max(ds$iChunk) > 1 )
  expect_true( table(ds$iChunk)[1] > 1 )
})


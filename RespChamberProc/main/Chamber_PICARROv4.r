##_____________________________________________________________________________________

###                             CHAMBER MEASUREMENTS WITH PICARRO
##_____________________________________________________________________________________


# install.packages("devtools")
# devtools::install_github("bgctw/RespChamberProc")
# 
# pck <- c("rlang", "changepoint", "nlme", "segmented", "tibble",  "dplyr", "purrr", "RespChamberProc")
#           
# new_pck <-pck[!pck %in% installed.packages()[, "Package"]]
#           if(length(new_pck)){install.packages(new_pck)}
#           
# sapply(pck, require, character.only=TRUE)
# 
# 
# 
# 
# ## Set working directory
setwd("/Users/ms/Library/CloudStorage/OneDrive-SharedLibraries-UniversidaddeCórdoba/Chamber - Chamber")


#-------------------------------------------------------------------------------------
##                      Preparing the Chamber data
##-------------------------------------------------------------------------------------

## 1. The .dat file is downloaded from the chamber logger after the field campaign. 

library("rlang")
library("changepoint")
library("nlme")
library("segmented")
library("tibble")
library("plyr")
library("dplyr")
library("purrr")
library("RespChamberProc")
library("devtools")
library("stringr")
library("ggplot2")

rel_pathname <- "Data_Jesus/" #"Data_Adrian/Agosto/19/"
fileName <- "JFAADS2174-20220531-080209-DataLog_User.dat"
  
#create folder with the name of the measurement archive to save results and plots therein
dir.create(paste0(rel_pathname,"results"))

results_dir <- paste0(rel_pathname,"results/",str_sub(fileName,end=-18))
dir.create(results_dir)


library(data.table)
ds0 <- fread(paste0(rel_pathname,fileName), sep ="auto")

library("lubridate") 
ds0$TIMESTAMP <- as.POSIXct(paste0(ds0$DATE," ",ds0$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")

## The logger is accumulating data from previous field campaigns. 
#  Here we subset data from a given field campaign. (Entrar hora del inicio de observationes y fin (convert to UTC))
ds <- subset(ds0, as.numeric(TIMESTAMP) >= as.numeric(as.POSIXctUTC("2022-05-01 00:00:00")) )
ds <- subset(ds, as.numeric(TIMESTAMP) <= as.numeric(as.POSIXctUTC("2025-04-15 23:00:00" )) )


##-- 1. Correcting for gas density and units conversions

## load function to automatically get elevation (AWS Mapzen elevation tiles: https://registry.opendata.aws/terrain-tiles/) 
## T and atmospheric pressure (data: https://open-meteo.com/en/docs/historical-weather-api) for a sampling location (to be entered in decimal degrees below)
source("functions/getAdditionalWeatherVariables.R")
Additional_Weather_Data <- getAdditionalWeatherVariables(37.915, -4.724,format(min(ds$TIMESTAMP),"%Y-%m-%d"),format(max(ds$TIMESTAMP),"%Y-%m-%d"))


# For the case of PICARRO IRGA gives dry mole fractions for CO2, N2O, CH4, but not for NH3 and H2O 
ds$solenoid_valvesInt <- ds$solenoid_valves %>% as.integer()
ds$H2Oppt <- ds$H2O*10 # H2O from PICARRO is in %.Needs to be in ppt --> We need to multiply by 10 
ds$N2O_dry <- ds$N2O_dry1min
ds$NH3_dry <- 10E-3*corrConcDilution(ds, colConc = "NH3", colVapour = "H2Oppt")  #NH3 from PICARRO is in ppb --> multiply colVapour by 10^-3 to get ppm
ds$H2O_dry <- corrConcDilution(ds, colConc = "H2Oppt", colVapour = "H2Oppt")

## (h, Pa, and AirTemp are obtained directly from freely available Meteo and Elevation data for given coordinates during the measurement time interval with the "getElevationAndAtmosphericPressure" script (see details therein). If desired, it can also be set here manually)
# ds$h = 106 # Córdoba elevation (m above sea level)
# ds$Pa <- 101325*(1 - 2.25577*10^-5*h)^5.25588   # (Pa)
# ds$AirTemp <- 25 #(degrees Celsius)

ds$VPD <- calcVPD( ds$AirTemp, ds$Pa, ds$H2Oppt) ## Here we calculate plant-to-air vapour pressure deficit


#--Extract duplicated rows
ds <- ds[!duplicated(ds$TIMESTAMP),]

#-- In order to process each measurement cycle independently, 
#-- we first determine parts of the time series that are contiguous, 
#-- i.e. without gaps and without change of an index variable, here variable collar.
Collar_df <- tibble("TIMESTAMP"=ds$TIMESTAMP,"Collar"=ds$solenoid_valvesInt)

# interpolate measurement timestamps for whole dataset
##create  continuous timestamp vector (here: interval=1 second)
regular_timesteps <- seq(min(ds$TIMESTAMP),max(ds$TIMESTAMP), by="1 sec")

## load function to interpolate measurement data to regular timesteps
source("functions/convertToRegularTimesteps.R")

library(tidyr)
ds <-convertToRegularTimesteps(ds,c("CO2_dry", "CH4_dry","H2Oppt", "NH3_dry","N2O_dry"),regular_timesteps)
ds <- left_join(ds,Collar_df,by=join_by("TIMESTAMP"=="TIMESTAMP")) %>% fill(.,Collar,.direction="down")

#join Temp_PressureData with ds  (approx does not work for more following NA's, just fill nas with same value as last one (function "fill" from tidyr package))
ds <- left_join(ds,Additional_Weather_Data ,by=join_by("TIMESTAMP"=="DATETIME_hourly")) %>% fill(.,h,AirTemp,Pa,rel_humidity,shortwave_radiation,ET0, .direction = "updown")


## get an overview of the data (for the whole subset)
plot(ds$TIMESTAMP, ds$H2Oppt, pch = ".", ylab = "H2O (ppt)", xlab = "Time")
plot(ds$TIMESTAMP,ds$CO2_dry, pch = ".", ylab = "CO2 (ppm)", xlab = "Time")
plot(ds$TIMESTAMP,ds$CH4_dry, pch = ".", ylab = "CH4 (ppm)", xlab = "Time")
plot(ds$TIMESTAMP,ds$NH3_dry, pch = ".")
plot(ds$TIMESTAMP,ds$N2O_dry, pch = ".")
plot(ds$TIMESTAMP,ds$AirTemp, pch = ".")
plot(ds$TIMESTAMP,ds$Pa, pch = ".")
plot(ds$TIMESTAMP,ds$rel_humidity, pch = ".")
plot(ds$TIMESTAMP,ds$shortwave_radiation, pch = ".")
plot(ds$TIMESTAMP,ds$ET0, pch = ".") #ET0 FAO Evapotranspiration (mm)

plot(ds$TIMESTAMP,ds$Collar, pch = ".", ylab = "Collar (Chamber)",xlab = "Time")

##play around with gaplength, minrec, min time

dsChunk <- subsetContiguous(ds, colTime = "TIMESTAMP", colIndex = "Collar",
                            gapLength = 1, minNRec = 180, minTime = 180, indexNA = 13) #indexNA excludes selected index columns (here: collar)
head(dsChunk)

#-- plot the time series
## load function to generate plots for each chunk for various gases
source("functions/ChunkPlots.R")

# Generate labels for each gas
labels_CO2 <- chunk_labels(dsChunk, CO2_dry, 1.05)
labels_H2O <- chunk_labels(dsChunk, H2Oppt, 1.05)
labels_CH4 <- chunk_labels(dsChunk, CH4_dry, 1.05)
labels_N2O <- chunk_labels(dsChunk, N2O_dry, 1.05)
labels_NH3 <- chunk_labels(dsChunk, NH3_dry, 1.05)

# Generate plots for each gas
p_CO2 <- chunk_plot(dsChunk, CO2_dry, labels_CO2, y_CO2)
p_H2O <- chunk_plot(dsChunk, H2Oppt, labels_H2O, y_H2O)
p_CH4 <- chunk_plot(dsChunk, CH4_dry, labels_CH4, y_CH4)
p_N2O <- chunk_plot(dsChunk, N2O_dry, labels_N2O, y_N2O)
p_NH3 <- chunk_plot(dsChunk, NH3_dry, labels_NH3, y_NH3)

##save the plots
for (p in c("p_CO2","p_H2O","p_CH4","p_N2O","p_NH3")) {
  ggsave(filename=paste0(results_dir,"/",str_sub(fileName,end=-18),"_allchunks_",p,".pdf"),get(p),width = 40,height = 30,units = "cm")
}


## flag bad chunks (based on manual inspection of facet plots 1=good, 0=bad) --> maybe we can automatize this? QUESTION: WHAT IS A "GOOD" chunk? (criteria to automatize). ALTERNATIVE: WE ASSUME ALL CHUNKS AS GOOD AT THIS STAGE AND WILL REJECT CHUNKS WHERE SD > MEAN IN "res" object.
# goodChunks <- c(1,2,6,11,15,20,24,28,34,39,45,53,58,64,69,73,82,84,88,92,95,98,103,106,110,114,118,123,125,128,133,141,145,149,153,157,162,166,169,174,177,181,187,191,195,203,206)
# dsChunk$flag <- ifelse(dsChunk$iChunk %in% goodChunks,1,0)

#select only good chunks
# dsChunk <- dsChunk %>% filter(.,flag==1)

##select just one chunk
selected_chunk=4

df <- dsChunk[dsChunk$iChunk==selected_chunk,]
plot(df$TIMESTAMP, df$CO2_dry)



##  Computing the flux

resFit <- calcClosedChamberFlux(df
                                , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                , colConc = "CO2_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
                                , colTemp = "AirTemp", colPressure = "Pa"		#Temp in degC, Pressure in Pa
                                , volume = 0.4*0.4*0.4, area = 0.4*0.4			# chamber dimensions m3 and m2
                                , minTLag = 60,  maxLag = 150 ,concSensitivity = 0.01	
)

plotResp(df, resFit, label = paste("Chunk",selected_chunk,sep = " "))	

resH2OFit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                   , colConc = "H2Oppt", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"	
                                   , volume = 0.4*0.4*0.4, area = 0.4*0.4				
                                   , minTLag = 60,  maxLag = 150 ,concSensitivity = 0.01	
)

plotResp(df, resH2OFit,colConc = "H2Oppt",ylab="H2O (ppt)", label = paste("Chunk",selected_chunk,sep = " "))	

resCH4Fit <- calcClosedChamberFlux(df
                                , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                , colConc = "CH4_dry", colTime = "TIMESTAMP"
                                , colTemp = "AirTemp", colPressure = "Pa"
                                , volume = 0.4*0.4*0.4, area = 0.4*0.4	
                                , minTLag = 60,  maxLag = 150 ,concSensitivity = 0.01	
)

plotResp(df, resCH4Fit,colConc = "CH4_dry",ylab="CH4_dry (ppm)", label = paste("Chunk",selected_chunk,sep = " "))	

resNH3Fit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                   , colConc = "NH3_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = 0.4*0.4*0.4, area = 0.4*0.4	
                                   , minTLag = 60,  maxLag = 150 ,concSensitivity = 0.01	
)

  plotResp(df, resNH3Fit,colConc = "NH3_dry",ylab="NH3_dry (ppm)", label = paste("Chunk",selected_chunk,sep = " "))	

resN2OFit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                   , colConc = "N2O_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = 0.4*0.4*0.4, area = 0.4*0.4	
                                   , minTLag = 60,  maxLag = 150 , concSensitivity = 0.01	
)

plotResp(df, resN2OFit,colConc = "N2O_dry",ylab="N2O_dry (ppm)", label = paste("Chunk",selected_chunk,sep = " "))	



##---------------------------------------------------------------------------

#--   3. Computing the fluxes in a field campaign
# -- Function calcClosedChamberFluxForChunks helps you with subsetting the data 
# -- and applying function calcClosedChamberFlux to each subset.


chamberVol = 0.4*0.4*0.4
surfaceArea = 0.4*0.4


library(plyr)
library(doSNOW)
nNode = 8	# number of processors
cl = makeCluster(nNode)		
registerDoSNOW(cl)
clusterEvalQ(cl, library(RespChamberProc))		# functions need to be loaded on remote hosts

system.time(res <- ddply(dsChunk, .(iChunk), function(dsi){
  collar <- dsi$Collar[1] 
  iChunk = dsi$iChunk[1]
  print( paste(iChunk, dsi$TIMESTAMP[1], " Collar: ",collar) ) 
  
  res <- calcClosedChamberFluxForChunks(
    dsi, fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)	
    #, debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "CO2_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
    , colTemp = "AirTemp", colPressure = "Pa"		# Temperature in K  (IS THAT TRUE IN K, OR DO WE NEED °C, AS STATED IN https://github.com/bgctw/RespChamberProc/blob/master/R/fluxEstimates.R), Pressure in Pa
    , volume = chamberVol, area = surfaceArea					    # chamber dimensions m3 and m2
    , minTLag = 60,  maxLag = 150 , concSensitivity = 0.01	
  )
  
  resH2O <- calcClosedChamberFluxForChunks(
    dsi, fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)	
    #, debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "H2Oppt", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
    , colTemp = "AirTemp", colPressure = "Pa"		
    , volume = chamberVol, area = surfaceArea	
    , minTLag = 60,  maxLag = 150 , concSensitivity = 0.01	
  )

  resCH4 <- calcClosedChamberFluxForChunks(
    dsi, fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)	
    #, debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "CH4_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
    , colTemp = "AirTemp", colPressure = "Pa"		
    , volume = chamberVol, area = surfaceArea	
    , minTLag = 60,  maxLag = 150 , concSensitivity = 0.01	
  )

  resNH3 <- calcClosedChamberFluxForChunks(
    dsi, fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)	
    #, debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "NH3_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
    , colTemp = "AirTemp", colPressure = "Pa"		
    , volume = chamberVol, area = surfaceArea	
    , minTLag = 60,  maxLag = 150 , concSensitivity = 0.01	
  )

  resN2O <- calcClosedChamberFluxForChunks(
    dsi, fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)	
    #, debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "N2O_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
    , colTemp = "AirTemp", colPressure = "Pa"		
    , volume = chamberVol, area = surfaceArea	
    , minTLag = 60,  maxLag = 150 , concSensitivity = 0.01	
  )
  
  
  dsi$device
  #   get additional environmental variables at the initial time
  to <- res$tLag
  to <- ifelse(to == 0, 1, to)
  dsiInitial <- dsi[ to, , drop=FALSE]
  cbind( data.frame( time=dsiInitial[,"TIMESTAMP"], collar=collar
                     , CO2_flux=res$fluxMedian, CO2_flux_sd=res$sdFlux, Fregress_CO2=res$iFRegress, r2_CO2=res$r2
                     , H2O_flux=resH2O$fluxMedian , H2O_flux_sd=resH2O$sdFlux, Fregress_H2O=resH2O$iFRegress, r2_H2O=resH2O$r2
                     , CH4_flux=resCH4$fluxMedian, CH4_flux_sd=resCH4$sdFlux, Fregress_CH4=resCH4$iFRegress, r2_CH4=resCH4$r2
                     , NH3_flux=resNH3$fluxMedian, NH3_flux_sd=resNH3$sdFlux, Fregress_NH3=resNH3$iFRegress, r2_NH3=resNH3$r2
                     , N2O_flux=resN2O$fluxMedian , N2O_flux_sd=resN2O$sdFlux, Fregress_N2O=resN2O$iFRegress, r2_N2O=resN2O$r2
  )
  , dsiInitial[,c("CO2_dry","CH4_dry","NH3_dry","N2O_dry", "AirTemp","Pa")] )
  
}
))

aver <- calcClosedChamberFluxForChunks(
  dsChunk, fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)	
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "CO2_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , colTemp = "AirTemp", colPressure = "Pa"	
  , volume = chamberVol, area = surfaceArea					    # chamber dimensions m3 and m2
  , minTLag = 200,  maxLag = 250 , concSensitivity = 0.1
)


#Plot the results including the fits as facet plots
CO2_fit_facets <- plotCampaignConcSeries(dsChunk,aver, plotsPerPage = 64L)
print( CO2_fit_facets$plot[[1]])

#flag chunks that are categorized as bad (i.e. if sd>mean, flag==0 (bad); else flag == 1). TO DISCUSS: So far only implemented for CO2. FOR EACH GAS, OR IS THERE A GAS THAT MOSTLY WORKS? we should do this step earlier to save computation time
res$flag <-  ifelse(abs(aver$fluxMedian) > abs(aver$sdFlux) & res$r2_CO2 > 0.8 ,1,0)

#select only good chunks
onlygoodresults<- res %>% filter(.,flag==1)

#save results file
save(res, file=paste0(results_dir,"/",str_sub(fileName,end=-18),".Rda"))







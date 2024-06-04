#=======function to automatically get the elevation and various weather variables from open-meteo.com
# These values include Temperature, atmospheric pressure, relative humidity, short wave radiation, and Evapotranspiration at a selected timestep for given geographic coordinates
# This may be useful if these values have not been collected
# Please note that we can only get weather variables that are more than 5 days old


## citation of the data sources:
#- Zippenfenig, P. (2023). Open-Meteo.com Weather API [Computer software]. Zenodo. https://doi.org/10.5281/ZENODO.7970649
#- Hersbach, H., Bell, B., Berrisford, P., Biavati, G., Horányi, A., Muñoz Sabater, J., Nicolas, J., Peubey, C., Radu, R., Rozum, I., Schepers, D., Simmons, A., Soci, C., Dee, D., Thépaut, J-N. (2023). ERA5 hourly data on single levels from 1940 to present [Data set]. ECMWF. https://doi.org/10.24381/cds.adbb2d47
#- Muñoz Sabater, J. (2019). ERA5-Land hourly data from 2001 to present [Data set]. ECMWF. https://doi.org/10.24381/CDS.E2161BAC
#- Schimanke S., Ridal M., Le Moigne P., Berggren L., Undén P., Randriamampianina R., Andrea U., Bazile E., Bertelsen A., Brousseau P., Dahlgren P., Edvinsson L., El Said A., Glinton M., Hopsch S., Isaksson L., Mladek R., Olsson E., Verrelle A., Wang Z.Q. (2021). CERRA sub-daily regional reanalysis data for Europe on single levels from 1984 to present [Data set]. ECMWF. https://doi.org/10.24381/CDS.622A565A


#load libraries
library(elevatr)
library(sf)
library(openmeteo) #retrieves historical weather data from https://open-meteo.com/en/docs/historical-weather-api


#start- and endtime needs to be in ""%Y-%m-%d" format, e.g. "2022-05-01"
getAdditionalWeatherVariables <- function(latDeciDeg,lonDeciDeg,starttime,endtime){
  ## Create a data frame with the coordinates (Geographical Coordinates (decimal degrees); crs =4326)
  sampling_location<- data.frame(
    lat = latDeciDeg, #37.91514875822371
    lon = lonDeciDeg #-4.72405536319233,
  )
}
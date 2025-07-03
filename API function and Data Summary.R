#Inital R code and data summary 

#Current Packages used
#install.packages('tidycensus')
#install.packages('jsonlite')
#install.packages("lubridate")
library(tidycensus)
library(jsonlite)
library(tidyverse)
library(httr)
library(lubridate)

api_key <-a2a0e6a4de5aa1eed7f2d631ad8848ff


#Function to obtain lat and log
get_lat_lon <- function(city="Philadelphia"){
  #Query API using the City 
  baseURL <-"http://api.openweathermap.org/geo/1.0/direct?"
  endpoint <-paste0("q=",city,"&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
  URL_ids <- paste0(baseURL,endpoint)
  id_info <-httr::GET(URL_ids)
  str(id_info, max.level = 1)
  parsed <- fromJSON(rawToChar(id_info$content))
  print(id_info)
  print(parsed)
  
  #Saving the lat and lon and returning them into a list 
  lat <-parsed$lat
  lon <-parsed$lon
  return(list=c(city=city,lat=lat,lon=lon))
}

list_coordinates <- get_lat_lon()

#Build function to query the API for daily temperatures 
get_dailytemperatures <-function(lat,lon,units="imperial"){
  
  #Query API for daily weather using lat and lon
  baseURL <-"https://api.openweathermap.org/data/3.0/onecall?"
  endpoint <-paste0("lat=",lat,"&lon=",lon,"&units=",units,
                    "&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
  URL_ids <- paste0(baseURL,endpoint)
  id_info <-httr::GET(URL_ids)
  str(id_info, max.level = 1)
  parsed <- fromJSON(rawToChar(id_info$content))
  
  #Create dataframe out of daily weather and clean it to make usable
  df_daily <- df_parsed$daily |> 
    mutate(date=as.POSIXct(dt,format = "%Y-%m-%d")) 
  
  #Create data frame out of hourly weather 
  df_hourly <- df_parsed$hourly |> 
    mutate(date=as.POSIXct(dt,format = "%Y-%m-%d")) 
  
  return(list(df1 =df_daily, df2=df_hourly))
}

result <-get_dailytemperatures(list_coordinates[2],list_coordinates[3])

df_daily <-result[1]
df_hourly <- result[2]

#Get historical weather
get_historicaltemperatures <- function(lat,lon,units="imperial",date="2025-07-03"){
  #Query API for historical weather using lat and lon
  baseURL <-"https://api.openweathermap.org/data/3.0/onecall/day_summary?"
  endpoint <-paste0("lat=",lat,"&lon=",lon,"&date=",date,
                    "&units=",units,
                    "&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
  URL_ids <- paste0(baseURL,endpoint)
  id_info <-httr::GET(URL_ids)
  str(id_info, max.level = 1)
  parsed <- fromJSON(rawToChar(id_info$content))
  return(parsed)
  #Creating a clean data frame 
}
historical_data <- get_historicaltemperatures(list_coordinates[2],list_coordinates[3])

#Create some contingency tables 




#Create numerical summaries for some quantitative variables

#Create at least four plots utlizing coloring and grouping 

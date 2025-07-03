#Inital R code and data summary 

#Current Packages used
#install.packages('tidycensus')
#install.packages('jsonlite')
library(tidycensus)
library(jsonlite)
library(tidyverse)
library(httr)

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
  lat <-parsed$lat
  lon <-parsed$lon
  return(list=c(city=city,lat=lat,lon=lon))
}

list_coordinates <- get_lat_lon()


#Build function to query the API for daily weather
get_dailyweather <-function(lat,lon,units="imperial"){
  #Query API for daily weather using lat and lon
  baseURL <-"https://api.openweathermap.org/data/3.0/onecall?"
  endpoint <-paste0("?lat=",lat,"&lon=",lon,"&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
  URL_ids <- paste0(baseURL,endpoint)
  id_info <-httr::GET(URL_ids)
  str(id_info, max.level = 1)
  parsed <- fromJSON(rawToChar(id_info$content))

  #Create dataframe out of daily weather 
  
  
  return()
}

URL_ids <- "https://api.openweathermap.org/data/3.0/onecall?&units=imperial&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff"
id_info <-httr::GET(URL_ids)
str(id_info, max.level = 1)
parsed <- fromJSON(rawToChar(id_info$content))


#Build function to query the API for previous date weather weather
URL_ids <- "https://api.openweathermap.org/data/3.0/onecall/day_summary?lat=39.9526&lon=-75.1652&date=2023-07-02&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff"
id_info <-httr::GET(URL_ids)
str(id_info, max.level = 1)
parsed <- fromJSON(rawToChar(id_info$content))

#Function to obtain lat and log



#Create some contingency tables 

#Create numerical summaries for some quantitative variables

#Create at least four plots utlizing coloring and grouping 

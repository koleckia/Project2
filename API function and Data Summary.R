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
library(ggplot2)


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

#Build function to query the API for daily weather
get_dailyweather <-function(lat,lon,units="imperial"){
  
  #Query API for daily weather using lat and lon
  baseURL <-"https://api.openweathermap.org/data/3.0/onecall?"
  endpoint <-paste0("lat=",lat,"&lon=",lon,"&units=",units,
                    "&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
  URL_ids <- paste0(baseURL,endpoint)
  id_info <-httr::GET(URL_ids)
  str(id_info, max.level = 1)
  df_parsed <- fromJSON(rawToChar(id_info$content))
  
  #Create data frame for plots 
  df_dailyplots <- df_parsed$daily |> 
    mutate(date=as.POSIXct(dt,format = "%Y-%m-%d")) 
  
  df_dailyplots$date <-as.Date(df_dailyplots$date)
  df_dailyplots <- as.data.frame(df_dailyplots)

  return(df_dailyplots)
}

df_daily_plots <-get_dailyweather(list_coordinates[2],list_coordinates[3])


#Modify code to get data for entire week from a year ago 
get_historicaltemperatures <- function(lat,lon,units="imperial"){
  #Create vector to have the dates from the week last year 
  today_date <- Sys.Date()
  year_ago <- today_date - 365
  week_year_ago <-as.Date(rep(NA,7))
  
  for(i in 0:6){
    week_year_ago[i+1] <-year_ago + i
  }
  
  #Query API for all dates 
  results <- list()
  for(i in 1:length(week_year_ago)){
    
  baseURL <-"https://api.openweathermap.org/data/3.0/onecall/day_summary?"
  endpoint <-paste0("lat=",lat,"&lon=",lon,"&date=",week_year_ago[i],
                    "&units=",units,
                    "&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
  URL_ids <- paste0(baseURL,endpoint)
  id_info <-httr::GET(URL_ids)
  str(id_info, max.level = 1)
  parsed <- fromJSON(rawToChar(id_info$content))
  results[[i]]<-parsed
}
  print(results)
}
historical_data <- get_historicaltemperatures(lat= list_coordinates[2],lon= list_coordinates[3])


#Line Chart: Compares daily temperatures over 7 days at the different times of the day
#Using df_daily_plots
#Reformat data for the plot 
df_dailytempplot <- df_daily_plots |>
      select(date,starts_with("temp")) |>
      unnest(temp) |>
      pivot_longer(cols = c("day","night","eve","morn"),
                   names_to= "time_of_day",
                   values_to = "temperature") |>
      select(-max,-min)

#Create Plot 
ggplot(df_dailytempplot , aes(x = date, y = temperature, color = time_of_day)) +
  geom_line(size = 1.2) +
  labs(title = "Daily Temperatures by Time Of Day",
       x = "Date",
       y = "Temperature",
       color = "Time of Day") +
  theme_minimal() 

#Bar Chart: Looks at precipitation levels over the next 7 days
#Reformat Data for the precipitation plot 
df_dailyprecipitationplot <- df_daily_plots |>
  select(date,rain) |>
  mutate(rain =ifelse(is.na(rain),0,rain))


#Create Plot 
ggplot(df_dailyprecipitationplot, aes(x = date, y = rain)) +
  geom_bar(stat = "identity") +
  labs(title = "Daily Precipitation",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_minimal()

#Radar: Looks at the day temperature, humidity and wind over 7 days 
#Reformat the Radar data 
df_dailyradarplot<- df_daily_plots |>
  unnest(temp) |>
  select(date,day,humidity,wind_speed) 


#Inital R code and data summary 

#Current Packages used
#install.packages('tidycensus')
#install.packages('jsonlite')
#install.packages("lubridate")

install.packages("purr")
library(tidycensus)
library(jsonlite)
library(tidyverse)
library(httr)
library(lubridate)
library(ggplot2)
library(purrr)
library(ggplot2)
library(tidyr)
library(dplyr)
install.packages("ggradar")


#API Functions 
#Function to obtain lat and log
get_lat_lon <- function(city="Philadelphia"){
  #Query API using the City 
  baseURL <-"http://api.openweathermap.org/geo/1.0/direct?"
  endpoint <-paste0("q=",city,"&appid=281a260e2af06dbb7c75a3ceed72b558")
  URL_ids <- paste0(baseURL,endpoint)
  id_info <-httr::GET(URL_ids)
  str(id_info, max.level = 1)
  parsed <- fromJSON(rawToChar(id_info$content))
  print(id_info)
  print(parsed)
  
  #Saving the lat and lon and returning them into a list 
  lat <-parsed$lat
  lon <-parsed$lon
  return(list(city=city,lat=lat,lon=lon))
}

get_dailyweather <- function(lat, lon, units = "imperial") {
  baseURL <- "https://api.openweathermap.org/data/3.0/onecall?"
  endpoint <- paste0("lat=", lat, "&lon=", lon, "&units=", units,
                     "&appid=281a260e2af06dbb7c75a3ceed72b558")
  URL_ids <- paste0(baseURL, endpoint)
  id_info <- httr::GET(URL_ids)
  
  if (id_info$status_code != 200) {
    stop("API request failed: ", id_info$status_code)
  }
  
  df_parsed <- fromJSON(rawToChar(id_info$content))
  
  if (is.null(df_parsed$daily)) {
    stop("No 'daily' weather data returned for this location.")
  }
  
  df_dailyplots <- df_parsed$daily |>
    mutate(date = as.Date(as.POSIXct(dt, origin = "1970-01-01", tz = "UTC"))) |>
    unnest(c(temp, feels_like ,weather),names_sep ="_")|>
    select(-dt,-sunrise,-sunset,-moonrise,-moonset,-moon_phase,-weather_icon,-weather_id) |>
    rename(temperature_min = temp_min,
           temperature_max = temp_max,
           temperature_night = temp_night,
           temperature_evening = temp_eve,
           temperature_morning = temp_morn,
           temperature_day =temp_day)
  
  df_dailyplots <- as.data.frame(df_dailyplots)
  
  return(df_dailyplots)
}




get_historicaldata <- function(lat,lon,units="imperial"){
  #Create vector to have the dates from the week last year 
  today_date <- Sys.Date()
  year_ago <- today_date - 365
  week_year_ago <-as.Date(rep(NA,7))
  
  for(i in 0:6){
    week_year_ago[i+1] <-year_ago + i
  }
  
  #Convert to work in API 
  week_year_ago_timestamp<-as.numeric(as.POSIXct(paste0(week_year_ago, "17:00:00"), tz ="UTC"))
  
  #Query first API for daily temperatures
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
  
  #Turn temperatures list into a dataframe 
  df_temperatures <-as.data.frame(do.call(rbind,results))
  df_temperatures <- df_temperatures |>
    unnest_wider(c(temperature),names_sep ="_" ) |>
    unnest("date")|>
    mutate(date=as.Date(date)) |>
    select("date",starts_with("temperature"))
  
  #Query API for rest of weather data
  list_results <- list()
  for(i in 1:length(week_year_ago)){
    baseURL <-"https://api.openweathermap.org/data/3.0/onecall/timemachine?"
    endpoint <-paste0("lat=",lat,"&lon=",lon,"&dt=",week_year_ago_timestamp [i],
                      "&units=",units,
                      "&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
    URL_ids <- paste0(baseURL,endpoint)
    id_info <-httr::GET(URL_ids)
    str(id_info, max.level = 1)
    parsed <- fromJSON(rawToChar(id_info$content))
    list_results[[i]]<-parsed
  }
  #Turn list into a dataframe 
  df_results <-as.data.frame(do.call(rbind,list_results))
  
  df_results <- df_results|>
    unnest(data) |>
    unnest(weather,names_sep = "_") |>
    mutate(date = as.Date(as.POSIXct(dt, origin = "1970-01-01", tz = "UTC"))) |>
    select(-temp,-dt,-sunrise,-sunset,-weather_icon,-weather_id)
  
  df_combined <- left_join(df_results,df_temperatures,by="date")
  
  return(df_combined)
}


merged_data <- function(df1,df2){
  df_merged <-bind_rows(df1,df2)
  return(df_merged)
}



df_long <- merged_plot |>
  pivot_longer(cols = c("Feels Like", "temperature"), names_to = "variable", values_to = "value")

#Line Chart: Compares daily temperatures over 7 days at the different times of the day
#Using df_daily_plots
#Reformat data for the plot 
ggplot(df_long, aes(x = date, y = value, color = time_of_day)) +
  geom_line(size = 1.1) +
  labs(
    title = "Comparing Weekly Real Feels to Actual Temperature",
    x = "Date",
    y = "Temperature (°)",  # or customize with y_axis if you're defining it
    color = "Time of Day"
  ) +
  facet_wrap(~ variable, scales = "free_y") +  # use scales="fixed" if you want same y-axis
  theme_minimal()


df_feels_like<- df1 |>
  filter(format(as.Date(date), "%Y") == "2025") |>
  select(date,starts_with("feels_like_")) |>
  pivot_longer(cols = starts_with("feels_like_"),
               names_to= "time_of_day",
               values_to = "Feels Like") |>
  mutate(time_of_day = sub("^feels_like_", "", time_of_day))|>
  mutate(time_of_day = ifelse(time_of_day =="morn","morning",
                              ifelse(time_of_day=="eve","evening",time_of_day)))




#Create Plot 
ggplot(df_temperature , aes(x = date, y = temperature, color = time_of_day)) +
  geom_line(size = 1.2) +
  labs(title = "Daily Temperatures by Time Of Day",
       x = "Date",
       y = "Temperature",
       color = "Time of Day") +
  theme_minimal() 


#Bar Chart: Looks at precipitation levels over the next 7 days
#Reformat Data for the precipitation plot 
df_dailyprecipitationplot <- weather_data |>
  filter(format(as.Date(date), "%Y") == "2025") |>
  select(date,rain) |>
  mutate(rain =ifelse(is.na(rain),0,rain))

df_plot <- weather_data |>
  filter(format(as.Date(date), "%Y") == "2025") |>
  select(date,rain,wind_speed,humidity) |>
  mutate(rain =ifelse(is.na(rain),0,rain))


#Create Plot 
ggplot(df_plot, aes(x = date, y = wind_speed)) +
  geom_bar(stat = "identity") +
  labs(title = "Daily Precipitation",
       x = "Date",
       y = "Precipitation (mm)") + 
  theme_minimal()

#Make heat map of weather conditions 
df_heat <- weather_data |>
  filter(format(as.Date(date), "%Y") == "2025") |>
  select(date, temperature_day, temperature_night, humidity, rain, wind_speed)|>
  pivot_longer(cols = -date, names_to = "variable", values_to = "value")

ggplot(df_heat, aes(x = date, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Weather Variable Heatmap Over Time", x = "Date", y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Create Weather Data 

weather_data_coords <- get_lat_lon("Portland")
weather_data <-get_dailyweather(weather_data_coords[2],weather_data_coords[3],)
weather_data2 <-get_historicaldata(weather_data_coords[2],weather_data_coords[3],)
weather_data <- merged_data(weather_data,weather_data2)

df_temperature <- weather_data |>
  filter(format(as.Date(date), "%Y") == "2025") |>
  select(date,starts_with("temperature_"),-temperature_min, -temperature_max, -temperature_afternoon) |>
  pivot_longer(cols = starts_with("temperature_"),
               names_to= "time_of_day",
               values_to = "temperature") |>
  mutate(time_of_day = sub("^temperature_", "", time_of_day))

df_feels_like <- weather_data |>
  filter(format(as.Date(date), "%Y") == "2025") |>
  select(date,starts_with("feels_like_")) |>
  pivot_longer(cols = starts_with("feels_like_"),
               names_to= "time_of_day",
               values_to = "Feels Like") |>
  mutate(time_of_day = sub("^feels_like_", "", time_of_day))|>
  mutate(time_of_day = ifelse(time_of_day =="morn","morning",
                              ifelse(time_of_day=="eve","evening",time_of_day)))

merged_plot <- full_join(df_feels_like,df_temperature,by=c("date","time_of_day"))

df_long <- merged_plot |>
  pivot_longer(cols = c("Feels Like", "temperature"), names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = date, y = value, color = time_of_day)) +
  geom_line(size = 1.1) +
  labs(
    title = "Comparing Weekly Real Feels to Actual Temperature",
    x = "Date",
    y = "Temperature (°)",  
    color = "Time of Day"
  ) +
  facet_wrap(~ variable, scales = "free_y") +  
  theme_minimal()


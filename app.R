#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(tidycensus)
library(jsonlite)
library(tidyverse)
library(httr)
library(lubridate)
library(ggplot2)
library(shinydashboard)

#API Functions 
#Function to obtain lat and log of any city 
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
  return(list(city=city,lat=lat,lon=lon))
}

get_dailyweather <- function(lat, lon, units = "imperial") {
  baseURL <- "https://api.openweathermap.org/data/3.0/onecall?"
  endpoint <- paste0("lat=", lat, "&lon=", lon, "&units=", units,
                     "&appid=a2a0e6a4de5aa1eed7f2d631ad8848ff")
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
    unnest(weather)
  
  df_dailyplots <- as.data.frame(df_dailyplots)
  
  return(df_dailyplots)
}


#Function to get past data from any previous week before January 1979
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
  
  #Turn list into a dataframe 
  df_historical <-as.data.frame(do.call(rbind,results))
  df_historical <- df_historical |>
    unnest_wider(c(temperature,cloud_cover,humidity,precipitation,
                   pressure,wind),names_sep ="_" ) |>
    unnest_wider(wind_max,names_sep="_")
}

# Define UI 
ui <- ui <- dashboardPage(
  dashboardHeader(title="Weather App"),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("circle-info")),
      menuItem("Data Download", tabName = "download", icon = icon("download")),
      menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-simple"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #About Tab Set Up 
      tabItem(tabName = "about",
              titlePanel("About My App"),
              fluidRow(
                box(title="Purpose of App", background = "light-blue",
                    "Describe the purpose of the app")),
              fluidRow(
                box(title="API Source", background = "light-blue",
                    "Briefly discuss the data and its source - providing a link to more information about the data")),
              fluidRow(
                tabBox(id ="tabset1",height="250px",
                    tabPanel("Data Exploration","Explaination of data exploration tab"),
                    tabPanel("Data Download", "Explaination of data download tab"))),
              fluidRow(
                box(title="Picture",background = "light-blue",
                    "Include a picture related to the data")),
      ),
      tabItem(tabName = "download",
              titlePanel("Data Download"),
              textInput(
                inputId = "city",
                label = "Please input a city",
                placeholder = "Enter city here"
              ),
              selectInput(
                inputId = "measurement",
                label = "Unit of measurement:",
                choices = c(
                  "Standard" = "standard",
                  "Metric" = "metric",
                  "Imperial" = "imperial"
                ),
                selected = "imperial"
              ),
              actionButton("go", "Generate data set"),
              br(), br(),
              dataTableOutput("weather_table")
      ),
      tabItem(tabName = "exploration",
              titlePanel("Data Exploration"),
              numericInput("number", "My Numeric Input", min = 0, max = 10, value = 5)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  weather_data <- eventReactive(input$go, {
    coords <- get_lat_lon(input$city)
    if (is.null(coords$lat) || is.null(coords$lon)) {
      return(NULL)
    }
    df <- get_dailyweather(coords[2], coords[3], input$measurement)
    return(df)
  })
  
  output$weather_table <- renderDataTable({
    req(weather_data())
    weather_data()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

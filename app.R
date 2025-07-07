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

#Pull in current weather
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

#Query past weather 

# Define UI 
ui <- ui <- dashboardPage(
  dashboardHeader(title="Weather App"),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("circle-info")),
      menuItem("Weather Download", tabName = "download", icon = icon("download")),
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
                    "App to compare weather conditions to ten years ago in any city")),
              fluidRow(
                box(title="API Source: Open Weather", background = "light-blue",
                    "This data source allows you to get the current and previous weather from any city.
                   This app currently allows you to pull the current weeks weather as well as weather from a year ago
                    to compare the change from 10 years ago. Here is a link to the API source for more information: 
                    https://openweathermap.org/api/one-call-3#history")),
              fluidRow(
                tabBox(id ="tabset1",height="250px",
                    tabPanel("Data Exploration","Explaination of data exploration tab"),
                    tabPanel("Data Download", "Explaination of data download tab"))),
              fluidRow(
                box(title="Picture",background = "light-blue",
                    "Include a picture related to the data")),
      ),
      tabItem(tabName = "download",
              titlePanel("Weather Download"),
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

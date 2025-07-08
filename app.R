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
  
  df_combined <- full_join(df_results,df_temperatures,by="date")
  
  return(df_combined)
}


merged_data <- function(df1,df2){
  df_merged <-bind_rows(df1,df2)
  return(df_merged)
}

# Define UI 
ui <- dashboardPage(
  dashboardHeader(title="Weather App"),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("circle-info")),
      menuItem("Weather Download", tabName = "download", icon = icon("download")),
      menuItem("Weekly Weather", tabName = "daily_exploration", icon = icon("chart-simple"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # About Tab
      tabItem(tabName = "about",
              titlePanel("About My App"),
              fluidRow(
                box(title = "Purpose of App", background = "light-blue",
                    "App to compare weather conditions to ten years ago in any city")),
              fluidRow(
                box(title = "API Source: Open Weather", background = "light-blue",
                    "This data source allows you to get the current and previous weather from any city.
                   This app currently allows you to pull the current week's weather as well as weather from a year ago
                   to compare the change from 10 years ago. Here is a link to the API source for more information: 
                   https://openweathermap.org/api/one-call-3#history")),
              fluidRow(
                tabBox(id = "tabset1", height = "250px",
                       tabPanel("Data Exploration", "Explanation of data exploration tab"),
                       tabPanel("Data Download", "Explanation of data download tab"))),
              fluidRow(
                box(title = "Picture", background = "light-blue",
                    "Include a picture related to the data"))
      ),
      
      # Download Tab
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
              dataTableOutput("weather_table"),
              downloadButton("downloadData", "Download")
      ),
      
      # Weekly Weather Tab
      tabItem(tabName = "daily_exploration",
              titlePanel("Weekly Weather"),
              fluidRow(
                box(selectInput(
                  inputId = "data_choice2",
                  label = "Select Data Type to View:",
                  choices = c("Rain" = "rain", 
                              "Wind" = "wind_speed",
                              "Humidity" = "humidity"),
                  selected = "Rain"
                ),
                selectInput(
                  inputId = "chart_choice",
                  label = "Select Chart Type:",
                  choices = c("Barchart" = "barchart", "Linechart" = "Linechart"),
                  selected = "Barchart"
                ),
                plotOutput(outputId = "barchart")
                )
              ),
              fluidRow(
                box(plotOutput(outputId = "lineplot"))
              ),
              fluidRow(
                box(selectInput(
                  inputId = "data_choice",
                  label = "Select Year:",
                  choices = c("2025" = "2025", "2024" = "2024"),
                  selected = "2025"
                ),
                plotOutput(outputId = "heatmap"))
              ),
      )
    )
  )
)



# Define Server
server <- function(input, output) {
  
  #Query API and Create Table 
  weather_data <- eventReactive(input$go, {
    coords <- get_lat_lon(input$city)
    if (is.null(coords$lat) || is.null(coords$lon)) {
      return(NULL)
    }
    df_daily <- get_dailyweather(coords[2], coords[3], input$measurement)
    df_historical <- get_historicaldata(coords[2], coords[3], input$measurement)
    df_weather <- merged_data(df_daily,df_historical)
    return(df_weather)
  })
  
  #Create data table
  output$weather_table <- renderDataTable({
    req(weather_data())
    weather_data()
  })
  
  #Daily Line Plot Code 
  output$lineplot <- renderPlot({
    
    df_temperature <- weather_data() |>
      filter(format(as.Date(date), "%Y") == "2025") |>
      select(date,starts_with("temperature_"),-temperature_min, -temperature_max, -temperature_afternoon) |>
      pivot_longer(cols = starts_with("temperature_"),
                   names_to= "time_of_day",
                   values_to = "temperature") |>
      mutate(time_of_day = sub("^temperature_", "", time_of_day))
    
    df_feels_like <- weather_data() |>
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
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(
        title = "Comparing Weekly Real Feels to Actual Temperature",
        x = "Date",
        y = "Temperature (°)",  
        color = "Time of Day"
      ) +
      facet_wrap(~ variable, scales = "free_y") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),  # remove vertical grid lines
        panel.grid.minor.x = element_blank()
      )
    
    
  })
  
  #Create daily bar chart
  output$barchart <- renderPlot({
    
    df_bar <- weather_data() |>
      filter(format(as.Date(date), "%Y") == "2025") |>
      select(date,rain,wind_speed,humidity) |>
      mutate(rain =ifelse(is.na(rain),0,rain))
    
    if(input$chart_choice == "barchart"){
    # Plot
    ggplot(df_bar, aes(x = date, y = .data[[input$data_choice2]])) +
      geom_bar(stat = "identity") +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d")+
      geom_text(aes(label = .data[[input$data_choice2]]), vjust = -0.5) +
      labs(title = paste("Daily", input$data_choice2),,
           x = "Date",
           y = "Precipitation (mm)") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major.x = element_blank(),  # remove vertical grid lines
          panel.grid.minor.x = element_blank()
        )
    }
    else if (input$chart_choice == "Linechart"){
    ggplot(df_bar, aes(x = date, y = .data[[input$data_choice2]])) +
      geom_line(color = "blue") +
      geom_point() +
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
      labs(title = paste("Trend of Daily", input$data_choice2),
           x = "Date", y = input$data_choice2) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })  
  
  #Create daily forecast heat map for current week vs. last year 
  output$heatmap <- renderPlot({
    
    if(input$data_choice == "2025"){
    df_heat <- weather_data() |>
      filter(format(as.Date(date), "%Y") == "2025") |>
      select(date, temperature_min, temperature_max, humidity, wind_speed)|>
      pivot_longer(cols = -date, names_to = "variable", values_to = "value")
    }
    else if(input$data_choice =="2024"){
    df_heat <- weather_data() |>
      filter(format(as.Date(date), "%Y") == "2024") |>
      select(date, temperature_min, temperature_max, humidity, wind_speed)|>
      pivot_longer(cols = -date, names_to = "variable", values_to = "value")
    }
    
    ggplot(df_heat, aes(x = date, y = variable, fill = value)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      labs(title = "Weather Variable Heatmap Over Time", x = "Date", y = "Variable") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  
  
  # Allow to download csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("weather_data_", input$city, ".csv")
    },
    content = function(file) {
      
      # Get the reactive result
      data <- weather_data()
      df_weather <- data[,!sapply(data,is.list)]
     
      write.csv(df_weather, file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

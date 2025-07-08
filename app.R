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

#Set Up the API Functions

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

#Create a function from API to get the weekly data 
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

#Get weather from a year ago 

get_historicaldata <- function(lat,lon,units="imperial"){
  #Create vector to have the dates from the week last year 
  today_date <- Sys.Date()
  year_ago <- today_date - 365
  week_year_ago <-as.Date(rep(NA,7))
  
  for(i in 0:6){
    week_year_ago[i+1] <-year_ago + i
  }
  
  #Convert date to work in API 
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
  
  #API into a list of temperatures 
  df_temperatures <-as.data.frame(do.call(rbind,results))
  df_temperatures <- df_temperatures |>
    unnest_wider(c(temperature),names_sep ="_" ) |>
    unnest("date")|>
    mutate(date=as.Date(date)) |>
    select("date",starts_with("temperature"))
  
  #Query API for rest of the weather data 
  
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
  
  #Modify dataframe to be able to merge with the weather dataframe 
  df_results <- df_results|>
    unnest(data) |>
    unnest(weather,names_sep = "_") |>
    mutate(date = as.Date(as.POSIXct(dt, origin = "1970-01-01", tz = "UTC"))) |>
    select(-temp,-dt,-sunrise,-sunset,-weather_icon,-weather_id)

  #Merge two historical data frames together
  df_combined <- full_join(df_results,df_temperatures,by="date")
  
  return(df_combined)
}

#Merge the historical and daily data frames together 
merged_data <- function(df1,df2){
  df_merged <-bind_rows(df1,df2)
  return(df_merged)
}

# Define UI function
ui <- dashboardPage(
  dashboardHeader(title="Weather App"),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("circle-info")),
      menuItem("Weather Download", tabName = "download", icon = icon("download")),
      menuItem("Weather Data Exploration", tabName = "daily_exploration", icon = icon("chart-simple"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # About Tab
      tabItem(tabName = "about",
              titlePanel("About My App"),
              fluidRow(
                box(title = "Purpose of App", background = "light-blue",
                    "The purpose of this app is to compare the weather conditions of most cities from today to 
                    exactly one year ago.")),
              fluidRow(
                box(title = "API Source: Open Weather", background = "light-blue",
                    "This data source allows you to get the current and previous weather from any city.
                    Here is a link to the API source for more information: 
                   https://openweathermap.org/api/one-call-3#history")),
              fluidRow(
                tabBox(id = "tabset1", height = "250px",
                       tabPanel("Data Exploration", "In the data exploration tab, you can analyze the current forecast of a city of your choosing and
                                compare that forecast to what the weather was exactly one year ago."),
                       tabPanel("Data Download", "The data download tab lets you select a city and a unit of measurement and will provide 
                                you the data from this current week as well as the weather from exactly one year ago in that location."))),
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
                valueBoxOutput("avg_temp"),
                valueBoxOutput("avg_humidity"),
                valueBoxOutput("max_rain"),
              ),
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
                box(
                  selectInput(
                    inputId = "facet_choice",
                    label = "Facet by:",
                    choices = c("None" = "none", "Variable" = "variable", "Time of Day" = "time_of_day"),
                    selected = "variable"
                  ),
                  plotOutput(outputId = "lineplot")
                )
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
  
  
  output$avg_temp_box <- renderValueBox({
    avg_temp <- mean(weather_data()$temperature_day, na.rm = TRUE)
    valueBox(
      value = paste0(round(avg_temp, 1), " °"),
      subtitle = "Average Day Temperature",
      icon = icon("thermometer-half"),
      color = "yellow"
    )
  })
  
  output$avg_humidity_box <- renderValueBox({
    avg_hum <- mean(weather_data()$humidity, na.rm = TRUE)
    valueBox(
      value = paste0(round(avg_hum, 0), " %"),
      subtitle = "Average Humidity",
      icon = icon("droplet"),
      color = "blue"
    )
  })
  
  output$max_rain <- renderValueBox({
    rain <- max(weather_data()$rain, na.rm = TRUE)
    valueBox(
      value = paste0(round(rain, 1), " mph"),
      subtitle = "Max Rain",
      icon = icon("rain"),
      color = "aqua"
    )
  })
  
  
  
  #Daily Line Plot Code 
  output$lineplot <- renderPlot({
    
    df_temperature <- weather_data() |>
      filter(format(as.Date(date), "%Y") == "2025") |>
      select(date, starts_with("temperature_"), -temperature_min, -temperature_max, -temperature_afternoon) |>
      pivot_longer(cols = starts_with("temperature_"),
                   names_to = "time_of_day",
                   values_to = "temperature") |>
      mutate(time_of_day = sub("^temperature_", "", time_of_day))
    
    df_feels_like <- weather_data() |>
      filter(format(as.Date(date), "%Y") == "2025") |>
      select(date, starts_with("feels_like_")) |>
      pivot_longer(cols = starts_with("feels_like_"),
                   names_to = "time_of_day",
                   values_to = "Feels Like") |>
      mutate(time_of_day = sub("^feels_like_", "", time_of_day)) |>
      mutate(time_of_day = ifelse(time_of_day == "morn", "morning",
                                  ifelse(time_of_day == "eve", "evening", time_of_day)))
    
    merged_plot <- full_join(df_feels_like, df_temperature, by = c("date", "time_of_day"))
    
    df_long <- merged_plot |>
      pivot_longer(cols = c("Feels Like", "temperature"), names_to = "variable", values_to = "value")
    
    base_plot <- ggplot(df_long, aes(x = date, y = value, color = time_of_day)) +
      geom_line(size = 1.1) +
      scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
      labs(
        title = "Comparing Daily Temperatures Throughout The Day",
        x = "Date",
        y = "Temperature",
        color = "Time of Day"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
    
    # Dynamic faceting
    if (input$facet_choice == "variable") {
      base_plot + facet_wrap(~ variable, scales = "free_y")
    } else if (input$facet_choice == "time_of_day") {
      base_plot + facet_wrap(~ time_of_day, scales = "free_y")
    } else {
      base_plot
    }
    
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
      geom_text(aes(label=value))+
      scale_fill_viridis_c(option = "C") +
      labs(title = "Daily Forecast In Heat Map", x = "Date", y = "Variable") +
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

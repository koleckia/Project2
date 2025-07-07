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

# Define UI for application that draws a histogram
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
              numericInput("number", "My Numeric Input", min = 0, max = 10, value = 5)
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
}

# Run the application 
shinyApp(ui = ui, server = server)

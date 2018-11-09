#### load packages & data ####
library(shiny)
library(tidyverse)
load("/home/nathan/Documents/stats-projects/stats-career-talk/tech-demo/application/autodat.RData")

#### shiny application ####
library(shiny)

ui <- fluidPage(
  
  # App Name
  headerPanel("Auto Insurance Pricing Tool"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Model Inputs
    sidebarPanel(
      
      # Exposure
      selectInput(inputId = "Exposure", 
                  label = "Policy Period:",
                  choices = list("6 Months"=0.5, "1 Year"=1), 
                  selected = "1 Year"),
      
      # VehPower
      sliderInput(inputId = "VehPower", 
                  label = "Vehicle Power:",
                  min = 4,
                  max = 15,
                  step = 1,
                  value = 4,
                  ticks = TRUE),
      
      # Vehicle Age
      sliderInput(inputId = "VehAge", 
                  label = "Vehicle Age:",
                  min = 0,
                  max = 100,
                  step = 1,
                  value = 0,
                  ticks = TRUE),
      
      # Driver Age
      sliderInput(inputId = "DrivAge", 
                  label = "Driver Age:",
                  min = 18,
                  max = 100,
                  step = 1,
                  value = 0,
                  ticks = TRUE),
      
      # Fuel Type
      selectInput(inputId = "VehGas", 
                  label = "Fuel Type:",
                  choices = c("Regular","Diesel"),
                  selected = "Regular"),
      
      # Bonus Malus
      sliderInput(inputId = "BonusMalus", 
                  label = "Bonus Malus:",
                  min = 50,
                  max = 230,
                  step = 5,
                  value = 50,
                  ticks = TRUE)
    ),
    
    # Output: Pure Premium
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
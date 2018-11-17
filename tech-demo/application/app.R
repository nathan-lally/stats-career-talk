#### load packages & data ####
library(shiny)
library(tidyverse)
library(rfCountData)
load("/home/nathan/Documents/stats-projects/stats-career-talk/tech-demo/application/autodat.RData")

#### shiny application ####
library(shiny)

ui <- fluidPage(
  
  # App Name
  headerPanel("Auto Insurance Claim Frequency Predictions"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Model Inputs
    sidebarPanel(
      
      # Vehicle body
      selectInput(inputId = "VehBody", 
                  label = "Vehicle Body:",
                  choices = c("bus","cabriolet","coupe","microvan","other microvan","sedan","sport utility vehicle","station wagon","van"), 
                  selected = "sedan"),
      
      # Vehicle Age
      selectInput(inputId = "VehAge", 
                  label = "Vehicle Age:",
                  choices = c("0","1","2","3","4","5","6-7","8-9","10+"), 
                  selected = "0"),
      
      # Vehicle Usage
      selectInput(inputId = "VehUsage", 
                  label = "Vehicle Usage:",
                  choices = c("Private","Private+trip to office","Professional","Professional run"), 
                  selected = "Private"),
      
      # Vehicle Max Speed
      selectInput(inputId = "VehMaxSpeed", 
                  label = "Vehicle Max Speed:",
                  choices = c("1-130 km/h", "130-140 km/h", "140-150 km/h", "150-160 km/h", "160-170 km/h", "170-180 km/h", "180-190 km/h", "190-200 km/h", "200-220 km/h", "220+ km/h"), 
                  selected = "1-130 km/h"),
      
      # Driver Gender
      selectInput(inputId = "Gender", 
                  label = "Vehicle Gender:",
                  choices = c("Female","Male"), 
                  selected = "Male"),
      
      # Driver Age
      sliderInput(inputId = "DrivAge", 
                  label = "Driver Age:",
                  min = 18,
                  max = 100,
                  step = 1,
                  value = 0,
                  ticks = TRUE),
      
      # License Age
      sliderInput(inputId = "LicAge", 
                  label = "Liscense Age (Mo):",
                  min = 0,
                  max = 940,
                  step = 12,
                  value = 0,
                  ticks = TRUE)
      # ,
      # actionButton("button", "Show")
      
    ),
    
    # Output: Pure Premium
    mainPanel(
      plotOutput(outputId = "plot", width = "100%", height = "650px")
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    # set up test data for predictions
    xpred <- data.frame(LicAge = as.integer(input$LicAge),
                        VehAge = as.factor(input$VehAge),
                        Gender = as.factor(input$Gender),
                        VehUsage = as.factor(input$VehUsage),
                        DrivAge = as.integer(input$DrivAge),
                        VehBody = as.factor(input$VehBody),
                        VehMaxSpeed = as.factor(input$VehMaxSpeed))
    levels(xpred$VehAge) <- levels(df$VehAge)
    levels(xpred$Gender) <- levels(df$Gender)
    levels(xpred$VehUsage) <- levels(df$VehUsage)
    levels(xpred$VehBody) <- levels(df$VehBody)
    levels(xpred$VehMaxSpeed) <- levels(df$VehMaxSpeed)
    overallrate <- sum(df$ClaimInd)/sum(df$Exposure)
    # model predictions
    modpred <- predict(m0, newdata = xpred, offset = 0)
    preddat <- data.frame(Group=c("Baseline","Prediction"), Value=c(overallrate,modpred))
    # make the plot
    ggplot(data=preddat, aes(x=Group,y=Value)) + 
      geom_col(fill="cyan", color="black") + 
      geom_hline(aes(yintercept=overallrate), color="red", linetype=2, size=1) +
      scale_y_continuous(breaks=seq(0,1,0.05), limits = c(0,1)) +
      labs(x="Category",y="Claim Frequency", title = "Claim Frequency: Predictions vs. Reference") + 
      theme_light(base_size = 20)
  })
}

shinyApp(ui, server)
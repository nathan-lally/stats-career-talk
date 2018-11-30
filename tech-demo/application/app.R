#### load packages ####
library(shiny)
library(tidyverse)
library(rfCountData)
# use the CASdatsets package to obtain auto claims data
library(CASdatasets)

#### load and prepare data ####
data(freMPL1)
df <- as_tibble(freMPL1)
df <- df %>%
  select(Exposure,ClaimInd,LicAge,VehAge,Gender,VehUsage,DrivAge,VehBody,VehMaxSpeed)
rm(freMPL1)
overallrate <- sum(df$ClaimInd)/sum(df$Exposure)
# prep for random forest
y <- df$ClaimInd
exposure <- df$Exposure
x <- df %>%
  select(-c(Exposure,ClaimInd)) %>%
  as.data.frame()

#### fit model ####
m0 <-  rfPoisson(y = y,
                 offset = log(exposure),
                 x = x,
                 mtry = 3,
                 nodesize = 2000,
                 ntree = 300)

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
                  label = "Driver Gender:",
                  choices = c("Female","Male"), 
                  selected = "Male"),
      
      # Driver Age
      sliderInput(inputId = "DrivAge", 
                  label = "Driver Age:",
                  min = 18,
                  max = 100,
                  step = 1,
                  value = 18,
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
  # set up test data for predictions
  preddat <- reactive(
    {
      xpred <- data.frame(LicAge = as.integer(input$LicAge),
                          VehAge = factor(input$VehAge, levels = levels(df$VehAge)),
                          Gender = factor(input$Gender, levels = levels(df$Gender)),
                          VehUsage = factor(input$VehUsage, levels = levels(df$VehUsage)),
                          DrivAge = as.integer(input$DrivAge),
                          VehBody = factor(input$VehBody, levels = levels(df$VehBody)),
                          VehMaxSpeed = factor(input$VehMaxSpeed, levels = levels(df$VehMaxSpeed)))
      # model predictions
      modpred <- predict(m0, newdata = xpred, offset = 0)
      preddat <- data.frame(Group=c("Baseline","Prediction"), Value=c(overallrate,modpred))
      return(preddat)
    }
  )
  # make the plot
  output$plot <- renderPlot({
    ggplot(data=preddat(), aes(x=Group,y=Value)) + 
      geom_col(fill="cyan", color="black") + 
      geom_hline(aes(yintercept=overallrate), color="red", linetype=2, size=1) +
      scale_y_continuous(breaks=seq(0,1,0.05), limits = c(0,1)) +
      labs(x="",y="Claim Frequency (Claims per Year)", title = "Claim Frequency: Predictions vs. Reference") + 
      theme_light(base_size = 20)
  })
}

shinyApp(ui, server)
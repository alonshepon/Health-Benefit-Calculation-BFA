
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny/data" # when testing
# codedir <- "shiny/code" # when testing

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read data
food <- readRDS(file.path(datadir, "COSIMO_2010_2030_food_by_scenario_cntry.Rds"))
nutrients <- readRDS(file.path(datadir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds"))


# Parameters
################################################################################

# Countries with data in 2030
countries2030 <- nutrients %>% pull(country) %>% unique() %>% sort()

# Base theme
base_theme <- theme(axis.text=element_text(size=14),
                    axis.title=element_text(size=16),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=16),
                    strip.text=element_text(size=16),
                    plot.subtitle=element_text(size=18),
                    plot.title=element_text(size=16),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# User interface
################################################################################

# User interface
ui <- fluidPage(
  
  # Title
  titlePanel("Aquatic foods for nourishing nations"),
           
  # Select species
  selectizeInput(inputId = "country", label = "Select a country:", 
                 choices = countries2030,  multiple = F, options = NULL),
  
  # Diets
  h4("Change in diets under baseline and high road scenarios"),
  
  # Food intake by country
  p(""),
  plotOutput(outputId = "plot_food_over_time", width=1000, height=800),
  br(),
  
  # Nutrient intake by country
  p(""),
  plotOutput(outputId = "plot_nutrients_over_time", width=1000, height=800),
  br(),
  
  # Diets
  h4("Relative difference between high road and baseline scenarios"),
  
  # Food intake by country
  p("The figure below illustrates the difference in mean daily food consumption under the high road and baseline scenario over time. A difference greater than zero indicates increased consumption of a food under the high road scenario relative to the baseline scenario. A difference less than zero indicates reduced consumption of a food under the high road scenario relative to the baseline scenario."),
  plotOutput(outputId = "plot_food_over_time_diff", width=1000, height=800),
  br(),
  
  # Nutrient intake by country
  p("The figure below illustrates the difference in mean daily nutrient intakes under the high road and baseline scenario over time. A difference greater than zero indicates increased intake of a nutrient under the high road scenario relative to the baseline scenario. A difference less than zero indicates reduced intake of a nutrient under the high road scenario relative to the baseline scenario."),
  plotOutput(outputId = "plot_nutrients_over_time_diff", width=1000, height=800),
  br(),
  
  # Summary exposure values
  h4("Summary Exposure Values (SEVs)"), 
  p(""),
  br(),
  
  # DALYs
  h4("Disability-Adjusted Life Years (DALYs)"), 
  p(""),
  br()

)


# Server
################################################################################

# Server
server <- function(input, output){
  
  # Plot food over time
  output$plot_food_over_time <- renderPlot({
    g <- plot_food_over_time(data=food, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot food over time
  output$plot_nutrients_over_time <- renderPlot({
    g <- plot_nutrients_over_time(data=nutrients, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot food over time (relative difference)
  output$plot_food_over_time_diff <- renderPlot({
    g <- plot_food_over_time_diff(data=food, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot food over time (relative difference)
  output$plot_nutrients_over_time_diff <- renderPlot({
    g <- plot_nutrients_over_time_diff(data=nutrients, country=input$country, base_theme=base_theme)
    g
  })
  
  
  
}

shinyApp(ui = ui, server = server)

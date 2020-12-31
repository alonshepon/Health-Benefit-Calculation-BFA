
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
nutrients_disagg <- readRDS(file.path(datadir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds"))
sevs <- readRDS(file.path(datadir, "2030_sevs_base_high_road_final.Rds"))
nutr_dists <- readRDS(file.path(datadir, "COSIMO2030_country_nutrient_age_sex_means_and_distributions.Rds"))
ndeficient <- readRDS(file.path(datadir, "2030_ndeficient_base_high.Rds"))

# Read EARS and reduce to nutrients in COSIMO output
ears <- readRDS(file.path(datadir, "ears.Rds")) %>% 
  filter(nutrient %in% unique(nutr_dists$nutrient) & nutrient !="Protein")

# Read DALYs
dalys <- readRDS(file.path(datadir, "2030_dalys_base_high_road_summarized.Rds"))


# Parameters
################################################################################

# Countries with data in 2030
countries2030 <- nutrients %>% pull(country) %>% unique() %>% sort()

# Nutrients
nutrients_show <- c("Calcium", "Iron", "Vitamin A, RAE", "Vitamin B-12", "Zinc", "Omega-3 fatty acids")

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
  h4("Change in food consumption under base and high road scenarios"),
  
  # Food intake by country
  p("The figure below shows the change in mean daily per capita food consumption under the base and high road scenarios."),
  plotOutput(outputId = "plot_food_over_time", width=1000, height=800),
  br(),
  
  # Food intake by country
  p("The figure below illustrates the difference in mean daily food consumption under the high road and baseline scenario over time. A difference greater than zero indicates increased consumption of a food under the high road scenario relative to the baseline scenario. A difference less than zero indicates reduced consumption of a food under the high road scenario relative to the baseline scenario."),
  plotOutput(outputId = "plot_food_over_time_diff", width=1000, height=800),
  br(),
  
  # Nutrient intakes
  h4("Change in nutrient intake under base and high road scenarios"),
  
  # Nutrient intake by country
  p("The figure below illustrates the change in mean daily per capita nutrient intake under the base and high road scenarios."),
  plotOutput(outputId = "plot_nutrients_over_time", width=1000, height=800),
  br(),
  
  # Nutrient intake by country
  p("The figure below illustrates the difference in mean daily nutrient intakes under the high road and baseline scenario over time. A difference greater than zero indicates increased intake of a nutrient under the high road scenario relative to the baseline scenario. A difference less than zero indicates reduced intake of a nutrient under the high road scenario relative to the baseline scenario."),
  plotOutput(outputId = "plot_nutrients_over_time_diff", width=1000, height=800),
  br(),
  
  # Impacts of diversity disaggregation
  h4("Impact of diversity disaggregation"),
  p("The figure below illustrates the change in mean daily per capita nutrient intake under the base and high road scenarios with and without the diversity disaggregation."),
  plotOutput(outputId = "plot_nutrients_over_time_disagg", width=1000, height=500),
  br(),
  
  p("The figure below illustrates the difference in mean daily nutrient intakes under the high road and baseline scenario over time, with and without the diversity disaggregation. A difference greater than zero indicates increased intake of a nutrient under the high road scenario relative to the baseline scenario. A difference less than zero indicates reduced intake of a nutrient under the high road scenario relative to the baseline scenario."),
  plotOutput(outputId = "plot_nutrients_over_time_diff_disagg", width=1000, height=500),
  br(),
  
  # Select species
  selectizeInput(inputId = "nutr_calc", label = "Select nutrient intake results to explore:", 
                 choices = c("Original", "Diversity disaggregation"),  multiple = F, options = NULL),
  
  # Subnational intake distributions
  h4("Subnational intake distributions"), 
  
  # Group means
  p("This shows subnational intake distributions in 2030 under the baseline and high road scenarios. The subnational distributions were derived from the COSIMO country-level mean and GENUS and SPADE derived scalars for determining the subnational group means. The lines show the EARS overlaid on top of the distributions."),
  plotOutput(outputId = "plot_subnational_nutr_dist_means", width=700, height=1400),
  br(),
  
  # Select species
  selectizeInput(inputId = "nutrient", label = "Select a nutrient:", 
                 choices = nutrients_show,  multiple = F, options = NULL),
  
  # Within group distribution
  p(""),
  plotOutput(outputId = "plot_nutr_dists", width=800, height=800),
  br(),
  
  # SEVs
  h4("Summary Exposure Values (SEVs)"), 
  
  # SEV raw values
  p("The summary exposure values (SEV) is a measure of a population’s exposure to a risk factor that takes into account the extent of exposure by risk level and the severity of that risk’s contribution to disease burden. A value of 0 indicates no risk and a value of 100 indicates the highest level of risk."),
  plotOutput(outputId = "plot_sevs", width=800, height=800),
  br(),
  
  # SEV differences
  p("The figure below illustrates the change in SEVs for a population under the high road scenario relative to the baseline. A difference less than zero indidates that diets under the high road scenario lowered risk. A difference greater than zero indicates that diets under the high road scenario increased risk."),
  plotOutput(outputId = "plot_sevs_diff", width=800, height=800),
  br(),
  
  # Micronutrient deficiencies
  h4("Micronutrient deficiencies"), 
  p("The figure below illustrates the change in micronutrient deficiencies for a population under the high road scenario relative to the baseline. A difference less than zero indidates that diets under the high road scenario reduced micronutrient deficiencies. A difference greater than zero indicates that diets under the high road scenario increased micronutrient deficiencies."),
  plotOutput(outputId = "plot_ndeficient_diff", width=800, height=800),
  br(),
  
  # DALYs
  h4("Disability-Adjusted Life Years (DALYs)"), 
  
  # DALY raw values
  p(""),
  plotOutput(outputId = "plot_dalys", width=800, height=300),
  br(),
  
  # DALY differences
  p(""),
  plotOutput(outputId = "plot_dalys_diff", width=800, height=300),
  br(),
  
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
  
  # Plot food over time (relative difference)
  output$plot_food_over_time_diff <- renderPlot({
    g <- plot_food_over_time_diff(data=food, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot nutrients over time
  output$plot_nutrients_over_time <- renderPlot({
    g <- plot_nutrients_over_time(data=nutrients, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot nutrients over time (relative difference)
  output$plot_nutrients_over_time_diff <- renderPlot({
    g <- plot_nutrients_over_time_diff(data=nutrients, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot nutrients over time with disagg
  output$plot_nutrients_over_time_disagg <- renderPlot({
    g <- plot_nutrients_over_time_disagg(data=nutrients_disagg, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot nutrients over time (relative difference) with disagg
  output$plot_nutrients_over_time_diff_disagg <- renderPlot({
    g <- plot_nutrients_over_time_diff_disagg(data=nutrients_disagg, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot subnational intake distributions
  output$plot_subnational_nutr_dist_means <- renderPlot({
    g <- plot_subnational_nutr_dist_means(data=nutr_dists, ears=ears, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot subnational intake distributions
  output$plot_nutr_dists <- renderPlot({
    g <- plot_nutr_dists(data=nutr_dists, country=input$country, nutrient=input$nutrient, base_theme=base_theme)
    g
  })
  
  # Plot SEVs
  output$plot_sevs <- renderPlot({
    g <- plot_sevs(data=sevs, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot SEV differences
  output$plot_sevs_diff <- renderPlot({
    g <- plot_sevs_diff(data=sevs, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot micronutrient deficiency differences
  output$plot_ndeficient_diff <- renderPlot({
    g <- plot_ndeficient_diff(data=ndeficient, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot DALYs
  output$plot_dalys <- renderPlot({
    g <- plot_dalys(data=dalys, country=input$country, base_theme=base_theme)
    g
  })
  
  # Plot DALY differences
  output$plot_dalys_diff <- renderPlot({
    g <- plot_dalys_diff(data=dalys, country=input$country, base_theme=base_theme)
    g
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)

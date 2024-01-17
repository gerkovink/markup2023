# Version 1.2.0-beta

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Data Reference: Venables, W. N. and Ripley, B. D. (2002) 
# Modern Applied Statistics with S. Fourth edition. Springer.

library(shiny)
library(shinythemes)
library(ggformula)
library(tidyverse)
library(RColorBrewer)
library(shinydashboard)
library(rjson)

# Load data
data(birthwt, package = "MASS")

df <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    race = factor(race, labels = c("White", "Afican American", "Other")),
    low = factor(low, labels = c("Low weight", "Normal weight")),
    ht = factor(ht, labels = c("No hypertension", "Hypertension")),
    ui = factor(ui, labels = c("No uterine irritability", "Uterine irritability"))
  ) %>%
  set_variable_labels(
    low = 'birth weight less than 2.5 kg',
    age = 'mother age in years',
    lwt = 'mother weight in pounds at last menstrual period',
    race = 'mother race',
    smoke = 'smoking status during pregnancy',
    ptl = 'number of previous premature labours',
    ht = 'history of hypertension',
    ui = 'presence of uterine irritability',
    ftv = 'number of physician visits during the first trimester',
    bwt = 'birth weight (g)',
  ) %>% 
  rename(
    "mother_age" = "age",
    "mother_weight" = "lwt",
    "mother_race" = "race",
    "smoking_status" = "smoke",
    "prev_premature_labours" = "ptl",
    "hypertension" = "ht",
    "uterine_irritability" = "ui",
    "nr_physician_visits" = "ftv"
  )

# Define UI elements
# ui <- dashboardPage(
#   dashboardHeader(title = "Risk Factors of Low Birth Weight"),
#   dashboardSidebar(
#     sidebarMenu(id = "tabs",style = "position:fixed;width:220px;",
#                 menuItem("Options", tabName = "Select Risk Factors", icon = icon("bar-chart")),
#                 selectInput("selected_categorical", "Select a Categorical Variable", 
#                             choices = c("mother_race", "smoking_status", "hypertension", "uterine_irritability")),
#                 selectInput("selected_continuous", "Select a Continuous Variable",       
#                             choices = c("mother_age", "mother_weight", "prev_premature_labours", "nr_physician_visits"))
#     )
#   ),
#   dashboardBody(
#     fluidRow(
#       box(
#         HTML("<p>This Shiny application is a platform to explore and visualize data on risk factors associated with low birth weight.
#              Developed using R and integrating libraries like ggplot2, shinydashboard, and tidyverse, the app provides a dashboard 
#              to analyze birth weight data from a study by Venables and Ripley (2002)</p>"),
#         width = 12
#       ),
#       box(plotOutput("boxplot"), width="100%", height=550),
#       box(plotOutput("violin"), width = "100%", height = 550),
#       box(plotOutput("scatter"), width = "100%", height = 550),
#       box(
#         HTML("<p>Venables, W. N. and Ripley, B. D. (2002). Modern Applied Statistics with S. Fourth edition. Springer.</p>"),
#         width = 12
#       )
#     )
#   )
# )

ui <- fluidPage(theme = shinytheme("flatly"),
                  titlePanel("Risk Factors of Low Birth Weight"),
                  sidebarLayout(
                    sidebarPanel(
                      # Place your input elements here
                      selectInput("selected_categorical", "Select a Categorical Variable", 
                                  choices = c("mother_race", "smoking_status", "hypertension", "uterine_irritability")),
                      selectInput("selected_continuous", "Select a Continuous Variable",       
                                  choices = c("mother_age", "mother_weight", "prev_premature_labours", "nr_physician_visits"))
                      # Add other input elements as needed
                    ),
                    mainPanel(
                      box(
                        p("This Shiny application is a platform to explore and visualize data on risk factors associated with low birth weight.
                        Developed using R and integrating libraries like ggplot2, shinydashboard, and tidyverse, the app provides a dashboard 
                        to analyze birth weight data from a study by Venables and Ripley (2002)."),
                        width="100%", height=150
                      ),
                      box(plotOutput("boxplot"), width="100%", height=500),
                      box(plotOutput("violin"), width = "100%", height = 500),
                      box(plotOutput("scatter"), width = "100%", height = 500),
                      box(
                        p("Venables, W. N. and Ripley, B. D. (2002). Modern Applied Statistics with S. Fourth edition. Springer."),
                        width="100%", height=100
                      )
                    )
                  )
)


# Define Server logic
server <- function(input, output) {
  
  output$boxplot <- renderPlot({
    selected_con <- input$selected_continuous
    
    ggplot(df, aes(x = !!sym(selected_con), y = low, color = low, fill = low)) +
      geom_boxplot(alpha = 0.5) +
      theme_classic() +
      scale_color_brewer(palette="Accent") +
      scale_fill_brewer(palette="Accent") +
      labs(x = selected_con, y = "Birth Weight, Dichotomous", 
           title = paste("Relation between", selected_con, "and Low Birth Weight")) +
      theme_classic() +
      theme(legend.position="none")
  })
  
  output$violin <- renderPlot({
    selected_cat <- input$selected_categorical

    ggplot(df, aes(x = !!sym(selected_cat), y = bwt, color = !!sym(selected_cat))) +
      geom_violin() +
      geom_jitter(alpha = 0.5) +
      geom_hline(aes(yintercept = 0.5), linetype="dashed") +
      labs(x = selected_cat, y = "Birth Weight in grams", 
           title = paste("Relation between", selected_cat, "and Low Birth Weight")) +
      theme_classic() +
      scale_color_brewer(palette="Accent")
  })
  
  output$scatter <- renderPlot({
    selected_cat <- input$selected_categorical
    selected_con <- input$selected_continuous
    
    ggplot(df, aes(x = !!sym(selected_con), y = bwt, color = !!sym(selected_cat))) +
      geom_point() +
      geom_smooth(aes(group = !!sym(selected_cat)), method = "lm", se = FALSE) +
      labs(x = selected_con, y = "Birth Weight in grams", 
           title = paste("Relation between", selected_con, 
                         "and Low Birth Weight divided by", selected_cat)) +
      theme_classic() +
      scale_color_brewer(palette="Accent")
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

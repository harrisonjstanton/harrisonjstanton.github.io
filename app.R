#
# This is a Shiny web application. You can run the application by clicking
# the Run App button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(shinyFeedback)
mlb23 <- read_csv("mlb23.csv")

#need to make it so team is not in here
column_names <- colnames(mlb23)
column_names <- column_names[-1]



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2023 MLB Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            helpText("This app allows you to compare team hitting statistics to run production"),
            selectInput("x_var", "Choose a variable for x-axis", 
                        choices = column_names),
            selectInput("y_var", "Choose a variable for y-axis",
                         choices = column_names),
            checkboxInput("bestFit", "Check to see line of best fit", value = FALSE),
            #checkboxInput("errorBands", "Check to see error bands", value = FALSE)
            uiOutput("errorbands")
        ),
        

        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           textOutput("rsquared_output"),
           textOutput("rsquared_analysis"),
           textOutput("message"),
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Shows my warning notification (think I could use this for the NFLapp to show what's selected)
  observe({
      if(input$x_var == input$y_var){
        showNotification("You have selected the same variable for x-axis and y-axis", type = "warning")
    }
    
  })


    output$scatterPlot <- renderPlot({
        

        # draw the histogram with the specified number of bins
          ggplot(mlb23, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
          geom_point() +
          xlab(input$x_var) +
          ylab(input$y_var) +
          ggtitle(paste(input$y_var, "vs.", input$x_var)) +
          if(input$bestFit){
              geom_smooth(method = "lm", se = input$errorBands)
            
          } else{
              NULL
          }
 
    })
    
    output$rsquared_output <- renderText({
      if (input$bestFit) {
          model <- lm(mlb23[[input$y_var]] ~ mlb23[[input$x_var]])
          paste("R-squared is:", round(summary(model)$r.squared, 3))
      }
        })

    output$rsquared_analysis <- renderText({
      model <- lm(mlb23[[input$y_var]] ~ mlb23[[input$x_var]])
      r_squared <- round(summary(model)$r.squared, 3)
      if(r_squared >= 0.7){
        "There is a strong correlation"
      }
      else if(r_squared < 0.7 & r_squared >= 0.4){
        "There is mild correlation"
      }
      else if(r_squared != 0){
        "There is little correlation"
      }
      else{
        "There is no correlation"
      }
    })
    
     output$errorbands <- renderUI({
    
     if(input$bestFit){
         checkboxInput("errorBands", "Check to see error bands", value = FALSE)
         
      }
    
     })
    
    output$message <- renderText({
      req(input$errorBands)
        if(input$errorBands){
         "Error bands checked"
        }
    })

}

# Run the application
shinyApp(ui = ui, server = server)

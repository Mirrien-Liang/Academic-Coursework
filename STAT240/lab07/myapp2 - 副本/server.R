library(shiny)
library(stringr)
library(tidyverse)


# Import and clean data
abalone_col_names <- c("Sex",
                       "Length",
                       "Diameter",
                       "Height",
                       "Whole_Weight",
                       "Shucked_Weight",
                       "Viscera_Weight",
                       "Shell_Weight",
                       "Rings")
abalone <- read_csv('abalone.data',col_names = abalone_col_names,
                    col_types = cols(
                      Length = col_double(),
                      Diameter = col_double(),
                      Height = col_double(),
                      Whole_Weight = col_double(),
                      Shucked_Weight = col_double(),
                      Viscera_Weight = col_double(),
                      Shell_Weight = col_double(),
                      Rings = col_integer()
                    )
)

# The data should be of a type of data.frame to use reactive()
abalone <- as.data.frame(select(abalone,-1))


# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    abalone[, input$xcol]
  })
  
  
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 1.5, 1))
    plot(density(selectedData()),
         main = paste("Density of", str_replace(input$xcol,"\\_"," ")))
  })
  
})

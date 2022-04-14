library(shiny)
library(tidyverse)
library(stringr)

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




  
  pageWithSidebar(
    headerPanel('Density of abalone properties'),
    sidebarPanel(
      selectInput('xcol', 'Variable', setdiff(names(abalone), "Variables")[1:5])
    ),
    mainPanel(
      plotOutput('plot1')
    )

)
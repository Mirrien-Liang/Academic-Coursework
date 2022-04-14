library(shiny)

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
abalone = read.csv("abalone.data", header = F)
names(abalone) = abalone_col_names
abalone = abalone[-1]




  
pageWithSidebar(
  headerPanel('Density of abalone properties'),
  sidebarPanel(
    selectInput('xcol', 'Variable', setdiff(names(abalone), "Variables")[1:5])
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
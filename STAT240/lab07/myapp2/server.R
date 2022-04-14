library(shiny)



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
         main = paste("Density of", gsub("\\_"," ",input$xcol)))
  })
  
})

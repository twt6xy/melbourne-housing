library(shiny)

# add elements as arguments to fluidPage()
ui <- fluidPage(
  # create reactive inputs with *Input(inputId = "uniqueID", label = "optional title", ...input specific arguments) functions,
  # display reactive results with *Output(outputId = "uniqueOutputID") functions
)

# use the server function to assemble inputs into outputs
server <- function(input, output) {
  #rules:
  # save objects to display to output$outputId <- #code
  # build objects to display output$outputId <- render*({code})
  # use input values with input$inputId
  
}

shinyApp(ui = ui, server = server)
library(shiny)
library(shinyMatrix)

ui <- fluidPage(
  
  # Application title
  titlePanel("QuantIT Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      textInput("pasteinput", label = "paste here", placeholder = "paste spreadsheet here"),
      
      
      
      matrixInput(inputId = "inputmat", 
                  value = matrix(data = 0, 
                                 nrow = 8, ncol = 12, 
                                 dimnames = list(LETTERS[1:8], 1:12))
                  ),
      actionButton("simulate", "Simulate!"),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("obs")
    )
  )
)

server <- function(input, output, session) {

  pasteInput <- reactive({
    raw.paste <- input$pasteinput
    y <- strsplit(raw.paste, "\n")
    z <- strsplit(y[[1]], "\t")
    paste.neat <- matrix(unlist(z), nrow = 8, byrow = T)
    return(paste.neat)
  })
  
  output$obs <- renderTable({pasteInput()})

}

shinyApp(ui = ui, server = server)

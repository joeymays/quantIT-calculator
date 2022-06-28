library(shiny)
library(shinyMatrix)

ui <- fluidPage(
  
  titlePanel("QuantIT Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("pasteinput", label = "paste here", placeholder = "paste spreadsheet here"),
      
      selectInput(inputId = "standardcolumn", "Standard Column", selected = 1, choices = 1:12)

    ),
    
    mainPanel(
      tableOutput("obs")
    )
  )
)

server <- function(input, output, session) {
    
    pasteInput <- reactive({
        req(input$pasteinput)
        raw.paste <- input$pasteinput
        y <- strsplit(raw.paste, "\\s")[[1]]
        
        if(length(y) < 96){
            y <- c(y, rep("", 96 - length(y)))
        }
        
        y[which(y == "")] <- NA
        paste.neat <- matrix(as.numeric(y), nrow = 8, byrow = T)
        return(paste.neat)
    })
    
    pastePreview <- reactive({
        round(pasteInput(), 1)
    })
    
    output$obs <- renderTable({
        req(input$pasteinput)
        pastePreview()})
    
}

shinyApp(ui = ui, server = server)

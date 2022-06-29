library(shiny)
#library(shinyMatrix)
library(ggplot2)

source("quantit-backend.R")

ui <- fluidPage(
  
  titlePanel("QuantIT Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("pasteinput", label = "paste here", placeholder = "paste spreadsheet here"),
      
      fluidRow(
        column(6,
               selectInput(inputId = "standardColumn", "Standard Column", selected = 1, choices = 1:12)
               ),
        column(6,
               numericInput("standardAmount", "Standard Amount (uL)", value = 10)
               ),
      ),
      numericInput("sampleAmount", "Sample Amount (uL)", value = 2, width = "45%"),
    ),
    
    mainPanel(
      tableOutput("tablePreview"),
      
      fluidRow(
        column(8,
      plotOutput("regressionPlot"),),
      column(4,
      textOutput("regressionParams")
      ),
      )
    )
  )
)

server <- function(input, output, session) {
  
  standard.default.concentrations <- c(0, 0.5, 1, 2, 4, 6, 8, 10)  
  
  pasteInput <- reactive({
        req(input$pasteinput)
        raw.paste <- input$pasteinput
        y <- strsplit(raw.paste, "\\s")[[1]]
        
        if(length(y) < 96){
            y <- c(y, rep("", 96 - length(y)))
        }
        
        y[which(y == "")] <- NA
        paste.neat <- matrix(as.numeric(y), nrow = 8, byrow = T, dimnames = list(LETTERS[1:8], paste0(1:12, " ")))
        return(paste.neat)
    })
    
    output$tablePreview <- renderTable({
        req(input$pasteinput)
      pasteInput()}, rownames = T, colnames = T, digits = 1)
    
    regressionDF <- reactive({
      req(input$pasteinput)
      data.frame(dna.amount = standard.default.concentrations * input$standardAmount, 
                 standard.signal = pasteInput()[,as.numeric(input$standardColumn)])
    })
    
    regressionParams <- reactive({
      print("RUN1")
      regression.results <- getRegression(x = regressionDF()$dna.amount, y = regressionDF()$standard.signal)
      print(str(regression.results))
      return(regression.results)
    })
    
    output$regressionParams <- renderText(paste0("slope = ", round(regressionParams()$slope, 1), "; intercept = ", 
                                                 round(regressionParams()$yint, 1), "; r.squared = ", 
                                                 round(regressionParams()$rsq), 2))
    
    
    output$regressionPlot <- renderPlot({
      ggplot(data = regressionDF(), aes(x = dna.amount, y = standard.signal)) +
        geom_point(size = 2) +
        geom_abline(slope = regressionParams()$slope, intercept = regressionParams()$yint) +
        theme_bw()
    })
    
    
    # output$scatterplot <- renderPlot({
    #   ggplot()
    # })
    # 
}

shinyApp(ui = ui, server = server)

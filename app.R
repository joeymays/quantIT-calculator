library(shiny)
#library(shinyMatrix)
library(ggplot2)

source("quantit-backend.R")

ui <- fluidPage(
  
  HTML(r"(<p style="text-align:center;">Joey Mays - Updated 2022-06-29</p>)"),
  
  titlePanel("QuantIT Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      
      p("Copy and Paste 96-well format spreadsheet from plate reader:"),
      textInput("pasteinput", label = "Paste Spreadsheet", placeholder = "paste spreadsheet here"),
      
      fluidRow(
        column(6,
               selectInput(inputId = "standardColumn", "Standard Column", selected = 1, choices = 1:12)
               ),
        column(6,
               numericInput("standardAmount", "Standard Amount (uL)", value = 10)
               ),
      ),
      numericInput("sampleAmount", "Sample Amount (uL)", value = 2, width = "45%"),
      downloadButton("transformedTableDownload", label = "Download As Table"),
      downloadButton("transformedTableListDownload", label = "Download As List"),
    ),
    
    mainPanel(
      p("Input Table:"),
      tableOutput("tablePreview"),
      
      fluidRow(
        column(8,
      plotOutput("regressionPlot"),),
      column(4,
      textOutput("regressionParams")
      ),
      ),
      p("Output Table:"),
      tableOutput("transformedTable")
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
      pasteInput()}, rownames = T, colnames = T, digits = 1)
    
    regressionDF <- reactive({
      req(input$pasteinput)
      data.frame(dna.amount = standard.default.concentrations * input$standardAmount, 
                 standard.signal = pasteInput()[,as.numeric(input$standardColumn)])
    })
    
    regressionParams <- reactive({
      regression.results <- getRegression(x = regressionDF()$dna.amount, y = regressionDF()$standard.signal)
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
    
    transformedTable <- reactive({
      temp.table <- pasteInput() - regressionParams()$yint
      temp.table <- temp.table/regressionParams()$slope
      temp.table <- temp.table/input$sampleAmount
    })
    
    output$transformedTable <- renderTable({transformedTable()}, rownames = T, colnames = T, digits = 2)
    
    output$transformedTableDownload <- downloadHandler(
      filename = function() {
        paste0("quantit-normalized_",as.character(Sys.Date()),".txt")
      },
      content = function(file) {
        write.table(transformedTable(), file, col.names =NA, row.names = T, quote = F, sep = "\t")
      }
    )
    
    transformedTableList <- reactive({
      data.frame(x = as.numeric(transformedTable()))
    })
    
    output$transformedTableListDownload <- downloadHandler(
      filename = function() {
        paste0("quantit-normalized-list_",as.character(Sys.Date()),".txt")
      },
      content = function(file) {
        write.table(transformedTableList(), file, quote = F, row.names = F, col.names = F, sep = '\t')
        
      }
    )
    
}

shinyApp(ui = ui, server = server)

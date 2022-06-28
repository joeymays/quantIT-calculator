
library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Quant-iT Normalizer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            fileInput("plate_upload", "Upload Plate Reader Measurements",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            radioButtons("plate_size", label = "Plate Format",
                         choices = list("96-Well" = 1), 
                         selected = 1),
            numericInput("std_col", label = "Standard Column", min = 1, max = 12, value = 12, step = 1),
            numericInput("std_amount", label = "Standard Volume (uL)", min = 1, max = 30, value = 10),
            numericInput("sample_amount", label = "Sample Volume (uL)", min = 1, max = 30, value = 2),
            downloadButton("plate.matrix.dl", label = "Download As Plate"),
            p(" "),
            downloadButton("plate.list.dl", label = "Download As List"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::DTOutput("plate.norm.out"),
           
        )
    )
)

server <- function(input, output) {

    std.stock <- c(0,0.5,1,2,4,6,8,10)
    
    plate.upload <- reactive({infile <- input$plate_upload
    if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
    }
    read.csv(infile$datapath, header = T, row.names = 1, stringsAsFactors = F)
    })
    
    plate.subset <- reactive({
      if(is.null(plate.upload())){
        return(NULL)
      }
      plate.upload()[1:8,1:12]
    })
    
    
    
    
    plate.norm <- reactive({
      if(is.null(plate.upload())){
        return(NULL)
      }
        std.abs <- std.stock * input$std_amount
        regression <- lm(std.abs ~ plate.upload()[,input$std_col])
        plate.norm <- round(((plate.upload() * regression$coefficients[2] + regression$coefficients[1]) / input$sample_amount),digits = 4)
        })
    
    output$plate.norm.out <- DT::renderDT({DT::datatable(plate.norm(), filter = "none", options = list(paging = F, searching = F, ordering =F))})
    
    output$plate.matrix.dl <- downloadHandler(
        filename = function() {
            paste0("normalized-plate_",as.character(Sys.Date()),".csv")
        },
        content = function(file) {
            write.table(plate.norm(), file, col.names =NA, row.names = T, quote = F, sep = ",")
        }
    )
    
    plate.list <- reactive({
        x <- expand.grid(c("A", "B", "C", "D", "E", "F", "G", "H"), c(1:12))
        y <- matrix(c(c(paste0(x[,1], x[,2])), unlist(plate.norm())), ncol = 2)
    })
    
    output$plate.list.dl <- downloadHandler(
        filename = function() {
            paste0("normalized-plate-list_",as.character(Sys.Date()),".txt")
        },
        content = function(file) {
            write.table(plate.list(), file, quote = F, row.names = F, col.names = F, sep = '\t')
            
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

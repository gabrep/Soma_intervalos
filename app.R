# SOMA DE INTERVALOS
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

soma.maria <- function(x, z) {
  y <- ncol(x)
  x <- x[,-y]
  rownames(x) <- x$V1
  x$V1 <- NULL
  fac <- (seq_len(nrow(x))-1) %/% z
  w <- apply(x, 2, function(v) tapply(v, fac, sum))
  w <- as.data.frame(w)
  x <- x[seq(1, nrow(x), by=z),]
  w$V1 <- rownames(x)
  w <- w %>% select(V1, everything())
  return(w)
}

ui <- fluidPage(
  titlePanel("Selecionar Linhas em Intervalos"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Selecione um arquivo:",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      numericInput("intervalo", "Selecione o intervalo:", value = 5, min = 1),
      actionButton("selecionar", "Selecionar"),
      downloadButton("download_dados", "Download dos dados selecionados"),
      img(src = "Logo_lab.png", height=573/5.5, width=1200/5.5),
    ),
    mainPanel(
      tableOutput("dados_selecionados")
    )
  )
)

server <- function(input, output, session) {
  dados <- reactive({
    req(input$file)
    read.delim(input$file$datapath, header = FALSE, sep = " ")
  })
  
  selecao <- eventReactive(input$selecionar, {
    soma.maria(dados(), input$intervalo)
  })
  
  output$dados_selecionados <- renderTable({
    selecao()
  })
  
  output$download_dados <- downloadHandler(
    filename = function() {
      paste0("dados_selecionados_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.csv(selecao(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

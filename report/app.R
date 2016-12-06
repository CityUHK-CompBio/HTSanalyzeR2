library(shiny)
library(DT)
library(igraph)
library(dplyr)
library(HTSanalyzeR2)

## the input data is a list, list(gsca = gsca, nwa = nwa, gscaNodeOptions = list(), nwaNodeOptions = list())
results <- readRDS(file = "./results.RData")
gsca <- results$gsca
gscaNodeOpts <- results$gscaNodeOptions
nwa <- results$nwa
nwaNodeOpts <- results$nwaNodeOptions

source("common.R")

if(!is.null(gsca)) {
  gsca <- appendLinks(gsca)
  gsca <- combineResults(gsca)
}

if(!is.null(gsca)) {
  if(file.exists("gsca_summary.md"))  file.remove("gsca_summary.md")
  knitr::knit("gsca_summary.Rmd", "gsca_summary.md")
}
if(!is.null(nwa)) {
  if(file.exists("nwa_summary.md"))  file.remove("nwa_summary.md")
  knitr::knit("nwa_summary.Rmd", "nwa_summary.md")
}



availableResults <- function(results, byRow = TRUE) {
  res <- c("HyperGeo", "GSEA", "Significant in both")
  if(byRow) {
    res <- res[which(!is.na(rowSums(results)))]
  } else {
    res <- colnames(results)[colSums(results, na.rm = TRUE) > 0]
  }
  res
}

namesToList <- function(li) {
  res <- as.list(names(li))
  names(res) <- names(li)
  res
}

createPanel <- function(tab = "enrich_result") {
  switch(tab,
         enrich_result = tabPanel("Enrichment Results",
                                  sidebarLayout(
                                    sidebarPanel(
                                      includeMarkdown("gsca_summary.md"),
                                      hr(),
                                      selectInput('analysis', 'Analysis', availableResults(gsca@summary$results, TRUE)),
                                      selectInput('genesets', 'Gene Sets Collection', c(availableResults(gsca@summary$results, FALSE), "ALL"))
                                    ),

                                    # Show gsca results in the main panel
                                    mainPanel(
                                      dataTableOutput("gsca_output")
                                    )
                                  )),
         enrich_map = tabPanel("Enrichment Map",
                               sidebarLayout(
                                 sidebarPanel(
                                   includeMarkdown("gsca_summary.md"),
                                   hr(),
                                   selectInput('analysis2', 'Analysis', availableResults(gsca@summary$results, TRUE)[-3]),
                                   selectInput('genesets2', 'Gene Sets Collection', availableResults(gsca@summary$results, FALSE)),
                                   radioButtons("nodename", "Node name", c("ID"="id", "Term"="term"), inline = TRUE),

                                   fluidRow(
                                     selectInput("selection", label = h4("Node Sets"), choices = c("All" = 'all', namesToList(gscaNodeOpts) ,"Selection Mode" = 'selection'),selected = 1),

                                     h4("View Options"),
                                     fluidRow(
                                       column(3, checkboxInput("label_visible", label = "Label",  value = TRUE)),
                                       column(3, checkboxInput("pause", label = "Pause",  value = FALSE))
                                     ),
                                     radioButtons("shape", label = "Shape", choices = list("Circle" = "circle", "Rect" = "rect"), inline = TRUE, selected = "circle"),
                                     sliderInput("scale",label = "Scale", min = 0.2, max = 3, value = 1),
                                     sliderInput("dist", "Distance", 10, 300, value = 100, step = 10),
                                     sliderInput("charge", "Charge", -1000, -100, value = -600, step = 50)
                                   )
                                 ),

                                 # Show gsca results in the main panel
                                 mainPanel(
                                   forceGraphOutput("network_output", height = "800px")
                                 )
                               )),
         network = tabPanel("Network Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                includeMarkdown("nwa_summary.md"),
                                hr(),

                                fluidRow(
                                  selectInput("selection2", label = h4("Node Sets"), choices = c("All" = 'all', namesToList(nwaNodeOpts) ,"Selection Mode" = 'selection'),selected = 1),

                                  h4("View Options"),
                                  fluidRow(
                                    column(3, checkboxInput("label_visible2", label = "Label",  value = TRUE)),
                                    column(3, checkboxInput("pause2", label = "Pause",  value = FALSE))
                                  ),
                                  radioButtons( "shape2", label = "Shape", choices = list("Circle" = "circle", "Rect" = "rect"), inline = TRUE, selected = "circle"),
                                  sliderInput("scale2",label = "Scale", min = 0.2, max = 3, value = 1),
                                  sliderInput("dist2", "Distance", 10, 300, value = 70, step = 10),
                                  sliderInput("charge2", "Charge", -1000, -100, value = -300, step = 50),
                                  sliderInput("process", "Process", 0, 30, value = 30, step = 1, animate = TRUE)
                                )
                              ),
                              # Show subnetwork results in the main panel
                              mainPanel(
                                forceGraphOutput("subnetwork_output", height = "800px")
                              )
                            ))
  )
}

ui <- NULL
if(!is.null(gsca) && !is.null(nwa)) {
  ui <- navbarPage("HTSanalyzeR2", createPanel("enrich_result"), createPanel("enrich_map"), createPanel("network"))
} else {
  if(is.null(gsca))
    ui <- navbarPage("HTSanalyzeR2", createPanel("network"))
  else
    ui <- navbarPage("HTSanalyzeR2", createPanel("enrich_result"), createPanel("enrich_map"))
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## response update
  observeEvent(input$pause, {
    output$network_output <- updateForceGraph(list(pause = input$pause))
  })
  observeEvent(input$selection, {
    output$network_output <- updateForceGraph(list(selection = input$selection))
  })
  observeEvent(input$shape, {
    output$network_output <- updateForceGraph(list(shape = input$shape))
  })
  observeEvent(input$label_visible, {
    output$network_output <- updateForceGraph(list(label = input$label_visible))
  })
  observeEvent(input$scale, {
    output$network_output <- updateForceGraph(list(scale = input$scale))
  })
  observeEvent(input$charge, {
    output$network_output <- updateForceGraph(list(charge = input$charge))
  })
  observeEvent(input$dist, {
    output$network_output <- updateForceGraph(list(distance = input$dist))
  })



  observeEvent(input$pause2, {
    output$subnetwork_output <- updateForceGraph(list(pause = input$pause2))
  })
  observeEvent(input$selection2, {
    output$subnetwork_output <- updateForceGraph(list(selection = input$selection2))
  })
  observeEvent(input$shape2, {
    output$subnetwork_output <- updateForceGraph(list(shape = input$shape2))
  })
  observeEvent(input$label_visible2, {
    output$subnetwork_output <- updateForceGraph(list(label = input$label_visible2))
  })
  observeEvent(input$scale2, {
    output$subnetwork_output <- updateForceGraph(list(scale = input$scale2))
  })
  observeEvent(input$charge2, {
    output$subnetwork_output <- updateForceGraph(list(charge = input$charge2))
  })
  observeEvent(input$dist2, {
    output$subnetwork_output <- updateForceGraph(list(distance = input$dist2))
  })
  observeEvent(input$process, {
    output$subnetwork_output <- updateForceGraph(list(process = input$process))
  })


  ## response reconstruct
  observeEvent(input$analysis2, {
    # enrichment map
    output$network_output <- updateNetwork(gsca, input)
  })

  observeEvent(input$genesets2, {
    # enrichment map
    output$network_output <- updateNetwork(gsca, input)
  })

  observeEvent(input$nodename, {
    # enrichment map
    output$network_output <- updateNetwork(gsca, input)
  })

  observeEvent(input$analysis, {
    output$gsca_output <- renderDataTable(selectDT(gsca, input))
  })
  observeEvent(input$genesets, {
    output$gsca_output <- renderDataTable(selectDT(gsca, input))
  })

  ## TODO: undefined behavior

  observeEvent({42}, {
    options <- list(charge = input$charge2, distance = input$dist2)
    output$subnetwork_output <- renderForceGraph(viewSubNet(nwa, nwaNodeOpts, options))
  })

  }

updateNetwork <- function(gsca, input) {
  options <- list(charge = input$charge, distance = input$dist)
  renderForceGraph(viewEnrichMap(gsca, resultName=paste0(input$analysis2, ".results"),
                                 gscs = c(input$genesets2),
                                 allSig=F, ntop = 30, gsNameType=input$nodename,
                                 nodeOptions = gscaNodeOpts, options = options))
}


# Run the application
shinyApp(ui = ui, server = server)

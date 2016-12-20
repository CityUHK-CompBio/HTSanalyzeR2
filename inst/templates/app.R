library(shiny)
library(DT)
library(igraph)
library(dplyr)
library(HTSanalyzeR2)

## ====================== Loading results ======================
## results: list(gsca = gsca, nwa = nwa, gscaNodeOptions = list(), nwaNodeOptions = list())
results <- readRDS(file = "./results.RData")
gsca <- results$gsca
nwa <- results$nwa
gscaNodeOpts <- results$gscaNodeOptions
nwaNodeOpts <- results$nwaNodeOptions


## ====================== Preprocessing ========================
processSlider <- NULL

if(!is.null(gsca)) {
  gsca <- HTSanalyzeR2:::appendLinks(gsca)
  gsca <- HTSanalyzeR2:::combineResults(gsca)

  availableAnalysis <- HTSanalyzeR2:::availableResults(gsca@summary$results, TRUE)
  availableGeneSets <- HTSanalyzeR2:::availableResults(gsca@summary$results, FALSE)
}
if(!is.null(nwa)) {
  if(is.matrix(nwa@phenotypes)) {
    seriesTicks <- colnames(nwa@phenotypes)
    processSlider <- sliderInput("process_net", "Process", 1, length(seriesTicks), value = length(seriesTicks), step = 1, animate = animationOptions(interval=1500))
  } else {
    processSlider <- sliderInput("process_net", "Process", 1, 2, 2, step = 1, animate = animationOptions(interval=800))
  }
}
file.remove(dir(".", pattern = "*\\.md", full.names = TRUE))
if (!is.null(gsca))  knitr::knit("gsca_summary.Rmd", "gsca_summary.md")
if (!is.null(nwa))  knitr::knit("nwa_summary.Rmd", "nwa_summary.md")

namesToList <- HTSanalyzeR2:::namesToList

## ==================== Define ui and server ====================
createPanelList <- function(tab) {
  sidebarStype <- "overflow-x:hidden; overflow-y:scroll; max-height:100vh;"

  switch(tab,
         enrich_result = list(style = sidebarStype,
                              includeMarkdown("gsca_summary.md"),
                              hr(),
                              selectInput('analysis_res', 'Analysis', availableAnalysis),
                              selectInput('genesets_res', 'Gene Sets Collection', c(availableGeneSets, "ALL"))),
         enrich_map = list(style = sidebarStype,
                           # includeMarkdown("gsca_summary.md"),
                           # hr(),
                           h3("Enrichment Map"),
                           selectInput('analysis_map', 'Analysis', availableAnalysis[-3]),
                           selectInput('genesets_map', 'Gene Sets Collection', availableGeneSets),
                           hr(),
                           selectInput("selection_map", h4("Node Sets"), choices = c("All" = 'all', namesToList(gscaNodeOpts) ,"Selection Mode" = 'selection'), selected = 1),
                           h4("View Options"),
                           fluidRow(
                             column(3, checkboxInput("label_visible_map", "Label",  value = TRUE)),
                             column(3, checkboxInput("pause_map", "Pause",  value = FALSE))
                           ),
                           radioButtons("nodename_map", "Node name", c("ID" = "id", "Term" = "term"), inline = TRUE),
                           radioButtons("shape_map", "Shape", c("Circle" = "circle", "Rect" = "rect"), inline = TRUE),
                           radioButtons("color_map", label = "Color Scheme", choices = list("Default" = "default", "Scheme 1" = "scheme1"), inline = TRUE, selected = "default"),
                           sliderInput("scale_map", "Scale", min = 0.2, max = 3, value = 1),
                           sliderInput("dist_map", "Distance", 10, 300, value = 100, step = 10),
                           sliderInput("charge_map", "Charge", -1000, -100, value = -600, step = 50)),
         network = list(style = sidebarStype,
                        includeMarkdown("nwa_summary.md"),
                        hr(),
                        selectInput("selection_net", label = h4("Node Sets"), choices = c("All" = 'all', namesToList(nwaNodeOpts) ,"Selection Mode" = 'selection'), selected = 1),
                        h4("View Options"),
                        fluidRow(
                          column(3, checkboxInput("label_visible_net", label = "Label",  value = TRUE)),
                          column(3, checkboxInput("pause_net", label = "Pause",  value = FALSE))
                        ),
                        radioButtons("shape_net", label = "Shape", choices = list("Circle" = "circle", "Rect" = "rect"), inline = TRUE, selected = "circle"),
                        radioButtons("color_net", label = "Color Scheme", choices = list("Default" = "default", "Scheme 1" = "scheme1"), inline = TRUE, selected = "default"),
                        sliderInput("scale_net",label = "Scale", min = 0.2, max = 3, value = 1),
                        sliderInput("dist_net", "Distance", 10, 300, value = 70, step = 10),
                        sliderInput("charge_net", "Charge", -1000, -100, value = -300, step = 50),
                        processSlider)
  )
}

createTab <- function(tab) {
  fontCssLink <- tags$head(tags$link(rel="stylesheet", href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"))
  switch(tab,
         enrich_result = tabPanel("Enrichment Results",
                                  sidebarLayout(
                                    do.call(sidebarPanel, createPanelList(tab)),
                                    mainPanel(
                                      dataTableOutput("gsca_output")
                                    )
                                  )),
         enrich_map = tabPanel("Enrichment Map", fontCssLink,
                               sidebarLayout(
                                 do.call(sidebarPanel, createPanelList(tab)),
                                 mainPanel(
                                   forceGraphOutput("network_output", height = "800px")
                                 )
                               )),
         network = tabPanel("Network Analysis", fontCssLink,
                            sidebarLayout(
                              do.call(sidebarPanel, createPanelList(tab)),
                              mainPanel(
                                forceGraphOutput("subnetwork_output", height = "800px")
                              )
                            ))
  )
}


ui <- NULL
if(!is.null(gsca) && !is.null(nwa)) {
  ui <- navbarPage("HTSanalyzeR2", createTab("enrich_result"), createTab("enrich_map"), createTab("network"))
} else if(!is.null(gsca)) {
  ui <- navbarPage("HTSanalyzeR2", createTab("enrich_result"), createTab("enrich_map"))
} else {
  ui <- navbarPage("HTSanalyzeR2", createTab("network"))
}


server <- function(input, output, session) {
  ## response update
  observeEvent(input$pause_map, {
    output$network_output <- updateForceGraph(list(pause = input$pause_map))
  })
  observeEvent(input$selection_map, {
    output$network_output <- updateForceGraph(list(selection = input$selection_map))
  })
  observeEvent(input$shape_map, {
    output$network_output <- updateForceGraph(list(shape = input$shape_map))
  })
  observeEvent(input$color_map, {
    output$network_output <- updateForceGraph(list(color = input$color_map))
  })
  observeEvent(input$label_visible_map, {
    output$network_output <- updateForceGraph(list(label = input$label_visible_map))
  })
  observeEvent(input$scale_map, {
    output$network_output <- updateForceGraph(list(scale = input$scale_map))
  })
  observeEvent(input$charge_map, {
    output$network_output <- updateForceGraph(list(charge = input$charge_map))
  })
  observeEvent(input$dist_map, {
    output$network_output <- updateForceGraph(list(distance = input$dist_map))
  })
  observeEvent(input$nodename_map, {
    output$network_output <- updateForceGraph(list(nodename = input$nodename_map))
  })


  observeEvent(input$pause_net, {
    output$subnetwork_output <- updateForceGraph(list(pause = input$pause_net))
  })
  observeEvent(input$selection_net, {
    output$subnetwork_output <- updateForceGraph(list(selection = input$selection_net))
  })
  observeEvent(input$shape_net, {
    output$subnetwork_output <- updateForceGraph(list(shape = input$shape_net))
  })
  observeEvent(input$color_net, {
    output$subnetwork_output <- updateForceGraph(list(color = input$color_net))
  })

  observeEvent(input$label_visible_net, {
    output$subnetwork_output <- updateForceGraph(list(label = input$label_visible_net))
  })
  observeEvent(input$scale_net, {
    output$subnetwork_output <- updateForceGraph(list(scale = input$scale_net))
  })
  observeEvent(input$charge_net, {
    output$subnetwork_output <- updateForceGraph(list(charge = input$charge_net))
  })
  observeEvent(input$dist_net, {
    output$subnetwork_output <- updateForceGraph(list(distance = input$dist_net))
  })
  observeEvent(input$process_net, {
    output$subnetwork_output <- updateForceGraph(list(process = input$process_net))
  })


  ## response reconstruct
  observeEvent(input$analysis_map, {
    output$network_output <- createNetwork(gsca, input)
  })

  observeEvent(input$genesets_map, {
    output$network_output <- createNetwork(gsca, input)
  })

  observeEvent(input$analysis_res, {
    output$gsca_output <- renderDataTable(selectDT(gsca, input$analysis_res, input$genesets_res))
  })

  observeEvent(input$genesets_res, {
    output$gsca_output <- renderDataTable(selectDT(gsca, input$analysis_res, input$genesets_res))
  })


  # observeEvent({42}, {
  #   options <- list(charge = input$charge_map, distance = input$dist_map)
  #   output$gsca_output <- renderForceGraph(viewEnrichMap(gsca,
  #                                 resultName=paste0(input$analysis_map, ".results"),
  #                                 gscs = c(input$genesets_map),
  #                                 allSig=TRUE, gsNameType=input$nodename_map,
  #                                 nodeOptions = gscaNodeOpts, options = options))
  # })

  ## TODO: undefined behavior
  observeEvent({42}, {
    options <- list(charge = input$charge_net, distance = input$dist_net)
    output$subnetwork_output <- renderForceGraph(viewSubNet(nwa, nwaNodeOpts, options))
  })

}


## ==================== Helper functions ====================
createNetwork <- function(gsca, input) {
  options <- list(charge = input$charge_map, distance = input$dist_map)
  renderForceGraph(viewEnrichMap(gsca, resultName=paste0(input$analysis_map, ".results"),
                                 gscs = c(input$genesets_map),
                                 allSig=TRUE, gsNameType=input$nodename_map,
                                 nodeOptions = gscaNodeOpts, options = options))
}

selectDT <- function(gsca, analysis, genesets) {
  jsRender <- JS("function(data, type, row, meta) { return type === 'display' && Number(data) < 0.001 ? '<0.001' : data }")
  jsCallback <- JS("table.page(0).draw(false)")

  if (analysis == "Significant in both") {
    analysis <- "Sig.adj.pvals.in.both"
    res <- gsca@result[[analysis]][[genesets]]
    res$HyperGeo.Adj.Pvalue <- round(res$HyperGeo.Adj.Pvalue, digits = 3)
    res$GSEA.Adj.Pvalue <- round(res$GSEA.Adj.Pvalue, digits = 3)

    dt <- DT::datatable(res, filter = 'top', rownames = FALSE, escape = FALSE,
                        options = list(pageLength = 10,
                                       columnDefs = list(list(
                                         targets = which(colnames(res) %in% c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue")) - 1,
                                         render = jsRender))), callback = jsCallback)
  } else {
    analysis <- paste0(analysis, ".results")
    res <- gsca@result[[analysis]][[genesets]]

    res$Pvalue <- round(res$Pvalue, digits = 3)
    res$Adjusted.Pvalue <- round(res$Adjusted.Pvalue, digits = 3)

    dt <- DT::datatable(res, filter = 'top', rownames = FALSE, escape = FALSE,
                        options = list(pageLength = 10,
                                       columnDefs = list(list(
                                         targets = which(colnames(res) %in% c("Pvalue", "Adjusted.Pvalue")) - 1,
                                         render = jsRender))), callback = jsCallback) %>%
      formatStyle('Adjusted.Pvalue', target = "row",
                  fontWeight = styleInterval(gsca@para$pValueCutoff, c('bold', 'weight'))
      )

    if (analysis == "HyperGeo.results") dt <- dt %>% formatRound("Expected.Hits", digits = 3)
    if (analysis == "GSEA.results") dt <- dt %>% formatSignif("Observed.score", digits = 3)
  }
  dt
}


# Run the application
shinyApp(ui = ui, server = server)

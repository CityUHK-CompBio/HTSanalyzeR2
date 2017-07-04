library(shiny)
library(shinythemes)
library(DT)
library(igraph)
library(dplyr)
library(HTSanalyzeR2)


## ============================================ Loading results ============================================
## results: list(gsca = gsca, nwa = nwa)
results <- readRDS(file = "./results.RData")
gsca <- results$gsca
nwa <- results$nwa
gscaObjs <- results$gsca

## ============================================ Preprocessing ==============================================
gscaTS <- !is.null(gsca) && class(gsca) == "list"
nwaTS <- !is.null(nwa) && is.matrix(nwa@phenotypes)

if(!is.null(gsca)) {
  gscaSeriesTicks <- NULL
  gscaSeriesTickInput <- NULL
  gscaProcessSlider <- NULL
  if(gscaTS) {
    gscaSeriesTicks <- names(gscaObjs)
    gscaSeriesTickInput <- selectInput('series_tick_res', 'Series Tick', gscaSeriesTicks)
    gscaProcessSlider <- sliderInput("process_map", h3("Process"), 1, length(gscaSeriesTicks), value = 1, step = 1, animate = animationOptions(interval=500))
    for(name in gscaSeriesTicks) {
      gscaObjs[[name]] <- HTSanalyzeR2:::appendLinks(gscaObjs[[name]])
      gscaObjs[[name]] <- HTSanalyzeR2:::combineResults(gscaObjs[[name]])
    }
    gsca <- gscaObjs[[1]]
  } else {
    gsca <- HTSanalyzeR2:::appendLinks(gsca)
    gsca <- HTSanalyzeR2:::combineResults(gsca)
  }

  availableAnalysis <- HTSanalyzeR2:::availableResults(gsca@summary$results, TRUE)
  availableGeneSets <- HTSanalyzeR2:::availableResults(gsca@summary$results, FALSE)
}

if(!is.null(nwa)) {
  # sliderInput("process_net", h3("Process"), 0, 1, 1, step = 1, animate = animationOptions(interval=800))
  nwaProcessSlider <- NULL
  if(nwaTS) {
    seriesTicks <- colnames(nwa@phenotypes)
    nwaProcessSlider <- sliderInput("process_net", h3("Process"), 0, length(seriesTicks), value = length(seriesTicks), step = 1, animate = animationOptions(interval=1000))
  }
}

file.remove(dir(".", pattern = "*\\.md", full.names = TRUE))
if (!is.null(gsca))  knitr::knit("gsca_summary.Rmd", "gsca_summary.md")
if (!is.null(nwa))  knitr::knit("nwa_summary.Rmd", "nwa_summary.md")

namesToList <- HTSanalyzeR2:::namesToList


## =========================================== Helper functions ============================================
create_sidebar_tab <- function(name, sidebar, content) {
  tabPanel(name, fluidRow(column(width = 3, sidebar), column(width = 9, content)))
}

create_sidebar_setting_tab <- function(name, sidebar, settings, content) {
  tabPanel(name, fluidRow(column(width = 3, sidebar), column(width = 9, settings, content)))
}


trim_result <- function(result, digits = 3) {
  signif_cols <- c("Observed.score")
  round_cols <- c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue", "Pvalue", "Adjusted.Pvalue", "Expected.Hits")

  cols <- intersect(colnames(result), round_cols)
  result[cols] <- round(result[cols], digits = 3)
  cols <- intersect(colnames(result), signif_cols)
  result[cols] <- signif(result[cols], digits = 3)

  result
}

create_data_table <- function(gscaObj, analysis, genesets) {
  jsRender <- JS("function(data, type, row, meta) { return type === 'display' && Number(data) < 0.001 ? '<0.001' : data }")
  jsCallback <- JS("table.page(0).draw(false)")

  analysis_to_show <- ifelse(analysis == "Significant in both", "Sig.adj.pvals.in.both", paste0(analysis, ".results"))
  res <- gscaObj@result[[analysis_to_show]][[genesets]]
  res <- trim_result(res, digits = 3)

  target_cols <- c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue", "Pvalue", "Adjusted.Pvalue")
  dt_options <- list(pageLength = 10,
                     columnDefs = list(list(targets = which(colnames(res) %in% target_cols) - 1, render = jsRender)))

  dt <- DT::datatable(res, filter = 'top', rownames = FALSE, escape = FALSE, options = dt_options, callback = jsCallback)
  if (analysis != "Significant in both") {
    dt <- formatStyle(dt, 'Adjusted.Pvalue', target = "row", fontWeight = styleInterval(gscaObj@para$pValueCutoff, c('bold', 'weight')))
  }
  dt
}

create_enrich_map <- function(gscaObj, input) {
  options <- list(charge = -400, distance = 200)
  viewEnrichMap(gscaObj,
                resultName=paste0(input$analysis_map, ".results"),
                gscs = c(input$genesets_map),
                allSig=TRUE,
                gsNameType="id",
                options = options,
                seriesObjs = gscaObjs)
}

create_network <- function(nwaObj) {
  options <- list(charge = -200, distance = 150)
  viewSubNet(nwaObj, options = options)
}


## ============================================ Define ui ==================================================
create_panel <- function(name) {
  switch(name,
         enrich_res_sidebar = wellPanel(
           includeMarkdown("gsca_summary.md"),
           hr(), gscaSeriesTickInput,
           selectInput('analysis_res', 'Analysis', availableAnalysis),
           selectInput('genesets_res', 'Gene Sets Collection', c(availableGeneSets, "ALL"))),
         enrich_res_content = dataTableOutput("gsca_output"),

         settings = includeHTML("settings.html"),
         enrich_map_sidebar = wellPanel(
           h3("Enrichment Map"),
           selectInput('analysis_map', 'Analysis', availableAnalysis[-3]),
           selectInput('genesets_map', 'Gene Sets Collection', availableGeneSets),
           hr(), gscaProcessSlider),
         enrich_map_content = forceGraphOutput("map_output"),

         network_sidebar = wellPanel(
           includeMarkdown("nwa_summary.md"),
           hr(), nwaProcessSlider),
         network_content = forceGraphOutput("network_output")
  )
}

tabs <- list()
if(!is.null(gsca)) {
  tabs <- c(tabs, list(create_sidebar_tab("Enrichment Result", create_panel("enrich_res_sidebar"), create_panel("enrich_res_content"))))
  tabs <- c(tabs, list(create_sidebar_setting_tab("Enrichment Map", create_panel("enrich_map_sidebar"), create_panel("settings"), create_panel("enrich_map_content"))))
}
if(!is.null(nwa)) {
  tabs <- c(tabs, list(create_sidebar_setting_tab("Network Analysis", create_panel("network_sidebar"), create_panel("settings"), create_panel("network_content"))))
}

ui <- do.call(navbarPage, c(list(title="HTSanalyzeR2", fluid = TRUE, theme = shinytheme("yeti")), tabs))

## ============================================ Define server ==============================================
server <- function(input, output, session) {
  observeEvent({input$series_tick_res
    input$analysis_res
    input$genesets_res}, {
      obj <- gsca
      if(gscaTS) {
        obj <- gscaObjs[[input$series_tick_res]]
      }
      output$gsca_output <- renderDataTable(create_data_table(obj, input$analysis_res, input$genesets_res))
    })

  observeEvent(input$process_map, {
    output$map_output <- updateForceGraph(list(process = input$process_map))
  })

  observeEvent({input$analysis_map
    input$genesets_map}, {
      output$map_output <- renderForceGraph(create_enrich_map(gsca, input))
    })

  observeEvent(input$process_net, {
    output$network_output <- updateForceGraph(list(process = input$process_net))
  })

  ## TODO: undefined behavior
  observeEvent({42}, {
    output$network_output <- renderForceGraph(create_network(nwa))
  })
}

## ============================================ Run application ============================================
shinyApp(ui, server)

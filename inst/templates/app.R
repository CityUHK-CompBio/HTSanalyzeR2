library(shiny)
library(shinydashboard)
library(DT)
library(colourpicker)
library(HTSanalyzeR2)

## ============================================ Loading results ============================================
## results: list(gsca = gsca, nwa = nwa)
results <- readRDS(file = "./results.RData")
gsca <- results$gsca
nwa <- results$nwa
gscaObjs <- NULL
nwaObjs <- NULL
specificGeneset <- results$specificGeneset
cutoff <- results$cutoff

## ============================================ Preprocessing ==============================================
gscaTS <- !is.null(gsca) && class(gsca) == "list"
nwaTS <- !is.null(nwa) && class(nwa) == "list"

if(!is.null(gsca)) {
  gscaSeriesTickInput <- NULL
  gscaProcessSlider <- NULL
  gscaTimePointName <- NULL
  if(gscaTS) {
    gscaObjs <- gsca
    gscaSeriesTicks <- names(gscaObjs)
    gscaSeriesTickInput <- selectInput('series_tick_res', 'Experiment', gscaSeriesTicks)
    gscaProcessSlider <- sliderInput("process_map", "Experiment", 1, length(gscaSeriesTicks), value = 1, step = 1, animate = animationOptions(interval=1000))
    gscaTimePointName <- menuItemOutput("series_tick_name")
    for(name in gscaSeriesTicks) {
      gscaObjs[[name]] <- HTSanalyzeR2:::appendLinks(gscaObjs[[name]])
      gscaObjs[[name]] <- HTSanalyzeR2:::combineResults(gscaObjs[[name]])
    }
    gsca <- gscaObjs[[1]]
  } else {
    gsca <- HTSanalyzeR2:::appendLinks(gsca)
    gsca <- HTSanalyzeR2:::combineResults(gsca)
  }


  availableAnalysis <- HTSanalyzeR2:::availableResults(gsca@result, TRUE)
  availableGeneSets <- HTSanalyzeR2:::availableResults(gsca@summary$results, FALSE)
  #
  # availableAnalysis <- HTSanalyzeR2:::availableResults(gsca@summary$results, TRUE)
  # availableGeneSets <- HTSanalyzeR2:::availableResults(gsca@summary$results, FALSE)
  specificGenesetItem <- NULL
  if(!is.null(specificGeneset)) {
    specificGenesetItem = "SpecificGeneset"
  }

}

if(!is.null(nwa)) {
  nwaProcessSlider <- NULL
  if(nwaTS) {
    nwaObjs <- nwa
    nwaSeriesTicks <- names(nwaObjs)
    nwaProcessSlider <- sliderInput("process_net", "Process", 1, length(nwaSeriesTicks), value = 1, step = 1, animate = animationOptions(interval=1000))
    nwa <- nwaObjs[[1]]
  }
}

HTMLSettings <- system.file("templates/settings.html", package="HTSanalyzeR2")
namesToList <- HTSanalyzeR2:::namesToList

## =========================================== Helper functions ============================================
trim_result <- function(result, digits = 3) {
  signif_cols <- c("Observed.score")
  round_cols <- c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue", "Pvalue", "Adjusted.Pvalue", "Expected.Hits")

  cols <- intersect(colnames(result), round_cols)
  result[cols] <- round(result[cols], digits = 3)
  cols <- intersect(colnames(result), signif_cols)
  result[cols] <- signif(result[cols], digits = 3)

  result
}

colValueRender <- function(minZeroValue) {
  JS(paste("function(data, type, row, meta) { return type === 'display' && Number(data) == 0 ? '<", minZeroValue, "' : data }"))
}

create_data_table <- function(gscaObj, analysis, genesets) {
  analysis_to_show <- ifelse(analysis == "Significant in both", "Sig.adj.pvals.in.both", paste0(analysis, ".results"))
  res <- gscaObj@result[[analysis_to_show]][[genesets]]
  res <- trim_result(res, digits = 3)


  gseaDispValue <- ifelse(is.null(gscaObj@para$nPermutations), "0",
                          paste(format((1/gscaObj@para$nPermutations), scientific=TRUE, digits = 1), sep = ""))
  jsRenders <- list(GSEA = colValueRender(gseaDispValue), HyperGeo = colValueRender('1e-06'))

  columnDefs <- list(list(targets = which(colnames(res) == "HyperGeo.Adj.Pvalue") - 1, render = jsRenders[["HyperGeo"]]),
                     list(targets = which(colnames(res) == "GSEA.Adj.Pvalue") - 1, render = jsRenders[["GSEA"]]))
  if (analysis != "Significant in both") {
    columnDefs <- list(list(targets = which(colnames(res) %in% c("Pvalue", "Adjusted.Pvalue")) - 1, render = jsRenders[[analysis]]))
  }

  dt_options <- list(pageLength = 13, dom = 'Bfrtip', buttons = c('copy', 'csv', 'pdf', 'print'), columnDefs = columnDefs)
  jsCallback <- JS("table.page(0).draw(false)")
  dt <- DT::datatable(res, filter = 'top', rownames = FALSE, escape = FALSE, extensions = 'Buttons', options = dt_options, callback = jsCallback)
  if (analysis != "Significant in both") {
    dt <- formatStyle(dt, 'Adjusted.Pvalue', target = "row", fontWeight = styleInterval(gscaObj@para$pValueCutoff, c('bold', 'weight')))
  }
  dt
}

ifNotNull <- function(obj, item) {
  if(is.null(obj)) return(NULL)
  item
}


## ============================================ Define ui ==================================================

header <- dashboardHeader(title = "HTSanalyzeR2", dropdownMenu(type = "messages", icon = icon("cogs"), badgeStatus = NULL))

sidebar <- dashboardSidebar(
  sidebarMenu(
    ifNotNull(gsca, menuItem("Enrichment Result",
                             tabName = "table_tab",
                             icon = icon("th-list"),
                             gscaSeriesTickInput,
                             selectInput('analysis_res', 'Analysis', availableAnalysis),
                             selectInput('genesets_res', 'Gene Sets Collection', c(availableGeneSets, "ALL"))
    )),
    ifNotNull(gsca, menuItem("Enrichment Map",
                             tabName = "map_tab",
                             icon = icon("area-chart"),
                             selectInput('analysis_map', 'Analysis', availableAnalysis[-3]),
                             selectInput('genesets_map', 'Gene Sets Collection', c(specificGenesetItem, availableGeneSets)),
                             gscaTimePointName,
                             gscaProcessSlider
    )),
    ifNotNull(nwa, menuItem("Network Analysis",
                            tabName = "network_tab",
                            icon = icon("area-chart"),
                            nwaProcessSlider))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "table_tab",
      fluidRow(
        column(width = 12,
               box(width = NULL, status = "success", solidHeader = FALSE, dataTableOutput("gsca_output"))),
        div(id = "analysis_info",
            valueBoxOutput("numGenesets", width = NULL),
            valueBoxOutput("numAboveMinimum", width = NULL),
            infoBoxOutput("para1", width = NULL),
            infoBoxOutput("para2", width = NULL),
            infoBoxOutput("para3", width = NULL),
            infoBoxOutput("para4", width = NULL),
            infoBoxOutput("para5", width = NULL))
      )
    ),

    tabItem(tabName = "map_tab",
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "success", solidHeader = TRUE,
                         title = "Enrichment Map",
                         HTSanalyzeR2:::forceGraphOutput("map_output")))
            )
    ),

    tabItem(tabName = "network_tab",
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "success", solidHeader = TRUE,
                         title = "Network Analysis",
                         HTSanalyzeR2:::forceGraphOutput("network_output"))),
              div(id = "network_info",
                  infoBoxOutput("para6", width = NULL),
                  infoBoxOutput("para7", width = NULL),
                  infoBoxOutput("para8", width = NULL),
                  infoBoxOutput("para9", width = NULL),
                  infoBoxOutput("para10", width = NULL))
            )
    ),

    tabItem(tabName = "dummy", fluidRow(
      colourpicker::colourInput("dummyColorInput", ""),
      sliderInput("dummySliderInput", "", 0, 1, 0)))
  ),

  includeHTML(HTMLSettings)
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "blue")

## ============================================ Define server ==============================================
renderGSCASummary <- function(input, output, gscaObj) {
  # HTSanalyzeR2:::generateGSCASummary(gscaObj)
  numGenesets <- ifelse(input$genesets_res != "ALL", gscaObj@summary$gsc[input$genesets_res, 1],  sum(gscaObj@summary$gsc[,1]))
  numAboveMinimum <- ifelse(input$genesets_res != "ALL", gscaObj@summary$gsc[input$genesets_res, 2],  sum(gscaObj@summary$gsc[,2]))

  output$numGenesets <- renderValueBox(valueBox(icon = icon("database"), color = "aqua",
                                                value = numGenesets,
                                                subtitle = paste("Gene sets in", input$genesets_res)))
  output$numAboveMinimum <- renderValueBox(valueBox(icon = icon("certificate"), color = "olive",
                                                    value = numAboveMinimum,
                                                    subtitle = "Above the minimum size."))

  if(input$analysis_res == "GSEA") {
    output$para1 <- renderValueBox(infoBox(color = "teal", title = "P-value Cutoff", subtitle = "Significant gene set cutoff p-value(adjusted)",
                                           value = gscaObj@summary$para$gsea[, "pValueCutoff"]))
    output$para2 <- renderValueBox(infoBox(color = "teal", title = "Minimum size", subtitle = "Minimum gene set size",
                                           value = gscaObj@summary$para$gsea[, "minGeneSetSize"]))
    output$para3 <- renderValueBox(infoBox(color = "teal", title = "Correction", subtitle = "MHT correction method",
                                           value = gscaObj@summary$para$gsea[, "pAdjustMethod"]))
    output$para4 <- renderValueBox(infoBox(color = "teal", title = "Permutations", subtitle = "Number of permutations",
                                           value = gscaObj@summary$para$gsea[, "nPermutations"]))
    output$para5 <- renderValueBox(infoBox(color = "teal", title = "Exponent", subtitle = "Exponent used",
                                           value = gscaObj@summary$para$gsea[, "exponent"]))
  } else if(input$analysis_res == "HyperGeo") {
    output$para1 <- renderValueBox(infoBox(color = "teal", title = "P-value Cutoff", subtitle = "Significant gene set cutoff p-value(adjusted)",
                                           value = gscaObj@summary$para$hypergeo[, "pValueCutoff"]))
    output$para2 <- renderValueBox(infoBox(color = "teal", title = "Minimum size", subtitle = "Minimum gene set size",
                                           value = gscaObj@summary$para$hypergeo[, "minGeneSetSize"]))
    output$para3 <- renderValueBox(infoBox(color = "teal", title = "Correction", subtitle = "MHT correction method",
                                           value = gscaObj@summary$para$hypergeo[, "pAdjustMethod"]))
    output$para4 <- renderValueBox(shiny::div(shiny::div()))
    output$para5 <- renderValueBox(shiny::div(shiny::div()))
  } else {
    output$para1 <- renderValueBox(shiny::div(shiny::div()))
    output$para2 <- renderValueBox(shiny::div(shiny::div()))
    output$para3 <- renderValueBox(shiny::div(shiny::div()))
    output$para4 <- renderValueBox(shiny::div(shiny::div()))
    output$para5 <- renderValueBox(shiny::div(shiny::div()))
  }

}

renderNWASummary <- function(input, output, nwaObj) {
  output$para6 <- renderValueBox(infoBox(color = "teal", title = "Interactome Name", subtitle = "",
                                         value = nwaObj@summary$db[, "name"]))
  output$para7 <- renderValueBox(infoBox(color = "teal", title = "Species", subtitle = "",
                                         value = nwaObj@summary$db[, "species"]))
  output$para8 <- renderValueBox(infoBox(color = "teal", title = "Interactome", subtitle = "Node Edges",
                                         value = paste(nwaObj@summary$db[, "node No"], nwaObj@summary$db[, "edge No"])))
  output$para9 <- renderValueBox(infoBox(color = "teal", title = "FDR", subtitle = "",
                                         value = nwaObj@summary$para[, "FDR"]))
  output$para10 <- renderValueBox(infoBox(color = "teal", title = "Identified subnetwork", subtitle = "Node Edges",
                                          value = paste(nwaObj@summary$result[, "node No"], nwaObj@summary$result[, "edge No"])))
  # HTSanalyzeR2:::generateNWASummary(nwaObj)
}

create_enrich_map <- function(gscaObj, seriesObjs, input) {
  genesets <- input$genesets_map
  specGeneset <- NULL
  if(!is.null(specificGeneset) && input$genesets_map == "SpecificGeneset") {
    genesets <- names(gscaObj@listOfGeneSetCollections)
    specGeneset <- specificGeneset
  }

  HTSanalyzeR2::viewEnrichMap(
    gscaObj,
    resultName=paste0(input$analysis_map, ".results"),
    gscs = genesets,
    allSig=TRUE,
    gsNameType="id",
    specificGeneset = specGeneset,
    cutoff = cutoff,
    seriesObjs = seriesObjs)
}

# ##
# gscaObj <- gscaTStmp[[1]]
# resultName <- "HyperGeo.results"
# gscs <- names(gscaObj@listOfGeneSetCollections)
# specificGeneset <- NULL
# seriesObjs <- gscaTStmp

create_network <- function(nwaObj, seriesObjs) {
  HTSanalyzeR2::viewSubNet(nwaObj, seriesObjs = seriesObjs)
}

server <- function(input, output, session) {
  observeEvent(
    { input$series_tick_res
      input$analysis_res
      input$genesets_res },
    {
      obj <- gsca
      if(gscaTS) {
        obj <- gscaObjs[[input$series_tick_res]]
      }
      output$gsca_output <- DT::renderDataTable(create_data_table(obj, input$analysis_res, input$genesets_res), server = FALSE)
      renderGSCASummary(input, output, obj)
    }
  )

  observeEvent(input$process_map, {
    output$series_tick_name <- renderMenu({ menuItem(names(gscaObjs)[input$process_map], icon = icon("time", lib = "glyphicon"))})
    output$map_output <- HTSanalyzeR2:::updateForceGraph(list(process = input$process_map))
  })

  observeEvent(
    { input$analysis_map
      input$genesets_map },
    {
      if(!is.null(input$process_map)){
        output$series_tick_name <- renderMenu({ menuItem(names(gscaObjs)[input$process_map], icon = icon("time", lib = "glyphicon"))})
        updateSliderInput(session, 'process_map', value = 1)
      }
      output$map_output <- HTSanalyzeR2:::renderForceGraph(create_enrich_map(gsca, gscaObjs, input))
    }
  )

  observeEvent(input$process_net, {
    output$network_output <- HTSanalyzeR2:::updateForceGraph(list(process = input$process_net))
    obj <- nwaObjs[[input$process_net]]
    renderNWASummary(input, output, obj) # nwaObjs[[input$process_net]]
  })

  ## TODO: undefined behavior
  observeEvent({42}, {
    output$network_output <- HTSanalyzeR2:::renderForceGraph(create_network(nwa, nwaObjs))
    renderNWASummary(input, output, nwa) # nwaObjs[[input$process_net]]
  })
}

## ============================================ Run application ============================================
shinyApp(ui, server)

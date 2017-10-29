library(shiny)
library(shinydashboard)
library(igraph)
library(dplyr)
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
specificGeneset = results$specificGeneset


## ============================================ Preprocessing ==============================================
gscaTS <- !is.null(gsca) && class(gsca) == "list"
nwaTS <- !is.null(nwa) && class(nwa) == "list"

if(!is.null(gsca)) {
  gscaSeriesTickInput <- NULL
  gscaProcessSlider <- NULL
  if(gscaTS) {
    gscaObjs <- gsca
    gscaSeriesTicks <- names(gscaObjs)
    gscaSeriesTickInput <- selectInput('series_tick_res', 'Series Tick', gscaSeriesTicks)
    gscaProcessSlider <- sliderInput("process_map", "Process", 1, length(gscaSeriesTicks), value = 1, step = 1, animate = animationOptions(interval=1000))
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

HTMLSettings <- system.file("templates/settings3.html", package="HTSanalyzeR2")
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

create_data_table <- function(gscaObj, analysis, genesets) {
  jsRender <- JS("function(data, type, row, meta) { return type === 'display' && Number(data) < 0.001 ? '<0.001' : data }")
  jsCallback <- JS("table.page(0).draw(false)")

  analysis_to_show <- ifelse(analysis == "Significant in both", "Sig.adj.pvals.in.both", paste0(analysis, ".results"))
  res <- gscaObj@result[[analysis_to_show]][[genesets]]
  res <- trim_result(res, digits = 3)

  target_cols <- c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue", "Pvalue", "Adjusted.Pvalue")
  dt_options <- list(pageLength = 13, dom = 'Bfrtip', buttons = c('copy', 'csv', 'pdf', 'print'),
                     columnDefs = list(list(targets = which(colnames(res) %in% target_cols) - 1, render = jsRender)))

  dt <- DT::datatable(res, filter = 'top', rownames = FALSE, escape = FALSE, extensions = 'Buttons', options = dt_options, callback = jsCallback)
  if (analysis != "Significant in both") {
    dt <- formatStyle(dt, 'Adjusted.Pvalue', target = "row", fontWeight = styleInterval(gscaObj@para$pValueCutoff, c('bold', 'weight')))
  }
  dt
}

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  # mi$children[[1]]$attribs['aria-expanded'] = "false"
  # if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
  #   mi$attribs$class=NULL
  # }
  mi
}

ifNotNull <- function(obj, item) {
  if(is.null(obj)) return(NULL)
  item
}


## ============================================ Define ui ==================================================

header <- dashboardHeader(title = "HTSAnalyzeR2", dropdownMenu(type = "messages", icon = icon("cogs"), badgeStatus = NULL))

sidebar <- dashboardSidebar(
  sidebarMenu(
    tags$head(
      includeScript(system.file("htmlwidgets/lib/btn.js", package="HTSanalyzeR2"))
    ),
    ifNotNull(gsca,
           convertMenuItem(
             menuItem("Enrichment Result",
                      tabName = "table_tab",
                      icon = icon("th-list"),
                      gscaSeriesTickInput,
                      selectInput('analysis_res', 'Analysis', availableAnalysis),
                      selectInput('genesets_res', 'Gene Sets Collection', c(availableGeneSets, "ALL"))
             ), "table_tab")),
    ifNotNull(gsca,
           convertMenuItem(
             menuItem("Enrichment Map",
                      tabName = "map_tab",
                      icon = icon("area-chart"),
                      selectInput('analysis_map', 'Analysis', availableAnalysis[-3]),
                      selectInput('genesets_map', 'Gene Sets Collection', c(specificGenesetItem, availableGeneSets)),
                      gscaProcessSlider
             ), "map_tab")),
    ifNotNull(nwa,
           convertMenuItem(
             menuItem("Network Analysis",
                      tabName = "network_tab",
                      icon = icon("area-chart"),
                      nwaProcessSlider
             ), "network_tab"))
    )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "table_tab",
      fluidRow(
        column(width = 9,
               box(width = NULL, status = "success", solidHeader = FALSE, dataTableOutput("gsca_output"))),
        column(width = 3,
               valueBoxOutput("numGenesets", width = NULL),
               valueBoxOutput("numAboveMinimum", width = NULL),
               infoBoxOutput("para1", width = NULL),
               infoBoxOutput("para2", width = NULL),
               infoBoxOutput("para3", width = NULL))
      )
    ),

    tabItem(tabName = "map_tab",
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "success", solidHeader = TRUE,
                         title = "Enrichment Map of GO_MF",
                         forceGraphOutput("map_output")))
            )
    ),

    tabItem(tabName = "network_tab",
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "success", solidHeader = TRUE,
                         title = "Network Analysis",
                         forceGraphOutput("network_output")))
            )
    ),

    tabItem(tabName = "dummy", fluidRow(colourInput("dummyColorInput", "")))
  ),

  includeHTML(HTMLSettings)
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "blue")

## ============================================ Define server ==============================================
renderGSCASummary <- function(input, output) {
  # HTSanalyzeR2:::generateGSCASummary(gscaObj)
  output$numGenesets <- renderValueBox(valueBox(icon = icon("database"), color = "aqua",
    value = 4015,
    subtitle = "Gene sets in GO_MF."))
  output$numAboveMinimum <- renderValueBox(valueBox(icon = icon("certificate"), color = "olive",
    value = 347,
    subtitle = "Above the minimum size."))
  output$para1 <- renderValueBox(infoBox(icon = icon("wrench"), color = "teal",
    value = 0.01,
    title = "P-value Cutoff", subtitle = "Significant gene set cutoff p-value (adjusted)"))
  output$para2 <- renderValueBox(infoBox(icon = icon("wrench"), color = "teal",
    value = 20,
    title = "Minimum size", subtitle = "Minimum gene set size"))
  output$para3 <- renderValueBox(infoBox(icon = icon("wrench"), color = "teal",
    value = "BH",
    title = "Correction", subtitle = "MHT correction method"))
}

renderNWASummary <- function(input, output) {
  # HTSanalyzeR2:::generateNWASummary(nwaObj)
}

create_enrich_map <- function(gscaObj, seriesObjs, input) {
  genesets <- ifelse(is.null(specificGeneset), input$genesets_map, names(gscaObj@listOfGeneSetCollections))
  HTSanalyzeR2::viewEnrichMap(
    gscaObj,
    resultName=paste0(input$analysis_map, ".results"),
    gscs = genesets,
    allSig=TRUE,
    gsNameType="id",
    specificGeneset = specificGeneset,
    seriesObjs = seriesObjs)
}

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
      renderGSCASummary(input, output)
    }
  )

  observeEvent(input$process_map, {
    output$map_output <- updateForceGraph(list(process = input$process_map))
  })

  observeEvent(
    { input$analysis_map
      input$genesets_map },
    {
      output$map_output <- renderForceGraph(create_enrich_map(gsca, gscaObjs, input))
    }
  )

  observeEvent(input$process_net, {
    output$network_output <- updateForceGraph(list(process = input$process_net))
    renderNWASummary(input, output) # nwaObjs[[input$process_net]]
  })

  ## TODO: undefined behavior
  observeEvent({42}, {
    output$network_output <- renderForceGraph(create_network(nwa, nwaObjs))
    renderNWASummary(input, output) # nwaObjs[[input$process_net]]
  })
}

## ============================================ Run application ============================================
shinyApp(ui, server)

library(shiny)
library(DT)
library(igraph)
library(dplyr)
library(HTSanalyzeR2)

## the input data is a list, list(gsca = gsca, nwa = nwa)
results <- readRDS(file = "./results.RData")
gsca <- results$gsca
gsca <- NULL
nwa <- results$nwa

## write Summary
generateGSCSummary <- function(gsca) {
  paste(sapply(1:length(gsca@listOfGeneSetCollections), function(i){
    (paste("- ", names(gsca@listOfGeneSetCollections)[i],
           " ( ", length(gsca@listOfGeneSetCollections[[i]]),
           " gene sets, of which ", gsca@summary$gsc[i, 2],
           " were above the minimum size )", sep=""))
  }), collapse = "\n")
}

generateMethodsSummary <- function(gsca) {
  summ <- ""
  if(!is.null(gsca@result$HyperGeo.results)) {
    summ <- paste(summ, "- Hypergeometric test",
                  "\n + Significant gene set cutoff p-value (adjusted): ", gsca@summary$para$hypergeo[, "pValueCutoff"],
                  "\n + MHT correction method: ", gsca@summary$para$hypergeo[, "pAdjustMethod"],
                  "\n + Minimum gene set size: ", gsca@summary$para$hypergeo[, "minGeneSetSize"], sep = "")
  }
  if(!is.null(gsca@result$GSEA.results)) {
    summ <- paste(summ, "\n\n- Gene Set Enrichment Analysis",
                  "\n + Significant gene set cutoff p-value (adjusted): ", gsca@summary$para$gsea[, "pValueCutoff"],
                  "\n + Minimum gene set size: ", gsca@summary$para$gsea[, "minGeneSetSize"],
                  "\n + MHT correction method: ", gsca@summary$para$gsea[, "pAdjustMethod"],
                  "\n + Number of permutations: ", gsca@summary$para$gsea[, "nPermutations"],
                  "\n + Exponent: ", gsca@summary$para$gsea[, "exponent"], sep = "")
  }
  summ
}

if(!is.null(gsca)) {
  if(file.exists("gsca_summary.md"))  file.remove("gsca_summary.md")
  knitr::knit("gsca_summary.Rmd", "gsca_summary.md")
}
if(!is.null(nwa)) {
  if(file.exists("nwa_summary.md"))  file.remove("nwa_summary.md")
  knitr::knit("nwa_summary.Rmd", "nwa_summary.md")
}


createLink <- function(val) {
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
}

createLink_KEGG <- function(val) {
  sprintf('<a href="http://www.genome.jp/dbget-bin/www_bget?pathway:%s" target="_blank" class="btn btn-primary">%s</a>',val,val)
}

createLink_GO <- function(val) {
  sprintf('<a href="http://www.ebi.ac.uk/QuickGO/GTerm?id=%s" target="_blank" class="btn btn-primary">%s</a>',val,val)
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

createPanel <- function(tab = "enrich_result") {
  switch(tab,
         enrich_result = tabPanel("Enrichment Results",
                                  sidebarLayout(
                                    sidebarPanel(
                                      includeMarkdown("gsca_summary.md"),
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
                                   selectInput('analysis2', 'Analysis', availableResults(gsca@summary$results, TRUE)[-3]),
                                   selectInput('genesets2', 'Gene Sets Collection', availableResults(gsca@summary$results, FALSE)),
                                   radioButtons("nodename", "Node name", c("ID"="id", "Term"="term"), inline = TRUE),
                                   sliderInput("dist", "Distance", 10, 300, value = 100, step = 10),
                                   sliderInput("charge", "Charge", -1000, -100, value = -600, step = 50)
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
                                sliderInput("dist2", "Distance", 10, 300, value = 70, step = 10),
                                sliderInput("charge2", "Charge", -1000, -100, value = -300, step = 50),

                                hr(),
                                fluidRow(
                                  selectInput("selection", label = h4("Node Sets"), choices = list("All" = 1,"Set 1" = 2,"Selected" = 3),selected = 1),

                                  h4("View Options"),
                                  fluidRow(
                                    column(3, checkboxInput("visible", label = "Label",  value = TRUE)),
                                    column(3, checkboxInput("pause", label = "Pause",  value = FALSE))
                                  ),
                                  radioButtons( "shape", label = "Shape", choices = list("Circle" = "circle", "Rect" = "rect"), inline = TRUE, selected = "circle"),
                                  sliderInput("size",label = "Size",min = 2,max = 16,value = 6)

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

  if(!is.null(gsca)) {
    ## add gsca terms
    for(analysis in c("HyperGeo.results", "GSEA.results", "Sig.pvals.in.both", "Sig.adj.pvals.in.both")) {
      for(genesets in names(gsca@result[[analysis]])) {
        if(grepl("KEGG", genesets)) {
          gsca@result[[analysis]][[genesets]] <-
            data.frame(Gene.Set=createLink_KEGG(rownames(gsca@result[[analysis]][[genesets]])),
                       gsca@result[[analysis]][[genesets]])
        } else if(grepl("GO", genesets)) {
          gsca@result[[analysis]][[genesets]] <-
            data.frame(Gene.Set=createLink_GO(rownames(gsca@result[[analysis]][[genesets]])),
                       gsca@result[[analysis]][[genesets]])
        } else {
          gsca@result[[analysis]][[genesets]] <-
            data.frame(Gene.Set=createLink(rownames(gsca@result[[analysis]][[genesets]])),
                       gsca@result[[analysis]][[genesets]])
        }
      }
    }

    # combine
    for(name in c("HyperGeo.results", "GSEA.results")) {
      if(!is.null(gsca@result[[name]])){
        gsca@result[[name]]$ALL <- do.call(rbind, gsca@result[[name]])
        gsca@result[[name]]$ALL <- gsca@result[[name]]$ALL[order(gsca@result[[name]]$ALL$Adjusted.Pvalue), ]
      }
    }

    if(!is.null(gsca@result$Sig.adj.pvals.in.both)){
      gsca@result$Sig.adj.pvals.in.both$ALL <- do.call(rbind, gsca@result$Sig.adj.pvals.in.both)
      gsca@result$Sig.adj.pvals.in.both$ALL <- gsca@result$Sig.adj.pvals.in.both$ALL[order(gsca@result$Sig.adj.pvals.in.both$ALL$HyperGeo.Adj.Pvalue, gsca@result$Sig.adj.pvals.in.both$ALL$GSEA.Adj.Pvalue), ]
    }
  }

  ## response update
  observeEvent(input$pause, {
    output$subnetwork_output <- updateForceGraph(list(pause = input$pause))
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

  observeEvent(input$dist, {
    # enrichment map
    output$network_output <- updateNetwork(gsca, input)
  })

  observeEvent(input$charge, {
    # enrichment map
    output$network_output <- updateNetwork(gsca, input)
  })

  observeEvent(input$analysis, {
    output$gsca_output <- renderDataTable(selectDT(gsca, input))
  })
  observeEvent(input$genesets, {
    output$gsca_output <- renderDataTable(selectDT(gsca, input))
  })

  observeEvent(input$dist2, {
    options <- list(charge = input$charge2, distance = input$dist2)
    output$subnetwork_output <- renderForceGraph(viewSubNet(nwa, options = options))
  })
  observeEvent(input$charge2, {
    options <- list(charge = input$charge2, distance = input$dist2)
    output$subnetwork_output <- renderForceGraph(viewSubNet(nwa, options = options))
  })


  }

updateNetwork <- function(gsca, input) {
  options <- list(charge = input$charge, distance = input$dist)
  renderForceGraph(viewEnrichMap(gsca, resultName=paste0(input$analysis2, ".results"),
                                 gscs = c(input$genesets2), allSig=F, ntop = 30, gsNameType=input$nodename,
                                 options = options))
}

selectDT <- function(gsca, input) {
  if (input$analysis == "Significant in both") {
    analysis <- "Sig.adj.pvals.in.both"
    res <- gsca@result[[analysis]][[input$genesets]]

    res$HyperGeo.Adj.Pvalue <- round(res$HyperGeo.Adj.Pvalue, digits = 3)
    res$GSEA.Adj.Pvalue <- round(res$GSEA.Adj.Pvalue, digits = 3)

    dt <- DT::datatable(res, filter = 'top', rownames = F, escape = F,
                        options = list(pageLength = 10,
                                       columnDefs = list(list(
                                         targets = which(colnames(res) %in% c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue"))-1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && Number(data) < 0.001 ?",
                                           "'<0.001' : data;",
                                           "}")
                                       ))), callback = JS('table.page(0).draw(false);'))

  } else {
    analysis <- paste0(input$analysis, ".results")
    res <- gsca@result[[analysis]][[input$genesets]]

    res$Pvalue <- round(res$Pvalue, digits = 3)
    res$Adjusted.Pvalue <- round(res$Adjusted.Pvalue, digits = 3)

    dt <- DT::datatable(res, filter = 'top', rownames = F, escape = F,
                        options = list(pageLength = 10,
                                       columnDefs = list(list(
                                         targets = which(colnames(res) %in% c("Pvalue", "Adjusted.Pvalue"))-1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && Number(data) < 0.001 ?",
                                           "'<0.001' : data;",
                                           "}")
                                       ))), callback = JS('table.page(0).draw(false);')) %>%

      formatStyle('Adjusted.Pvalue', target = "row",
                  fontWeight = styleInterval(gsca@para$pValueCutoff, c('bold', 'weight'))
                  # backgroundColor = styleInterval(gsca@para$pValueCutoff, c('green', 'gray')),
      )

    if (analysis == "HyperGeo.results") dt <- dt %>% formatRound("Expected.Hits", digits = 3)
    if (analysis == "GSEA.results") dt <- dt %>% formatSignif("Observed.score", digits = 3)

  }
  dt
}





# Run the application
shinyApp(ui = ui, server = server)

if(!isGeneric("extractEnrichMap"))
  setGeneric("extractEnrichMap", function(object, ...)
    standardGeneric("extractEnrichMap"), package = "HTSanalyzeR2")
if (!isGeneric("viewEnrichMap2"))
  setGeneric("viewEnrichMap2", function(object, ...)
    standardGeneric("viewEnrichMap2"), package = "HTSanalyzeR2")

#' @export
#' @importFrom igraph V graph.adjacency simplify
setMethod("extractEnrichMap", signature = "GSCA",
          function(object,
                   resultName = "GSEA.results",
                   gscs,
                   ntop = NULL,
                   allSig = TRUE,
                   gsNameType = "id") {
            ## get top gene sets
            topGS <-
              getTopGeneSets(object, resultName, gscs, ntop, allSig)

            if (length(unlist(topGS, recursive = FALSE)) == 0)
              stop("No significant gene sets found!\n")

            gsInUni <- list()
            tempList <- list()

            uniIDs <- names(object@geneList)
            sapply(seq_along(topGS), function(i) {
              if (length(topGS[[i]]) > 0) {
                gscName <- names(topGS)[i]
                ## compute overlapped genes between gene sets and universe
                gsInUni[[i]] <<- list()
                gsInUni[[i]] <<- sapply(topGS[[i]], function(j)
                  intersect(object@listOfGeneSetCollections[[gscName]][[j]],
                            uniIDs), simplify = FALSE)

                names(gsInUni)[i] <<- gscName
                tempList[[i]] <<-
                  data.frame(gsID = topGS[[i]],
                             gscID = gscName,
                             object@result[[resultName]][[gscName]][topGS[[i]], , drop =
                                                                      FALSE])
                names(tempList)[i] <<- gscName
              }
            })

            ## collapse to a data frame
            tempdf <-
              do.call("rbind",
                      lapply(tempList, data.frame, stringsAsFactors = FALSE))
            if (gsNameType == "term" &&
                !("Gene.Set.Term" %in% colnames(tempdf)))
              stop(
                "No gene set terms found in results!\n Please use the method 'appendGSTerms' or add a column named 'Gene.Set.Term' to the results!\n"
              )

            ## function to compute overlapped genes
            map.mat <- diag(1, nrow(tempdf), nrow(tempdf))
            map.diag <- sapply(1:nrow(tempdf),
                               function(i)
                                 length(gsInUni[[as.character(tempdf[i, "gscID"])]][[as.character(tempdf[i, "gsID"])]]))
            rownames(map.mat) <- rownames(tempdf)
            colnames(map.mat) <- rownames(tempdf)

            if (nrow(tempdf) >= 2) {
              sapply(1:(nrow(tempdf) - 1), function(i) {
                map.mat[i, (i + 1):nrow(tempdf)] <<-
                  sapply((i + 1):nrow(tempdf), function(j) {
                    length(intersect(gsInUni[[as.character(tempdf[i, "gscID"])]][[as.character(tempdf[i, "gsID"])]],
                                     gsInUni[[as.character(tempdf[j, "gscID"])]][[as.character(tempdf[j, "gsID"])]])) /
                      length(union(gsInUni[[as.character(tempdf[i, "gscID"])]][[as.character(tempdf[i, "gsID"])]],
                                   gsInUni[[as.character(tempdf[j, "gscID"])]][[as.character(tempdf[j, "gsID"])]]))
                  })
                map.mat[(i + 1):nrow(tempdf), i] <<-
                  map.mat[i, (i + 1):nrow(tempdf)]
              })

              ## generate igraph from adjacency matrix
              ### "Node name" controlled by the rownames of tempList
              g <- graph.adjacency(
                adjmatrix = map.mat,
                mode = "undirected",
                weighted = TRUE,
                diag = TRUE
              )
              g <- simplify(g, remove.loops = TRUE)
            } else if (nrow(tempdf) == 1) {
              diag(map.mat) <- 0
              ## generate igraph from adjacency matrix
              # "Node name" controlled by the rownames of tempList
              g <-
                graph.adjacency(
                  adjmatrix = map.mat,
                  mode = "undirected",
                  weighted = NULL,
                  diag = FALSE
                )
            }

            ## add an user-defined attribute 'geneSetSize' to igraph
            ## "Node size" controlled by the "size of gene set"
            V(g)$geneSetSize <- map.diag
            V(g)$adjPvalue <- tempdf[, "Adjusted.Pvalue"]

            if (resultName == "GSEA.results") {
              V(g)$obsPvalue <- tempdf[, "Observed.score"]
            }

            ##labels attributes
            if (gsNameType == "id") {
              V(g)$label <- as.character(tempdf[, "gsID"])
            } else if (gsNameType == "term") {
              V(g)$label <- as.character(tempdf[, "Gene.Set.Term"])
            }
            g
          }
)


#' @export
#' @importFrom igraph as_data_frame
setMethod("viewEnrichMap2", signature = "GSCA",
          function(object,
                   resultName = "GSEA.results",
                   gscs,
                   ntop = NULL,
                   allSig = TRUE,
                   gsNameType = "id",
                   options = list(charge = -200, distance = 200)
                   ) {

            g <- extractEnrichMap(object, resultName, gscs, ntop, allSig, gsNameType)

            em_nodes <- as_data_frame(g, "vertices")
            em_links <- as_data_frame(g, "edge")

            nMappings <- list(id = "name", size = "geneSetSize", color = "adjPvalue", label = "label")
            lMappings <- list(source = "from",target = "to", weight = "weight")

            title <- "Enrichment Map of"
            if (resultName=="GSEA.results") {
              title <- paste(title, "GSEA on", paste(gscs, collapse =", "))
            } else if (resultName=="HyperGeo.results") {
              title <- paste(title, "Hypergeometric tests on", paste(gscs, collapse =", "))
            }
            legendTitle = "Adjusted p-values"

            forceGraph(em_nodes, em_links, nMappings, lMappings,
                       title = title,
                       legendTitle = legendTitle,
                       charge = options$charge,
                       distance = options$distance
                       )
          })


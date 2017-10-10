if(!isGeneric("appendGSTerms"))
  setGeneric("appendGSTerms",function(object,...)
    standardGeneric("appendGSTerms"), package="HTSanalyzeR2")
if(!isGeneric("extractEnrichMap"))
  setGeneric("extractEnrichMap", function(object, ...)
    standardGeneric("extractEnrichMap"), package = "HTSanalyzeR2")
if (!isGeneric("viewEnrichMap"))
  setGeneric("viewEnrichMap", function(object, ...)
    standardGeneric("viewEnrichMap"), package = "HTSanalyzeR2")


#' Append gene set terms to GSCA results
#'
#' This is a generic function.
#' When implemented as the S4 method for objects of class GSCA, this function
#' finds corresponding annotation terms for GO, KEGG and MSigDB gene sets and
#' inserts a column named "Gene.Set.Term" to each data frame in the GSCA results.
#'
#' @rdname appendGSTerms
#'
#' @param object An object. When this function is implemented as the S4 method
#' of class 'GSCA', this argument is an object of class 'GSCA'.
#' @param keggGSCs A character vector of names of all KEGG gene set collections
#' @param goGSCs A character vector of names of all GO gene set collections
#' @param msigdbGSCs A character vector of names of all MSigDB gene set collections
#'
#'
#' @return In the end, this function will return an updated object of class GSCA.
#'
#' @details This function makes the GSCA results more readable by appending a
#' column of terms for KEGG and GO gene sets. To do this, the user needs to
#' specify the names of the gene set collections based on GO, KEGG and MSigDB,
#' respectively.
#'
#' For each GO gene set, the GO id will be mapped to corresponding GO term by
#' the function mapIds of the package AnnotationDbi.
#'
#' For each KEGG gene set, the species code in the KEGG id will be trimmed off,
#' and then mapped to its corresponding annotation term using the package KEGGREST
#'
#' For each MSigDB gene set, the corresponding annotation terms based on the
#' built-in database in this package.
#'
#' @examples
#' ## Not run:
#' library(org.Dm.eg.db)
#' library(GO.db)
#' ## load data for enrichment analyses
#' data(data4enrich)
#' ## select hits
#' hits <- names(data4enrich)[abs(data4enrich) > 2]
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Dm", ontologies=c("MF"))
#' ListGSC <- list(GO_MF=GO_MF)
#' ## create an object of class 'GSCA'
#' gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = data4enrich, hits = hits)
#' ## print gsca
#' gsca
#' ## do preprocessing
#' gsca <- preprocess(gsca, species="Dm", initialIDs="FLYBASECG", keepMultipleMappings=TRUE, duplicateRemoverMethod="max", orderAbsValue=FALSE)
#' ## do hypergeometric tests and GSEA
#' gsca <- analyze(gsca, para=list(pValueCutoff=0.05, pAdjustMethod ="BH", nPermutations=100, minGeneSetSize=200, exponent=1))
#' ## append gene set terms to results
#' gsca <- appendGSTerms(gsca, goGSCs=c("GO_MF"), keggGSCs=NULL, msigdbGSCs=NULL)
#' ## view an enrichment map for GSEA results
#' viewEnrichMap(gsca, gscs="GO_MF", allSig = F, ntop = 7, gsNameType = "term")
#' @export
#'
setMethod(
  "appendGSTerms", signature = "GSCA",
  function(object, keggGSCs=NULL, goGSCs=NULL, msigdbGSCs=NULL) {
    if(length(object@result)==0)
      stop("No results generated!\n")

    gsc.names<-names(object@listOfGeneSetCollections)

    if(!is.null(keggGSCs)) {
      paraCheck("Report", "keggGSCs", keggGSCs)
      if(!all(keggGSCs %in% gsc.names))
        stop("Wrong gene set collection names specified in 'keggGSCs'!\n")
    }
    if(!is.null(goGSCs)) {
      paraCheck("Report", "goGSCs", goGSCs)
      if(!all(goGSCs %in% gsc.names))
        stop("Wrong gene set collection names specified in 'goGSCs'!\n")
    }
    if(!is.null(msigdbGSCs)) {
      paraCheck("Report", "msigdbGSCs", msigdbGSCs)
      if(!all(msigdbGSCs %in% gsc.names))
        stop("Wrong gene set collection names specified in 'msigdbGSCs'!\n")
    }

    result <- object@result
    ## add gene set terms if possible
    for (rs in 1:length(result)) {
      if (names(result)[rs] %in% c(
        "HyperGeo.results",
        "GSEA.results",
        "Sig.pvals.in.both",
        "Sig.adj.pvals.in.both"
      )) {
        sapply(names(result[[rs]]),
         function(gsc) {
           if (gsc %in% names(object@listOfGeneSetCollections)) {
             if ("Gene.Set.Term" %in% colnames(result[[rs]][[gsc]])) {
               warning(paste(
                 "--Gene Set terms already exsit in gene set collection ", gsc,
                 " of ", names(result)[rs],
                 ", and will be overwritten by new gene set terms!\n", sep = ""))

               result[[rs]][[gsc]] <-
                 result[[rs]][[gsc]][, setdiff(colnames(result[[rs]][[gsc]]),
                                               "Gene.Set.Term"), drop = FALSE]
             }

             if (nrow(result[[rs]][[gsc]]) >= 1) {
               if (gsc %in% keggGSCs)
                 result[[rs]][[gsc]] <<- appendKEGGTerm(result[[rs]][[gsc]])
               else if (gsc %in% goGSCs)
                 result[[rs]][[gsc]] <<- appendGOTerm(result[[rs]][[gsc]])
               else if (gsc %in% msigdbGSCs)
                 result[[rs]][[gsc]] <<- appendMSigDBTerm(result[[rs]][[gsc]])
               else
                 result[[rs]][[gsc]] <<- data.frame(Gene.Set.Term = "--",
                                                    result[[rs]][[gsc]],
                                                    stringsAsFactors = FALSE)
             }
           } #if
         }) # sapply function
      } # if
    } # for

    object@result <- result
    object
  }
)

#' @importFrom AnnotationDbi mapIds
appendGOTerm <- function(df) {
  require(GO.db)
  goterms <- mapIds(GO.db, keys=row.names(df), keytype = "GOID", column = "TERM")
  goterms[which(is.na(goterms))] <- "NA"
  names(goterms)[which(is.na(names(goterms)))] <-
    row.names(df)[which(is.na(names(goterms)))]
  data.frame(Gene.Set.Term = goterms, df, stringsAsFactors = FALSE)
}


#' @importFrom KEGGREST keggList
#' @importFrom stringr str_sub
appendKEGGTerm<-function(df) {
  mappings <- KEGGREST::keggList("pathway")
  names(mappings) <- stringr::str_sub(names(mappings), -5)
  keggnames <- stringr::str_sub(row.names(df), -5)
  keggterms <- mappings[keggnames]
  keggterms[which(is.na(keggterms))] <- "NA"
  names(keggterms)[which(is.na(names(keggterms)))] <-
    row.names(df)[which(is.na(names(keggterms)))]
  newdf <-
    data.frame(Gene.Set.Term = keggterms, df, stringsAsFactors = FALSE)
  row.names(newdf) <- row.names(df)
  newdf
}

appendMSigDBTerm <- function(df) {
  data.frame(Gene.Set.Term = row.names(df), df, stringsAsFactors = FALSE)
}

#' Extract the enrichment map result from GSCA object
#'
#' This is a generic function.
#' @export
#' @importFrom igraph V graph.adjacency simplify
setMethod("extractEnrichMap", signature = "GSCA",
          function(object,
                   resultName = "GSEA.results",
                   gscs,
                   ntop = NULL,
                   allSig = TRUE,
                   gsNameType = "id",
                   specificGeneset = NULL) {
            paraCheck("Report", "gsNameType", gsNameType)
            ## get top gene sets
            if(is.null(specificGeneset)){
            topGS <-
              getTopGeneSets(object, resultName, gscs, ntop, allSig)
            }else{
              paraCheck("Summarize", "specificGeneset", specificGeneset)
              topGSTMP <- getTopGeneSets(object, resultName, gscs, ntop = 20000, allSig = FALSE)
              if (!all(names(specificGeneset) %in% gscs))
                stop("Wrong Gene Set Collection name(s) in 'specificGeneset'! \n")
              for(i in 1:length(specificGeneset)){
                topGSTMP1 <- topGSTMP[[names(specificGeneset)[i]]]
                if(!all(specificGeneset[[i]] %in% topGSTMP1)){
                  stop("'specificGeneset' should be a subset of all genesets in result!\n")
                }
              }
              topGS <- specificGeneset
            }

            if (length(unlist(topGS, recursive = FALSE)) == 0) {
              warning("No significant gene sets found!\n")
              return(NULL)
            }

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
              gsID <- as.character(tempdf[["gsID"]])
              gscID <- as.character(tempdf[["gscID"]])
              sapply(1:(nrow(tempdf) - 1), function(i) {
                a <- gsInUni[[gscID[i]]][[gsID[i]]]
                map.mat[i, (i + 1):nrow(tempdf)] <<-
                  sapply((i + 1):nrow(tempdf),
                         function(j) {
                           b <- gsInUni[[gscID[j]]][[gsID[j]]]
                           length(intersect(a, b)) / length(union(a,b))
                         }
                  )
                map.mat[(i + 1):nrow(tempdf), i] <<- map.mat[i, (i + 1):nrow(tempdf)]
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
              E(g)$weight <- 2
            }

            ## add an user-defined attribute 'geneSetSize' to igraph
            # "Node size" controlled by the "size of gene set"
            # "Node color" controlled by the "Adjusted Pvalue" and "Observed.score"
            V(g)$geneSetSize <- map.diag
            V(g)$adjPvalue <- tempdf[, "Adjusted.Pvalue"]
            if(resultName=="GSEA.results") {
              V(g)$obsPvalue <- tempdf[, "Observed.score"]
              V(g)$colorScheme <- "Pos"
              V(g)$colorScheme[tempdf[, "Observed.score"] < 0] <- "Neg"
            } else if (resultName=="HyperGeo.results") {
              V(g)$colorScheme <- ""
            }

            ##labels attributes
            if (gsNameType == "id") {
              V(g)$label <- as.character(tempdf[, "gsID"])
            } else if (gsNameType == "term") {
              V(g)$label <- as.character(tempdf[, "Gene.Set.Term"])
            }

            V(g)$label_id <- as.character(tempdf[, "gsID"])

            if ("Gene.Set.Term" %in% colnames(tempdf)) {
              V(g)$label_term <- as.character(tempdf[, "Gene.Set.Term"])
            } else {
              warning("No appended terms, please run appendGSTerms.")
              V(g)$label_term <- as.character(tempdf[, "gsID"])
            }

            g
          }
)

#' Plot a figure of the enrichment map for GSEA or Hypergeometric tests
#'
#' This is a generic function.When implemented as the S4 method for objects
#' of class GSCA, this function will plot an enrichment map for GSEA or
#' Hypergeometric test results.
#'
#' @param object 	an object. When this function is implemented as the S4 method
#'  of class GSCA, this argument is an object of class GSCA.
#' @param resultName a single character value: 'HyperGeo.results' or 'GSEA.results'
#' @param gscs a character vector specifying the names of gene set collections
#' of which the top significant gene sets will be plotted
#' @param ntop a single integer or numeric value specifying how many gene
#' sets of top significance will be plotted.
#' @param allSig a single logical value. If 'TRUE', all significant gene sets
#' (GSEA adjusted p-value < 'pValueCutoff' of slot 'para') will be used; otherwise,
#' only top 'ntop' gene sets will be used.
#' @param gsNameType a single character value specifying the type of the gene set names that
#' will be displayed as the names of nodes in the enrichment map. The type of the gene
#' set names should be one of the following: "id", "term" or "none".

#' @details The idea of this function is similar to the PLoS one paper by Merico et al.
#'
#' An enrichment map is a network to help better visualize and interpret the GSEA or
#'Hypergeometric test results. In an enrichment map, the nodes represent gene sets
#'and the edges denote the Jaccard similarity coefficient between two gene sets.
#'Node colors are scaled according to the adjusted p-values (the darker the more significant).
#'For GSEA, nodes are colored by the sign of the enrichment scores (red:+, blue: -).
#'The size of nodes illustrates the size of gene sets, while the width of edges denotes the
#'Jaccard coefficient.
#'@return an object of igraph with all attributes about the enrichement map
#'@examples
#'#' ## Not run:
#' library(org.Dm.eg.db)
#' library(GO.db)
#' ## load data for enrichment analyses
#' data(data4enrich)
#' ## select hits
#' hits <- names(data4enrich)[abs(data4enrich) > 2]
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Dm", ontologies=c("MF"))
#' ListGSC <- list(GO_MF=GO_MF)
#' ## create an object of class 'GSCA'
#' gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = data4enrich, hits = hits)
#' ## print gsca
#' gsca
#' ## do preprocessing
#' gsca <- preprocess(gsca, species="Dm", initialIDs="FLYBASECG", keepMultipleMappings=TRUE, duplicateRemoverMethod="max", orderAbsValue=FALSE)
#' ## do hypergeometric tests and GSEA
#' gsca <- analyze(gsca, para=list(pValueCutoff=0.05, pAdjustMethod ="BH", nPermutations=100, minGeneSetSize=200, exponent=1))
#' ## append gene set terms to results
#' gsca <- appendGSTerms(gsca, goGSCs=c("GO_MF"), keggGSCs=NULL, msigdbGSCs=NULL)
#' ## view an enrichment map for GSEA results
#' viewEnrichMap(gsca, gscs="GO_MF", allSig = F, ntop = 7, gsNameType = "term")
#' @export
#' @importFrom igraph as_data_frame
setMethod("viewEnrichMap", signature = "GSCA",
          function(object,
                   resultName = "GSEA.results",
                   gscs,
                   ntop = NULL,
                   allSig = TRUE,
                   gsNameType = "id",
                   specificGeneset = NULL,
                   options = list(distance = 400),
                   seriesObjs = NULL) {

            g <- extractEnrichMap(object, resultName, gscs, ntop, allSig, gsNameType, specificGeneset)

            em_nodes <- as_data_frame(g, "vertices")
            em_links <- as_data_frame(g, "edge")

            nMappings <- list(id = "name", size = "geneSetSize", color = "adjPvalue", scheme = "colorScheme",
                              label = "label", label_id = "label_id", label_term = "label_term")
            lMappings <- list(source = "from",target = "to", weight = "weight")

            title <- "Enrichment Map of"
            if (resultName=="GSEA.results") {
              title <- paste(title, "GSEA on", paste(gscs, collapse =", "))
              scheme = "dual"
            } else if (resultName=="HyperGeo.results") {
              title <- paste(title, "Hypergeometric tests on", paste(gscs, collapse =", "))
              scheme = "linear2"
            }

            series <- NULL
            if(!is.null(seriesObjs)) {
              ## TODO: paraCheck of seriesObj
              series <- names(seriesObjs)
              defaultKey <- series[1]
              # seriesDF: (nodes = nodeDF, edges = edgeDF, nodeSeriesCols = nodeCols, edgeSeriesCols = edgeCols)
              seriesDF <- fetchGSCASeriesValues(seriesObjs, resultName, gscs, ntop, allSig, gsNameType)
              # Create series mappings
              nodeCols <- seriesDF$nodeSeriesCols
              nodeColNames <- sub("adjPvalue", "color", nodeCols)
              nodeColNames <- sub("colorScheme", "scheme", nodeColNames)
              names(nodeCols) <- nodeColNames
              edgeCols <- seriesDF$edgeSeriesCols
              names(edgeCols) <- edgeCols
              # Append series data
              nMappings <- c(nMappings, nodeCols)
              lMappings <- c(lMappings, edgeCols)
              nMappings[c("color", "scheme")] <- paste(nMappings[c("color", "scheme")], defaultKey, sep=".")
              # lMappings[c("weight")] <- paste(lMappings[c("weight")], defaultKey, sep=".")
              em_nodes <- seriesDF$nodes
              em_links <- seriesDF$edges
            }

            options$nodeScheme = scheme
            defaultOptions = list(distance = 400, title = title, label = gsNameType, legendTitle = "Adjusted p-values")
            graphOptions <- modifyList(defaultOptions, options)

            forceGraph(em_nodes, em_links, nMappings, lMappings, graphOptions, seriesData = series)
          })

## Available graphOptions:
#
# distance: 200,
#
# title: "title",
# titleSize: 22,
# legendTitle: "legend",
#
# label: "id",
# labelColor: "#000000",  // black
# labelOpacity: 0.8,
# labelScale: 1,
#
# nodeScale: 1,
# ~~ nodeScheme: "linear2" ~~
# nodeShape: "circle",
# nodeBorderColor: "#808080", // grey
# nodeBorderWidth: 1,
# nodeBorderOpacity: 1,
#
# edgeScale: 1,
# edgeColor: "#808080",  // grey
# edgeOpacity: 0.6,


#' FetchGSCASeriesValues
#' @importFrom igraph as_data_frame
fetchGSCASeriesValues <- function(gscaObjs, resultName = "GSEA.results", gscs,
                            ntop = NULL, allSig = TRUE, gsNameType = "id") {
  # TODO: check the objs
  extractedValues <- lapply(seq_along(gscaObjs), function(i) {
    g <- extractEnrichMap(gscaObjs[[i]], resultName, gscs, ntop, allSig, gsNameType)
    dfList <- list( edges = data.frame(from=character(0), to=character(0), weight=numeric(0)),
           vertices=data.frame(name=character(0), geneSetSize=numeric(0), adjPvalue=numeric(0), obsPvalue=numeric(0),
                              colorScheme=character(0), label=character(0), label_id=character(0), label_term=character(0)))
    if (!is.null(g)) {
      dfList <- igraph::as_data_frame(g, "both")
    }
    # Vertices - ("name", "geneSetSize", "adjPvalue", "obsPvalue", "colorScheme", "label", "label_id", "label_term")
    colsToAppend <- colnames(dfList$vertices) %in% c("adjPvalue", "obsPvalue", "colorScheme")
    colnames(dfList$vertices)[colsToAppend] <- paste(colnames(dfList$vertices), names(gscaObjs)[i], sep=".")[colsToAppend]
    dfList$vertices <- unique(dfList$vertices)
    # Edges - ("from", "to", "weight")
    colsToAppend <- colnames(dfList$edges) %in% c()
    colnames(dfList$edges)[colsToAppend] <- paste(colnames(dfList$edges), names(gscaObjs)[i], sep=".")[colsToAppend]
    dfList$edges <- unique(dfList$edges)
    rownames(dfList$edges) <- paste0(dfList$edges$from, dfList$edges$to)
    dfList
  })

  #Combine nodes
  colsInCommon <- c("name", "label", "label_id", "label_term")
  nodeCols <- setdiff(unlist(lapply(extractedValues, function(li) {colnames(li$vertices)})), colsInCommon)
  nodeDF <- unique(Reduce(rbind, lapply(extractedValues, function(li){li$vertices[colsInCommon]})))
  nodeDF[, nodeCols] <- NA
  for(li in extractedValues) {
    cols <- setdiff(colnames(li$vertices), colsInCommon)
    nodeDF[rownames(li$vertices), cols] <- li$vertices[, cols]
  }

  #Combine edges
  colsInCommon <- c("from", "to", "weight")
  edgeCols <- setdiff(unlist(lapply(extractedValues, function(li) {colnames(li$edges)})), colsInCommon)
  edgeDF <- unique(Reduce(rbind, lapply(extractedValues, function(li){li$edges[colsInCommon]})))
  edgeDF[, edgeCols] <- NA
  for(li in extractedValues) {
    cols <- setdiff(colnames(li$edges), colsInCommon)
    edgeDF[rownames(li$edges), cols] <- li$edges[, cols]
  }
  rownames(edgeDF) <- NULL

  list(nodes = nodeDF, edges = edgeDF, nodeSeriesCols = nodeCols, edgeSeriesCols = edgeCols)
}

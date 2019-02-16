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
#' @aliases appendGSTerms
#' @param object A GSCA object.
#' @param keggGSCs A character vector of names of all KEGG gene set collections.
#' @param goGSCs A character vector of names of all GO gene set collections.
#' @param msigdbGSCs A character vector of names of all MSigDB gene set collections.
#'
#'
#' @return In the end, this function will return an updated object of class GSCA.
#'
#' @details This function makes the GSCA results more readable by appending a
#' column of terms for KEGG and GO gene sets. To do this, the user needs to
#' specify the names of the gene set collections based on GO, KEGG and MSigDB respectively.
#'
#' For each GO gene set, the GO id will be mapped to corresponding GO term by
#' the function mapIds of the package AnnotationDbi.
#'
#' For each KEGG gene set, the species code in the KEGG id will be trimmed off,
#' and then mapped to its corresponding annotation term using the package KEGGREST.
#'
#' For each MSigDB gene set, the corresponding annotation terms are based on the
#' built-in database in this package.
#'
#' @examples
#' library(org.Hs.eg.db)
#' library(GO.db)
#' library(KEGGREST)
#' ## load data for enrichment analyses
#' data(d7)
#' phenotype <- as.vector(d7$neg.lfc)
#' names(phenotype) <- d7$id
#'
#' ## select hits if you also want to do GSOA, otherwise ignore it
#' hits <-  names(phenotype[which(abs(phenotype) > 2)])
#'
#' ## set up a list of gene set collections
#' GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
#' PW_KEGG <- KeggGeneSets(species="Hs")
#' ListGSC <- list(GO_MF=GO_MF, PW_KEGG=PW_KEGG)
#'
#' ## create an object of class 'GSCA'
#' gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = phenotype, hits = hits)
#'
#' ## do preprocessing
#' gsca1 <- preprocess(gsca, species="Hs", initialIDs="SYMBOL", keepMultipleMappings=TRUE,
#'                    duplicateRemoverMethod="max", orderAbsValue=FALSE)
#'
#' ## support parallel calculation using doParallel package
#' if (requireNamespace("doParallel", quietly=TRUE)) {
#' doParallel::registerDoParallel(cores=2)
#' } else {
#' }
#'
#' ## do hypergeometric tests and GSEA
#' gsca2 <- analyze(gsca1, para=list(pValueCutoff=0.01, pAdjustMethod ="BH",
#'                                 nPermutations=100, minGeneSetSize=10, exponent=1),
#'                                 doGSOA = TRUE, doGSEA = TRUE)
#'
#' ## summarize gsca2
#' summarize(gsca2)
#' head(getResult(gsca2)$GSEA.results$GO_MF)
#'
#' ## append gene set terms to results
#' gsca3 <- appendGSTerms(gsca2, goGSCs=c("GO_MF"), keggGSCs=c("PW_KEGG"), msigdbGSCs=NULL)
#' head(getResult(gsca3)$GSEA.results$GO_MF)
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
#' @import GO.db
appendGOTerm <- function(df) {
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


#' Extract the enrichment map as an igraph object
#'
#' Extract the enrichment map from an analyzed GSCA object as igraph object for further external using.
#' Users can also modify the igraph object.
#'
#' @param object A GSCA object.
#' @param resultName A single character value specifying draw an enrichment map based on
#' which result. Could only be 'HyperGeo.results' or 'GSEA.results'.
#' @param gscs A character vector specifying the names of gene set collections
#' of which the top significant gene sets will be plotted.
#' @param ntop A single integer or numeric value specifying how many gene
#' sets of top significance will be plotted.
#' @param allSig A single logical value. If 'TRUE', all significant gene sets
#' (GSEA adjusted p-value < 'pValueCutoff' of slot 'para') will be used; otherwise,
#' only top 'ntop' gene sets will be used.
#' @param gsNameType A single character value specifying the type of the gene set names that
#' will be displayed as the names of nodes in the enrichment map. The type of the gene
#' set names should be one of the following: "id", "term" or "none".
#' @param specificGeneset A named list of specific gene sets. Specifically, this term needs to be
#' a subset of all analyzed gene sets which can be roughly gotten by
#' \strong{getTopGeneSets(object, resultName, gscs, ntop = 20000, allSig = FALSE)}.
#' @param cutoff A numeric value between 0 and 1. This parameter is setted as a cutoff of edge weight in the enrichment
#' map for better visualization. When the edge weight, namely the Jaccard coefficient between two gene sets, is less than
#' this cutoff, this edge would not be showed in the enrichment map.
#' The order of the list must match the order of results gotten by aboved function \strong{getTopGeneSets}.
#'
#' @export
#' @aliases extractEnrichMap
#' @importFrom igraph graph.adjacency simplify V V<- E E<-
#' @return An object of igraph with all attributes about the enrichement map.
#' @examples
#' ## load a GSCA object(see the examples of 'analyze' GSCA for details)
#' library(igraph)
#' data(d7_gsca)
#'
#' ## extract the enrichment map for top 30 significant 'GO_MF' gene sets of GSEA results in an igraph object
#' extractEnrichMap(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF"),
#'                 allSig = FALSE, ntop = 30, gsNameType = "term")
#'
setMethod("extractEnrichMap", signature = "GSCA",
          function(object,
                   resultName = "GSEA.results",
                   gscs,
                   ntop = NULL,
                   allSig = TRUE,
                   gsNameType = "id",
                   specificGeneset = NULL,
                   cutoff = NULL) {
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
                  stop("The ",i, "th element of 'specificGeneset' should be a subset of all '",
                       gscs[i], "' genesets in result!\n")
                }
                if(!is.character(specificGeneset[[i]])){
                  stop("Each element in the list of 'specificGeneset' should be a character vector!\n")
                }
              }
              topGS <- specificGeneset
            }

            if (length(unlist(topGS, recursive = FALSE)) == 0) {
              warning("No significant gene sets found!\n")
              g <- makeEmptyGraph(c("name", "geneSetSize", "adjPvalue", "obsPvalue", "colorScheme", "label", "label_id", "label_term"),
                                  c("from", "to", "weight"))
              return(g)
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
            rownames(tempdf) <- paste(tempdf$gscID, tempdf$gsID, sep = '.')
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
                           tmp <- length(intersect(a, b)) / length(union(a,b))
                           ## set cutoff to show edge
                           if(is.null(cutoff)) {tmp} else if(tmp < cutoff){tmp <- 0}
                           tmp
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
              V(g)$colorScheme <- "pos"
              V(g)$colorScheme[tempdf[, "Observed.score"] < 0] <- "neg"
            } else if (resultName=="HyperGeo.results") {
              V(g)$colorScheme <- "pos"
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

#' Plot the enrichment map for GSEA or GSOA result
#'
#' This is a generic function. When implemented as the S4 method for objects
#' of class GSCA, this function will plot an enrichment map for GSEA or
#' Hypergeometric test results.
#'
#' @param object A GSCA object.
#' @param resultName A single character value specifying draw an enrichment map based on
#' which result. Could only be 'HyperGeo.results' or 'GSEA.results'.
#' @param gscs A character vector specifying the names of gene set collections
#' of which the top significant gene sets will be plotted.
#' @param ntop A single integer or numeric value specifying how many gene
#' sets of top significance will be plotted.
#' @param allSig A single logical value. If 'TRUE', all significant gene sets
#' (GSEA adjusted p-value < 'pValueCutoff' of slot 'para') will be used; otherwise,
#' only top 'ntop' gene sets will be used.
#' @param gsNameType A single character value specifying the type of the gene set names that
#' will be displayed as the names of nodes in the enrichment map. The type of the gene
#' set names should be one of the following: "id", "term" or "none".
#' @param specificGeneset A named list of specific gene sets. Specifically, this term needs to be
#' a subset of all analyzed gene sets which can be roughly gotten by
#' \strong{getTopGeneSets(object, resultName, gscs, ntop = 20000, allSig = FALSE)}.
#' @param cutoff A numeric value between 0 and 1. This parameter is setted as a cutoff of edge weight in the enrichment
#' map for better visualization. When the edge weight, namely the Jaccard coefficient between two gene sets, is less than
#' this cutoff, this edge would not be showed in the enrichment map.
#' The order of the list must match the order of results gotten by aboved function \strong{getTopGeneSets}.
#' @param options A list of options to modify the enrichmentmap. Details are not showed here due to too
#' many options. Users are highly recommended to modify the enrichment map in a shiny report by
#' \code{\link[HTSanalyzeR2]{report}}.
#' @param seriesObjs A list of GSCA object. Internally used in the shiny report for visualizing the
#' enrichment map of time series data. No need to explicitly set it!
#'
#' @details The idea of this function is similar to the PLoS one paper by Merico et al.
#'
#'An enrichment map is a network to help better visualize and interpret the GSEA or
#'Hypergeometric test results. In an enrichment map, the nodes represent gene sets
#'and the edges denote the Jaccard similarity coefficient between two gene sets.
#'Node colors are scaled according to the adjusted p-values (the darker the more significant).
#'For GSEA, nodes are colored by the sign of the enrichment scores (red:+, blue: -).
#'The size of nodes illustrates the size of gene sets, while the width of edges denotes the
#'Jaccard coefficient.
#'
#'A useful application of this function is that user can define specificGeneset to plot an
#'enrichment map with their interested gene sets instead of the top significant ones or all the significant
#'ones. Especially when the number of the significant gene sets are huge, which would make the enrichment
#'map a mess as well as of no information. To this end, user can set the parameter \strong{specificGeneset}.
#'
#' @examples
#' ## load a GSCA object(see the examples of 'analyze' GSCA for details)
#' library(igraph)
#' data(d7_gsca)
#'
#' ## Example1: view an enrichment map for top 30 significant 'GO_MF' gene sets of GSEA results
#' \dontrun{
#' viewEnrichMap(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF"),
#'               allSig = FALSE, ntop = 30, gsNameType = "term")
#' }
#'
#' ## Example2: view an enrichment map for top 15 significant 'GO_MF'
#' ## and 'PW_KEGG' gene sets of GSEA results
#' \dontrun{
#' viewEnrichMap(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF", "PW_KEGG"),
#'               allSig = FALSE, ntop = 15, gsNameType = "term")
#' }
#'
#' ## Example3: view an enrichment map for top 15 significant 'GO_MF'
#' ## and 'PW_KEGG' gene sets of GSEA results, edge Jaccard coefficient less than 0.05
#' ## would not be showed in the enrichment map.
#' \dontrun{
#' viewEnrichMap(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF", "PW_KEGG"),
#'               allSig = FALSE, ntop = 15, gsNameType = "term", cutoff = 0.05)
#' }
#'
#' ## Example4: view an enrichment map with specificGenesets in 'GO_MF' gene sets of GSEA results
#' ## As told previously, specificGeneset needs to be a subset of all analyzed gene sets
#' ## which can be roughly gotten by:
#' tmp <- getTopGeneSets(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF"),
#'                       ntop = 20000, allSig = FALSE)
#' ## In that case, we can define specificGeneset as below:
#' GO_MF_geneset <- tmp$GO_MF[c(4,2,6,9,12)]
#' ## the name of specificGenesets also needs to match with the names of tmp
#' specificGeneset <- list("GO_MF"=GO_MF_geneset)
#' \dontrun{
#' viewEnrichMap(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF"),
#'               allSig = FALSE, gsNameType = "term",
#'               ntop = NULL, specificGeneset = specificGeneset)
#' }
#'
#' ## Example5: view an enrichment map with specificGenesets in 'GO_MF'
#' ## and 'PW_KEGG' gene sets of GSEA results
#' tmp <- getTopGeneSets(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF", "PW_KEGG"),
#'                       ntop = 20000, allSig = FALSE)
#' GO_MF_geneset <- tmp$GO_MF[c(6,3,5,9,12)]
#' PW_KEGG_geneset <- tmp$PW_KEGG[c(7,2,5,1,9)]
#' specificGeneset <- list("GO_MF"=GO_MF_geneset, "PW_KEGG"=PW_KEGG_geneset)
#' \dontrun{
#' viewEnrichMap(d7_gsca, resultName = "GSEA.results", gscs=c("GO_MF", "PW_KEGG"),
#'               allSig = FALSE, gsNameType = "term",
#'               ntop = NULL, specificGeneset = specificGeneset)
#' }
#' @export
#' @references
#' Merico D, Isserlin R, Stueker O, Emili A, Bader GD (2010) Enrichment Map:
#' A Network-Based Method for Gene-Set Enrichment Visualization and Interpretation.
#' PLoS ONE5(11): e13984. https://doi.org/10.1371/journal.pone.0013984
#' @importFrom igraph as_data_frame
#' @importFrom stringr str_replace
#' @importFrom utils modifyList
#' @aliases viewEnrichMap
setMethod("viewEnrichMap", signature = "GSCA",
          function(object,
                   resultName = "GSEA.results",
                   gscs,
                   ntop = NULL,
                   allSig = TRUE,
                   gsNameType = "id",
                   specificGeneset = NULL,
                   cutoff = NULL,
                   options = list(),
                   seriesObjs = NULL) {

            g <- extractEnrichMap(object, resultName, gscs, ntop, allSig, gsNameType, specificGeneset, cutoff)

            em_nodes <- as_data_frame(g, "vertices")
            em_links <- as_data_frame(g, "edge")

            nMappings <- list(id = "name", size = "geneSetSize", color = "adjPvalue", scheme = "colorScheme",
                              label = "label", label_id = "label_id", label_term = "label_term")
            lMappings <- list(source = "from",target = "to", weight = "weight")

            title <- "Enrichment Map of"
            if (resultName=="GSEA.results") {
              title <- paste(title, "GSEA on", paste(gscs, collapse =", "))
            } else if (resultName=="HyperGeo.results") {
              title <- paste(title, "Hypergeometric tests on", paste(gscs, collapse =", "))
            }

            series <- NULL
            if(!is.null(seriesObjs)) {
              ## TODO: paraCheck of seriesObj
              series <- names(seriesObjs)
              defaultKey <- series[1]
              # seriesDF: (nodes = nodeDF, edges = edgeDF, nodeSeriesCols = nodeCols, edgeSeriesCols = edgeCols)
              seriesDF <- fetchGSCASeriesValues(seriesObjs, resultName, gscs,
                                                ntop, allSig, gsNameType, specificGeneset, cutoff)
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

            options$nodeScheme = "dual"
            options$label = list(text = gsNameType)
            options$colorScaler = "log10"
            options$nPermutations = object@para$nPermutations
            defaultOptions = list(title = title, legendTitle = "-Log10(Adjusted p-values)",
                                  type = stringr::str_replace(resultName, ".results", ""))
            graphOptions <- modifyList(defaultOptions, options)

            forceGraph(em_nodes, em_links, nMappings, lMappings, graphOptions, seriesData = series)
          })

#' Extract and combine enrichment map information of given GSCA objects.
#'
#' This method is internally used for force-graph drawing. The enrichment map data of all the GSCA objects
#' are extacted and combined into corresponding dataframes. The attributes that are changes with the time
#' are also returned.
#'
#' @importFrom igraph as_data_frame
fetchGSCASeriesValues <- function(gscaObjs, resultName = "GSEA.results", gscs,
                            ntop = NULL, allSig = TRUE, gsNameType = "id", specificGeneset = NULL, cutoff = NULL) {
  # TODO: check the objs
  extractedValues <- lapply(seq_along(gscaObjs), function(i) {
    g <- extractEnrichMap(gscaObjs[[i]], resultName, gscs, ntop, allSig, gsNameType, specificGeneset, cutoff)
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


#' Generate an empty igraph object with given vertex/edge attributes
#'
#' @importFrom igraph make_empty_graph set_vertex_attr
makeEmptyGraph <- function(NAttributes, EAttributes) {
  g <- igraph::make_empty_graph(n = 0, directed = TRUE)
  for (attr in NAttributes) {
    g <- igraph::set_vertex_attr(g, attr, value = 0)
  }
  for (attr in EAttributes) {
    g <- igraph::set_edge_attr(g, attr, value = 0)
  }
  g
}

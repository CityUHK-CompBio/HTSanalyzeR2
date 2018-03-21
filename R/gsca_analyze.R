## summarize & default show
if (!isGeneric("analyze")) {
  setGeneric("analyze", function(object, ...) standardGeneric("analyze"), package = "HTSanalyzeR2")
}

#' Gene Set Collection Analysis or NetWork Analysis
#'
#' This is a generic function.
#' @describeIn analyze The function will perform gene set collection analysis
#' on a GSCA object and update information about these results to slot
#' \emph{summary} of class GSCA.
#' @aliases analyze
#' @param object An object. When this function is implemented as the S4
#' method of class 'GSCA' or 'NWA', this argument is an object of class
#' 'GSCA' or 'NWA'.
#' @param para A list of parameters for GSEA and hypergeometric tests.
#' This argument is only for class GSCA.
#' @param pValueCutoff
#' A single numeric value specifying the cutoff for adjusted p-values considered
#' significant.
#' @param pAdjustMethod
#' A single character value specifying the p-value adjustment method to be used
#' (see 'p.adjust' for details).
#' @param nPermutations
#' A single integer or numeric value specifying the number of permutations for
#' deriving p-values in GSEA.
#' @param minGeneSetSize
#' A single integer or numeric value specifying the minimum number of elements
#' shared by a gene set and the input total genes. Gene sets with
#' fewer than this number are removed from both hypergeometric analysis and GSEA.
#' @param exponent A single integer or numeric value used in weighting phenotypes in GSEA.
#' @param verbose A single logical value specifying to display detailed messages
#'  (when verbose=TRUE) or not (when verbose=FALSE).
#' @param doGSOA A single logical value specifying to perform gene set
#' overrepresentation analysis(hypergeometric test) (when doGSOA=TRUE) or not (when doGSOA=FALSE).
#' @param doGSEA A single logical value specifying to perform gene set
#' enrichment analysis (when doGSEA=TRUE) or not (when doGSEA=FALSE).
#'
#' @return In the end, this function will return an updated object of
#' class GSCA or NWA. All the analyzed results could be found in slot \emph{result}.
#' @include gsca_class.R
#' @export
#' @references
#' Aravind Subramanian, Pablo Tamayo, Vamsi K. Mootha, Sayan Mukherjee, Benjamin L. Ebert,
#' Michael A. Gillette, Amanda Paulovich, Scott L. Pomeroy, Todd R. Golub, Eric S. Lander, and Jill P. Mesirov
#' Gene set enrichment analysis: A knowledge-based approach for interpreting genome-wide expression profiles
#' PNAS 2005 102 (43) 15545-15550; published ahead of print September 30, 2005, doi:10.1073/pnas.0506580102
#' @examples
#' # ====================================================
#' # Gene Set Collection Analysis Part
#' library(org.Hs.eg.db)
#' library(GO.db)
#' library(KEGGREST)
#' ## load data for enrichment analyse
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
#'                     duplicateRemoverMethod="max", orderAbsValue=FALSE)
#'
#' ## support parallel calculation using doParallel package
#' if (requireNamespace("doParallel", quietly=TRUE)) {
#' doParallel::registerDoParallel(cores=2)
#' } else {
#' }
#'
#' ## do hypergeometric tests and GSEA
#' gsca2 <- analyze(gsca1, para=list(pValueCutoff=0.01, pAdjustMethod ="BH",
#'                                   nPermutations=100, minGeneSetSize=10, exponent=1),
#'                                   doGSOA = TRUE, doGSEA = TRUE)
#'
#' ## summarize gsca2 and get results
#' summarize(gsca2)
#' head(getResult(gsca2)$GSEA.results$GO_MF)
#' head(getResult(gsca2)$HyperGeo.results$PW_KEGG)
#'

setMethod("analyze", signature = "GSCA",
          function(object,
                   para = list(
                     pValueCutoff = 0.05,
                     pAdjustMethod = "BH",
                     nPermutations = 1000,
                     minGeneSetSize = 15,
                     exponent = 1),
                   verbose = TRUE,
                   doGSOA = FALSE,
                   doGSEA = TRUE) {

            paraCheck("General", "verbose", verbose)
            paraCheck("Analyze", "doGSOA", doGSOA)
            paraCheck("Analyze", "doGSEA", doGSEA)
            paraCheck("Analyze", "GSCAPara", para)
            if ((!doGSOA) && (!doGSEA))
              stop("Please set doGSOA (hypergeometric tests) and/or doGSEA (GSEA) to TURE!\n")


            object@para <- para
            ## update summary information
            object@summary$para$hypergeo[1, ] <- c(
              object@para$minGeneSetSize,
              object@para$pValueCutoff,
              object@para$pAdjustMethod
            )
            object@summary$para$gsea[1, ] <- c(
              object@para$minGeneSetSize,
              object@para$pValueCutoff,
              object@para$pAdjustMethod,
              object@para$nPermutations,
              object@para$exponent
            )

            ## do analysis
            object@result <-
              analyzeGeneSetCollections(
                listOfGeneSetCollections = object@listOfGeneSetCollections,
                geneList = object@geneList,
                hits = object@hits,
                pAdjustMethod = object@para$pAdjustMethod,
                pValueCutoff = object@para$pValueCutoff,
                nPermutations = object@para$nPermutations,
                minGeneSetSize = object@para$minGeneSetSize,
                exponent = object@para$exponent,
                verbose = verbose,
                doGSOA = doGSOA,
                doGSEA = doGSEA
              )

            ## update summary information(first analyze and then update summary)
            cols <- colnames(object@summary$results)
            object@summary$results[1:3, ] <- NA
            if (doGSOA) {
              object@summary$gsc[, 2] <-
                unlist(lapply(object@result$HyperGeo.results, nrow))[cols]
              object@summary$results["HyperGeo", ] <-
                unlist(lapply(object@result$HyperGeo.results, function(df) {
                  sum(df[, "Adjusted.Pvalue"] < object@para$pValueCutoff)
                }))[cols]
            }
            if (doGSEA) {
              object@summary$gsc[, 2] <-
                unlist(lapply(object@result$GSEA.results, nrow))[cols]
              object@summary$results["GSEA", ] <-
                unlist(lapply(object@result$GSEA.results, function(df) {
                  sum(df[, "Adjusted.Pvalue"] < object@para$pValueCutoff)
                }))[cols]
            }
            if (doGSOA && doGSEA) {
              object@summary$results["Both", ] <-
                unlist(lapply(object@result$Sig.adj.pvals.in.both, nrow))[cols]
            }
            object
          })

#=====================================================================================
##This function takes a list of gene set collections, a named phenotype
##vector (with names(phenotype vector)=GeneUniverse), a vector of hits
##(names only) and returns the results of hypergeometric and gene set
##enrichment analysis for all of the gene set collections (with multiple
##hypothesis testing correction).
analyzeGeneSetCollections <-
  function(listOfGeneSetCollections,
           geneList,
           hits,
           pAdjustMethod = "BH",
           pValueCutoff = 0.05,
           nPermutations = 1000,
           minGeneSetSize = 15,
           exponent = 1,
           verbose = TRUE,
           doGSOA = TRUE,
           doGSEA = TRUE) {
    ## check arguments
    if ((!doGSOA) && (!doGSEA))
      stop(
        "Please choose to perform hypergeometric tests and/or GSEA by specifying 'doGSOA' and 'doGSEA'!\n"
      )
    if (doGSOA || doGSEA) {
      paraCheck("GSCAClass", "gscs", listOfGeneSetCollections)
      paraCheck("GSCAClass", "genelist", geneList)
    }
    if (doGSOA) {
      paraCheck("Analyze", "hits", hits)
    }

    numGeneSetCollections <- length(listOfGeneSetCollections)

    ## filter 'listOfGeneSetCollections' by 'minGeneSetSize'
    maxOverlap <- 0
    for (i in seq_along(listOfGeneSetCollections)) {
      gs.size <- vapply(listOfGeneSetCollections[[i]],
                        function(x) length(intersect(x, names(geneList))), integer(1))
      maxOverlap <- max(max(gs.size), maxOverlap)
      discarded <- sum(gs.size < minGeneSetSize)
      listOfGeneSetCollections[[i]] <-
        listOfGeneSetCollections[[i]][gs.size >= minGeneSetSize]

      ## output information about filtering of gene set collections
      if (verbose && discarded > 0)
        cat(
          paste(
            "--",
            discarded,
            " gene sets don't have >= ",
            minGeneSetSize,
            " overlapped genes with universe in gene",
            " set collection named ",
            names(listOfGeneSetCollections)[i],
            "!\n",
            sep = ""
          )
        )
    }

    ## stop when no gene set passes the size requirement
    if (sum(sapply(listOfGeneSetCollections, length)) == 0) {
      stop(
        paste(
          "No gene set has >= ",
          minGeneSetSize,
          " overlapped ",
          "genes with universe!\n The largest number of overlapped ",
          "genes between gene sets and universe is: ",
          maxOverlap,
          sep = ""
        )
      )
    }

    ## Hypergeometric test
    if (doGSOA) {
      HGTresults <- calcHyperGeo(
        listOfGeneSetCollections,
        geneList,
        hits,
        minGeneSetSize,
        pAdjustMethod,
        verbose
      )
      cat("-Hypergeometric analysis complete\n\n")
    } else {
      HGTresults = NULL
    }


    ## GSEA
    if (doGSEA) {
      GSEA.results.list <-
        calcGSEA(
          listOfGeneSetCollections,
          geneList,
          exponent,
          nPermutations,
          minGeneSetSize,
          pAdjustMethod,
          verbose
        )
      cat("-Gene set enrichment analysis complete \n")
      cat("==============================================\n\n")
    } else {
      GSEA.results.list = NULL
    }

    if (doGSOA && doGSEA) {
      ## identify gene set collections with hypergeometric test
      ## pvalues < pValueCutoff
      sign.hgt <- lapply(HGTresults, function(x) {
        if (nrow(x) > 0) {
          a <- which(x[, "Pvalue"] < pValueCutoff)
          x <- x[a, , drop = FALSE]
          if (length(a) > 1) {
            x <- x[order(x[, "Pvalue"]), , drop = FALSE]
          }
          return(x)
        }
      })
      ## identify gene set collections with hypergeometric test adjusted
      ## pvalues < pValueCutoff
      sign.hgt.adj <- lapply(HGTresults, function(x) {
        if (nrow(x) > 0) {
          a <- which(x[, "Adjusted.Pvalue"] < pValueCutoff)
          x <- x[a, , drop = FALSE]
          if (length(a) > 1) {
            x <- x[order(x[, "Adjusted.Pvalue"]), , drop = FALSE]
          }
          return(x)
        }
      })

      ## identify gene set collections with GSEA pvalues < pValueCutoff
      sign.gsea <- lapply(GSEA.results.list, function(x) {
        if (nrow(x) > 0) {
          a <- which(x[, "Pvalue"] < pValueCutoff)
          x <- x[a, , drop = FALSE]
          if (length(a) > 1) {
            x <- x[order(x[, "Pvalue"]), , drop = FALSE]
          }
          return(x)
        }
      })
      ## identify gene set collections with adjusted GSEA pvalues < pValueCutoff
      sign.gsea.adj <- lapply(GSEA.results.list, function(x) {
        if (nrow(x) > 0) {
          a <- which(x[, "Adjusted.Pvalue"] <= pValueCutoff)
          x <- x[a, , drop = FALSE]
          if (length(a) > 1) {
            x <- x[order(x[, "Adjusted.Pvalue"]), , drop = FALSE]
          }
          return(x)
        }
      })

      overlap <- list()
      overlap.adj <- list()
      ## identify gene set collections with significant pvalues and/or
      # adjusted pvalues from both GSEA and hypergeometric testing
      sapply(1:numGeneSetCollections, function(i) {
        a1 <- intersect(rownames(sign.gsea[[i]]),
                        rownames(sign.hgt[[i]]))
        a2 <- intersect(rownames(sign.gsea.adj[[i]]),
                        rownames(sign.hgt.adj[[i]]))
        Hypergeometric.Pvalue <-
          HGTresults[[i]][a1, "Pvalue", drop = FALSE]
        Hypergeometric.Adj.Pvalue <-
          HGTresults[[i]][a2, "Adjusted.Pvalue", drop = FALSE]
        GSEA.Pvalue <-
          GSEA.results.list[[i]][a1, "Pvalue", drop = FALSE]
        GSEA.Adj.Pvalue <-
          GSEA.results.list[[i]][a2, "Adjusted.Pvalue", drop = FALSE]

        overlap[[i]] <<- cbind(Hypergeometric.Pvalue, GSEA.Pvalue)
        colnames(overlap[[i]]) <<-
          c("HyperGeo.Pvalue", "GSEA.Pvalue")
        overlap.adj[[i]] <<-
          cbind(Hypergeometric.Adj.Pvalue, GSEA.Adj.Pvalue)
        colnames(overlap.adj[[i]]) <<-
          c("HyperGeo.Adj.Pvalue", "GSEA.Adj.Pvalue")
      })
      names(overlap) <- names(listOfGeneSetCollections)
      names(overlap.adj) <- names(listOfGeneSetCollections)
    } else {
      overlap = NULL
      overlap.adj = NULL
    }

    final.results <- list(
      "HyperGeo.results" = HGTresults,
      "GSEA.results" = GSEA.results.list,
      "Sig.pvals.in.both" = overlap,
      "Sig.adj.pvals.in.both" = overlap.adj
    )
    return(final.results)
  }

#================================================================================
#' @importFrom stats p.adjust
calcHyperGeo <- function (listOfGeneSetCollections,
                          geneList,
                          hits,
                          minGeneSetSize = 15,
                          pAdjustMethod = "BH",
                          verbose = TRUE) {
  if (verbose) {
    cat("-Performing hypergeometric analysis ...\n")
  }

  combinedGeneSets <-
    unlist(listOfGeneSetCollections,
           recursive = FALSE,
           use.names = FALSE)
  names(combinedGeneSets) <-
    unlist(lapply(listOfGeneSetCollections, names), use.names = FALSE)

  universe = names(geneList)
  overlappedSize <-
    sapply(combinedGeneSets, function(geneSet)
      sum(geneSet %in% universe))

  if (all(overlappedSize < minGeneSetSize)) {
    stop(
      paste(
        "The largest number of overlapped genes of gene sets with universe is: ",
        max(overlappedSize),
        ", which is < ",
        minGeneSetSize,
        "!\n",
        sep = ""
      )
    )
  }

  res <-
    sapply(combinedGeneSets[overlappedSize >= minGeneSetSize], calcHGTScore, universe, hits)
  res <- t(res)
  res[, "Adjusted.Pvalue"] <-
    p.adjust(res[, "Pvalue"], method = pAdjustMethod)

  results <- list()
  ## Extract results dataframe for each gene set collection and orders them
  # by adjusted p-value
  sapply(seq_along(listOfGeneSetCollections), function(i) {
    extracted <-
      res[rownames(res) %in% names(listOfGeneSetCollections[[i]]), , drop = FALSE]
    extracted <-
      extracted[order(extracted[, "Adjusted.Pvalue"]), , drop = FALSE]
    results[[i]] <<- extracted
  })

  names(results) <- names(listOfGeneSetCollections)
  return(results)
}

##This function takes in a single gene set (GeneSet), a vector
##(GeneList) of gene symbols for all tested genes, a vector of "hits"
##(hits), and a p-value adjustment method. It outputs a vector
##containing the size of the gene universe, the size of the gene set
##within this universe (i.e. how many genes from the universe map to
##this gene set), the total number of hits, the number of hits expected
##to occur in the gene set, the actual hits observed in the gene set,
##and the pvalue from a hypergeometric test.
## calculate the hypergeometric Test Score for one set
calcHGTScore <- function(geneSet, universe, hits) {
  geneSet <- intersect(geneSet, universe)
  overlap <- intersect(geneSet, hits)

  N <- length(universe)
  m <- length(geneSet)
  k <- length(overlap)
  n <- length(hits)
  ex <- (n / N) * m
  HGTresult <-
    ifelse(m == 0, NA, stats::phyper(k - 1, m, N - m, n, lower.tail = FALSE))
  hyp.vec <- c(N, m, n, ex, k, HGTresult, NA)
  names(hyp.vec) <-
    c(
      "Universe Size",
      "Gene Set Size",
      "Total Hits",
      "Expected Hits",
      "Observed Hits",
      "Pvalue",
      "Adjusted.Pvalue"
    )
  return(hyp.vec)
}


#' @import foreach
#' @importFrom  stats p.adjust
calcGSEA <-
  function(listOfGeneSetCollections,
           geneList,
           exponent = 1,
           nPermutations = 1000,
           minGeneSetSize = 15,
           pAdjustMethod = "BH",
           verbose = TRUE) {

    listNames <- names(geneList)

    combinedGeneSets <-
      unlist(listOfGeneSetCollections,
             recursive = FALSE,
             use.names = FALSE)
    names(combinedGeneSets) <-
      unlist(lapply(listOfGeneSetCollections, names), use.names = FALSE)
    groupInfo <-
      rep(names(listOfGeneSetCollections),
          lapply(listOfGeneSetCollections, length))

    if (verbose) {
      cat("-Performing gene set enrichment analysis ...", "\n")
      cat("--Calculating the permutations ...", "\n")
    }

    ## filter gene sets

    combinedGeneSets <- lapply(combinedGeneSets, intersect, listNames)
    overlaps <- sapply(combinedGeneSets, length)

    ind <- overlaps >= minGeneSetSize

    combinedGeneSets <- combinedGeneSets[ind]
    overlaps <- overlaps[ind]

    ## get scores

    # gScores <- sapply(combinedGeneSets, function(geneSet) calcGScoreCPP(listNames %in% geneSet, geneList, exponent))

    n_sep <- 1000
    gScores <-
      foreach(idx = 1:(ceiling(length(combinedGeneSets)/n_sep)), .combine = c, .packages="HTSanalyzeR2") %dopar% {
        sapply(combinedGeneSets[((idx-1)*n_sep + 1):min(idx*n_sep, length(combinedGeneSets))],
               function(geneSet) calcGScoreCPP(listNames %in% geneSet, geneList, exponent))
      }

    groups <- split(gScores, overlaps)
    overlaps2 <- as.integer(names(groups))

    res <-
      foreach(idx = seq_along(overlaps2), .combine = cbind, .packages="HTSanalyzeR2") %dopar% {

        overlap <- overlaps2[idx]
        hits <-
          rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
        perm <- sapply(1:nPermutations, function(x) {
          calcGScoreCPP(sample(hits), geneList, exponent)
        })

        values <- sapply(groups[[idx]], function(gScore) {
          p <- mean(gScore > perm)
          pVal <- 2 * (ifelse(p > 0.5, 1 - p, p))

          c(gScore, pVal, NA, overlap)
        })
      }

    res <- t(res)
    colnames(res) <-
      c("Observed.score",
        "Pvalue",
        "Adjusted.Pvalue",
        "overlap")

    res[, "Adjusted.Pvalue"] <-
      p.adjust(res[, "Pvalue"], method = pAdjustMethod)

    res <-
      res[, c("Observed.score", "Pvalue", "Adjusted.Pvalue")]

    results <- list()
    ## Extract results dataframe for each gene set collection and orders them
    # by adjusted p-value
    sapply(seq_along(listOfGeneSetCollections), function(i) {
      GSEA.res.mat <-
        res[rownames(res) %in% names(listOfGeneSetCollections[[i]]), , drop = FALSE]
      GSEA.res.mat <-
        GSEA.res.mat[order(GSEA.res.mat[, "Adjusted.Pvalue"]), , drop = FALSE]
      results[[i]] <<- GSEA.res.mat
    })

    names(results) <- names(listOfGeneSetCollections)
    return(results)
  }



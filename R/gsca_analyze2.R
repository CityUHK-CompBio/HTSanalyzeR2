## summarize & default show
setGeneric("analyze2", function(object, ...)
  standardGeneric("analyze2"), package = "HTSanalyzeR2")

#' @include gsca_class.R
#' @export analyze2
setMethod("analyze2",
          "GSCA",
          function(object,
                   para = list(
                     pValueCutoff = 0.05,
                     pAdjustMethod = "BH",
                     nPermutations = 1000,
                     minGeneSetSize = 15,
                     exponent = 1
                   ),
                   verbose = TRUE,
                   doGSOA = TRUE,
                   doGSEA = TRUE) {
            paraCheck(name = "doGSOA", para = doGSOA)
            paraCheck(name = "doGSEA", para = doGSEA)
            if ((!doGSOA) && (!doGSEA))
              stop("Please set doGSOA (hypergeometric tests) and/or doGSEA (GSEA) to TURE!\n")
            paraCheck(name = "verbose", para = verbose)
            # object@para <- paraCheck(name = "gsca.para", para = para)
            object@para <- para

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

            #######################
            ##    do analysis
            #######################
            object@result <-
              analyzeGeneSetCollections2(
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

            ##update summary information
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


##This function takes a list of gene set collections, a named phenotype
##vector (with names(phenotype vector)=GeneUniverse), a vector of hits
##(names only) and returns the results of hypergeometric and gene set
##enrichment analysis for all of the gene set collections (with multiple
##hypothesis testing correction).
analyzeGeneSetCollections2 <-
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
    ##check arguments
    if ((!doGSOA) && (!doGSEA))
      stop(
        "Please choose to perform hypergeometric tests and/or GSEA by specifying 'doGSOA' and 'doGSEA'!\n"
      )
    if (doGSOA || doGSEA) {
      paraCheck("gscs", listOfGeneSetCollections)
      paraCheck("genelist", geneList)
      paraCheck("pAdjustMethod", pAdjustMethod)
      paraCheck("pValueCutoff", pValueCutoff)
      paraCheck("minGeneSetSize", minGeneSetSize)
      paraCheck("verbose", verbose)
    }
    if (doGSOA) {
      paraCheck("hits", hits)
    }
    if (doGSEA) {
      paraCheck("nPermutations", nPermutations)
      paraCheck("exponent", exponent)
    }

    numGeneSetCollections <- length(listOfGeneSetCollections)

    ##filter 'istOfGeneSetCollections' by 'minGeneSetSize'
    maxOverlap <- 0
    for (i in seq_along(listOfGeneSetCollections)) {
      gs.size <- vapply(listOfGeneSetCollections[[i]], function(x) length(intersect(x, names(geneList))), integer(1))
      maxOverlap <- max(max(gs.size), maxOverlap)
      discarded <- sum(gs.size < minGeneSetSize)
      listOfGeneSetCollections[[i]] <- listOfGeneSetCollections[[i]][gs.size >= minGeneSetSize]

      ##output information about filtering of gene set collections
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

    ##stop when no gene set passes the size requirement
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

    ######################
    ###Hypergeometric test
    ######################
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


    ######################
    ###GSEA
    ######################
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
    } else {
      GSEA.results.list = NULL
    }

    if (doGSOA && doGSEA) {
      ##identify gene set collections with hypergeometric test
      ##pvalues < pValueCutoff
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
      ##identify gene set collections with hypergeometric test adjusted
      ##pvalues < pValueCutoff
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

      ##identify gene set collections with GSEA pvalues < pValueCutoff
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
      ##identify gene set collections with adjusted GSEA pvalues < pValueCutoff
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
      ##identify gene set collections with significant pvalues and/or
      ##adjusted pvalues from both GSEA and hypergeometric testing
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
    unlist(lapply(listOfGeneSetCollections, names), use.names = F)

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
  #Extract results dataframe for each gene set collection and orders them by adjusted p-value
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
    ifelse(m == 0, NA, stats::phyper(k - 1, m, N - m, n, lower.tail = F))
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
      unlist(lapply(listOfGeneSetCollections, names), use.names = F)
    groupInfo <-
      rep(names(listOfGeneSetCollections),
          lapply(listOfGeneSetCollections, length))
    # gli <- split(cobinedGeneSets, groupInfo)

    if (verbose) {
      cat("-Performing gene set enrichment analysis ...", "\n")
      cat("--Calculating the permutations ...", "\n")
    }

    ##tag the gene sets that can be used in the analysis, i.e. those
    ##that are smaller than the size of the gene list and that have more
    ##than 'minGeneSetSize' elements that can be found in the geneList
    tagGeneSets <- sapply(combinedGeneSets, function(geneSet)
      length(geneSet) < length(geneList) &&
        sum(geneSet %in% listNames) >= minGeneSetSize)
    combinedGeneSets <- combinedGeneSets[tagGeneSets]

    overlaps <-
      sapply(combinedGeneSets, function(geneSet)
        sum(listNames %in% geneSet))
    gScores <-
      sapply(combinedGeneSets, function(geneSet)
        calcGScoreCPP(listNames %in% geneSet, geneList))
    groups <- split(gScores, overlaps)
    overlaps2 <- as.integer(names(groups))

    permScores <-
      foreach(idx = seq_along(overlaps2), .combine = rbind) %dopar% {
        overlap <- overlaps2[idx]
        hits <-
          rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
        perm <- sapply(1:nPermutations, function(x) {
          calcGScoreCPP(sample(hits), geneList)
        })
        perm
      }

    rownames(permScores) <- names(groups)

    res <-
      foreach(idx = seq_along(groups), .combine = cbind) %dopar% {
        overlap <- overlaps2[idx]
        ## observedScore, Pvalue, Adjusted.Pvalue, FDR, overlap
        values <- sapply(groups[[idx]], function(gScore) {
          pVal <-
            ifelse(gScore > 0,
                   mean(permScores[idx,] > gScore),
                   mean(permScores[idx,] < gScore))
          c(gScore, pVal, NA, NA, overlap)
        })
      }



    res <- t(res)
    colnames(res) <-
      c("Observed.score",
        "Pvalue",
        "Adjusted.Pvalue",
        "FDR",
        "overlap")
    res[, "Adjusted.Pvalue"] <-
      p.adjust(res[, "Pvalue"], method = pAdjustMethod)
    res[, "FDR"] <-
      calcFDR(res[, "Observed.score"], res[, "overlap"], permScores)
    res <-
      res[, c("Observed.score", "Pvalue", "Adjusted.Pvalue", "FDR")]

    results <- list()
    #Extract results dataframe for each gene set collection and orders them by adjusted p-value
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


calcFDR <- function(gScores, overlaps, permScores) {
  permScores <- permScores[as.character(overlaps),]
  ldataScores <- length(gScores)
  FDRgeneset = rep(0, ldataScores)

  sapply(1:ldataScores, function(i) {
    pos <- which(permScores[i,] >= 0)
    neg <- which(permScores[i,] <= 0)
    PosAvg <- abs(mean(permScores[i, pos]))
    NegAvg <- abs(mean(permScores[i, neg]))

    permScores[i, pos] <<- permScores[i, pos] / PosAvg
    permScores[i, neg] <<- permScores[i, neg] / NegAvg

    gScores[i] <<-
      ifelse((gScores[i] < 0), (gScores[i] / NegAvg), (gScores[i] / PosAvg))
  })

  negtot <- sum(permScores <= 0)
  postot <- sum(permScores >= 0)

  sapply(1:ldataScores, function(i) {
    if (is.na(gScores[i])) {
      FDRgeneset[i] <<- 1
    } else if (gScores[i] < 0) {
      FDRgeneset[i] <<-
        (sum(permScores <= gScores[i]) / negtot) / (sum(gScores <= gScores[i]) / sum(gScores <= 0))
    } else {
      FDRgeneset[i] <<-
        (sum(permScores >= gScores[i]) / postot) / (sum(gScores >= gScores[i]) / sum(gScores >= 0))
    }
  })

  FDRgeneset[FDRgeneset > 1] <- 1

  names(FDRgeneset) <- names(gScores)
  return(FDRgeneset)
}


calcGScore <- function(hits, geneList, exponent = 1) {
  nh <- sum(hits)
  N <- length(geneList)
  ES <- 0
  runningES <- rep(0, N)
  if (nh) {
    tmp <- rep(0, N)
    NR = sum(abs(geneList[hits]) ^ exponent)
    tmp[hits] <- (abs(geneList[hits]) ^ exponent) / NR
    tmp[!hits] <- -1 / (N - nh)
    runningES <- cumsum(tmp)

    ESmax <- max(runningES)
    ESmin <- min(runningES)
    ES <- ifelse(abs(ESmin) > abs(ESmax), ESmin, ESmax)
  }
  ES
}

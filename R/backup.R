##Opt 4, user calcGScoreCPP
NULL

## opt2, combine genesets from overlap from all collections, then calculate the overlap
calcGSEAOpt2 <-
  function(listOfGeneSetCollections,
           geneList,
           exponent = 1,
           nPermutations = 100,
           minGeneSetSize = 15,
           pAdjustMethod = "BH",
           verbose = TRUE) {
    cat("hello calcGSEA\n")

    listNames <- names(geneList)

    combinedGeneSets <- unlist(listOfGeneSetCollections, recursive = FALSE, use.names = FALSE)
    names(combinedGeneSets) <- unlist(lapply(listOfGeneSetCollections, names), use.names = F)
    groupInfo <- rep(names(listOfGeneSetCollections), lapply(listOfGeneSetCollections, length))
    # gli <- split(cobinedGeneSets, groupInfo)

    if (verbose) {
      cat("--For", "doing gsea ", "\n")
    }

    ##tag the gene sets that can be used in the analysis, i.e. those
    ##that are smaller than the size of the gene list and that have more
    ##than 'minGeneSetSize' elements that can be found in the geneList
    nGeneSets <- length(combinedGeneSets)
    tagGeneSets <- rep(FALSE, nGeneSets)
    tagGeneSets[which(sapply(combinedGeneSets, length) < length(geneList))] <- TRUE
    tagGeneSets[which(sapply(lapply(combinedGeneSets, intersect, y = listNames), length) < minGeneSetSize)] <- FALSE
    combinedGeneSets <- combinedGeneSets[tagGeneSets]


    overlaps <- sapply(combinedGeneSets, function(geneSet) sum(listNames %in% geneSet))
    gScores <- sapply(combinedGeneSets, function(geneSet) calcGScore(listNames %in% geneSet, geneList))

    groups <- split(gScores, overlaps)
    overlaps2 <- as.integer(names(groups))

    res <- do.call(cbind, sapply(seq_along(groups), function(idx) {
      overlap <- overlaps2[idx]
      hits <- rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
      permutationRes <- sapply(1:nPermutations, function(x) {
        calcGScore(sample(hits), geneList)
      })

      pVals <- sapply(groups[[idx]], function(gScore) {
        pVal <- ifelse(gScore > 0, mean(permutationRes > gScore), mean(permutationRes < gScore))
        c(gScore, pVal, 1, 1, overlap)
      })
      pVals
    }))

    res <- t(res)
    res[, 3] <- p.adjust(res[, 2], method = pAdjustMethod)
    colnames(res) <- c("Observed.score", "Pvalue", "Adjusted.Pvalue", "FDR", "overlap")
    res[order(res[, 3]), ]

    results <- list()

    ##Extract results dataframe for each gene set collection and orders them by adjusted p-value
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


## for each collection. user overlap
calcGSEAOpt1 <-
  function(listOfGeneSetCollections,
           geneList,
           exponent = 1,
           nPermutations = 100,
           minGeneSetSize = 15,
           pAdjustMethod = "BH",
           verbose = TRUE) {
    cat("hello calcGSEA\n")


    listNames <- names(geneList)
    results <- lapply(listOfGeneSetCollections, function(geneSets) {

      if (verbose) {
        cat("--calc GSEA, each collection", "\n")
      }

      ##tag the gene sets that can be used in the analysis, i.e. those
      ##that are smaller than the size of the gene list and that have more
      ##than 'minGeneSetSize' elements that can be found in the geneList
      nGeneSets <- length(geneSets)
      tagGeneSets <- rep(FALSE, nGeneSets)
      tagGeneSets[which(sapply(geneSets, length) < length(geneList))] <- TRUE
      tagGeneSets[which(sapply(lapply(geneSets, intersect, y = listNames), length) < minGeneSetSize)] <- FALSE
      geneSets <- geneSets[tagGeneSets]

      overlaps <- sapply(geneSets, function(geneSet) sum(listNames %in% geneSet))
      gScores <- sapply(geneSets, function(geneSet) calcGScore(listNames %in% geneSet, geneList))

      groups <- split(gScores, overlaps)
      overlaps2 <- as.integer(names(groups))

      res <- do.call(cbind, sapply(seq_along(groups), function(idx) {
        overlap <- overlaps2[idx]
        hits <- rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
        permutationRes <- sapply(1:nPermutations, function(x) {
          calcGScore(sample(hits), geneList)
        })

        pVals <- sapply(groups[[idx]], function(gScore) {
          pVal <- ifelse(gScore > 0, mean(permutationRes > gScore), mean(permutationRes < gScore))
          c(gScore, pVal, 1, 1, overlap)
        })
        pVals
      }))

      res <- t(res)
      res[, 3] <- p.adjust(res[, 2], method = pAdjustMethod)
      colnames(res) <- c("Observed.score", "Pvalue", "Adjusted.Pvalue", "FDR", "overlap")
      res[order(res[, 3]), ]
    })

    return(results)
  }


## raw, permutations for each set, same with previous
calcGSEARaw <-
  function(listOfGeneSetCollections,
           geneList,
           exponent = 1,
           nPermutations = 100,
           minGeneSetSize = 15,
           pAdjustMethod = "BH",
           verbose = TRUE) {
    cat("hello calcGSEA\n")

    listNames <- names(geneList)

    results <- lapply(listOfGeneSetCollections, function(geneSets) {
      if (verbose) {
        cat("--calc GSEA raw", "\n")
      }

      ##tag the gene sets that can be used in the analysis, i.e. those
      ##that are smaller than the size of the gene list and that have more
      ##than 'minGeneSetSize' elements that can be found in the geneList
      nGeneSets <- length(geneSets)
      tagGeneSets <- rep(FALSE, nGeneSets)
      tagGeneSets[which(sapply(geneSets, length) < length(geneList))] <- TRUE
      tagGeneSets[which(sapply(lapply(geneSets, intersect, y = listNames), length) < minGeneSetSize)] <- FALSE
      geneSets <- geneSets[tagGeneSets]

      res <- t(sapply(geneSets, function(geneSet) {
        hits <- listNames %in% geneSet
        gScore <- calcGScore(hits, geneList)
        permutationRes <- sapply(1:nPermutations, function(x) {
          calcGScore(sample(hits), geneList)
        })

        ## gao feng
        ## p <- mean(gScore > permutationRes)
        ## pVal <- 2 * ifelse(p > 0.5, 1 - p, p)
        pVal <- ifelse(gScore > 0, mean(permutationRes > gScore), mean(permutationRes < gScore))

        c(gScore, pVal, 1, 1)
      }))

      res[, 3] <- p.adjust(res[, 2], method = pAdjustMethod)
      colnames(res) <- c("Observed.score", "Pvalue", "Adjusted.Pvalue", "FDR")
      res[order(res[, 3]), ]
    })

    return(results)
  }


calcGSEA_comp <-
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
    }

    ##tag the gene sets that can be used in the analysis, i.e. those
    ##that are smaller than the size of the gene list and that have more
    ##than 'minGeneSetSize' elements that can be found in the geneList
    nGeneSets <- length(combinedGeneSets)
    tagGeneSets <- rep(FALSE, nGeneSets)
    tagGeneSets[which(sapply(combinedGeneSets, length) < length(geneList))] <-
      TRUE
    tagGeneSets[which(sapply(lapply(combinedGeneSets, intersect, y = listNames), length) < minGeneSetSize)] <-
      FALSE
    combinedGeneSets <- combinedGeneSets[tagGeneSets]


    overlaps <-
      sapply(combinedGeneSets, function(geneSet)
        sum(listNames %in% geneSet))
    gScores <-
      sapply(combinedGeneSets, function(geneSet)
        calcGScoreCPP(listNames %in% geneSet, geneList))

    groups <- split(gScores, overlaps)
    overlaps2 <- as.integer(names(groups))

    res <- do.call(cbind, sapply(seq_along(groups), function(idx) {
      overlap <- overlaps2[idx]
      hits <- rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
      permutationRes <- sapply(1:nPermutations, function(x) {
        calcGScoreCPP(sample(hits), geneList)
      })

      pVals <- sapply(groups[[idx]], function(gScore) {
        pVal <-
          ifelse(gScore > 0,
                 mean(permutationRes > gScore),
                 mean(permutationRes < gScore))
        c(gScore, pVal, NA, NA, overlap)
      })
      pVals
    }))

    res <- t(res)
    colnames(res) <-
      c("Observed.score",
        "Pvalue",
        "Adjusted.Pvalue",
        "FDR",
        "overlap")

    res[, "Adjusted.Pvalue"] <-
      p.adjust(res[, "Pvalue"], method = pAdjustMethod)
    # res[, "FDR"] <- calcFDR();

    results <- list()
    ##Extract results dataframe for each gene set collection and orders them by adjusted p-value
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

##This function computes the FDR associated with a permutation-based
##p-value from the GSEA on a list of gene sets

FDRcollectionGsea2 <- function(permScores, dataScores) {
  ##check arguments
  if (!is.matrix(permScores))
    stop("'permScores' should be a matrix!\n")
  if (!is.numeric(dataScores) && !is.integer(dataScores))
    stop("'dataScores' should be an integer or numeric vector!\n")
  if (is.null(names(dataScores)))
    stop("'dataScores' should be named (by gene set identifier)")
  if (nrow(permScores) != length(dataScores))
    stop(
      paste(
        "The number of rows of the 'permScores' matrix ",
        "should be the same as the length of the 'dataScores' vector",
        sep = ""
      )
    )
  ##create a vector to store the FDRs
  ldataScores <- length(dataScores)
  FDRgeneset = rep(0, ldataScores)
  ##Compute the normalized enrichment score (i.e. divide each ES
  ##(experimental or permutation-based) by the mean of all negative/positive
  ##(depending on the sign of the ES) permutation based scores for that gene set)
  ##This is done gene set by gene set
  sapply(1:ldataScores, function(i) {
    ##Get the indices of all negative permutation-based score
    neg <- which(permScores[i,] <= 0)
    ##Get the indices of all positive permutation-based score
    pos <- which(permScores[i,] >= 0)
    ##Average the values, separately for positive and negative scores
    NegAvg <- abs(mean(permScores[i, neg]))
    PosAvg <- abs(mean(permScores[i, pos]))
    ##Normalize the permutation-based scores,
    ##separately for negative and positive scores
    permScores[i, neg] <<- permScores[i, neg] / NegAvg
    permScores[i, pos] <<- permScores[i, pos] / PosAvg
    ##Normalize the observed scores, separately for negative and positive scores
    dataScores[i] <<- ifelse((dataScores[i] < 0),
                             (dataScores[i] / NegAvg),
                             (dataScores[i] / PosAvg))
  })
  ##Compute the total number of negative/positive scores across all
  ##permutations and all gene sets
  negtot <- length(which(permScores <= 0))
  postot <- length(which(permScores >= 0))
  ##Compute the FDR by comparing:
  ##for negative observed ES:
  ##the number of permutation-based scores under the observed ES
  ##divided by the total number of negative permutation based scores
  ##to the number of observed scores under the observed ES divided
  ##by the total number of negative observed scores
  ##for positive observed ES:
  ##the number of permutation-based scores over the observed ES
  ##divided by the total number of positive permutation based scores
  ##to the number of observed scores over the observed ES divided
  ##by the total number of positive observed scores
  sapply(1:ldataScores, function(i) {
    if (is.na(dataScores[i])) {
      FDRgeneset[i] <<- 1
    } else if (dataScores[i] < 0) {
      FDRgeneset[i] <<-
        (sum(permScores <= dataScores[i]) / negtot) / (sum(dataScores <= dataScores[i]) / sum(dataScores <= 0))
    } else {
      FDRgeneset[i] <<-
        (sum(permScores >= dataScores[i]) / postot) / (sum(dataScores >= dataScores[i]) / sum(dataScores >= 0))
    }
    FDRgeneset[i] <<- ifelse(FDRgeneset[i] > 1, 1, FDRgeneset[i])
  })
  #name the FDRs (by gene set name) and return the vector
  names(FDRgeneset) <- names(dataScores)
  return(FDRgeneset)
}


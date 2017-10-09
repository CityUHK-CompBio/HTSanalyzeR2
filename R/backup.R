#' #' Load the gene set
#' #'
#' #' temporary function for loading gene sets
#' #'
#' #' @return a list of genesets
#' #'
#' loadGeneSet <- function () {
#'   data("gene_sets", envir = environment())
#'   ListGSC
#' }
#'
#'
#' ##Opt 4, user calcGScoreCPP
#' NULL
#'
#' ## opt2, combine genesets from overlap from all collections, then calculate the overlap
#' calcGSEAOpt2 <-
#'   function(listOfGeneSetCollections,
#'            geneList,
#'            exponent = 1,
#'            nPermutations = 100,
#'            minGeneSetSize = 15,
#'            pAdjustMethod = "BH",
#'            verbose = TRUE) {
#'     cat("hello calcGSEA\n")
#'
#'     listNames <- names(geneList)
#'
#'     combinedGeneSets <- unlist(listOfGeneSetCollections, recursive = FALSE, use.names = FALSE)
#'     names(combinedGeneSets) <- unlist(lapply(listOfGeneSetCollections, names), use.names = F)
#'     groupInfo <- rep(names(listOfGeneSetCollections), lapply(listOfGeneSetCollections, length))
#'     # gli <- split(cobinedGeneSets, groupInfo)
#'
#'     if (verbose) {
#'       cat("--For", "doing gsea ", "\n")
#'     }
#'
#'     ##tag the gene sets that can be used in the analysis, i.e. those
#'     ##that are smaller than the size of the gene list and that have more
#'     ##than 'minGeneSetSize' elements that can be found in the geneList
#'     nGeneSets <- length(combinedGeneSets)
#'     tagGeneSets <- rep(FALSE, nGeneSets)
#'     tagGeneSets[which(sapply(combinedGeneSets, length) < length(geneList))] <- TRUE
#'     tagGeneSets[which(sapply(lapply(combinedGeneSets, intersect, y = listNames), length) < minGeneSetSize)] <- FALSE
#'     combinedGeneSets <- combinedGeneSets[tagGeneSets]
#'
#'
#'     overlaps <- sapply(combinedGeneSets, function(geneSet) sum(listNames %in% geneSet))
#'     gScores <- sapply(combinedGeneSets, function(geneSet) calcGScore(listNames %in% geneSet, geneList))
#'
#'     groups <- split(gScores, overlaps)
#'     overlaps2 <- as.integer(names(groups))
#'
#'     res <- do.call(cbind, sapply(seq_along(groups), function(idx) {
#'       overlap <- overlaps2[idx]
#'       hits <- rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
#'       permutationRes <- sapply(1:nPermutations, function(x) {
#'         calcGScore(sample(hits), geneList)
#'       })
#'
#'       pVals <- sapply(groups[[idx]], function(gScore) {
#'         pVal <- ifelse(gScore > 0, mean(permutationRes > gScore), mean(permutationRes < gScore))
#'         c(gScore, pVal, 1, 1, overlap)
#'       })
#'       pVals
#'     }))
#'
#'     res <- t(res)
#'     res[, 3] <- p.adjust(res[, 2], method = pAdjustMethod)
#'     colnames(res) <- c("Observed.score", "Pvalue", "Adjusted.Pvalue", "FDR", "overlap")
#'     res[order(res[, 3]), ]
#'
#'     results <- list()
#'
#'     ##Extract results dataframe for each gene set collection and orders them by adjusted p-value
#'     sapply(seq_along(listOfGeneSetCollections), function(i) {
#'       GSEA.res.mat <-
#'         res[rownames(res) %in% names(listOfGeneSetCollections[[i]]), , drop = FALSE]
#'       GSEA.res.mat <-
#'         GSEA.res.mat[order(GSEA.res.mat[, "Adjusted.Pvalue"]), , drop = FALSE]
#'       results[[i]] <<- GSEA.res.mat
#'     })
#'
#'     names(results) <- names(listOfGeneSetCollections)
#'
#'     return(results)
#'   }
#'
#'
#' ## for each collection. user overlap
#' calcGSEAOpt1 <-
#'   function(listOfGeneSetCollections,
#'            geneList,
#'            exponent = 1,
#'            nPermutations = 100,
#'            minGeneSetSize = 15,
#'            pAdjustMethod = "BH",
#'            verbose = TRUE) {
#'     cat("hello calcGSEA\n")
#'
#'
#'     listNames <- names(geneList)
#'     results <- lapply(listOfGeneSetCollections, function(geneSets) {
#'
#'       if (verbose) {
#'         cat("--calc GSEA, each collection", "\n")
#'       }
#'
#'       ##tag the gene sets that can be used in the analysis, i.e. those
#'       ##that are smaller than the size of the gene list and that have more
#'       ##than 'minGeneSetSize' elements that can be found in the geneList
#'       nGeneSets <- length(geneSets)
#'       tagGeneSets <- rep(FALSE, nGeneSets)
#'       tagGeneSets[which(sapply(geneSets, length) < length(geneList))] <- TRUE
#'       tagGeneSets[which(sapply(lapply(geneSets, intersect, y = listNames), length) < minGeneSetSize)] <- FALSE
#'       geneSets <- geneSets[tagGeneSets]
#'
#'       overlaps <- sapply(geneSets, function(geneSet) sum(listNames %in% geneSet))
#'       gScores <- sapply(geneSets, function(geneSet) calcGScore(listNames %in% geneSet, geneList))
#'
#'       groups <- split(gScores, overlaps)
#'       overlaps2 <- as.integer(names(groups))
#'
#'       res <- do.call(cbind, sapply(seq_along(groups), function(idx) {
#'         overlap <- overlaps2[idx]
#'         hits <- rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
#'         permutationRes <- sapply(1:nPermutations, function(x) {
#'           calcGScore(sample(hits), geneList)
#'         })
#'
#'         pVals <- sapply(groups[[idx]], function(gScore) {
#'           pVal <- ifelse(gScore > 0, mean(permutationRes > gScore), mean(permutationRes < gScore))
#'           c(gScore, pVal, 1, 1, overlap)
#'         })
#'         pVals
#'       }))
#'
#'       res <- t(res)
#'       res[, 3] <- p.adjust(res[, 2], method = pAdjustMethod)
#'       colnames(res) <- c("Observed.score", "Pvalue", "Adjusted.Pvalue", "FDR", "overlap")
#'       res[order(res[, 3]), ]
#'     })
#'
#'     return(results)
#'   }
#'
#'
#' ## raw, permutations for each set, same with previous
#' calcGSEARaw <-
#'   function(listOfGeneSetCollections,
#'            geneList,
#'            exponent = 1,
#'            nPermutations = 100,
#'            minGeneSetSize = 15,
#'            pAdjustMethod = "BH",
#'            verbose = TRUE) {
#'     cat("hello calcGSEA\n")
#'
#'     listNames <- names(geneList)
#'
#'     results <- lapply(listOfGeneSetCollections, function(geneSets) {
#'       if (verbose) {
#'         cat("--calc GSEA raw", "\n")
#'       }
#'
#'       ##tag the gene sets that can be used in the analysis, i.e. those
#'       ##that are smaller than the size of the gene list and that have more
#'       ##than 'minGeneSetSize' elements that can be found in the geneList
#'       nGeneSets <- length(geneSets)
#'       tagGeneSets <- rep(FALSE, nGeneSets)
#'       tagGeneSets[which(sapply(geneSets, length) < length(geneList))] <- TRUE
#'       tagGeneSets[which(sapply(lapply(geneSets, intersect, y = listNames), length) < minGeneSetSize)] <- FALSE
#'       geneSets <- geneSets[tagGeneSets]
#'
#'       res <- t(sapply(geneSets, function(geneSet) {
#'         hits <- listNames %in% geneSet
#'         gScore <- calcGScore(hits, geneList)
#'         permutationRes <- sapply(1:nPermutations, function(x) {
#'           calcGScore(sample(hits), geneList)
#'         })
#'
#'         ## gao feng
#'         ## p <- mean(gScore > permutationRes)
#'         ## pVal <- 2 * ifelse(p > 0.5, 1 - p, p)
#'         pVal <- ifelse(gScore > 0, mean(permutationRes > gScore), mean(permutationRes < gScore))
#'
#'         c(gScore, pVal, 1, 1)
#'       }))
#'
#'       res[, 3] <- p.adjust(res[, 2], method = pAdjustMethod)
#'       colnames(res) <- c("Observed.score", "Pvalue", "Adjusted.Pvalue", "FDR")
#'       res[order(res[, 3]), ]
#'     })
#'
#'     return(results)
#'   }
#'
#'
#' calcGSEA_comp <-
#'   function(listOfGeneSetCollections,
#'            geneList,
#'            exponent = 1,
#'            nPermutations = 1000,
#'            minGeneSetSize = 15,
#'            pAdjustMethod = "BH",
#'            verbose = TRUE) {
#'     listNames <- names(geneList)
#'
#'     combinedGeneSets <-
#'       unlist(listOfGeneSetCollections,
#'              recursive = FALSE,
#'              use.names = FALSE)
#'     names(combinedGeneSets) <-
#'       unlist(lapply(listOfGeneSetCollections, names), use.names = F)
#'     groupInfo <-
#'       rep(names(listOfGeneSetCollections),
#'           lapply(listOfGeneSetCollections, length))
#'     # gli <- split(cobinedGeneSets, groupInfo)
#'
#'     if (verbose) {
#'       cat("-Performing gene set enrichment analysis ...", "\n")
#'     }
#'
#'     ##tag the gene sets that can be used in the analysis, i.e. those
#'     ##that are smaller than the size of the gene list and that have more
#'     ##than 'minGeneSetSize' elements that can be found in the geneList
#'     nGeneSets <- length(combinedGeneSets)
#'     tagGeneSets <- rep(FALSE, nGeneSets)
#'     tagGeneSets[which(sapply(combinedGeneSets, length) < length(geneList))] <-
#'       TRUE
#'     tagGeneSets[which(sapply(lapply(combinedGeneSets, intersect, y = listNames), length) < minGeneSetSize)] <-
#'       FALSE
#'     combinedGeneSets <- combinedGeneSets[tagGeneSets]
#'
#'
#'     overlaps <-
#'       sapply(combinedGeneSets, function(geneSet)
#'         sum(listNames %in% geneSet))
#'     gScores <-
#'       sapply(combinedGeneSets, function(geneSet)
#'         calcGScoreCPP(listNames %in% geneSet, geneList))
#'
#'     groups <- split(gScores, overlaps)
#'     overlaps2 <- as.integer(names(groups))
#'
#'     res <- do.call(cbind, sapply(seq_along(groups), function(idx) {
#'       overlap <- overlaps2[idx]
#'       hits <- rep(c(TRUE, FALSE), c(overlap, length(geneList) - overlap))
#'       permutationRes <- sapply(1:nPermutations, function(x) {
#'         calcGScoreCPP(sample(hits), geneList)
#'       })
#'
#'       pVals <- sapply(groups[[idx]], function(gScore) {
#'         pVal <-
#'           ifelse(gScore > 0,
#'                  mean(permutationRes > gScore),
#'                  mean(permutationRes < gScore))
#'         c(gScore, pVal, NA, NA, overlap)
#'       })
#'       pVals
#'     }))
#'
#'     res <- t(res)
#'     colnames(res) <-
#'       c("Observed.score",
#'         "Pvalue",
#'         "Adjusted.Pvalue",
#'         "FDR",
#'         "overlap")
#'
#'     res[, "Adjusted.Pvalue"] <-
#'       p.adjust(res[, "Pvalue"], method = pAdjustMethod)
#'     # res[, "FDR"] <- calcFDR();
#'
#'     results <- list()
#'     ##Extract results dataframe for each gene set collection and orders them by adjusted p-value
#'     sapply(seq_along(listOfGeneSetCollections), function(i) {
#'       GSEA.res.mat <-
#'         res[rownames(res) %in% names(listOfGeneSetCollections[[i]]), , drop = FALSE]
#'       GSEA.res.mat <-
#'         GSEA.res.mat[order(GSEA.res.mat[, "Adjusted.Pvalue"]), , drop = FALSE]
#'       results[[i]] <<- GSEA.res.mat
#'     })
#'
#'     names(results) <- names(listOfGeneSetCollections)
#'     return(results)
#'   }
#'
#' ##This function computes the FDR associated with a permutation-based
#' ##p-value from the GSEA on a list of gene sets
#'
#' FDRcollectionGsea2 <- function(permScores, dataScores) {
#'   ##check arguments
#'   if (!is.matrix(permScores))
#'     stop("'permScores' should be a matrix!\n")
#'   if (!is.numeric(dataScores) && !is.integer(dataScores))
#'     stop("'dataScores' should be an integer or numeric vector!\n")
#'   if (is.null(names(dataScores)))
#'     stop("'dataScores' should be named (by gene set identifier)")
#'   if (nrow(permScores) != length(dataScores))
#'     stop(
#'       paste(
#'         "The number of rows of the 'permScores' matrix ",
#'         "should be the same as the length of the 'dataScores' vector",
#'         sep = ""
#'       )
#'     )
#'   ##create a vector to store the FDRs
#'   ldataScores <- length(dataScores)
#'   FDRgeneset = rep(0, ldataScores)
#'   ##Compute the normalized enrichment score (i.e. divide each ES
#'   ##(experimental or permutation-based) by the mean of all negative/positive
#'   ##(depending on the sign of the ES) permutation based scores for that gene set)
#'   ##This is done gene set by gene set
#'   sapply(1:ldataScores, function(i) {
#'     ##Get the indices of all negative permutation-based score
#'     neg <- which(permScores[i,] <= 0)
#'     ##Get the indices of all positive permutation-based score
#'     pos <- which(permScores[i,] >= 0)
#'     ##Average the values, separately for positive and negative scores
#'     NegAvg <- abs(mean(permScores[i, neg]))
#'     PosAvg <- abs(mean(permScores[i, pos]))
#'     ##Normalize the permutation-based scores,
#'     ##separately for negative and positive scores
#'     permScores[i, neg] <<- permScores[i, neg] / NegAvg
#'     permScores[i, pos] <<- permScores[i, pos] / PosAvg
#'     ##Normalize the observed scores, separately for negative and positive scores
#'     dataScores[i] <<- ifelse((dataScores[i] < 0),
#'                              (dataScores[i] / NegAvg),
#'                              (dataScores[i] / PosAvg))
#'   })
#'   ##Compute the total number of negative/positive scores across all
#'   ##permutations and all gene sets
#'   negtot <- length(which(permScores <= 0))
#'   postot <- length(which(permScores >= 0))
#'   ##Compute the FDR by comparing:
#'   ##for negative observed ES:
#'   ##the number of permutation-based scores under the observed ES
#'   ##divided by the total number of negative permutation based scores
#'   ##to the number of observed scores under the observed ES divided
#'   ##by the total number of negative observed scores
#'   ##for positive observed ES:
#'   ##the number of permutation-based scores over the observed ES
#'   ##divided by the total number of positive permutation based scores
#'   ##to the number of observed scores over the observed ES divided
#'   ##by the total number of positive observed scores
#'   sapply(1:ldataScores, function(i) {
#'     if (is.na(dataScores[i])) {
#'       FDRgeneset[i] <<- 1
#'     } else if (dataScores[i] < 0) {
#'       FDRgeneset[i] <<-
#'         (sum(permScores <= dataScores[i]) / negtot) / (sum(dataScores <= dataScores[i]) / sum(dataScores <= 0))
#'     } else {
#'       FDRgeneset[i] <<-
#'         (sum(permScores >= dataScores[i]) / postot) / (sum(dataScores >= dataScores[i]) / sum(dataScores >= 0))
#'     }
#'     FDRgeneset[i] <<- ifelse(FDRgeneset[i] > 1, 1, FDRgeneset[i])
#'   })
#'   #name the FDRs (by gene set name) and return the vector
#'   names(FDRgeneset) <- names(dataScores)
#'   return(FDRgeneset)
#' }
#'
#'
#'
#'
#'calcGScore <- function(hits, geneList, exponent = 1) {
# nh <- sum(hits)
# N <- length(geneList)
# ES <- 0
# runningES <- rep(0, N)
# if (nh) {
#   tmp <- rep(0, N)
#   NR = sum(abs(geneList[hits]) ^ exponent)
#   tmp[hits] <- (abs(geneList[hits]) ^ exponent) / NR
#   tmp[!hits] <- -1 / (N - nh)
#   runningES <- cumsum(tmp)
#
#   ESmax <- max(runningES)
#   ESmin <- min(runningES)
#   ES <- ifelse(abs(ESmin) > abs(ESmax), ESmin, ESmax)
# }
# ES
# }



# ##This is the central function for argument checking
# paraCheck.old <- function(name, para) {
#   if(name=="normCellHTSobject") {
#     if(!is(para,"cellHTS"))
#       stop("The argument 'cellHTSobject/normCellHTSobject' should be a cellHTS object")
#     if(!state(para)["configured"])
#       stop("The cellHTS object should be configured to perform the statistical tests")
#     if(!state(para)["normalized"])
#       warning("Your cellHTS object has not been normalized, this could impact the results of these tests",immediate.=TRUE)
#     if(state(para)["scored"])
#       stop("This cellHTS object has been scored; the statistical analysis should be performed on the normalized signal intensities",immediate.=TRUE)
#     if(!state(para)["annotated"])
#       stop("This cellHTS object has not been annotated",immediate.=TRUE)
#   }
#   if(name=="scoreSign") {
#     if(!is.character(para) || length(para)!=1 || !(para %in% c("+","-")))
#       stop("'scoreSign' should be either '+' or '-'!\n")
#   }
#   if(name=="scoreMethod") {
#     if(!is.character(para) || length(para)!=1 || !(para %in% c("none","zscore","NPI")))
#       stop("'scoreMethod' should be either 'none', 'zscore' or 'NPI'!\n")
#   }
#   if(name=="summarizeMethod") {
#     if(!is.character(para) || length(para)!=1 || !(para %in% c("min","mean","median","max","rms","closestToZero","FurthestFromZero")))
#       stop("'summarizeMethod' should be either 'min', 'mean', 'median', 'max', 'rms', 'closestToZero' or 'FurthestFromZero'!\n")
#   }
#   if(name=="annotationColumn") {
#     if(!is.character(para) || length(para)!=1 )
#       stop("'annotationColumn' should be a character value!\n")
#   }
#   if(name=="cutoffHitsEnrichment") {
#     if(length(para) != 1 || (!is.integer(para) && !is.numeric(para)))
#       stop("'cutoffHitsEnrichment' should be a single integer! \n ")
#   }
#   if(name=="nwStatsControls") {
#     if(!(is.character(para) && length(para)==1))
#       stop("'controls/nwStatsControls' should be a character value!\n ")
#   }
#   if(name=="nwStatsAlternative") {
#     if(!(is.character(para) && length(para)==1 && (para %in% c("two.sided","less","greater")) ))
#       stop("'alternative/nwStatsAlternative' should be one in 'two.sided','less' and 'greater'!\n ")
#   }
#   if(name=="nwStatsTests") {
#     if(!is.character(para) || length(para)==0 || !(para %in% c("T-test","MannWhitney","RankProduct")))
#       stop("'tests/nwStatsTests' should be one or more in 'T-test', 'MannWhitney' and 'RankProduct'!\n ")
#   }
#   if(name=="nwAnalysisOrder") {
#     if(!(is.numeric(para) || is.integer(para)) || length(para)!=1 || para<0)
#       stop("'order/nwAnalysisOrder' should be a positive numeric/integer value!\n")
#   }
#   if(name=="nwStatsColumns") {
#     if(!is.character(para) || length(para)==0)
#       stop("'nwStatsColumns' should be a character vector with length > 0!\n ")
#   }
#   if(name=="gscs") {
#     if(!is.list(para))
#       stop("'listOfGeneSetCollections' must be a list of gene set collections!\n")
#     if(is.null(names(para)))
#       stop("'listOfGeneSetCollections' must be a list of named gene set collections!\n")
#     if(!all(unlist(lapply(para,is.list))))
#       stop("Each gene set collection in 'listOfGeneSetCollections' must be a list of gene sets!\n")
#     if(any(unlist(lapply(para,length))==0))
#       stop("Empty gene set collection(s) in 'listOfGeneSetCollections'!\n")
#   }
#   if(name=="gsc") {
#     if(!is.list(para))
#       stop("A gene set collection must be a list of gene sets!\n")
#     if(length(para)==0)
#       stop("No gene set found in input gene set collection!\n")
#   }
#   if(name=="gs") {
#     if(!is.character(para) || length(para)==0 || any(is.na(para)) || any(para==""))
#       stop("'geneSet/GeneSet' should be a character vector with length > 0, without NA or empty names!\n")
#   }
#   if(name=="gs.single") {
#     if(!is.character(para) || length(para)!=1 || is.na(para) || para=="")
#       stop("'geneSet/GeneSet' should be single character!\n")
#   }
#   if(name=="gscs.names") {
#     if(!is.character(para) || length(para)==0)
#       stop("'gscs' should be a character! \n")
#   }
#   if(name=="gsc.name") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'gsc' should be a single character! \n")
#   }
#   if(name=="keggGSCs") {
#     if(!is.character(para) || length(para)==0)
#       stop("'keggGSCs' should be a character!\n")
#   }
#   if(name=="goGSCs") {
#     if(!is.character(para) || length(para)==0)
#       stop("'goGSCs' should be a character!\n")
#   }
#   if(name=="genelist") {
#     if(!(is.numeric(para) || is.integer(para)) || length(para)==0 || is.null(names(para)))
#       stop("'geneList' should be a named numeric or integer vector with length > 0!\n")
#     #if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)==""))
#   }
#   if(name=="universe") {
#     if(!is.character(para) || any(is.na(para)) || any(para==""))
#       stop("'universe' should be a character vector without any NA or empty values!\n")
#   }
#   if(name=="genelist.general") {
#     if(is.matrix(para)) {
#       if(is.null(rownames(para)) || any(is.na(rownames(para))) || any(rownames(para)==""))
#         stop("geneList should be a matrix with rownames!\n")
#     } else {
#       if(!(is.numeric(para) || is.integer(para)) || length(para)==0)
#         stop("'geneList' should be a numeric or integer vector with length > 0!\n")
#       if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)==""))
#         stop("'geneList' should be a named numeric or integer vector!\n")
#     }
#   }
#   if(name=="hits") {
#     if(!is.character(para) || length(para)==0)
#       stop("'hits' should be a character vector with length > 0!\n")
#   }
#   if(name=="gsca.para") {
#     if(missing(para))
#       stop("'para' should be provided as a list!\n")
#     ##default parameters
#     para.default<-list(pValueCutoff = 0.05,pAdjustMethod = "BH", nPermutations = 1000, minGeneSetSize = 15,exponent = 1)
#     ##check if input parameters are supported
#     if(length(setdiff(names(para),names(para.default)))>0)
#       stop("Some parameters in 'para' are not supported. Check the right format of para!\n")
#     ##fill out default parameters for non-specified ones
#     para.unspecified<-setdiff(names(para.default),names(para))
#     if(length(para.unspecified)>0)
#       for(i in 1:length(para.unspecified)) {
#         para[[para.unspecified[i]]]<-para.default[[para.unspecified[i]]]
#       }
#     ##check data type in para
#     if(!(is.integer(para$pValueCutoff) || is.numeric(para$pValueCutoff)) || length(para$pValueCutoff)!=1 || para$pValueCutoff>1)
#       stop("'pValueCutoff' should be an integer or numeric value <=1!\n")
#     if(!is.character(para$pAdjustMethod) || length(para$pAdjustMethod)!=1 ||
#        !(para$pAdjustMethod %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
#       stop("'pAdjustMethod' should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
#     if(!(is.integer(para$nPermutations) || is.numeric(para$nPermutations)) || length(para$nPermutations)!=1 || para$nPermutations<1)
#       stop("'nPermutations' should be an integer >=1 !\n'")
#     if(!(is.integer(para$minGeneSetSize) || is.numeric(para$minGeneSetSize)) || length(para$minGeneSetSize)!=1 || para$minGeneSetSize<1)
#       stop("'minGeneSetSize' should be an integer >=1 !\n'")
#     if(!(is.integer(para$exponent) || is.numeric(para$exponent)) || length(para$pValueCutoff)!=1 || para$exponent<1)
#       stop("'exponent' should be an integer or numeric value >=1 !\n")
#     return(para)
#   }
#   if(name=="pAdjustMethod") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
#       stop("'pAdjustMethod' should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
#   }
#   if(name=="pValueCutoff") {
#     if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para>1 || para<0)
#       stop("'pValueCutoff' should be an integer or numeric value <=1 and >=0!\n")
#   }
#   if(name=="nPermutations") {
#     if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
#       stop("'nPermutations' should be an integer >=1 !\n'")
#   }
#   if(name=="minGeneSetSize") {
#     if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
#       stop("'minGeneSetSize' should be an integer >=1 !\n'")
#
#   }
#   if(name=="exponent") {
#     if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
#       stop("'exponent' should be an integer or numeric value >=1 !\n")
#   }
#   if(name=="species") {
#     if(!is.character(para) || length(para) != 1)
#       stop("'species' should be a character!\n")
#     if(!(para %in% c("Dm","Hs","Rn","Mm","Ce"))) {
#       stop("'species' does not match any of the names recognized by this function, please provide one of the following character strings: 'Dm' ('Drosophila_melanogaster'), 'Hs' ('Homo_sapiens'), 'Rn' ('Rattus_norvegicus'), 'Mm' ('Mus_musculus'), 'Ce' ('Caenorhabditis_elegans')")
#     }
#   }
#   if(name=="mam.species") {
#     if(!is.character(para) || length(para) != 1)
#       stop("'species' should be a character!\n")
#     if(!(para %in% c("Hs","Rn","Mm"))) {
#       stop("'species' does not match any of the names recognized by this function, please provide one of the following character strings: 'Hs' ('Homo_sapiens'), 'Rn' ('Rattus_norvegicus'), 'Mm' ('Mus_musculus')")
#     }
#   }
#   if(name=="initialIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt","wormbase")))
#       stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt','wormbase'! \n")
#   }
#   if(name=="finalIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt","wormbase")))
#       stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt','wormbase'!\n")
#   }
#   if(name=="dro.initialIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt")))
#       stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt'! \n")
#   }
#   if(name=="dro.finalIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt")))
#       stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt'!\n")
#   }
#   if(name=="cel.initialIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","wormbase")))
#       stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','wormbase'! \n")
#   }
#   if(name=="cel.finalIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","wormbase")))
#       stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','wormbase'!\n")
#   }
#   if(name=="mam.initialIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank")))
#       stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank'! \n")
#   }
#   if(name=="mam.finalIDs") {
#     if(!is.character(para) || length(para)!=1 ||
#        !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank")))
#       stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank'! \n")
#
#   }
#   if(name=="keepMultipleMappings") {
#     if(!is.logical(para) || length(para)!=1)
#       stop("keepMultipleMappings should be a logical value!\n")
#   }
#   if(name=="duplicateRemoverMethod") {
#     if(!is.character(para) || length(para) != 1 || !(para %in% c("max","min","average","fold.change.average")))
#       stop("'duplicateRemoverMethod' should be only one of the following character strings: 'max', 'min', 'average', 'fc.avg(fold change average)'")
#   }
#   if(name=="orderAbsValue") {
#     if(!is.logical(para) || length(para)!=1)
#       stop("'orderAbsValue' should be a logical value!\n")
#   }
#   if(name=="verbose") {
#     if(!is.logical(para) || length(para)!=1)
#       stop("'verbose' should be a logical value!\n")
#   }
#   if(name=="filepath") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'filepath' should be a character!\n")
#   }
#   if(name=="dataDirectory") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'dataDirectory' should be a character!\n")
#   }
#   if(name=="filename") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'filename' should be a character!\n")
#   }
#   if(name=="ntop") {
#     if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<=0)
#       stop("'ntop' should be a integer or numeric value >0 ! \n")
#   }
#   if(name=="allSig") {
#     if(!is.logical(para) || length(para)!=1)
#       stop("'allSig' should be a logical value!\n")
#   }
#   if(name=="gsNameType") {
#     if(!is.character(para) || length(para)!=1 || !(para%in% c("id","term","none")))
#       stop("'gsNameType' should be a single character value: 'id', 'term' or 'none'!\n")
#   }
#   if(name=="displayEdgeLabel") {
#     if(!is.logical(para) || length(para)!=1)
#       stop("'displayEdgeLabel' should be a logical value!\n")
#   }
#   if(name=="layout") {
#     if(!is.character(para) || length(para)!=1 || !(para%in%c("layout.fruchterman.reingold", "layout.spring", "layout.circle", "layout.kamada.kawai")))
#       stop("'layout' must be one of 'layout.fruchterman.reingold', 'layout.spring', 'layout.circle' and 'layout.kamada.kawai'!\n")
#   }
#   if(name=="resultName") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'resultName' should be a character!\n")
#   }
#   if(name=="pvalues") {
#     if(!is.numeric(para) || length(para)==0 || is.null(names(para)))
#       stop("'pvalues' should be a named numeric vector with length > 0!\n")
#   }
#   if(name=="phenotypes") {
#     if(!(is.numeric(para) || is.integer(para)) || length(para)==0 || is.null(names(para)))
#       stop("'phenotypes/phenotypeVector' should be a named numeric vector with length > 0!\n")
#     #if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)==""))
#   }
#   if(name=="interactome") {
#     # if(!is(para,"igraph") && (numNodes(para)==0 || numEdges(para)==0))
#     # 	stop("Input 'interactome/graph' should be a igraph object with node and edge No > 0!\n")
#   }
#   if(name=="fdr") {
#     if(!is.numeric(para) || para>1)
#       stop("'fdr' should be <=1 ! \n")
#   }
#   if(name=="interactionMatrix") {
#     #If a data matrix is specified, check that it contains the right columns
#     if(!is.matrix(para))
#       stop("'interactionMatrix' should be a matrix")
#     if(!all(c("InteractionType","InteractorA","InteractorB") %in% colnames(para)))
#       stop("'interactionMatrix' should contain the following named columns: 'InteractionType','InteractorA','InteractorB'")
#   }
#   if(name=="link") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'link' should be a character!\n")
#   }
#   if(name=="reportDir") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'reportDir' should be a character!\n")
#   }
#   if(name=="genetic") {
#     if(!is.logical(para) || length(para)!=1)
#       stop("'genetic' should be a logical value!\n")
#   }
#   if(name=="what.nwa") {
#     if(!any(para %in% c("ALL","Pval","Phenotype","Interactome","Para","Result")) || !is.character(para))
#       stop("Wrong what input! Please input \"ALL\"(all summary information), \"Pval\"(p-values), \"Phenotype\", \"Interactome\", \"Para\"(parameters for analysis) and \"Result\"\n")
#
#   }
#   if(name=="what.gsca") {
#     if(!is.character(para) || !any(para %in% c("ALL","GSC","GeneList","Hits","Para","Result")))
#       stop("Wrong what input! Please input \"ALL\"(all summary information), \"GSC\"(gene set collection), \"GeneList\", \"Hits\", \"Para\"(parameters for analysis) and \"Result\"\n")
#
#   }
#   if(name=="experimentName") {
#     if(!is.character(para) || length(para)!=1)
#       stop("'experimentName' should be a character!\n ")
#   }
#   if(name=="ontologies") {
#     if(!is.character(para) || length(para)==0 || !all(para %in% c("BP","MF","CC")))
#       stop("'ontologies' should be a character vector containing any non redundant combination of 'BP','MF','CC'!\n")
#   }
#   if(name=="output") {
#     if(!is.character(para) || length(para)!=1 || !all(para %in% c("png","pdf")))
#       stop("'output' should be 'png' or 'pdf'!\n")
#   }
#   if(name=="gseaScore.mode") {
#     if(!is.character(para) || length(para)!=1 || !(para %in% c("graph", "score"))) {
#       stop("'mode' should be 'graph' or 'score'!\n")
#     }
#   }
#   if(name=="cutoffHitsEnrichment") {
#     if(length(para) != 1 || (!is.integer(para) && !is.numeric(para)))
#       stop("'cutoffHitsEnrichment' should be a single integer!\n ")
#   }
#   if(name=="doGSOA" || name=="doGSEA") {
#     if(length(para) != 1 || !is.logical(para))
#       stop("'doGSOA' and 'doGSEA' should be a single logical value!\n ")
#   }
# }




# if(!isGeneric("viewEnrichMap"))
#   setGeneric("viewEnrichMap",function(object,...)
#     standardGeneric("viewEnrichMap"), package="HTSanalyzeR2")
# if(!isGeneric("plotEnrichMap"))
#   setGeneric("plotEnrichMap",function(object,...)
#     standardGeneric("plotEnrichMap"), package="HTSanalyzeR2")
#
#
# #' @export
# setMethod(
#   "plotEnrichMap", signature = "GSCA",
#   function(object,
#            resultName = "GSEA.results",
#            gscs,
#            ntop = NULL,
#            allSig = TRUE,
#            gsNameType = "id",
#            displayEdgeLabel = TRUE,
#            layout = "layout.fruchterman.reingold",
#            filepath = ".",
#            filename = "test.png",
#            output = "png",
#            ...) {
#     paraCheck("Report", "filepath", filepath)
#     paraCheck("Report", "filename", filename)
#     paraCheck("Report", "output", output)
#
#     if(output == "pdf" )
#       pdf(file.path(filepath, filename), ...=...)
#     if(output == "png" )
#       png(file.path(filepath, filename), ...=...)
#     viewEnrichMap(object, resultName, gscs, ntop, allSig,
#                   gsNameType, displayEdgeLabel, layout)
#     dev.off()
#   }
# )
#
# #' @export
# #' @importFrom igraph V graph.adjacency
# setMethod(
#   "viewEnrichMap", signature = "GSCA",
#   function(object, resultName="GSEA.results", gscs, ntop=NULL,
#            allSig=TRUE, gsNameType="id", displayEdgeLabel=TRUE,
#            layout="layout.fruchterman.reingold", plot=TRUE) {
#     if(missing(gscs))
#       stop("Please specify the name(s) of Gene Set Collections in 'gscs'! \n")
#     paraCheck("Summarize", "gscsNames", gscs)
#     paraCheck("Report", "resultName", resultName)
#     if(!(resultName %in% names(object@result)))
#       stop("No results found in object!\n")
#     if(is.null(object@result[[resultName]]))
#       stop("Please run Hypergeometric or GSEA analysis before using this function!\n")
#     gsc.names<-names(object@result[[resultName]])
#     if(!all(gscs %in% gsc.names))
#       stop("Wrong Gene Set Collection name(s) in 'gscs'! \n")
#
#     if(!is.null(ntop))
#       paraCheck("Summarize", "ntop", ntop)
#     paraCheck("Summarize", "allSig", allSig)
#     if((is.null(ntop) && !allSig)||(!is.null(ntop) && allSig))
#       stop("Either specify 'ntop' or set 'allSig' to be TRUE!\n")
#
#     paraCheck("Record", "plot", plot)
#     paraCheck("Record", "layout", layout)
#     paraCheck("Record", "gsNameType", gsNameType)
#     paraCheck("Record", "displayEdgeLabel", displayEdgeLabel)
#
#     ## get top gene sets
#     topGS<-getTopGeneSets(object, resultName, gscs, ntop, allSig)
#     if(sum(unlist(lapply(topGS, length)))==0)
#       stop("No significant gene sets found!\n")
#     gsInUni<-list()
#     uniIDs<-names(object@geneList)
#     tempList<-list()
#     junk<-sapply(1:length(topGS), function(i) {
#       if(length(topGS[[i]])>0) {
#         gsc.name<-names(topGS)[i]
#         ## compute overlapped genes between gene sets and universe
#         gsInUni[[i]]<<-list()
#         gsInUni[[i]]<<-sapply(topGS[[i]], function(j) intersect(object@listOfGeneSetCollections[[gsc.name]][[j]], uniIDs),simplify=FALSE)
#         names(gsInUni)[i]<<-gsc.name
#         tempList[[i]]<<-data.frame(gsID=topGS[[i]], gscID=gsc.name, object@result[[resultName]][[gsc.name]][topGS[[i]],,drop=FALSE])
#         names(tempList)[i]<<-gsc.name
#       }
#     })
#
#     ## collapse to a data frame
#     tempdf<-do.call("rbind", lapply(tempList, data.frame, stringsAsFactors = FALSE))
#     if(gsNameType=="term" && !("Gene.Set.Term" %in% colnames(tempdf)))
#       stop("No gene set terms found in results!\n Please use the method 'appendGSTerms' or add a column named 'Gene.Set.Term' to the results!\n")
#
#     ## function to compute overlapped genes
#     map.mat<-matrix(0,nrow(tempdf),nrow(tempdf))
#     diag(map.mat)<-1
#     map.diag<-sapply(1:nrow(tempdf), function(i) length(gsInUni[[as.character(tempdf[i,"gscID"])]][[as.character(tempdf[i,"gsID"])]]))
#     if(nrow(tempdf)>=2) {
#       sapply(1:(nrow(tempdf)-1), function(i) {
#         map.mat[i, (i+1):nrow(tempdf)]<<-sapply((i+1):nrow(tempdf), function(j) {
#           length(intersect(gsInUni[[as.character(tempdf[i,"gscID"])]][[as.character(tempdf[i,"gsID"])]],
#                            gsInUni[[as.character(tempdf[j,"gscID"])]][[as.character(tempdf[j,"gsID"])]]))/
#             length(union(gsInUni[[as.character(tempdf[i,"gscID"])]][[as.character(tempdf[i,"gsID"])]],
#                          gsInUni[[as.character(tempdf[j,"gscID"])]][[as.character(tempdf[j,"gsID"])]]))
#         })
#         map.mat[(i+1):nrow(tempdf),i]<<-map.mat[i, (i+1):nrow(tempdf)]
#       })
#       rownames(map.mat)<-rownames(tempdf)
#       colnames(map.mat)<-rownames(tempdf)
#       ## generate igraph from adjacency matrix
#       ## "Node name" controlled by the rownames of tempList
#       g<-graph.adjacency(adjmatrix=map.mat, mode="undirected", weighted=TRUE, diag=TRUE)
#       g<-igraph::simplify(g, remove.loops = TRUE)
#     } else if(nrow(tempdf)==1) {
#       diag(map.mat)<-0
#       rownames(map.mat)<-rownames(tempdf)
#       colnames(map.mat)<-rownames(tempdf)
#       ## generate igraph from adjacency matrix
#       ## "Node name" controlled by the rownames of tempList
#       g<-graph.adjacency(adjmatrix=map.mat, mode="undirected", weighted=NULL, diag=FALSE)
#     }
#     ## add an user-defined attribute 'geneNum' to igraph
#     igraph::V(g)$geneSetSize<-map.diag
#     if(length(igraph::V(g))>=2) {
#       ## "Node size" controlled by the "size of gene set"
#       v.max.size<-18
#       v.min.size<-4
#       if(max(map.diag)!=min(map.diag))
#         igraph::V(g)$size<-v.min.size+(v.max.size-v.min.size)*(map.diag-min(map.diag))/(max(map.diag)-min(map.diag))
#       else
#         igraph::V(g)$size<-6
#     } else if(length(igraph::V(g))==1) {
#       igraph::V(g)$size<-4
#     }
#     p.vec<-tempdf[,"Adjusted.Pvalue"]
#     p.cutoff.vec<-c(0, 10^c(-3, -2.5), 0.01, 10^(-1.5), 0.05, 10^(-c(1.0, 0.5, 0)))
#
#     if(resultName=="GSEA.results") {
#       posids<-which(tempdf[,"Observed.score"]>=0)
#       negids<-which(tempdf[,"Observed.score"]<=0)
#
#       redCols<-colorRampPalette(colors = c("red", "white"))
#       redVec<-redCols(length(p.cutoff.vec))
#
#       blueCols<-colorRampPalette(colors = c("blue", "white"))
#       blueVec<-blueCols(length(p.cutoff.vec))
#       igraph::V(g)$color<-""
#       if(length(posids)>0)
#         igraph::V(g)$color[posids]<-redVec[as.integer(cut(x=p.vec[posids],breaks=c(-1,p.cutoff.vec), labels=1:(length(p.cutoff.vec))))]
#       if(length(negids)>0)
#         igraph::V(g)$color[negids]<-blueVec[as.integer(cut(x=p.vec[negids],breaks=c(-1,p.cutoff.vec), labels=1:(length(p.cutoff.vec))))]
#     } else if(resultName=="HyperGeo.results") {
#       redCols<-colorRampPalette(colors = c("red", "white"))
#       redVec<-redCols(length(p.cutoff.vec))
#       igraph::V(g)$color<-redVec[as.integer(cut(x=p.vec,breaks=c(-1,p.cutoff.vec), labels=1:(length(p.cutoff.vec))))]
#     }
#     ## labels attributes
#     graphLabelWrapper<-function(x, width=32) {paste(strwrap(x,width=width),collapse="\n")}
#     if(gsNameType=="id") {
#       igraph::V(g)$label<-as.character(tempdf[,"gsID"])
#     } else if(gsNameType=="term") {
#       templabels<-as.character(tempdf[,"Gene.Set.Term"])
#       igraph::V(g)$label<-sapply(templabels, graphLabelWrapper)
#     }
#
#     igraph::V(g)$label.dist<-0.4
#     igraph::V(g)$label.cex<-0.75
#     igraph::V(g)$label.font<-3
#     igraph::V(g)$label.color<-"black"
#
#     ## "Node color" controlled by the "adjusted pvalue"
#     if(length(igraph::V(g))>=2)
#       E(g)$color<-grey(0.7)
#     if(displayEdgeLabel) {
#       edgeWeights<-round(E(g)$weight*100)
#       edgeWeights[edgeWeights==0]<-""
#       E(g)$label<-edgeWeights
#     }
#     ## "Edge thickness" controlled by the "size of overlapped genes" between two gene sets
#     edge.max.w<-14
#     edge.min.w<-1
#     if(length(igraph::V(g))>=2) {
#       edgeWeightVec<-round(edge.min.w+(edge.max.w-edge.min.w)*(E(g)$weight))
#       E(g)$width<-edgeWeightVec
#     }
#     if(plot) {
#       ## plot graph
#       plot(g,layout=eval(parse(text=layout)))
#       ## title
#
#       ## p-value color legend
#       if(resultName=="GSEA.results") {
#         title(main=paste("Enrichment Map of GSEA on \n\"", lapply(list(gscs), paste, collapse=",")[[1]], "\"",sep=""))
#         colVec<-c(redVec[1:(length(redVec)-1)],rev(blueVec))
#         p.cutoff.labels<-rep("",length(colVec))
#         p.cutoff.labels[c(1,4,6,9,12,14,17)]<-c(0,0.01,0.05,1,0.05,0.01,0)
#       } else if(resultName=="HyperGeo.results") {
#         title(main=paste("Enrichment Map of Hypergeometric tests on \n\"", lapply(list(gscs), paste, collapse=",")[[1]],"\"", sep=""))
#         colVec<-redVec
#         p.cutoff.labels<-rep("",length(colVec))
#         p.cutoff.labels[c(1,4,6,9)]<-c(0,0.01,0.05,1)
#       }
#
#       points(
#         x = rep(-1.2, length(colVec)),
#         y = seq(0.5, (0.5-(0.05*length(colVec))), length.out = length(colVec)),
#         pch = 15, col = colVec
#       )
#
#       text(
#         x = rep(-1.3, length(colVec)),
#         y = seq(0.5, (0.5-(0.05*length(colVec))), length.out = length(colVec)),
#         labels = p.cutoff.labels,
#         cex = 0.8,
#         adj=1
#       )
#       text(
#         x = -1.25,
#         y = 0.7,
#         labels = "Adjusted\np-values",
#         cex=0.8,
#         adj= 0.5,
#         font=2
#       )
#     }
#     return(g)
#   }
# )



# if (!isGeneric("viewSubNet")) {
#   setGeneric("viewSubNet", function(object, ...)
#     standardGeneric("viewSubNet"), package = "HTSanalyzeR2")
# }
# if (!isGeneric("plotSubNet")) {
#   setGeneric("plotSubNet", function(object, ...)
#     standardGeneric("plotSubNet"), package = "HTSanalyzeR2")
# }
#
# ##view subnetwork
# #' @export
# setMethod("viewSubNet",
#           "NWA",
#           function(object) {
#             networkPlot(nwAnalysisOutput = object@result,
#                         phenotypeVector = object@phenotypes)
#           })
#
# ##This function takes in a subnetwork module resulted from function
# ##networkAnalysis, a vector of labels for nodes in the module and a
# ##phenotype vector (optional) and generate a figure stored to
# ## "filepath" with the name "filename".
# #' @importFrom igraph vertex_attr vcount
# #' @importFrom BioNet plotModule
# networkPlot <- function(nwAnalysisOutput, phenotypeVector = NULL) {
#   ##check arguments
#   if (!is.list(nwAnalysisOutput) ||
#       !(c("subnw") %in% names(nwAnalysisOutput)))
#     stop("'nwAnalysisOutput' should contain a subnetwork module!\n")
#
#   subnw <- nwAnalysisOutput$subnw
#   labels <- nwAnalysisOutput$labels
#
#   if (!is(subnw, "igraph"))
#     stop("The module in 'nwAnalysisOutput' should be an object ",
#          "of class 'igraph'!\n")
#
#   ##If no phenotype vector is specified, then we can just plot the module
#   if (is.null(phenotypeVector)) {
#     ##png("EnrichedSubNw.png", width = 900, height = 900)
#     BioNet::plotModule(subnw, labels = labels)
#     ##dev.off()
#   } else {
#     paraCheck("NWAClass", "phenotypes", phenotypeVector)
#     ## "diff.expr" holds the phenotype for the nodes of the sub-network
#     diff.expr <- phenotypeVector[vertex_attr(subnw, "name")]
#     names(diff.expr) <- vertex_attr(subnw, "name")
#     ## "present" contains the information of wether a node has an
#     ##associated phenotype (1) or not (-1), will be used to give a
#     ##different shape to the nodes of the network
#     present <- rep(1, vcount(subnw))
#     present[which(is.na(diff.expr))] <- -1
#     ##replaces all phenotypes of non-phenotyped nodes by a zero
#     diff.expr[which(is.na(diff.expr))] <- 0
#     names(present) <- vertex_attr(subnw, "name")
#     ##Plot the module
#
#     if (vcount(subnw) == 1) {
#       ##png(file.path(filepath, filename), width = 900, height = 900)
#       BioNet::plotModule(subnw,
#                          labels = labels,
#                          scores = present,
#                          diff.expr = diff.expr)
#       ##dev.off()
#     } else {
#       Tcolors <- diff.expr
#       Tcolors2 <- diff.expr
#       if (max(abs(Tcolors)) < 5)
#         Tcolors <- Tcolors * 5
#       ## set red colors
#       if (any(Tcolors > 0)) {
#         maxRed <- max(ceiling(abs(Tcolors[which(Tcolors > 0)])))
#         redCols <- colorRampPalette(colors = c("white", "red"))
#         redVec <- redCols(maxRed)
#         Tcolors2[which(Tcolors > 0)] <-
#           redVec[ceiling(abs(Tcolors[which(Tcolors > 0)]))]
#       }
#       ##set the greens
#       if (any(Tcolors < 0)) {
#         maxGreen <- max(ceiling(abs(Tcolors[which(Tcolors < 0)])))
#         greenCols <- colorRampPalette(colors = c("white", "green"))
#         greenVec <- greenCols(maxGreen)
#         Tcolors2[which(Tcolors < 0)] <-
#           greenVec[ceiling(abs(Tcolors[which(Tcolors < 0)]))]
#       }
#       colScale <- unique(Tcolors2)
#       colboundary <- rep(0, length(colScale))
#       colboundary <- sapply(colScale, function(c) {
#         values <- diff.expr[which(Tcolors2 == c)]
#         values[which(abs(values) == max(abs(values)))[1]]
#       })
#
#       colMatrix <- cbind(colboundary[order(colboundary)],
#                          colScale[order(colboundary)])
#       ##png(file.path(filepath, filename), width = 900, height = 900)
#       BioNet::plotModule(subnw,
#                          labels = labels,
#                          scores = present,
#                          diff.expr = diff.expr
#       )
#       points(
#         x = rep(-1.2, length(unique(Tcolors2))),
#         y = seq(1.2, (1.2 - (
#           0.05 * length(colMatrix[, 2])
#         )),
#         length.out = length(colMatrix[, 2])),
#         pch = 15,
#         col = colMatrix[, 2]
#       )
#       text(
#         x = rep(-1.1, length(unique(Tcolors2))),
#         y = seq(1.2, (1.2 - (
#           0.05 * length(colMatrix[, 2])
#         )),
#         length.out = length(colMatrix[, 2])),
#         labels = signif(as.numeric(colMatrix[, 1]), digits = 2),
#         cex = 0.8
#       )
#       ##dev.off()
#     }
#   }
# }
#
#
# ##plot subnetwork
# setMethod("plotSubNet",
#           "NWA",
#           function(object,
#                    filepath = ".",
#                    filename = "test",
#                    output = "png",
#                    ...) {
#             if (missing(filepath) || missing(filename))
#               stop("Please specify 'filepath' and 'filename' ",
#                    "to save network plot! \n")
#             paraCheck("Report", "filepath", filepath)
#             paraCheck("Report", "filename", filename)
#             paraCheck("Report", "output", output)
#             if (output == "pdf")
#               pdf(file.path(filepath, filename), ... = ...)
#             if (output == "png")
#               png(file.path(filepath, filename), ... = ...)
#             networkPlot(nwAnalysisOutput = object@result,
#                         phenotypeVector = object@phenotypes)
#             dev.off()
#           })
#
#
#







## summarize & default show
setGeneric("analyze", function(object, ...)
  standardGeneric("analyze"), package = "HTSanalyzeR2")
#' @export analyze
setMethod("analyze",
          "GSCA",
          function(object,
                   para = list(
                     pValueCutoff = 0.05,
                     pAdjustMethod = "BH",
                     # nPermutations = 1000,
                     nPermutations = 20,
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
            object@para <- paraCheck(name = "gsca.para", para = para)
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
            ##if(!is.data.frame(object@summary$para$hypergeo))
            ##  object@summary$para$hypergeo<-data.frame(object@summary$para$hypergeo)
            ##if(!is.data.frame(object@summary$para$gsea))
            ##  object@summary$para$gsea<-data.frame(object@summary$para$gsea)
            #######################
            ##    do analysis
            #######################
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
            ##update summary information
            cols <- colnames(object@summary$results)
            object@summary$results[1:3, ] <- NA
            if (doGSOA) {
              object@summary$gsc[, 2] <- unlist(lapply(object@result$HyperGeo.results, nrow))[cols]
              object@summary$results["HyperGeo", ] <- unlist(lapply(object@result$HyperGeo.results, function(df) {
                sum(df[, "Adjusted.Pvalue"] < object@para$pValueCutoff)
              }))[cols]
            }
            if (doGSEA) {
              object@summary$gsc[, 2] <- unlist(lapply(object@result$GSEA.results, nrow))[cols]
              object@summary$results["GSEA", ] <- unlist(lapply(object@result$GSEA.results, function(df) {
                sum(df[, "Adjusted.Pvalue"] < object@para$pValueCutoff)
              }))[cols]
            }
            if (doGSOA && doGSEA) {
              object@summary$results["Both", ] <- unlist(lapply(object@result$Sig.adj.pvals.in.both, nrow))[cols]
            }
            object
          })


##This function takes a list of gene set collections, a named phenotype
##vector (with names(phenotype vector)=GeneUniverse), a vector of hits
##(names only) and returns the results of hypergeometric and gene set
##enrichment analysis for all of the gene set collections (with multiple
##hypothesis testing correction).
analyzeGeneSetCollections <- function(listOfGeneSetCollections, geneList,
                                      hits, pAdjustMethod = "BH", pValueCutoff = 0.05, nPermutations=1000,
                                      minGeneSetSize=15, exponent=1, verbose=TRUE, doGSOA=TRUE, doGSEA=TRUE) {
  ##check arguments
  if((!doGSOA)&&(!doGSEA))
    stop("Please choose to perform hypergeometric tests and/or GSEA by specifying 'doGSOA' and 'doGSEA'!\n")
  if(doGSOA || doGSEA) {
    paraCheck("gscs",listOfGeneSetCollections)
    paraCheck("genelist",geneList)
    paraCheck("pAdjustMethod",pAdjustMethod)
    paraCheck("pValueCutoff",pValueCutoff)
    paraCheck("minGeneSetSize",minGeneSetSize)
    paraCheck("verbose",verbose)
  }
  if(doGSOA) {
    paraCheck("hits",hits)
  }
  if(doGSEA) {
    paraCheck("nPermutations",nPermutations)
    paraCheck("exponent",exponent)
  }
  numGeneSetCollections<-length(listOfGeneSetCollections)

  ##GSEA.results.list <- vector("list", (numGeneSetCollections))
  ##filter 'istOfGeneSetCollections' by 'minGeneSetSize'
  max.size<-0
  for(l in 1:numGeneSetCollections) {
    gs.size <- unlist(
      lapply(
        lapply(
          listOfGeneSetCollections[[l]], intersect,
          y = names(geneList)
        ),
        length
      )
    )
    max.size <- max(max(gs.size), max.size)
    gs.id <- which(gs.size >= minGeneSetSize)
    n.gs.discarded <- length(listOfGeneSetCollections[[l]]) -
      length(gs.id)
    listOfGeneSetCollections[[l]] <-
      listOfGeneSetCollections[[l]][gs.id]
    ##output information about filtering of gene set collections
    if(verbose && n.gs.discarded > 0)
      cat(paste("--", n.gs.discarded, " gene sets don't have >= ",
                minGeneSetSize, " overlapped genes with universe in gene",
                " set collection named ", names(listOfGeneSetCollections)[l],
                "!\n",sep=""))
  }
  ##stop when no gene set passes the size requirement
  if(all(unlist(lapply(listOfGeneSetCollections,length))==0))
    stop(paste("No gene set has >= ",minGeneSetSize, " overlapped ",
               "genes with universe!\n The largest number of overlapped ",
               "genes between gene sets and universe is: ", max.size, sep=""))
  result.names<-names(listOfGeneSetCollections)
  ######################
  ###Hypergeometric test
  ######################
  if(doGSOA) {
    HGTresults<-list()
    cat("-Performing hypergeometric analysis ...\n")

    for(i in 1 : length(listOfGeneSetCollections)) {
      if(verbose) {
        cat("--For", names(listOfGeneSetCollections)[i], "\n")
      }
      if(length(listOfGeneSetCollections[[i]]) > 0)
        HGTresults[[i]] <- multiHyperGeoTest(
          listOfGeneSetCollections[[i]], universe=names(geneList),
          hits = hits, minGeneSetSize = minGeneSetSize,
          pAdjustMethod = pAdjustMethod, verbose = verbose)
      else {
        HGTresults[[i]] <- matrix(, nrow=0, ncol=7)
        colnames(HGTresults[[i]]) <- c("Universe Size",
                                       "Gene Set Size", "Total Hits", "Expected Hits",
                                       "Observed Hits", "Pvalue", "Adjusted.Pvalue")
      }
    }
    pvals <- NULL
    ##loop combines pvalues from all gene set collections' results
    ##into one vector for multiple hypothesis testing pvalue adjustement
    sapply(1:numGeneSetCollections, function(i) {
      if(nrow(HGTresults[[i]])>0) {
        pv<-HGTresults[[i]][,"Pvalue"]
        names(pv)<-rownames(HGTresults[[i]])
        pvals<<-c(pvals,pv)
      }
    })
    ##Adjustment of pvalues
    HGTpvals<-p.adjust(pvals,method=pAdjustMethod)
    sapply(1:numGeneSetCollections, function(i) {
      if(nrow(HGTresults[[i]])>0) {
        ind<-match(rownames(HGTresults[[i]]),names(HGTpvals))
        Adjusted.Pvalue<-HGTpvals[ind]
        #HGTresults[[i]]<-cbind(HGTresults[[i]],Adjusted.Pvalue)
        HGTresults[[i]][,"Adjusted.Pvalue"]<<-Adjusted.Pvalue
      }
    })

    names(HGTresults)<-result.names
    cat("-Hypergeometric analysis complete\n\n")

    ##identify gene set collections with hypergeometric test
    ##pvalues < pValueCutoff
    sign.hgt<-lapply(HGTresults, function(x) {
      if(nrow(x)>0) {
        a<-which(x[,"Pvalue"]<pValueCutoff)
        x<-x[a,,drop=FALSE]
        if(length(a)>1) {
          x<-x[order(x[,"Pvalue"]),,drop=FALSE]
        }
        return(x)
      }
    })
    ##identify gene set collections with hypergeometric test adjusted
    ##pvalues < pValueCutoff
    sign.hgt.adj<-lapply(HGTresults, function(x) {
      if(nrow(x)>0) {
        a<-which(x[,"Adjusted.Pvalue"]<pValueCutoff)
        x<-x[a,,drop=FALSE]
        if (length(a)>1) {
          x<-x[order(x[,"Adjusted.Pvalue"]),,drop=FALSE]
        }
        return(x)
      }
    })
  } else {
    HGTresults=NULL
  }
  ######################
  ###GSEA
  ######################
  if(doGSEA) {
    GSEA.results.list <- list()
    cat("-Performing gene set enrichment analysis ...\n")
    ##Calculate enrichment scores for all gene sets in all collections
    test.collection<-list()
    sapply(1:length(listOfGeneSetCollections), function(i) {
      if(verbose) {
        cat("--For", names(listOfGeneSetCollections)[i], "\n")
      }
      if(length(listOfGeneSetCollections[[i]]) > 0)
        test.collection[[i]] <<- collectionGsea(
          listOfGeneSetCollections[[i]],
          geneList=geneList,exponent=exponent,
          nPermutations=nPermutations,
          minGeneSetSize=minGeneSetSize,verbose=verbose)
      else {
        test.collection[[i]]<<-list(Observed.scores=NULL,
                                    Permutation.scores=NULL)
      }
    }
    )
    ##Combine observed and permutation scores for all gene set
    ##collections so that fdr and pvalue functions can be performed on
    ##the entire set of results
    obs.scores<-NULL
    prm.scores<-NULL
    sapply(1:numGeneSetCollections, function(i) {
      obs.scores <<- c(obs.scores,
                       test.collection[[i]]$Observed.scores)
      prm.scores <<- rbind(prm.scores,
                           test.collection[[i]]$Permutation.scores)
    }
    )
    if(length(obs.scores) > 0) {
      total.test.collect <- list("Observed.scores" = obs.scores,
                                 "Permutation.scores" = prm.scores)
      test.FDR.collection <- FDRcollectionGsea(
        permScores = total.test.collect$Permutation.scores,
        dataScores = total.test.collect$Observed.scores)
      test.pvalues.collection <- permutationPvalueCollectionGsea(
        permScores = total.test.collect$Permutation.scores,
        dataScores = total.test.collect$Observed.scores)
      gsea.adjust.pval <- p.adjust(test.pvalues.collection,
                                   method = pAdjustMethod)
      test.GSEA.results <- cbind(total.test.collect$Observed.scores,
                                 test.pvalues.collection, gsea.adjust.pval, test.FDR.collection)
      colnames(test.GSEA.results) <- c("Observed.score", "Pvalue",
                                       "Adjusted.Pvalue", "FDR")

      ##Extract results dataframe for each gene set collection and
      ##orders them by adjusted p-value
      sapply(1 : numGeneSetCollections, function(i) {
        if(!is.null(test.collection[[i]])) {
          match.ind <- match(names(listOfGeneSetCollections[[i]]),
                             rownames(test.GSEA.results))
          match.ind <- match.ind[which(!is.na(match.ind))]
          GSEA.res.mat <- test.GSEA.results[match.ind, , drop=FALSE]
          GSEA.res.mat <- GSEA.res.mat[order(
            GSEA.res.mat[, "Adjusted.Pvalue"]), , drop=FALSE]
          GSEA.results.list[[i]] <<- GSEA.res.mat
        }
      }
      )

      names(GSEA.results.list) <- result.names
    } else {
      ##GSEA.results.list <- vector("list", (numGeneSetCollections))
      sapply(1 : numGeneSetCollections, function(i) {
        GSEA.results.list[[i]] <<- matrix(, nrow=0, ncol=4)
        colnames(GSEA.results.list[[i]]) <<- c("Observed.score",
                                               "Pvalue", "Adjusted.Pvalue", "FDR")
      }
      )
    }
    ##identify gene set collections with GSEA pvalues < pValueCutoff
    sign.gsea<-lapply(GSEA.results.list, function(x) {
      if(nrow(x)>0) {
        a<-which(x[,"Pvalue"]<pValueCutoff)
        x<-x[a,,drop=FALSE]
        if (length(a)>1) {
          x<-x[order(x[,"Pvalue"]),,drop=FALSE]
        }
        return(x)
      }
    })
    ##identify gene set collections with adjusted GSEA pvalues < pValueCutoff
    sign.gsea.adj<-lapply(GSEA.results.list, function(x) {
      if(nrow(x)>0) {
        a<-which(x[,"Adjusted.Pvalue"]<=pValueCutoff)
        x<-x[a,,drop=FALSE]
        if(length(a)>1) {
          x<-x[order(x[,"Adjusted.Pvalue"]),,drop=FALSE]
        }
        return(x)
      }
    })
  } else {
    GSEA.results.list=NULL
  }
  if(doGSOA && doGSEA) {
    overlap<-list()
    overlap.adj<-list()
    ##identify gene set collections with significant pvalues and/or
    ##adjusted pvalues from both GSEA and hypergeometric testing
    sapply(1:numGeneSetCollections, function(i) {
      a1 <- intersect(rownames(sign.gsea[[i]]),
                      rownames(sign.hgt[[i]]))
      a2 <- intersect(rownames(sign.gsea.adj[[i]]),
                      rownames(sign.hgt.adj[[i]]))
      Hypergeometric.Pvalue <-
        HGTresults[[i]][a1, "Pvalue", drop=FALSE]
      Hypergeometric.Adj.Pvalue <-
        HGTresults[[i]][a2, "Adjusted.Pvalue", drop=FALSE]
      GSEA.Pvalue <-
        GSEA.results.list[[i]][a1, "Pvalue", drop=FALSE]
      GSEA.Adj.Pvalue <-
        GSEA.results.list[[i]][a2, "Adjusted.Pvalue", drop=FALSE]
      overlap[[i]] <<- cbind(Hypergeometric.Pvalue, GSEA.Pvalue)
      colnames(overlap[[i]])<<-c("HyperGeo.Pvalue","GSEA.Pvalue")
      overlap.adj[[i]] <<-
        cbind(Hypergeometric.Adj.Pvalue, GSEA.Adj.Pvalue)
      colnames(overlap.adj[[i]])<<-c("HyperGeo.Adj.Pvalue","GSEA.Adj.Pvalue")
    }
    )
    names(overlap) <- result.names
    names(overlap.adj) <- result.names
  } else {
    overlap=NULL
    overlap.adj=NULL
  }

  cat("-Gene set enrichment analysis complete \n")
  final.results <- list("HyperGeo.results" = HGTresults,
                        "GSEA.results" = GSEA.results.list, "Sig.pvals.in.both" = overlap,
                        "Sig.adj.pvals.in.both" = overlap.adj)
  return(final.results)
}


##This function performs hypergeometric tests for over-representation
##of hits, on a list of gene sets. This function applies the
##hyperGeoTest function to an entire list of gene sets and returns a
##data frame.

multiHyperGeoTest <- function(collectionOfGeneSets, universe, hits,
                              minGeneSetSize = 15, pAdjustMethod = "BH", verbose = TRUE) {
  ##check arguments
  paraCheck("gsc", collectionOfGeneSets)
  paraCheck("universe", universe)
  paraCheck("hits", hits)
  paraCheck("minGeneSetSize", minGeneSetSize)
  paraCheck("pAdjustMethod", pAdjustMethod)
  paraCheck("verbose", verbose)
  l.GeneSet <- length(collectionOfGeneSets)
  geneset.size <- unlist(
    lapply(
      lapply(collectionOfGeneSets, intersect, y = universe), length
    )
  )
  if(all(geneset.size < minGeneSetSize))
    stop(paste("The largest number of overlapped genes of gene ",
               "sets with universe is: ", max(geneset.size), ", which is < ",
               minGeneSetSize, "!\n", sep = ""))
  geneset.filtered <- which(geneset.size >= minGeneSetSize)
  ##if verbose, create a progress bar to monitor computation progress
  if(verbose)
    pb <- txtProgressBar(style=3)
  results <- t(
    sapply(geneset.filtered,
           function(i) {
             if(verbose)
               setTxtProgressBar(pb, i/l.GeneSet)
             hyperGeoTest(collectionOfGeneSets[i], universe, hits)
           }
    )
  )
  if(verbose)
    close(pb)
  if(length(results) > 0) {
    ##results <- t(as.data.frame(results))
    ##rownames(results) <- names(collectionOfGeneSets)
    ##remove gene sets with genes < minGeneSetSize in the geneList
    ##Adjust pvalues
    adjPvals <- p.adjust(results[, "Pvalue"], method = pAdjustMethod)
    results <- cbind(results, adjPvals)
    colnames(results)[ncol(results)] <- "Adjusted.Pvalue"
    results <- results[order(results[, "Adjusted.Pvalue"]), , drop=FALSE]
  } else {
    reuslts <- matrix(, nrow=0, ncol=7)
    colnames(results) <- c("Universe Size", "Gene Set Size",
                           "Total Hits", "Expected Hits", "Observed Hits", "Pvalue",
                           "Adjusted.Pvalue")
  }
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

hyperGeoTest <- function(geneSet, universe, hits) {
  ##number of genes in universe
  N <- length(universe)
  ##remove genes from gene set that are not in universe
  geneSet <- intersect(geneSet[[1]], universe)
  ##size of gene set
  m <- length(geneSet)
  Nm <- N-m
  ##hits in gene set
  overlap <- intersect(geneSet, hits)
  ##number of hits in gene set
  k <- length(overlap)
  n <- length(hits)
  HGTresults <- phyper(k-1, m, Nm, n, lower.tail = F)
  ex <- (n/N)*m
  if(m == 0) HGTresults <- NA
  hyp.vec <- c(N, m, n, ex, k, HGTresults)
  names(hyp.vec) <- c("Universe Size", "Gene Set Size", "Total Hits",
                      "Expected Hits", "Observed Hits", "Pvalue")
  return(hyp.vec)
}


##This function computes observed and permutation-based scores associated
##with a gene set enrichment analysis for a collection of Gene Sets.

collectionGsea <- function(collectionOfGeneSets, geneList, exponent=1,
                           nPermutations=1000, minGeneSetSize=15, verbose=TRUE) {
  ##check input arguments
  paraCheck("gsc", collectionOfGeneSets)
  paraCheck("genelist", geneList)
  paraCheck("exponent", exponent)
  paraCheck("minGeneSetSize", minGeneSetSize)
  geneList.names <- names(geneList)
  paraCheck("nPermutations", nPermutations)
  ##tag the gene sets that can be used in the analysis, i.e. those
  ##that are smaller than the size of the gene list and that have more
  ##than 'minGeneSetSize' elements that can be found in the geneList
  nGeneSets <- length(collectionOfGeneSets)
  tagGeneSets <- rep(FALSE, nGeneSets)
  tagGeneSets[which(unlist(lapply(collectionOfGeneSets, length)) <
                      length(geneList))] <- TRUE
  tagGeneSets[which(unlist(lapply(lapply(collectionOfGeneSets,
                                         intersect, y=geneList.names), length)) < minGeneSetSize)] <- FALSE
  ##check that there are actually some gene sets that pass the max
  ##and min cutoffs
  n.tagGeneSets <- sum(tagGeneSets)
  if(n.tagGeneSets == 0)
    warning(paste("There are no gene sets in your collection",
                  " that pass the cutoffs on size", sep=""))
  if(n.tagGeneSets > 0) {
    ##Generate a matrix to store the permutation-based scores, with
    ##one row for each gene set (that has been tagged) and one column
    ##for each permutation
    scoresperm <- matrix(rep(0, (nPermutations * n.tagGeneSets)),
                         nrow=n.tagGeneSets)
    rownames(scoresperm) <- names(collectionOfGeneSets)[which(tagGeneSets)]
    ##Generate a vector to store the experimental scores
    ##one entry for each gene set (that has been tagged)
    scoresObserved <- rep(0, n.tagGeneSets)
    names(scoresObserved) <- names(collectionOfGeneSets)[which(tagGeneSets)]
    ##Compute the scores
    ##create permutation gene list
    perm.gL <- sapply(1:nPermutations, function(n) names(geneList)[
      sample(1:length(geneList), length(geneList),replace=FALSE)])
    perm.gL<-cbind(names(geneList),perm.gL)
    ##check if package snow has been loaded and a cluster object
    ##has been created for HTSanalyzeR
    if(is(getOption("cluster"), "cluster") &&
       "package:snow" %in% search()) {
      scores <- gseaScoresBatchParallel(geneList, geneNames.perm = perm.gL,
                                        collectionOfGeneSets=collectionOfGeneSets[which(tagGeneSets)],
                                        exponent=exponent,nPermutations=nPermutations)
      sapply(1:n.tagGeneSets, function(i) {
        scoresperm[i,]<<-unlist(scores["scoresperm",i])
        scoresObserved[i]<<-unlist(scores["scoresObserved",i])
      }
      )
    } else {
      if(verbose)
        pb <- txtProgressBar(style=3)
      for(i in 1:n.tagGeneSets) {
        scores <- gseaScoresBatch(geneList, geneNames.perm=perm.gL,
                                  geneSet=collectionOfGeneSets[[which(tagGeneSets)[i]]],
                                  exponent=exponent, nPermutations=nPermutations)
        scoresObserved[i] <- scores$scoresObserved
        scoresperm[i,] <- scores$scoresperm
        if(verbose)
          setTxtProgressBar(pb, i/n.tagGeneSets)
      }
      if(verbose)
        close(pb)
    }
  } else {
    scoresObserved <- NULL
    scoresperm <- NULL
  }
  return(list("Observed.scores" = scoresObserved , "Permutation.scores" = scoresperm))
}



##This function computes enrichment score for both input 'geneList'
##and its permutations for one gene set.

gseaScoresBatch <- function(geneList, geneNames.perm, geneSet,
                            exponent=1, nPermutations=1000) {
  ##check input arguments
  paraCheck("genelist", geneList)
  paraCheck("gs", geneSet)
  paraCheck("exponent", exponent)
  paraCheck("nPermutations", nPermutations)
  if(!is.matrix(geneNames.perm))
    stop("'geneNames.perm' should be a matrix!\n")
  if(ncol(geneNames.perm) != (nPermutations+1))
    stop("The No of columns of 'geneNames.perm' should be equal to 'nPermutations'!\n")
  geneList.names <- names(geneList)

  ##The geneSet should be a subset of the gene universe, i.e. we keep
  ##only those element of the gene set that appear in the geneList
  geneSet<-intersect(geneList.names,geneSet)
  ##Compute the size of the gene set and of the genelist
  nh<-length(geneSet)
  N<-length(geneList)

  ES<-rep(0,nPermutations+1)
  Phit<-matrix(0,nrow=N,ncol=nPermutations+1)
  Pmiss<-Phit
  runningES<-NULL

  if(nh>N) {
    stop("Gene Set is larger than Gene List")
  } else {
    hits <- matrix(FALSE, nrow = N, ncol = nPermutations+1)
    hits[which(!is.na(match(geneNames.perm, geneSet)))] <- TRUE
    hits <- matrix(hits,ncol = nPermutations+1 , byrow = FALSE)
    if(sum(hits[,1]) > 0) {
      junk <- sapply(1:(nPermutations+1), function(i)
        Phit[which(hits[, i]), i] <<-
          abs(geneList[which(hits[, i])])^exponent)
      NR <- colSums(Phit)
      Pmiss[which(!hits)] <- 1/(N-nh)
      Pmiss <- sapply(1:(nPermutations+1), function(i)
        cumsum(Pmiss[, i]))
      Phit <- sapply(1:(nPermutations+1), function(i)
        cumsum(Phit[, i])/NR[i])
      runningES <- Phit - Pmiss
      ESrange <- sapply(1:(nPermutations+1), function(i)
        range(runningES[, i]))
      ES <- sapply(1:(nPermutations+1), function(i)
        ESrange[which.max(abs(ESrange[, i])), i])
      if(is.list(ES)) ES<-unlist(ES)
    }
  }
  #Return the relevant information according to mode
  ES<-list(scoresObserved=ES[1], scoresperm=ES[2:(nPermutations+1)])
  return(ES)
}



##This function computes the FDR associated with a permutation-based
##p-value from the GSEA on a list of gene sets

FDRcollectionGsea <- function(permScores, dataScores){
  ##check arguments
  if(!is.matrix(permScores))
    stop("'permScores' should be a matrix!\n")
  if(!is.numeric(dataScores) && !is.integer(dataScores))
    stop("'dataScores' should be an integer or numeric vector!\n")
  if(is.null(names(dataScores)))
    stop("'dataScores' should be named (by gene set identifier)")
  if(nrow(permScores) != length(dataScores))
    stop(paste("The number of rows of the 'permScores' matrix ",
               "should be the same as the length of the 'dataScores' vector",
               sep=""))
  ##create a vector to store the FDRs
  ldataScores<-length(dataScores)
  FDRgeneset=rep(0,ldataScores)
  ##Compute the normalized enrichment score (i.e. divide each ES
  ##(experimental or permutation-based) by the mean of all negative/positive
  ##(depending on the sign of the ES) permutation based scores for that gene set)
  ##This is done gene set by gene set
  sapply(1:ldataScores, function(i) {
    ##Get the indices of all negative permutation-based score
    neg<-which(permScores[i,] <= 0)
    ##Get the indices of all positive permutation-based score
    pos<-which(permScores[i,] >= 0)
    ##Average the values, separately for positive and negative scores
    NegAvg<-abs(mean(permScores[i,neg]))
    PosAvg<-abs(mean(permScores[i,pos]))
    ##Normalize the permutation-based scores,
    ##separately for negative and positive scores
    permScores[i,neg]<<-permScores[i,neg]/NegAvg
    permScores[i,pos]<<-permScores[i,pos]/PosAvg
    ##Normalize the observed scores, separately for negative and positive scores
    dataScores[i] <<- ifelse((dataScores[i] < 0),
                             (dataScores[i]/NegAvg), (dataScores[i]/PosAvg))
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
    if(is.na(dataScores[i])) {
      FDRgeneset[i] <<- 1
    } else if(dataScores[i] < 0) {
      FDRgeneset[i] <<- (sum(permScores <= dataScores[i])/negtot)/
        (sum(dataScores <= dataScores[i])/sum(dataScores <= 0))
    } else {
      FDRgeneset[i] <<- (sum(permScores >= dataScores[i])/postot)/
        (sum(dataScores >= dataScores[i])/sum(dataScores >= 0))
    }
    FDRgeneset[i] <<- ifelse(FDRgeneset[i]>1, 1, FDRgeneset[i])
  })
  #name the FDRs (by gene set name) and return the vector
  names(FDRgeneset) <- names(dataScores)
  return(FDRgeneset)
}




##This function compute the nominal p-value associated with a GSEA for a
##collection of gene sets, from the outputs of collectionGsea

permutationPvalueCollectionGsea <- function(permScores, dataScores){
  ##check 'permScores'
  if(!is.matrix(permScores))
    stop("The argument permScores should be a matrix")
  #check 'dataScores'
  if(!is.vector(dataScores) || is.null(names(dataScores)))
    stop("The argument dataScores should be a named vector")
  if(!is.integer(dataScores) && !is.numeric(dataScores))
    stop("The argument dataScores should be a numerical vector")
  ##check that the dimensions of permScores and dataScores match to
  ##what is expected
  l.dataScores <- length(dataScores)
  nPerm <- ncol(permScores)
  if(nrow(permScores) != l.dataScores)
    warning("The number of rows of the permScores matrix is not ",
            "equal to the length of dataScores")
  ##initialize a pvalues vector
  pval <- rep(1, l.dataScores)
  ##go through each element of dataScores and see how many permScores
  ##in the corresponding row are higher (in magnitude)
  ##this is done separately for negative and positive scores
  ##the scores of zero are simply not treated because they are left
  ##at the initial pvalue of 1
  valid.id <- which(!is.na(dataScores))
  pval <- sapply(valid.id, function(i) {
    ifelse(
      dataScores[i] > 0,
      sum(permScores[i, ] > dataScores[i])/nPerm,
      sum(permScores[i, ] < dataScores[i])/nPerm
    )

  })
  names(pval)<-names(dataScores)
  return(pval)
}





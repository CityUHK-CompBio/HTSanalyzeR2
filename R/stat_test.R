#' Perform statistical tests on a cellHTS object
#'
#' This function takes a normalized, configured and annotated cellHTS object and
#' performs statistical tests on it for the significance of a set of observations
#' for each condition tested in a high-throughput screen.
#'
#' @param cellHTSobject An object of class cellHTS.
#' @param annotationColumn A single character value specifying the name of the
#' column in the fData(cellHTSobject) data frame from which the feature
#' identifiers will be extracted.
#' @param controls A single character value specifying the name of the controls
#' to be used as a control population in the two-sample tests (this HAS to be
#' corresponding to how these control wells have been annotated in the column
#' "controlStatus" of the fData(cellHTSobject) data frame). If nothing is
#' specified, the function will look for negative controls labelled "neg".
#' @param alternative A single character value specifying the alternative
#' hypothesis: "two.sided", "less" or "greater".
#' @param logged A single logical value specifying whether or not the data has
#' been logged during the normalization process.
#' @param tests A single character value specifying the tests to be performed:
#' "T-test", "MannWhitney" or "RankProduct". If nothing is specified, all three
#' tests will be performed. Be aware that the Rank Product test is slower than
#' the other two, and returns a percent false discovery (equivalent to a FDR,
#' not a p-value).
#'
#' @details
#' The tests are computed taking into account only the wells labelled "sample"
#' in the column "controlStatus" of the fData(cellHTSobject).
#' The two sample tests compare the set of observations for one construct to the
#' values obtained for a population considered as "control". The one-sample tests
#' compare the set of observations for one construct to the median of all values
#' obtained across all constructs labelled as "sample". This type of test assumes
#' that most constructs are expected to show a negligible effect. It is therefore
#' not advised to use this type of tests when the constructs tested have been
#' pre-screened for being associated with a phenotype.
#' Please be aware that both types of tests are less reliable when the number
#' of replicates for each construct is low.
#'
#' @return A matrix with two columns, one for each type of test (two-sample and
#' one-sample test) except the Rank Product (no alternative), and a row for each
#' construct (row names corresponding to the identifiers given by the
#' "annotationcolumn" entry).
#'
#' @references
#' Michael Boutros, Ligia P. Bras L and Wolfgang Huber. Analysis of cell-based
#' RNAi screens. Genome Biology 7:7 R66 (2006)."
#'
#' @export
#' @importFrom cellHTS2 Data
#' @importFrom stats median t.test wilcox.test
#' @examples
#' data(xn)
#' test.stats <- cellHTS2OutputStatTests(cellHTSobject=xn, annotationColumn="GeneID",
#'                                       alternative="two.sided", tests=c("T-test"))
cellHTS2OutputStatTests <- function(cellHTSobject,
                                    annotationColumn = "GeneID",
                                    controls = "neg",
                                    alternative = "two.sided",
                                    logged = FALSE,
                                    tests = "T-test") {

  ## check arguments
  paraCheck("StatTest", "normCellHTSobject", cellHTSobject)
  paraCheck("StatTest", "annotationColumn", annotationColumn)

  ## check that the annotationColumn is one column in the
  #  fData(cellHTSobject) dataframe
  if(!(annotationColumn %in% colnames(Biobase::fData(cellHTSobject))))
    stop(paste("The 'annotationColumn' parameter does not match ",
               "any column in your cellHTS object", sep=""))

  ##check that 'controls' matches a status in the 'controlStatus'
  # column of the cellHTS cellHTSobject
  paraCheck("StatTest", "nwStatsControls", controls)
  if(!(controls %in% Biobase::fData(cellHTSobject)[, "controlStatus"]))
    stop(paste("The 'controls' parameter does not match to any ",
               "status in the 'controlStatus' column of your cellHTS object",
               sep=""))

  paraCheck("StatTest", "nwStatsAlternative", alternative)
  paraCheck("StatTest", "nwStatsTests", tests)


  ##make a named data matrix (only samples) rows=features,
  ##columns=replicates, with row names = identifiers in the
  ##"annotationColumn" of the fData() data frame
  dataNw <- Data(cellHTSobject)[, 1:ncol(Data(cellHTSobject)), 1]
  rownames(dataNw) <- Biobase::fData(cellHTSobject)[, annotationColumn]
  dataNw <- dataNw[which(Biobase::fData(cellHTSobject)[, "controlStatus"] ==
                           "sample"), ]
  dataNw <- dataNw[which(!is.na(rownames(dataNw))), ]
  ##make a vector of data for the control 	population
  controlData <- Data(cellHTSobject)[
    which(Biobase::fData(cellHTSobject)[, "controlStatus"] == controls),
    1:ncol(Data(cellHTSobject)), 1]
  controlData <- as.vector(controlData)
  ##compute the median of all samples, for the one sample tests
  mu = median(as.vector(dataNw), na.rm = TRUE)
  ##make a list of the data (one entry per unique ID): each entry in
  ##the list correspond to a unique name, and the element under that
  ##entry is a vector of data of replicates for that unique construct
  ##formatting this as a list allows us to have different number of
  ##replicates for each construct
  replicatesNames <- unique(rownames(dataNw))
  replicates <- as.list(rep(0, length(replicatesNames)))
  names(replicates) <- replicatesNames
  nreplicates <- length(replicates)
  sapply(1:nreplicates, function(i) {
    replicates[[i]] <- c(dataNw[
      which(rownames(dataNw) == names(replicates)[i]), ])
  })
  if("T-test" %in% tests) {
    ##Compute the one sample t-test (only possible for those entries
    ##of the list that contain more than one replicate measurement
    ##otherwise the pvalue will be left at the default value of 1)
    t.test.pvalues.one.sample<-rep(1,nreplicates)
    names(t.test.pvalues.one.sample)<-names(replicates)
    sapply(1:nreplicates, function(i) {
      if(sum(!is.na(replicates[[i]])) >= 2)
        t.test.pvalues.one.sample[i] <-
          t.test(x = replicates[[i]], mu = mu ,
                 alternative = alternative)$p.value
    })
    ##Compute the two samples t-test (only possible for those entries
    ##of the list that contain more than one replicate measurement
    ##otherwise the pvalue will be left at the default value of 1)
    t.test.pvalues.two.samples <- rep(1, nreplicates)
    names(t.test.pvalues.two.samples) <- names(replicates)
    sapply(1:nreplicates, function(i) {
      if(sum(!is.na(replicates[[i]])) >= 2)
        t.test.pvalues.two.samples[i] <-
          t.test(x = replicates[[i]], y = controlData,
                 alternative = alternative)$p.value
    })
  }
  if("MannWhitney" %in% tests) {
    ##Compute the one sample mann-whitney test(only possible for
    ##those entries of the list that contain more than one replicate
    ##measurement otherwise the pvalue will be left at the default
    ##value of 1)
    mannW.test.pvalues.one.sample<-rep(1,nreplicates)
    names(mannW.test.pvalues.one.sample)<-names(replicates)
    sapply(1:nreplicates, function(i) {
      if(sum(!is.na(replicates[[i]])) >= 2)
        mannW.test.pvalues.one.sample[i] <-
          wilcox.test(x = replicates[[i]], mu = mu,
                      alternative = alternative)$p.value
    })
    ##Compute the two samples mann-whitney test(only possible for
    ##those entries of the list that contain more than one replicate
    ##measurement otherwise the pvalue will be left at the default
    ##value of 1)
    mannW.test.pvalues.two.samples<-rep(1,nreplicates)
    names(mannW.test.pvalues.two.samples)<-names(replicates)
    sapply(1:nreplicates, function(i) {
      if(sum(!is.na(replicates[[i]])) >= 2)
        mannW.test.pvalues.two.samples[i] <-
          wilcox.test(x = replicates[[i]], y = controlData,
                      alternative = alternative)$p.value
    })
  }
  if("RankProduct" %in% tests) {
    ##Prepare the data for the Rank Product test: the function 'RP'
    ##requires as input a matrix with a row for each construct and
    ##a column for each replicate (this function was built for
    ##microarrays, where each column could correspond to an array
    ##with a different treatment class this is not the case here,
    ##hence we set the class argument of the RP function to 1 for
    ##all columns
    ##Since our data might include varying number of replicates, a
    ##matrix of maximal dimensions (number of columns=max number of
    ##replicates) will be built with NAs when necessary
    lengthreplicates <- sapply(replicates, length)
    maxlength <- max(lengthreplicates)
    replicatesmatrix <- c(replicates[[1]],
                          rep(NA, (maxlength-length(replicates[[1]]))))
    sapply(2:length(replicates), function(i) {
      if(length(replicates[[i]]) < maxlength)
        replicatesmatrix <- rbind(replicatesmatrix,
                                   c(replicates[[i]], rep(NA,(maxlength-length(replicates[[i]])))))
      if(length(replicates[[i]]) == maxlength)
        replicatesmatrix <- rbind(replicatesmatrix, c(replicates[[i]]))
      NULL
    })
    rownames(replicatesmatrix) <- names(replicates)
    #Compute the Rank Product test
    rankptest <- RankProd::RP(data = replicatesmatrix,
                    cl=rep(1, ncol(replicatesmatrix)), logged=logged,
                    gene.names = rownames(replicatesmatrix))
  }
  ##Assemble the results as a column for each test and a row for each
  ##construct: if all 3 tests are performed: the RP test produces one
  ##column for up-regulated genes and one for down-regulated ones
  ##(which constrasts with the other two types of tests that produce
  ##only one result per alternative, therefore, the RP column that
  ##will be in the output is different depending on the alternative
  ##chosen, which is why there are 3 parts to this assembly)
  if(length(tests) == 3) {
    stats <- cbind(t.test.pvalues.one.sample,
                   t.test.pvalues.two.samples, mannW.test.pvalues.one.sample,
                   mannW.test.pvalues.two.samples)
    rownames(stats) <- names(replicates)
    if(alternative == "two.sided") {
      upRP <- rankptest$pfp[,1]
      names(upRP) <- rownames(replicatesmatrix)
      downRP <- rankptest$pfp[,2]
      names(downRP) <- rownames(replicatesmatrix)
      stats <- cbind(stats,upRP,downRP)
      colnames(stats) <- c("t.test.pvalues.one.sample",
                           "t.test.pvalues.two.samples", "mannW.test.pvalues.one.sample",
                           "mannW.test.pvalues.two.samples", "rank.product.pfp.greater",
                           "rank.product.pfp.less")
    } else if(alternative == "greater") {
      upRP <- rankptest$pfp[,1]
      names(upRP) <- rownames(replicatesmatrix)
      stats <- cbind(stats,upRP)
      colnames(stats) <- c("t.test.pvalues.one.sample",
                           "t.test.pvalues.two.samples", "mannW.test.pvalues.one.sample",
                           "mannW.test.pvalues.two.samples", "rank.product.pfp.greater")
    } else if(alternative == "less") {
      downRP <- rankptest$pfp[,2]
      names(downRP) <- rownames(replicatesmatrix)
      stats <- cbind(stats,downRP)
      colnames(stats) <- c("t.test.pvalues.one.sample",
                           "t.test.pvalues.two.samples", "mannW.test.pvalues.one.sample",
                           "mannW.test.pvalues.two.samples", "rank.product.pfp.less")
    }
  }
  ##Assemble the results as a column for each test and a row for each
  ##construct: if only 1 test is performed:
  else if(length(tests) == 1) {
    if(tests == "T-test") {
      stats <- cbind(t.test.pvalues.one.sample, t.test.pvalues.two.samples)
      rownames(stats) <- names(replicates)
      colnames(stats) <- c("t.test.pvalues.one.sample",
                           "t.test.pvalues.two.samples")
    } else if(tests == "MannWhitney") {
      stats <- cbind(mannW.test.pvalues.one.sample, mannW.test.pvalues.two.samples)
      rownames(stats) <- names(replicates)
      colnames(stats) <- c("mannW.test.pvalues.one.sample",
                           "mannW.test.pvalues.two.samples")
    } else if(tests == "RankProduct") {
      if(alternative == "two.sided") {
        upRP <- rankptest$pfp[,1]
        names(upRP) <- rownames(replicatesmatrix)
        downRP <- rankptest$pfp[,2]
        names(downRP) <- rownames(replicatesmatrix)
        stats <- cbind(upRP,downRP)
        rownames(stats) <- names(replicates)
        colnames(stats) <- c("rank.product.pfp.greater",
                             "rank.product.pfp.less")
      } else if(alternative == "greater") {
        upRP<-rankptest$pfp[,1]
        names(upRP)<-rownames(replicatesmatrix)
        stats<-as.matrix(upRP,ncol=1)
        rownames(stats)<-names(replicates)
        colnames(stats)<-c("rank.product.pfp.greater")
      } else if(alternative == "less") {
        downRP<-rankptest$pfp[,2]
        names(downRP)<-rownames(replicatesmatrix)
        stats<-as.matrix(downRP,ncol=1)
        rownames(stats)<-names(replicates)
        colnames(stats)<-c("rank.product.pfp.less")
      }
    }
  }
  ##Assemble the results as a column for each test and a row for each
  ##construct: if 2 test are performed (one block for each combination
  ##of tests):
  else if(length(tests) == 2) {
    if(all(c("T-test", "MannWhitney") %in% tests)) {
      stats <- cbind(t.test.pvalues.one.sample, t.test.pvalues.two.samples,
                     mannW.test.pvalues.one.sample, mannW.test.pvalues.two.samples)
      rownames(stats) <- names(replicates)
      colnames(stats) <- c("t.test.pvalues.one.sample",
                           "t.test.pvalues.two.samples", "mannW.test.pvalues.one.sample",
                           "mannW.test.pvalues.two.samples")
    } else if(all(c("T-test","RankProduct") %in% tests)) {
      stats <- cbind(t.test.pvalues.one.sample, t.test.pvalues.two.samples)
      rownames(stats) <- names(replicates)
      if(alternative == "two.sided") {
        upRP <- rankptest$pfp[,1]
        names(upRP) <- rownames(replicatesmatrix)
        downRP <- rankptest$pfp[,2]
        names(downRP) <- rownames(replicatesmatrix)
        stats <- cbind(stats, upRP, downRP)
        colnames(stats) <- c("t.test.pvalues.one.sample",
                             "t.test.pvalues.two.samples", "rank.product.pfp.greater",
                             "rank.product.pfp.less")
      } else if(alternative == "greater") {
        upRP <- rankptest$pfp[,1]
        names(upRP) <- rownames(replicatesmatrix)
        stats <- cbind(stats,upRP)
        colnames(stats) <- c("t.test.pvalues.one.sample",
                             "t.test.pvalues.two.samples", "rank.product.pfp.greater")
      } else if(alternative == "less") {
        downRP <- rankptest$pfp[,2]
        names(downRP) <- rownames(replicatesmatrix)
        stats <- cbind(stats,downRP)
        colnames(stats) <- c("t.test.pvalues.one.sample",
                             "t.test.pvalues.two.samples", "rank.product.pfp.less")
      }
    } else if(all(c("MannWhitney", "RankProduct") %in% tests)) {
      stats <- cbind(mannW.test.pvalues.one.sample,
                     mannW.test.pvalues.two.samples)
      rownames(stats) <- names(replicates)
      if(alternative == "two.sided") {
        upRP <- rankptest$pfp[, 1]
        names(upRP) <- rownames(replicatesmatrix)
        downRP <- rankptest$pfp[, 2]
        names(downRP) <- rownames(replicatesmatrix)
        stats <- cbind(stats, upRP, downRP)
        colnames(stats) <- c("mannW.test.pvalues.one.sample",
                             "mannW.test.pvalues.two.samples", "rank.product.pfp.greater",
                             "rank.product.pfp.less")
      } else if(alternative == "greater") {
        upRP <- rankptest$pfp[, 1]
        names(upRP) <- rownames(replicatesmatrix)
        stats <- cbind(stats,upRP)
        colnames(stats) <- c("mannW.test.pvalues.one.sample",
                             "mannW.test.pvalues.two.samples", "rank.product.pfp.greater")
      } else if(alternative == "less") {
        downRP <- rankptest$pfp[, 2]
        names(downRP) <- rownames(replicatesmatrix)
        stats <- cbind(stats, downRP)
        colnames(stats) <- c("mannW.test.pvalues.one.sample",
                             "mannW.test.pvalues.two.samples", "rank.product.pfp.less")
      }
    }
  }
  return(stats)
}


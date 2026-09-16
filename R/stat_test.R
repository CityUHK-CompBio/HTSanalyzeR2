#' Statistical tests for high-throughput screen data
#'
#' Performs one-sample and two-sample tests on the observations of every
#' construct of a high-throughput screen, for each condition tested.
#'
#' These tests only need a matrix of measurements, a feature annotation and a
#' sample/control labelling, so they work on plain R objects. They used to be
#' reachable only through a `cellHTS2` object. That package has left
#' Bioconductor, so the tests now take the measurements, the annotation and the
#' sample/control labelling directly.
#'
#' @param data A numeric matrix with one row per feature (well) and one column
#' per replicate.
#' @param annotation A vector of feature identifiers, one per row of `data`.
#' Replicates of the same construct must share an identifier.
#' @param controlStatus A character vector, one per row of `data`, labelling
#' each row as `"sample"` or as a control group.
#' @param controls A single character value giving the name of the control group
#' used as the control population in the two-sample tests. If nothing is
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
#' @details
#' The tests are computed taking into account only the rows labelled "sample" in
#' `controlStatus`.
#' The two sample tests compare the set of observations for one construct to the
#' values obtained for a population considered as "control". The one-sample tests
#' compare the set of observations for one construct to the median of all values
#' obtained across all constructs labelled as "sample". This type of test assumes
#' that most constructs are expected to show a negligible effect. It is therefore
#' not advised to use this type of tests when the constructs tested have been
#' pre-screened for being associated with a phenotype.
#' Please be aware that both types of tests are less reliable when the number
#' of replicates for each construct is low.
#' @return A matrix with two columns, one for each type of test (two-sample and
#' one-sample test) except the Rank Product (no alternative), and a row for each
#' construct (row names corresponding to `annotation`).
#' @references
#' Michael Boutros, Ligia P. Bras L and Wolfgang Huber. Analysis of cell-based
#' RNAi screens. Genome Biology 7:7 R66 (2006)."
#' @export
#' @importFrom stats median t.test wilcox.test
#' @examples
#' data <- matrix(rnorm(24), nrow = 6,
#'                dimnames = list(paste0("well", 1:6), paste0("rep", 1:4)))
#' annotation <- c("geneA", "geneA", "geneB", "geneB", "geneC", "geneC")
#' status <- c(rep("sample", 4), "neg", "neg")
#' screenStatTests(data, annotation, status, tests = c("T-test", "MannWhitney"))
screenStatTests <- function(data,
                            annotation,
                            controlStatus,
                            controls = "neg",
                            alternative = "two.sided",
                            logged = FALSE,
                            tests = "T-test") {

  ## check arguments
  if (!is.matrix(data) || !is.numeric(data))
    stop("'data' must be a numeric matrix with one row per feature.\n",
         call. = FALSE)
  if (length(annotation) != nrow(data))
    stop("'annotation' must provide one identifier per row of 'data'.\n",
         call. = FALSE)
  if (length(controlStatus) != nrow(data))
    stop("'controlStatus' must provide one label per row of 'data'.\n",
         call. = FALSE)
  paraCheck("StatTest", "nwStatsControls", controls)
  if (!(controls %in% controlStatus))
    stop("The 'controls' parameter does not match any value in 'controlStatus'.\n",
         call. = FALSE)
  paraCheck("StatTest", "nwStatsAlternative", alternative)
  paraCheck("StatTest", "nwStatsTests", tests)

  ## RankProd carries a non-FOSS licence, so it is an optional dependency.
  if ("RankProduct" %in% tests && !requireNamespace("RankProd", quietly = TRUE)) {
    stop(
      "The 'RankProduct' test requires the optional 'RankProd' package.\n",
      "Please install it with BiocManager::install(\"RankProd\"), or run the ",
      "tests without 'RankProduct'.\n",
      call. = FALSE
    )
  }

  ## make a named data matrix (only samples) rows = features, columns =
  ## replicates, with row names = the feature identifiers
  dataNw <- data[controlStatus == "sample", , drop = FALSE]
  rownames(dataNw) <- annotation[controlStatus == "sample"]
  dataNw <- dataNw[!is.na(rownames(dataNw)), , drop = FALSE]
  ## make a vector of data for the control population
  controlData <- as.vector(data[controlStatus == controls, , drop = FALSE])
  ##compute the median of all samples, for the one sample tests
  mu = median(as.vector(dataNw), na.rm = TRUE)
  ##make a list of the data (one entry per unique ID): each entry in
  ##the list correspond to a unique name, and the element under that
  ##entry is a vector of data of replicates for that unique construct
  ##formatting this as a list allows us to have different number of
  ##replicates for each construct
  replicatesNames <- unique(rownames(dataNw))
  ## one entry per construct, holding its replicate measurements. The names are
  ## the construct identifiers and they end up as the row names of the result,
  ## so they must be kept (and kept in the original order).
  replicates <- split(seq_len(nrow(dataNw)), rownames(dataNw))[replicatesNames]
  replicates <- lapply(replicates, function(rows) {
    as.vector(dataNw[rows, , drop = FALSE])
  })
  names(replicates) <- replicatesNames
  nreplicates <- length(replicates)
  if("T-test" %in% tests) {
    ##Compute the one sample t-test (only possible for those entries
    ##of the list that contain more than one replicate measurement
    ##otherwise the pvalue will be left at the default value of 1)
    t.test.pvalues.one.sample<-rep(1,nreplicates)
    names(t.test.pvalues.one.sample)<-names(replicates)
    valid <- vapply(replicates, function(x) sum(!is.na(x)) >= 2, logical(1))
    t.test.pvalues.one.sample[valid] <- vapply(replicates[valid], function(x) {
      t.test(x = x, mu = mu, alternative = alternative)$p.value
    }, numeric(1))
    ##Compute the two samples t-test (only possible for those entries
    ##of the list that contain more than one replicate measurement
    ##otherwise the pvalue will be left at the default value of 1)
    t.test.pvalues.two.samples <- rep(1, nreplicates)
    names(t.test.pvalues.two.samples) <- names(replicates)
    valid <- vapply(replicates, function(x) sum(!is.na(x)) >= 2, logical(1))
    t.test.pvalues.two.samples[valid] <- vapply(replicates[valid], function(x) {
      t.test(x = x, y = controlData, alternative = alternative)$p.value
    }, numeric(1))
  }
  if("MannWhitney" %in% tests) {
    ##Compute the one sample mann-whitney test(only possible for
    ##those entries of the list that contain more than one replicate
    ##measurement otherwise the pvalue will be left at the default
    ##value of 1)
    mannW.test.pvalues.one.sample<-rep(1,nreplicates)
    names(mannW.test.pvalues.one.sample)<-names(replicates)
    valid <- vapply(replicates, function(x) sum(!is.na(x)) >= 2, logical(1))
    mannW.test.pvalues.one.sample[valid] <- vapply(replicates[valid], function(x) {
      wilcox.test(x = x, mu = mu, alternative = alternative)$p.value
    }, numeric(1))
    ##Compute the two samples mann-whitney test(only possible for
    ##those entries of the list that contain more than one replicate
    ##measurement otherwise the pvalue will be left at the default
    ##value of 1)
    mannW.test.pvalues.two.samples<-rep(1,nreplicates)
    names(mannW.test.pvalues.two.samples)<-names(replicates)
    valid <- vapply(replicates, function(x) sum(!is.na(x)) >= 2, logical(1))
    mannW.test.pvalues.two.samples[valid] <- vapply(replicates[valid], function(x) {
      wilcox.test(x = x, y = controlData, alternative = alternative)$p.value
    }, numeric(1))
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
    lengthreplicates <- vapply(replicates, length, integer(1))
    maxlength <- max(lengthreplicates)
    replicatesmatrix <- do.call(rbind, lapply(replicates, function(x) {
      if (length(x) < maxlength) {
        c(x, rep(NA_real_, maxlength - length(x)))
      } else {
        x
      }
    }))
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

#' @include utils.R
# TSImport ----------------------------------------------------------------
setClass("TSImport",
         slot = c(
         experimentName = "character",
         phenotypeTS = "list",
         pvaluesTS = "list",
         DoGSOA = "logical",
         GSOADesign.matrix = "matrix",
         results = "list"
         ),
         prototype = prototype(
         DoGSOA = FALSE
         ))


setMethod("initialize",
          signature = "TSImport",
          function(.Object,
                   experimentName,
                   phenotypeTS,
                   pvaluesTS = list(),
                   DoGSOA = FALSE,
                   GSOADesign.matrix = matrix(NA, nrow = 1, ncol = 2, dimnames = list(c("cutoff"), c("phenotype", "pvalues")))) {
            #------------------------------------------------------------------
            ## check parameters
            paraCheck("TSImport", "experimentName", experimentName)
            paraCheck("TSImport", "phenotypeTS", phenotypeTS)
            if(length(experimentName) != length(phenotypeTS)){
              stop("length of 'experimentName' should equal to the column number of 'phenotypeTS'!\n")
            }
            .Object@experimentName <- experimentName
            .Object@phenotypeTS <- phenotypeTS
            #--------------------------------------------------------------------
            ## package and update phenotypeTS
            names(phenotypeTS) <- experimentName
            .Object@results$phenotypeTS <- phenotypeTS
            #----------------------------------------------------------------------
            ## package hitsTS
            .Object@DoGSOA <- DoGSOA
           if(any(!is.na(GSOADesign.matrix))){
             if(!.Object@DoGSOA){
               warning("Please set DoGSOA as TRUE, otherwise hypergeometric test analysis would not be conducted!\n")
             }
             paraCheck("TSImport", "GSOADesign.matrix", GSOADesign.matrix)
              if(all(!is.na(GSOADesign.matrix))){
                warning("both metrics have value, would only use 'phenotype' to choose hits!\n")
              } else if(!is.na(GSOADesign.matrix[, "phenotype"])){
                tmphitsTS <- lapply(phenotypeTS, function(x){
                  tmp1 <- x[which(abs(x) > GSOADesign.matrix[, "phenotype"])]
                  names(tmp1)
              })
              } else {
                if(any(!is.na(pvaluesTS))){
               paraCheck("TSImport", "pvaluesTS", pvaluesTS)
                tmphitsTS <- lapply(pvaluesTS, function(x){
                  tmp1 <- x[which(x < GSOADesign.matrix[, "pvalues"])]
                  names(tmp1)
                })} else {
                  stop("There is no pvalues!\n")
                }
                } ## END else
             names(tmphitsTS) <- experimentName
             .Object@results$hitsTS <- tmphitsTS
           }  ## END IF
            .Object@GSOADesign.matrix <- GSOADesign.matrix
           #-----------------------------------------------------------------------------
          ## package pvaluesTS
            if(length(pvaluesTS) > 0){
          paraCheck("TSImport", "pvaluesTS", pvaluesTS)
          if(length(pvaluesTS) != length(phenotypeTS)){
            stop("'pvaluesTS' should have the same length as 'phenotypeTS'!\n")
          }
              names(pvaluesTS) <- experimentName
              .Object@pvaluesTS <- pvaluesTS
              .Object@results$pvaluesTS <- pvaluesTS
            } # END IF
         #------------------------------------------------------------------------
            .Object
          })

## constructed function
#' An S4 class for Time Series data import.
#'
#' This S4 class packages time-series data for further GSCA and network analysis.
#'
#' @slot experimentName A character vector specifying the experiment name for each time point.
#' @slot phenotypeTS A list of phenotypes, each element of this list is a numeric vector phenotypes named by gene
#' identifiers for each time point. Note: the order of each element of this list must match the experimentName.
#' @slot pvaluesTS A list of pvalues, each element of this list is a numeric vector pvalues named by gene
#' identifiers for each time point. Note: the order of each element of this list must match the experimentName.
#' @slot DoGSOA a single logical value specifying to perform gene set
#' overrepresentation analysis (when DoGSOA=TRUE) or not (when DoGSOA=FALSE),
#' default is FALSE.
#' @slot GSOADesign.matrix A numeric matrix to specify how to choose hits when DoGSOA = TRUE. It must be a 1*2 matrix
#' with rownames named as "cutoff" and colnames named as "phenotype" and "pvalues"
#'
#' @slot results A list of packaged Time Series import data. It contains phenotypeTS, pvaluesTS and hitsTS.
#'
#' @export


TSImport <- function(experimentName, phenotypeTS, pvaluesTS = list(),
                     DoGSOA = FALSE,
                     GSOADesign.matrix = matrix(NA, nrow = 1, ncol = 2, dimnames = list(c("cutoff"), c("phenotype", "pvalues")))) {
  paraCheck("TSImport", "experimentName", experimentName)
  paraCheck("TSImport", "phenotypeTS", phenotypeTS)


  object <- new(
    Class = "TSImport",
    experimentName = experimentName,
    phenotypeTS = phenotypeTS,
    pvaluesTS = pvaluesTS,
    DoGSOA = DoGSOA,
    GSOADesign.matrix = GSOADesign.matrix
  )
}

# show --------------------------------------------------------------------
setMethod("show", signature = "TSImport", function(object) {
  cat("A TSImport (Time series Import data) object:\n\n")
  ## experimentName
  cat("-experiment:\n")
  cat("   Length:", length(object@experimentName), "\n")
  cat("   experimentName by order:", object@experimentName, "\n\n")
  ## phenotypeTS
  phenotypeTSLength <- unlist(lapply(object@phenotypeTS, length))
  phenotypeTSLength <- matrix(phenotypeTSLength, nrow = 1, dimnames = list(c("length"), c(object@experimentName)))
  cat("-phenotypeTS:\n")
  print(phenotypeTSLength, quote = F)
  cat("\n")
  ## pvaluesTS
  pvaluesTSLenght <- unlist(lapply(object@pvaluesTS, length))
  if(is.null(pvaluesTSLenght)){
    cat("-pvaluesTS:\n", NA, "\n\n")
  } else{
    cat("-pvaluesTS:\n")
    print(matrix(pvaluesTSLenght, nrow = 1, dimnames = list(c("length"), c(object@experimentName))), quote = F)
    cat("\n\n")
  }
  ## DoGSOA
  cat("-DoGSOA:\n", object@DoGSOA, "\n\n")
  ## GSOADesign.matrix
  cat("-GSOADesign.matrix\n")
  print(object@GSOADesign.matrix, quote = F)
  cat("\n\n")
  ## hitsTS
  if(!object@DoGSOA){
    cat("-hitsTS:", NA, "\n")
  } else{
    hitsTSLenght <- unlist(lapply(object@results$hitsTS, length))
    cat("-hitsTS:\n")
    print(matrix(hitsTSLenght, nrow = 1, dimnames = list(c("length"), c(object@experimentName))), quote = F)
    cat("\n")
  }
})




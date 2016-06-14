#' @include classUnion.R
GSCA <- setClass(
  "GSCA",
  representation(
    listOfGeneSetCollections = "list",
    geneList = "numeric_Or_integer",
    # geneList = "numeric",
    hits = "character",
    para = "list",
    result = "list",
    summary = "list",
    preprocessed = "logical"
  ),
  prototype = list(
    listOfGeneSetCollections = list(),
    geneList = numeric(),
    hits = character(),
    para = list(
      pValueCutoff = 0.05,
      pAdjustMethod = "BH",
      nPermutations = 1000,
      minGeneSetSize = 15,
      exponent = 1
    ),
    result = list(),
    summary = list(),
    preprocessed = FALSE
  )
)

setMethod("initialize",
          "GSCA",
          function(.Object,
                   listOfGeneSetCollections,
                   geneList,
                   hits) {
            #######################
            ##check input arguments
            #######################
            # paraCheck(name = "gscs", listOfGeneSetCollections)
            # paraCheck(name = "genelist", geneList)
            # paraCheck(name = "hits", hits)
            #[para<-paraCheck(name="gsca.para",para)
            #######################
            ##  initialization
            #######################
            .Object@listOfGeneSetCollections <-
              listOfGeneSetCollections
            .Object@geneList <- geneList
            .Object@hits <- hits
            ##[.Object@para<-para]
            ##check result summary and preprocessed status
            .Object@result <- list()
            .Object@preprocessed <- FALSE

            ##summary info--framework
            ###gene set collections
            sum.info.gsc <-
              matrix(NA, length(listOfGeneSetCollections), 2)
            rownames(sum.info.gsc) <-
              names(listOfGeneSetCollections)
            colnames(sum.info.gsc) <-
              c("input", paste("above min size", sep = ""))
            ###gene list
            sum.info.gl <- matrix(NA, 1, 4)
            colnames(sum.info.gl) <-
              c("input", "valid", "duplicate removed", "converted to entrez")
            rownames(sum.info.gl) <- "Gene List"
            ###hits
            sum.info.hits <- matrix(NA, 1, 2)
            colnames(sum.info.hits) <- c("input", "preprocessed")
            rownames(sum.info.hits) <- "Hits"
            ###parameters
            sum.info.para <- list()
            sum.info.para$hypergeo <- matrix(NA, 1, 3)
            colnames(sum.info.para$hypergeo) <-
              c("minGeneSetSize", "pValueCutoff", "pAdjustMethod")
            rownames(sum.info.para$hypergeo) <- "HyperGeo Test"

            sum.info.para$gsea <- matrix(NA, 1, 5)
            colnames(sum.info.para$gsea) <-
              c("minGeneSetSize",
                "pValueCutoff",
                "pAdjustMethod",
                "nPermutations",
                "exponent")
            rownames(sum.info.para$gsea) <- "GSEA"
            ###results
            sum.info.results <-
              matrix(NA, 3, length(listOfGeneSetCollections))
            colnames(sum.info.results) <-
              names(listOfGeneSetCollections)
            rownames(sum.info.results) <-
              c("HyperGeo", "GSEA", "Both")

            ##summary info--initialize
            sum.info.gsc[, "input"] <-
              unlist(lapply(listOfGeneSetCollections, length))
            sum.info.gl[, "input"] <- length(geneList)
            sum.info.hits[, "input"] <- length(hits)

            .Object@summary <-
              list(
                gsc = sum.info.gsc,
                gl = sum.info.gl,
                hits = sum.info.hits,
                para = sum.info.para,
                results = sum.info.results
              )
            .Object
          })

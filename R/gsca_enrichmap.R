if(!isGeneric("appendGSTerms"))
  setGeneric("appendGSTerms",function(object,...)
    standardGeneric("appendGSTerms"), package="HTSanalyzeR2")


#' Append gene set terms to GSCA results
#'
#' This is a generic function.
#' When implemented as the S4 method for objects of class GSCA, this function
#' finds corresponding annotation terms for GO, KEGG and MSigDB gene sets and
#' inserts a column named "Gene.Set.Term" to each data frame in the GSCA results.
#'
#' @rdname appendGSTerms
#'
#' @param object an object. When this function is implemented as the S4 method
#' of class 'GSCA', this argument is an object of class 'GSCA'.
#' @param keggGSCs a character vector of names of all KEGG gene set collections
#' @param goGSCs a character vector of names of all GO gene set collections
#' @param msigdbGSCs a character vector of names of all MSigDB gene set collections
#'
#' @return In the end, this function will return an updated object of class GSCA.
#'
#' @details This function makes the GSCA results more readable by appending a
#' column of terms for KEGG and GO gene sets. To do this, the user needs to
#' specify the names of the gene set collections based on GO, KEGG and MSigDB,
#' respectively.
#'
#' For each GO gene set, the GO id will be mapped to corresponding GO term by
#' the function mapIds of the package AnnotationDbi.
#'
#' For each KEGG gene set, the species code in the KEGG id will be trimmed off,
#' and then mapped to its corresponding annotation term using the package KEGGREST
#'
#' For each MSigDB gene set, the corresponding annotation terms based on the
#' built-in database in this package.
#'
#' @export
setMethod(
  "appendGSTerms", signature = "GSCA",
  function(object, keggGSCs=NULL, goGSCs=NULL, msigdbGSCs=NULL) {
    if(length(object@result)==0)
      stop("No results generated!\n")

    gsc.names<-names(object@listOfGeneSetCollections)

    if(!is.null(keggGSCs)) {
      paraCheck("Record", "keggGSCs", keggGSCs)
      if(!all(keggGSCs %in% gsc.names))
        stop("Wrong gene set collection names specified in 'keggGSCs'!\n")
    }
    if(!is.null(goGSCs)) {
      paraCheck("Record", "goGSCs", goGSCs)
      if(!all(goGSCs %in% gsc.names))
        stop("Wrong gene set collection names specified in 'goGSCs'!\n")
    }
    if(!is.null(msigdbGSCs)) {
      paraCheck("Record", "msigdbGSCs", msigdbGSCs)
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
appendGOTerm <- function(df) {
  require(GO.db)
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



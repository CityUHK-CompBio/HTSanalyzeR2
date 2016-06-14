#' Load the gene set
#'
#' tmp method for loading gene sets
#'
#' @return a list of genesets
#' @export
#'
loadGeneSet <- function () {
  data("gene_sets", envir = environment())
  ListGSC
}

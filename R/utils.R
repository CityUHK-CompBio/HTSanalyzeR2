geneMatrix <- function(rowNames, colNames) {
  mat <-
    matrix(
      NA,
      nrow =  length(rowNames),
      ncol = length(colNames),
      dimnames = list(rowNames, colNames)
    )
  mat
}

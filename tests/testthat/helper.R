## Stable fixtures shared by the modernization baseline tests.
make_small_gene_list <- function() {
  set.seed(1)
  genes <- paste0("g", seq_len(20))
  setNames(rnorm(20), genes)
}

make_small_gsca <- function() {
  gl <- make_small_gene_list()
  genes <- names(gl)
  GSCA(
    listOfGeneSetCollections = list(
      example = list(gs1 = genes[1:10], gs2 = genes[5:15])
    ),
    geneList = gl,
    hits = genes[1:5]
  )
}

make_small_nwa <- function() {
  gl <- make_small_gene_list()
  pvalues <- setNames(abs(gl) / 20, names(gl))
  phenotypes <- gl
  edges <- cbind(
    rep(names(gl)[1:10], each = 2),
    c(names(gl)[11:20], names(gl)[1:10])
  )
  interactions <- cbind(
    InteractorA = edges[, 1],
    InteractorB = edges[, 2],
    InteractionType = rep("physical", 20)
  )
  object <- NWA(pvalues = pvalues, phenotypes = phenotypes)
  interactome(object, interactionMatrix = interactions, genetic = FALSE)
}

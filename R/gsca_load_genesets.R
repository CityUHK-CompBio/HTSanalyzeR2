#' Create a list of gene sets for Homo Sapiens based on MSigDB collection terms
#'
#' This function creates gene set collections for Homo Sapiens based on MSigDB of version 6.1. Currently only
#' commonly used 'c2' and 'c5' gene sets could be gotten by this function.
#' It is collection-specific, and returns a list of gene sets collection with
#' the elements of the gene sets represented by Entrez Gene IDs.
#'
#' @param collection A single character value specifying a choice of collection.
#' It should be either 'c2'(curated gene sets) or 'c5'(GO gene sets). More details please refer to
#' \href{http://software.broadinstitute.org/gsea/msigdb}{MSigDB}.
#'
#' @return Return a list of gene sets of specific collection for Homo Sapiens in
#' \href{http://software.broadinstitute.org/gsea/msigdb}{MSigDB} of version 6.1.
#'
#' @seealso \code{\link[HTSanalyzeR2]{GOGeneSets}}, \code{\link[HTSanalyzeR2]{KeggGeneSets}}
#'
#' @examples
#' C2_MSig <- MSigDBGeneSets(collection = "c2")
#'
#' @export
MSigDBGeneSets <- function(collection = "c2") {
  paraCheck("LoadGeneSets", "collection", collection)

  gene.sets <- MSigDB[[paste(collection, ".db", sep = "")]]
  return(gene.sets)
}



#' Create a list of KEGG gene sets
#'
#' This function creates a list of gene sets based on KEGG pathways terms.
#' It is species-specific, and returns a list of gene sets, each of which
#' is a character vector of Entrez gene identifiers.
#'
#' @param species A single character value specifying a choice of species,
#' such as "Dm" ("Drosophila_melanogaster"), "Hs" ("Homo_sapiens"),
#' "Rn" ("Rattus_norvegicus") or "Mm" ("Mus_musculus").
#'
#' @return A list of gene sets, with names as KEGG pathway IDs. Each gene
#' set is a group of genes named by Entrez gene identifiers.
#'
#' @details This function needs Internet connection and relies on the
#' following packages: KEGGREST.
#'
#' @seealso \code{\link[HTSanalyzeR2]{GOGeneSets}}, \code{\link[HTSanalyzeR2]{MSigDBGeneSets}}
#'
#' @examples
#' library(KEGGREST)
#' library(org.Hs.eg.db)
#' HS_KEGG <- KeggGeneSets(species = "Hs")
#' @export
#' @importFrom KEGGREST keggLink keggConv
#' @importFrom stringr str_replace
KeggGeneSets <- function(species = "Hs") {
  paraCheck("LoadGeneSets", "species", species)
  species <- switch(
    species,
    "Ag" = "aga",
    "At" = "ath",
    "Bt" = "bta",
    "Ce" = "cel",
    "Cf" = "cfa",
    "Dm" = "dme",
    "Dr" = "dre",
    "EcK12" = "eco",
    "EcSakai" = "ecs",
    "Gg" = "gga",
    "Hs" = "hsa",
    "Mm" = "mmu",
    "Mmu" = "mcc",
    "Pf" = "pfa",
    "Pt" = "ptr",
    "Rn" = "rno",
    "Ss" = "ssc",
    "Xl" = "xla",
    species
  )

  err_func <- function(e) {
    stop(paste(
      'Please load library KEGGREST',
      ' and connect to the Internet',
      ' before running this function!',
      sep = ""
    ))
  }

  links <- tryCatch(
    KEGGREST::keggLink(species, "pathway"),
    error = err_func
  )
  conv <- tryCatch(
    KEGGREST::keggConv("ncbi-geneid", species),
    error = err_func
  )

  pw.names <- names(links)
  pw.sets <- conv[links]
  names(pw.sets) <- NULL

  pw.names <- stringr::str_replace(pw.names, "path:", "")
  pw.sets <- stringr::str_replace(pw.sets, "ncbi-geneid:", "")
  pw.kegg <- tapply(pw.sets, pw.names, c)
  pw.kegg <- as.list(pw.kegg)

  return(pw.kegg)
}


#' Create a list of gene sets based on GO terms
#'
#' This function creates a list of gene sets based on GO terms.
#' It is species-specific, and returns a list of gene sets, each
#' of which is a character vector of Entrez identifiers.
#'
#' @param species A single character value specifying a choice of species,
#' such as "Dm" ("Drosophila_melanogaster"), "Hs" ("Homo_sapiens"),
#' "Rn" ("Rattus_norvegicus") or "Mm" ("Mus_musculus").
#'
#' @param ontologies A single character value or a character vector
#' specifying an ontology or multiple ontologies. Valid format could be any
#' combination of "BP", "MF" and "CC".
#'
#' @return A list of gene sets, with names as GO IDs. Each gene set is
#' a character vector of Entrez identifiers.
#'
#' @details This function relies on the following packages:
#' AnnotationDbi, GO.db and the species db, such as org.Dm.eg.db.
#'
#' @seealso \code{\link[HTSanalyzeR2]{KeggGeneSets}}, \code{\link[HTSanalyzeR2]{MSigDBGeneSets}}
#'
#' @examples
#' library(GO.db)
#' library(org.Hs.eg.db)
#' HS_GO_CC <- GOGeneSets(species="Hs",ontologies=c("CC"))
#'
#' HS_GO <- GOGeneSets(species="Hs",ontologies=c("CC", "MF", "BP"))
#' @export
#' @importFrom AnnotationDbi GOID Ontology as.list

GOGeneSets <- function(species = "Hs", ontologies = c("MF")) {
  paraCheck("LoadGeneSets", "species", species)
  paraCheck("LoadGeneSets", "ontologies", ontologies)

  err_func <- function(library) {
    func <- function(e) {
      stop(paste('Please load library ', library,
        ' before running this function!', sep = ""
      ))
    }
  }

  annopc <- paste("org", species, "eg", "db", sep = ".")
  annodb <- paste("org", species, "egGO2EG", sep = ".")

  ThisGO <- tryCatch(
    get(annodb),
    error = err_func(annopc)
  )

  GOTERM <- tryCatch(
    get("GOTERM"),
    error = err_func('GO.db')
  )

  all.go.id <- GOID(GOTERM)
  names(all.go.id) <- NULL
  ## all GO types
  all.go.ontology <- Ontology(GOTERM)
  ## GO terms of species 'species'
  this.go.list <- as.list(ThisGO)
  ## find unique genes of each GO term
  this.go.list <- lapply(this.go.list, unique)

  this.go.id <- names(this.go.list)
  sapply(seq_along(this.go.id),
         function(s)
           names(this.go.list[[s]]) <<- NULL)

  ## tag all GO terms of types in 'ontologies'
  this.go.ontology.tag <- rep(FALSE, length(this.go.id))
  this.go.ontology.id <- unlist(sapply(seq_along(ontologies),
                           function(n) {
                             match.id <- match(this.go.id, all.go.id)
                             which(all.go.ontology[match.id] == ontologies[n])
                           }))

  this.go.ontology.tag[this.go.ontology.id] <- TRUE
  this.go.list <- this.go.list[which(this.go.ontology.tag)]

  return(this.go.list)
}


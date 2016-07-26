#This function creates gene set collections based on MSigDB.
#It is index-specific, and returns a GeneSetCollection objects with
#the elements of the gene sets represented by Entrez Gene IDs.
#
#' @export
#' @import GSEABase
MSigDBGeneSets<- function(index = 1) {
  gmt_file <- system.file("data", "msigdb.v5.1.entrez.gmt", package = "HTSanalyzeR2")

  msig <- GSEABase::getGmt(gmt_file, geneIdType=GSEABase::EntrezIdentifier())
  MSigDB <- GSEABase::geneIds(msig)
  gene.sets <- MSigDB[index]

  remove(msig, MSigDB)
  gc()

  return(gene.sets)
}


#This function creates gene set collections based on Kegg pathways terms.
#It is species-specific, and returns a GeneSetCollection objects with
#the elements of the gene sets represented by Entrez Gene IDs.
#
#' @export
#' @import KEGGREST stringr
KeggGeneSets <- function(species = "Dm") {
  # paraCheck("species", species)
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



##This function creates gene set collections based on GO terms. It is
##species-specific, and returns a GeneSetCollection objects with the
##elements of the gene sets represented by Entrez Gene IDs.
#' @export
GOGeneSets <- function(species = "Dm",
                       ontologies = "MF") {
  ##check arguments
  # paraCheck("species", species)
  # paraCheck("ontologies", ontologies)

  annopc <- paste("org", species, "eg", "db", sep = ".")
  annodb <- paste("org", species, "egGO2EG", sep = ".")
  ThisGO <- tryCatch(
    get(annodb),
    error = function(e) {
      stop(paste(
        'Please load library ',
        annopc,
        ' before running this function!',
        sep = ""
      ))
    }
  )

  GOTERM <- tryCatch(
    get("GOTERM"),
    error = function(e) {
      stop(paste(
        'Please load library ',
        'GO.db',
        ' before running this function!',
        sep = ""
      ))
    }
  )

  all.go.id <- GOID(GOTERM)
  names(all.go.id) <- NULL
  ##all GO types
  all.go.ontology <- Ontology(GOTERM)
  ##GO terms of species 'species'
  this.go.list <- as.list(ThisGO)
  ##find unique genes of each GO term
  this.go.list <- lapply(this.go.list, unique)

  this.go.id <- names(this.go.list)
  sapply(seq_along(this.go.id),
         function(s)
           names(this.go.list[[s]]) <<- NULL)

  ##tag all GO terms of types in 'ontologies'
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



#' Load the gene set
#'
#' temporary function for loading gene sets
#'
#' @return a list of genesets
#' @export
#'
loadGeneSet <- function () {
  data("gene_sets", envir = environment())
  ListGSC
}

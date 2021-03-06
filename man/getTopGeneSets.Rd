% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsca_summarize.R
\name{getTopGeneSets,GSCA-method}
\alias{getTopGeneSets,GSCA-method}
\alias{getTopGeneSets}
\title{Select top significant gene sets from results of GSCA object}
\usage{
\S4method{getTopGeneSets}{GSCA}(object, resultName, gscs, ntop = NULL, allSig = FALSE)
}
\arguments{
\item{object}{A GSCA object.}

\item{resultName}{A single character value: 'HyperGeo.results' or
'GSEA.results'.}

\item{gscs}{A character vector specifying the names of gene set collections
from which the top significant gene sets will be selected.}

\item{ntop}{A single integer or numeric value specifying to select how many
gene sets of top significance.}

\item{allSig}{A single logical value. If 'TRUE', all significant gene sets
(adjusted p-value < 'pValueCutoff' of slot 'para') will be selected regardless of 'ntop';
otherwise, only top 'ntop' gene sets will be selected.}
}
\value{
A named list of character vectors, each element contains the names of top
significant gene sets for each gene set collection.
}
\description{
This is a generic function.
This function selects top significant gene sets from results of an GSCA object for
user-specified gene collections. If 'ntop' is given, then top 'ntop'
significant gene sets in gene set collections 'gscs' will be selected
and their names will be returned. If 'allSig=TRUE', then all significant
(adjusted p-value < 'pValueCutoff' see help("analyze")) gene sets will
be selected and their names will be returned.
}
\examples{
## load a GSCA object(see the examples of analyze GSCA for details)
data(d7_gsca)

summarize(d7_gsca)
## print top significant gene sets in GO_MF
topGS_GO_MF <- getTopGeneSets(d7_gsca, "GSEA.results", gscs = "GO_MF", allSig=TRUE)

## print top significant gene sets in GO_MF and PW_KEGG
topGS <- getTopGeneSets(d7_gsca, "GSEA.results", gscs = c("GO_MF", "PW_KEGG"), allSig=TRUE)
}

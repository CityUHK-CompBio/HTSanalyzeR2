% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsca_class.R
\name{GSCA}
\alias{GSCA}
\alias{GSCA-class}
\title{An S4 class for Gene Set Collection Analyses on high-throughput data}
\usage{
GSCA(listOfGeneSetCollections, geneList, hits = character())
}
\arguments{
\item{listOfGeneSetCollections}{A list of gene set collections (a 'gene
set collection' is a list of gene sets).}

\item{geneList}{A numeric or integer vector of phenotypes named by gene
identifiers.}

\item{hits}{A character vector of the gene identifiers (used as hits in
the hypergeometric tests).It's needed if you want to do GSOA
(gene set overrepresentation analysis).}
}
\value{
This function will create a new object of 'GSCA' class.
}
\description{
This S4 class includes a series of methods to do gene set enrichment analysis
and hypergeometric test for high-throughput data.
}
\section{Slots}{

\describe{
\item{\code{para}}{A list of parameters for hypergeometric test and GSEA. These
parameters are pValueCutoff, pAdjustMethod, nPermutations, minGeneSetSize
and exponent.}

\item{\code{result}}{A list of results.}

\item{\code{summary}}{A list of summary information for listOfGeneSetCollections,
geneList, hits, para, and result.}

\item{\code{preprocessed}}{A single logical value specifying whether or not the
input data has been preprocessed.}
}}

\examples{
library(org.Hs.eg.db)
library(GO.db)
## load data for enrichment analyses
data(d7)
phenotype <- as.vector(d7$neg.lfc)
names(phenotype) <- d7$id

## select hits if you also want to do GSOA, otherwise ignore it
hits <- names(phenotype[which(abs(phenotype) > 2)])

## set up a list of gene set collections
GO_MF <- GOGeneSets(species="Hs", ontologies=c("MF"))
ListGSC <- list(GO_MF=GO_MF)

## Example1: create an object of class 'GSCA' with hits
gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = phenotype, hits = hits)
getSummary(gsca)

## Example2: create an object of class 'GSCA' without hits
gsca <- GSCA(listOfGeneSetCollections = ListGSC, geneList = phenotype)
getSummary(gsca)
}
\seealso{
\code{\link[HTSanalyzeR2]{preprocess}},
\code{\link[HTSanalyzeR2]{analyze}},
\code{\link[HTSanalyzeR2]{appendGSTerms}},
\code{\link[HTSanalyzeR2]{summarize}},
\code{\link[HTSanalyzeR2]{report}}
}

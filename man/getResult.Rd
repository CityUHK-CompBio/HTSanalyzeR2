% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessor.R
\name{getResult,GSCA-method}
\alias{getResult,GSCA-method}
\alias{getResult}
\alias{getResult,NWA-method}
\title{Accessors for the 'result' slot of a 'GSCA' or 'NWA' object.}
\usage{
\S4method{getResult}{GSCA}(object)

\S4method{getResult}{NWA}(object)
}
\arguments{
\item{object}{An object of 'GSCA' or 'NWA'.}
}
\value{
This function will return all the results as a list.
}
\description{
This 'result' slot stores all the results of an analyzed 'GSCA' or 'NWA' object.
}
\examples{
# ===========================================================
# GSCA class
data(d7_gsca)
rslt <- getResult(d7_gsca)

# ===========================================================
# NWA class
data(d7_nwa)
rslt <- getResult(d7_nwa)

}

# HTSanalyzeR2
Development version of HTSanalyzeR2

## Installation

```
# Please make sure you've installed these dependent packages before installing HTSanalyzeR2
BiocInstaller::biocLite(c("cellHTS2", "GSEABase", "BioNet", "KEGGREST", "data.table", "GO.db", "devtools"))

# Install HTSanalyzeR2
## Since now this package has been submitted into Bioconductor and is under testing in the development version of Bioconductor and R, so you need to install the branch of this package depending on the current version of R.
devtools::install_github("CityUHK-CompBio/HTSanalyzeR2", ref = "devel")
```

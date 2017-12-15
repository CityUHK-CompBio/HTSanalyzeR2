# HTSanalyzeR2
Development version of HTSanalyzeR2

## Installation

```
# Before installing HTSanalyzeR2, please install Bioconductor first if you've not.

source("https://bioconductor.org/biocLite.R")
biocLite()

# Then you need to install the following github installer package and dependent package.
BiocInstaller::biocLite(c("devtools", "GO.db"))

# Install HTSanalyzeR2.
devtools::install_github("CityUHK-CompBio/HTSanalyzeR2")
```

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to HTSanalyzeR2!\n"
                        , appendLF = FALSE)
}

.onLoad <- function(libname, pkgname) {
  if(getRversion() >= "3.5") {
    utils::globalVariables(names = c("idx", "tags"), add=FALSE, package = "HTSanalyzeR2")
    }
}

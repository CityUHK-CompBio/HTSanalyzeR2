.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome message (HTSanalyzeR2)\n"
                        , appendLF = FALSE)
}

# .onLoad <- function(libname, pkgname) {
#   if(getRversion() >= "3.4") {
#     utils::globalVariables(names = c("idx", "tags"), add=FALSE, package = "HTSanalyzeR2")
#     }
# }

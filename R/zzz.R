.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Welcome message (HTSanalyzeR2)\n"
                        , appendLF = FALSE)
}

# detach the rcpp library
.onUnload <- function (libpath) {
  library.dynam.unload("HTSanalyzeR2", libpath)
}

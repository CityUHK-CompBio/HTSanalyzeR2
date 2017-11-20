.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome message (HTSanalyzeR2)\n"
                        , appendLF = FALSE)
  # # CRAN Note avoidance
  # if(getRversion() >= "2.10.1")
  #   utils::suppressForeignCheck (
  #     # sample file names from taxstats
  #     c("idx", "tags")
  #   )
  # invisible()
}

.onLoad <- function(libname, pkgname) {
  if(getRversion() >= "2.10.1")  utils::globalVariables(c("idx", "tags"))
}

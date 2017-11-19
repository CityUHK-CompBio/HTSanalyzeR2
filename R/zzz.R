.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome message (HTSanalyzeR2)\n"
                        , appendLF = FALSE)
  # # CRAN Note avoidance
  # if(getRversion() >= "2.15.1")
  #   utils::globalVariables(
  #     # sample file names from taxstats
  #     c("GO.db", "idx", "tags"     )
  #   )
  # invisible()
}


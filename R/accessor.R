################################################
########  getResult
################################################
if (!isGeneric("getResult")) {
  setGeneric("getResult", function(object, ...)
    standardGeneric("getResult"), package = "HTSanalyzeR2")
}

#' Accessors for the 'result' slot of a 'GSCA' or 'NWA' object.
#'
#' This 'result' slot stores all the results of an analyzed 'GSCA' or 'NWA' object.
#' @include gsca_class.R
#' @rdname getResult
#' @aliases getResult
#' @param object An object of 'GSCA' or 'NWA'.
#' @export
#' @return This function will return all the results as a list.
#' @examples
#' # ===========================================================
#' # GSCA class
#' data(d7_gsca)
#' rslt <- getResult(d7_gsca)
setMethod("getResult", signature = "GSCA", function(object) object@result)
#' @rdname getResult
#' @include nwa_class.R
#' @export
#' @examples
#'
#' # ===========================================================
#' # NWA class
#' data(d7_nwa)
#' rslt <- getResult(d7_nwa)
#'
setMethod("getResult", signature = "NWA", function(object) object@result)


################################################
########  getSummary
################################################
if (!isGeneric("getSummary")) {
  setGeneric("getSummary", function(object, ...)
    standardGeneric("getSummary"), package = "HTSanalyzeR2")
}

#' Accessors for the 'summary' slot of a 'GSCA' or 'NWA' object.
#'
#' This 'summary' slot summarized an analyzed 'GSCA' or 'NWA' object.
#'
#' @rdname getSummary
#' @include gsca_class.R
#' @aliases getSummary
#' @param object An object of 'GSCA' or 'NWA'.
#' @return This function will return the summary information.
#' @export
#' @examples
#' # ===========================================================
#' # GSCA class
#' data(d7_gsca)
#' s1 <- getSummary(d7_gsca)
setMethod("getSummary", signature = "GSCA", function(object) object@summary)

#' @rdname getSummary
#' @include nwa_class.R
#' @export
#' @examples
#' # ===========================================================
#' # NWA class
#' data(d7_nwa)
#' s1 <- getSummary(d7_nwa)
#'
setMethod("getSummary", signature = "NWA", function(object) object@summary)


################################################
########  getPara
################################################
if (!isGeneric("getPara")) {
  setGeneric("getPara", function(object, ...)
    standardGeneric("getPara"), package = "HTSanalyzeR2")
}

#' Accessors for the 'para' slot of a 'GSCA' or 'fdr' slot of a 'NWA' object.
#'
#' This function get all the parameters used in 'GSCA' or 'NWA' analysis.
#'
#' @rdname getPara
#' @aliases getPara
#' @include gsca_class.R
#' @param object An object of 'GSCA' or 'NWA'.
#' @return This function will return all the parameters.
#' @export
#' @examples
#' # ===========================================================
#' # GSCA class
#' data(d7_gsca)
#' para1 <- getPara(d7_gsca)
setMethod("getPara", signature = "GSCA", function(object) object@para)

#' @rdname getPara
#' @include nwa_class.R
#' @export
#' @examples
#'
#' # ===========================================================
#' # NWA class
#' data(d7_nwa)
#' para1 <- getPara(d7_nwa)
#'
setMethod("getPara", signature = "NWA", function(object) object@fdr)


################################################
########  getInteractome
################################################
if (!isGeneric("getInteractome")) {
  setGeneric("getInteractome", function(object, ...)
    standardGeneric("getInteractome"), package = "HTSanalyzeR2")
}

#' Accessors for the 'interactome' slot of a 'NWA' object.
#'
#' This function get the interactome used in 'NWA' analysis.
#'
#' @rdname getInteractome
#' @aliases getInteractome
#' @include nwa_class.R
#' @param object An object of 'NWA'.
#' @return This function will return interactome used in 'NWA' analysis.
#' @export
#' @examples
#' data(d7_nwa)
#' s1 <- getInteractome(d7_nwa)
setMethod("getInteractome", signature = "NWA", function(object) object@interactome)

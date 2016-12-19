# definitions of class unions
setOldClass("igraph")
setClassUnion("igraph_or_logical", c("igraph", "logical"))
setClassUnion("numeric_or_integer", c("numeric", "integer"))
setClassUnion("numeric_or_matrix", c("numeric_or_integer", "matrix"))

if(!isGeneric("generateEnrichMap")){
  setGeneric("generateEnrichMap",function(object,...) standardGeneric("generateEnrichMap"), package="HTSanalyzeR2")
}

#' @export
#' @importFrom igraph V graph.adjacency
setMethod(
  "generateEnrichMap",
  "GSCA",
  function(object, resultName="GSEA.results", gscs, ntop=NULL, allSig=TRUE, gsNameType="id", displayEdgeLabel=TRUE) {

    ##get top gene sets
    topGS<-getTopGeneSets(object, resultName, gscs, ntop, allSig)

    if(length(unlist(topGS, recursive = FALSE)) == 0)
      stop("No significant gene sets found!\n")

    gsInUni <- list()
    tempList <- list()
    uniIDs <- names(object@geneList)
    sapply(1:length(topGS), function(i) {
      if (length(topGS[[i]]) > 0) {
        gsc.name <- names(topGS)[i]
        ##compute overlapped genes between gene sets and universe
        gsInUni[[i]] <<- list()
        gsInUni[[i]] <<- sapply(topGS[[i]], function(j) intersect(object@listOfGeneSetCollections[[gsc.name]][[j]], uniIDs), simplify = FALSE)
        names(gsInUni)[i] <<- gsc.name
        tempList[[i]] <<- data.frame(gsID = topGS[[i]], gscID = gsc.name, object@result[[resultName]][[gsc.name]][topGS[[i]], , drop =FALSE])
        names(tempList)[i] <<- gsc.name
      }
    })

    ##collapse to a data frame
    tempdf<-do.call("rbind", lapply(tempList, data.frame, stringsAsFactors = FALSE))
    if(gsNameType=="term" && !("Gene.Set.Term" %in% colnames(tempdf)))
      stop("No gene set terms found in results!\n Please use the method 'appendGSTerms' or add a column named 'Gene.Set.Term' to the results!\n")

    ##function to compute overlapped genes
    map.mat<-diag(1, nrow(tempdf), nrow(tempdf))
    map.diag<-sapply(1:nrow(tempdf),
                     function(i)
                       length(gsInUni[[as.character(tempdf[i,"gscID"])]][[as.character(tempdf[i,"gsID"])]]))
    rownames(map.mat)<-rownames(tempdf)
    colnames(map.mat)<-rownames(tempdf)

    if(nrow(tempdf)>=2) {
      sapply(1:(nrow(tempdf)-1), function(i) {
        map.mat[i, (i+1):nrow(tempdf)]<<-sapply((i+1):nrow(tempdf), function(j) {
          length(intersect(gsInUni[[as.character(tempdf[i,"gscID"])]][[as.character(tempdf[i,"gsID"])]],
                           gsInUni[[as.character(tempdf[j,"gscID"])]][[as.character(tempdf[j,"gsID"])]]))/
            length(union(gsInUni[[as.character(tempdf[i,"gscID"])]][[as.character(tempdf[i,"gsID"])]],
                         gsInUni[[as.character(tempdf[j,"gscID"])]][[as.character(tempdf[j,"gsID"])]]))
        })
        map.mat[(i+1):nrow(tempdf),i]<<-map.mat[i, (i+1):nrow(tempdf)]
      })
      ##generate igraph from adjacency matrix
      ### "Node name" controlled by the rownames of tempList
      g<-igraph::graph.adjacency(adjmatrix=map.mat, mode="undirected", weighted=TRUE, diag=TRUE)
      g<-igraph::simplify(g, remove.loops = TRUE)
    } else if(nrow(tempdf)==1) {
      diag(map.mat)<-0
      ##generate igraph from adjacency matrix
      ### "Node name" controlled by the rownames of tempList
      g<-igraph::graph.adjacency(adjmatrix=map.mat, mode="undirected", weighted=NULL, diag=FALSE)
    }

    ##add an user-defined attribute 'geneNum' to igraph
    V(g)$geneSetSize<-map.diag
    if(length(V(g))>=2) {
      ### "Node size" controlled by the "size of gene set"
      s.max <- 20
      s.min <- 6
      V(g)$size <- s.min + (s.max - s.min)*(map.diag-min(map.diag))/(max(map.diag)-min(map.diag))
    } else if(length(V(g))==1) {
      V(g)$size <- 4
    }

    V(g)$adjPvalue <- tempdf[,"Adjusted.Pvalue"]
    p.vec<-tempdf[,"Adjusted.Pvalue"]


    if(resultName=="GSEA.results") {
      negids<-which(tempdf[,"Observed.score"]<=0)
      posids<-which(tempdf[,"Observed.score"]>=0)

      V(g)$color<-""
      val.range <- range(p.vec[negids])
      V(g)$color[negids] <- ifelse((val.range[2] - val.range[1]) ==0, 0, (p.vec[negids]) * 50 / (val.range[2] - val.range[1]))
      val.range <- range(p.vec[posids])
      V(g)$color[posids] <- ifelse((val.range[2] - val.range[1]) ==0, 50, (p.vec[posids]) * 50 / (val.range[2] - val.range[1]) + 50)


    } else if(resultName=="HyperGeo.results") {
      val.range <- range(p.vec)
      V(g)$color <- ifelse((val.range[2] - val.range[1]) ==0, 0, (p.vec) * 50 / (val.range[2] - val.range[1]))
    }

    ##labels attributes
    graphLabelWrapper<-function(x, width=32) {paste(strwrap(x,width=width),collapse="\n")}
    if(gsNameType=="id") {
      V(g)$label<-as.character(tempdf[,"gsID"])
    } else if(gsNameType=="term") {
      templabels<-as.character(tempdf[,"Gene.Set.Term"])
      V(g)$label<-sapply(templabels, graphLabelWrapper)
    }

    ### "Edge thickness" controlled by the "size of overlapped genes" between two gene sets
    if(length(V(g))>=2) {
      edge.max.w<-14
      edge.min.w<-1
      E(g)$width<-round(edge.min.w+(edge.max.w-edge.min.w)*(E(g)$weight))
    }

    em_nodes <- igraph::as_data_frame(g, "vertices")
    em_links <- igraph::as_data_frame(g, "edge")
    idx <- 0:(nrow(em_nodes) - 1)
    names(idx) <- row.names(em_nodes)
    E(g)$source <- idx[em_links[, "from"]]
    E(g)$target <- idx[em_links[, "to"]]

    return(g)
  }
)



#' @export
plotD3Graph <- function(g, link_dist = 50, charge = -600, colorDomain = NULL, colorRange = NULL) {
  em_nodes <- igraph::as_data_frame(g, "vertices")
  em_links <- igraph::as_data_frame(g, "edge")

  # linkDistance = JS("function(d){return d.value * 10}")

  if(is.null(colorRange)) {
    colorRange = c('#67001f', '#b2182b', '#d6604d', '#f4a582', '#fddbc7', '#dddddd',
                   '#d1e5f0', '#92c5de', '#4393c3', '#2166ac', '#053061')
    colorDomain = seq(from = 0, to = 100, by = 10)
  }

  scale <- paste("d3.scale.linear().domain([",
                 paste(colorDomain, collapse = ","), "]).range(['",
                 paste(colorRange, collapse = "','"),"'])", sep="")

  # Create graph with legend and varying radius and a bounded box
  forceNetwork(Links = em_links, Nodes = em_nodes, Source = "source",
               Target = "target", Value = "width", NodeID = "label",
               colourScale = JS(scale),
               Nodesize = 'size', radiusCalculation = "d.nodesize",
               linkDistance = link_dist, charge = charge,
               Group = "color", opacity = 1, legend = FALSE, bounded = T,
               fontSize = 12, opacityNoHover = 0.7)
}



###  para checking
# ##check arguments
# if(missing(gscs))
#   stop("Please specify the name(s) of Gene Set Collections in 'gscs'! \n")
# paraCheck(name="gscs.names",para=gscs)
# ##resultName<-"GSEA.results"
# paraCheck(name="resultName",para=resultName)
# if(!(resultName %in% names(object@result)))
#   stop("No results found in object!\n")
# if(is.null(object@result[[resultName]]))
#   stop("Please run Hypergeometric or GSEA analysis before using this function!\n")
# gsc.names<-names(object@result[[resultName]])
# if(!all(gscs %in% gsc.names))
#   stop("Wrong Gene Set Collection name(s) in 'gscs'! \n")
# if(!is.null(ntop))
#   paraCheck(name="ntop",para=ntop)
# paraCheck(name="allSig",para=allSig)
# if((is.null(ntop) && !allSig)||(!is.null(ntop) && allSig))
#   stop("Either specify 'ntop' or set 'allSig' to be TRUE!\n")
# paraCheck(name="gsNameType", gsNameType)
# paraCheck(name="displayEdgeLabel", displayEdgeLabel)
# paraCheck("layout", layout)
# paraCheck(name="plot",para=plot)
#

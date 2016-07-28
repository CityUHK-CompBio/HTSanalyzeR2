if(!isGeneric("viewEnrichMap2"))
  setGeneric("viewEnrichMap2",function(object,...) standardGeneric("viewEnrichMap2"), package="HTSanalyzeR2")

#' @export
#' @import igraph
setMethod(
  "viewEnrichMap2",
  "GSCA",
  function(object, resultName="GSEA.results", gscs, ntop=NULL, allSig=TRUE, gsNameType="id", displayEdgeLabel=TRUE,
           layout="layout.fruchterman.reingold", plot=FALSE) {

    ##get top gene sets
    topGS<-getTopGeneSets(object, resultName, gscs, ntop, allSig)

    if(sum(unlist(lapply(topGS, length)))==0)
      stop("No significant gene sets found!\n")


    gsInUni<-list()
    uniIDs<-names(object@geneList)
    tempList<-list()
    sapply(1:length(topGS), function(i) {
      if(length(topGS[[i]])>0) {
        gsc.name<-names(topGS)[i]
        ##compute overlapped genes between gene sets and universe
        gsInUni[[i]]<<-list()
        gsInUni[[i]]<<-sapply(topGS[[i]], function(j) intersect(object@listOfGeneSetCollections[[gsc.name]][[j]], uniIDs),simplify=FALSE)
        names(gsInUni)[i]<<-gsc.name
        tempList[[i]]<<-data.frame(gsID=topGS[[i]], gscID=gsc.name, object@result[[resultName]][[gsc.name]][topGS[[i]],,drop=FALSE])
        names(tempList)[i]<<-gsc.name
      }
    })


    ##collapse to a data frame
    tempdf<-do.call("rbind", lapply(tempList, data.frame, stringsAsFactors = FALSE))
    if(gsNameType=="term" && !("Gene.Set.Term" %in% colnames(tempdf)))
      stop("No gene set terms found in results!\n Please use the method 'appendGSTerms' or add a column named 'Gene.Set.Term' to the results!\n")
    ##function to compute overlapped genes
    map.mat<-matrix(0,nrow(tempdf),nrow(tempdf))
    diag(map.mat)<-1
    map.diag<-sapply(1:nrow(tempdf), function(i) length(gsInUni[[as.character(tempdf[i,"gscID"])]][[as.character(tempdf[i,"gsID"])]]))
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
      rownames(map.mat)<-rownames(tempdf)
      colnames(map.mat)<-rownames(tempdf)
      ##generate igraph from adjacency matrix
      ### "Node name" controlled by the rownames of tempList
      g<-igraph::graph.adjacency(adjmatrix=map.mat, mode="undirected", weighted=TRUE, diag=TRUE)
      g<-igraph::simplify(g, remove.loops = TRUE)
    } else if(nrow(tempdf)==1) {
      diag(map.mat)<-0
      rownames(map.mat)<-rownames(tempdf)
      colnames(map.mat)<-rownames(tempdf)
      ##generate igraph from adjacency matrix
      ### "Node name" controlled by the rownames of tempList
      g<-igraph::graph.adjacency(adjmatrix=map.mat, mode="undirected", weighted=NULL, diag=FALSE)
    }


    ##add an user-defined attribute 'geneNum' to igraph
    V(g)$geneSetSize<-map.diag
    if(length(V(g))>=2) {
      ### "Node size" controlled by the "size of gene set"
      v.max.size<-18
      v.min.size<-4
      if(max(map.diag)!=min(map.diag))
        V(g)$size<-v.min.size+(v.max.size-v.min.size)*(map.diag-min(map.diag))/(max(map.diag)-min(map.diag))
      else
        V(g)$size<-6
    } else if(length(V(g))==1) {
      V(g)$size<-4
    }

    p.vec<-tempdf[,"Adjusted.Pvalue"]
    p.cutoff.vec<-c(0, 10^c(-3, -2.5), 0.01, 10^(-1.5), 0.05, 10^(-c(1.0, 0.5, 0)))

    if(resultName=="GSEA.results") {
      posids<-which(tempdf[,"Observed.score"]>=0)
      negids<-which(tempdf[,"Observed.score"]<=0)

      redCols<-colorRampPalette(colors = c("red", "white"))
      redVec<-redCols(length(p.cutoff.vec))

      blueCols<-colorRampPalette(colors = c("blue", "white"))
      blueVec<-blueCols(length(p.cutoff.vec))
      V(g)$color<-""
      if(length(posids)>0)
        V(g)$color[posids]<-redVec[as.integer(cut(x=p.vec[posids],breaks=c(-1,p.cutoff.vec), labels=1:(length(p.cutoff.vec))))]
      if(length(negids)>0)
        V(g)$color[negids]<-blueVec[as.integer(cut(x=p.vec[negids],breaks=c(-1,p.cutoff.vec), labels=1:(length(p.cutoff.vec))))]
    } else if(resultName=="HyperGeo.results") {
      redCols<-colorRampPalette(colors = c("red", "white"))
      redVec<-redCols(length(p.cutoff.vec))
      V(g)$color<-redVec[as.integer(cut(x=p.vec,breaks=c(-1,p.cutoff.vec), labels=1:(length(p.cutoff.vec))))]
    }



    ##labels attributes
    graphLabelWrapper<-function(x, width=32) {paste(strwrap(x,width=width),collapse="\n")}
    if(gsNameType=="id") {
      V(g)$label<-as.character(tempdf[,"gsID"])
    } else if(gsNameType=="term") {
      templabels<-as.character(tempdf[,"Gene.Set.Term"])
      V(g)$label<-sapply(templabels, graphLabelWrapper)
    }

    V(g)$label.dist<-0.4
    V(g)$label.cex<-0.75
    V(g)$label.font<-3
    V(g)$label.color<-"black"
    ### "Node color" controlled by the "adjusted pvalue"
    if(length(V(g))>=2)
      E(g)$color<-grey(0.7)
    if(displayEdgeLabel) {
      edgeWeights<-round(E(g)$weight*100)
      edgeWeights[edgeWeights==0]<-""
      E(g)$label<-edgeWeights
    }
    ### "Edge thickness" controlled by the "size of overlapped genes" between two gene sets
    edge.max.w<-14
    edge.min.w<-1
    if(length(V(g))>=2) {
      edgeWeightVec<-round(edge.min.w+(edge.max.w-edge.min.w)*(E(g)$weight))
      E(g)$width<-edgeWeightVec
    }


    if(plot) {
      ##plot graph
      plot(g,layout=eval(parse(text=layout)))
      ##title

      ##p-value color legend
      if(resultName=="GSEA.results") {
        title(main=paste("Enrichment Map of GSEA on \n\"", lapply(list(gscs), paste, collapse=",")[[1]], "\"",sep=""))
        colVec<-c(redVec[1:(length(redVec)-1)],rev(blueVec))
        p.cutoff.labels<-rep("",length(colVec))
        p.cutoff.labels[c(1,4,6,9,12,14,17)]<-c(0,0.01,0.05,1,0.05,0.01,0)
      } else if(resultName=="HyperGeo.results") {
        title(main=paste("Enrichment Map of Hypergeometric tests on \n\"", lapply(list(gscs), paste, collapse=",")[[1]],"\"", sep=""))
        colVec<-redVec
        p.cutoff.labels<-rep("",length(colVec))
        p.cutoff.labels[c(1,4,6,9)]<-c(0,0.01,0.05,1)
      }

      points(x = rep(-1.2, length(colVec)), y = seq(0.5, (0.5-(0.05*length(colVec))), length.out = length(colVec)), pch = 15, col = colVec)
      text( x = rep(-1.3, length(colVec)), y = seq(0.5, (0.5-(0.05*length(colVec))), length.out = length(colVec)), labels = p.cutoff.labels, cex = 0.8, adj=1)
      text(x = -1.25, y = 0.7, labels = "Adjusted\np-values", cex=0.8, adj= 0.5, font=2)

    }

    return(g)
  }
)


#' @export
#' @import networkD3
plotD3Graph <- function(g) {
  em_nodes <- igraph::as_data_frame(g, "vertices")
  em_links <- igraph::as_data_frame(g, "edge")

  idx <- 0:(nrow(em_nodes) - 1)
  names(idx) <- row.names(em_nodes)
  df_idx <- data.frame(idx[em_links[, "from"]], idx[em_links[, "to"]])
  names(df_idx) <- c("source", "target")
  em_links <- cbind(em_links, df_idx)

  # Create graph with legend and varying radius and a bounded box
  networkD3::forceNetwork(Links = em_links, Nodes = em_nodes, Source = "source",
               Target = "target", Value = "width", NodeID = "label",
               Nodesize = 'size', radiusCalculation = "Math.sqrt(d.nodesize)+6",
               linkDistance = JS("function(d){return d.value * 10}"),
               charge = -600,
               Group = "color", opacity = 1, legend = TRUE, bounded = FALSE,
               fontSize = 16, opacityNoHover = 0.7)
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

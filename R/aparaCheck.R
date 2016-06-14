##This is the central function for argument checking
paraCheck <- function(name, para) {
	if(name=="normCellHTSobject") {
		if(!is(para,"cellHTS"))
			stop("The argument 'cellHTSobject/normCellHTSobject' should be a cellHTS object")
		if(!state(para)["configured"]) 
			stop("The cellHTS object should be configured to perform the statistical tests")
		if(!state(para)["normalized"]) 
			warning("Your cellHTS object has not been normalized, this could impact the results of these tests",immediate.=TRUE)
		if(state(para)["scored"]) 
			stop("This cellHTS object has been scored; the statistical analysis should be performed on the normalized signal intensities",immediate.=TRUE)
		if(!state(para)["annotated"]) 
			stop("This cellHTS object has not been annotated",immediate.=TRUE)
	}
	if(name=="scoreSign") {
		if(!is.character(para) || length(para)!=1 || !(para %in% c("+","-")))
			stop("'scoreSign' should be either '+' or '-'!\n")
	}
	if(name=="scoreMethod") {
		if(!is.character(para) || length(para)!=1 || !(para %in% c("none","zscore","NPI")))
			stop("'scoreMethod' should be either 'none', 'zscore' or 'NPI'!\n")
	}
	if(name=="summarizeMethod") {
		if(!is.character(para) || length(para)!=1 || !(para %in% c("min","mean","median","max","rms","closestToZero","FurthestFromZero")))
			stop("'summarizeMethod' should be either 'min', 'mean', 'median', 'max', 'rms', 'closestToZero' or 'FurthestFromZero'!\n")
	}
	if(name=="annotationColumn") {
		if(!is.character(para) || length(para)!=1 )
			stop("'annotationColumn' should be a character value!\n")
	}
	if(name=="cutoffHitsEnrichment") {
		if(length(para) != 1 || (!is.integer(para) && !is.numeric(para))) 
			stop("'cutoffHitsEnrichment' should be a single integer! \n ")
	}
	if(name=="nwStatsControls") {
		if(!(is.character(para) && length(para)==1)) 
			stop("'controls/nwStatsControls' should be a character value!\n ")
	}
	if(name=="nwStatsAlternative") {
		if(!(is.character(para) && length(para)==1 && (para %in% c("two.sided","less","greater")) )) 
			stop("'alternative/nwStatsAlternative' should be one in 'two.sided','less' and 'greater'!\n ")
	}
	if(name=="nwStatsTests") {
		if(!is.character(para) || length(para)==0 || !(para %in% c("T-test","MannWhitney","RankProduct")))
			stop("'tests/nwStatsTests' should be one or more in 'T-test', 'MannWhitney' and 'RankProduct'!\n ")
	}
	if(name=="nwAnalysisOrder") {
		if(!(is.numeric(para) || is.integer(para)) || length(para)!=1 || para<0)
			stop("'order/nwAnalysisOrder' should be a positive numeric/integer value!\n")
	}
	if(name=="nwStatsColumns") {
		if(!is.character(para) || length(para)==0) 
			stop("'nwStatsColumns' should be a character vector with length > 0!\n ")
	}
 	if(name=="gscs") {
		if(!is.list(para))
			stop("'listOfGeneSetCollections' must be a list of gene set collections!\n")
		if(is.null(names(para)))
			stop("'listOfGeneSetCollections' must be a list of named gene set collections!\n")
		if(!all(unlist(lapply(para,is.list))))
			stop("Each gene set collection in 'listOfGeneSetCollections' must be a list of gene sets!\n")
		if(any(unlist(lapply(para,length))==0))
			stop("Empty gene set collection(s) in 'listOfGeneSetCollections'!\n")
	}
	if(name=="gsc") {
		if(!is.list(para))
			stop("A gene set collection must be a list of gene sets!\n")
		if(length(para)==0)
			stop("No gene set found in input gene set collection!\n")
	}
	if(name=="gs") {
		if(!is.character(para) || length(para)==0 || any(is.na(para)) || any(para==""))
			stop("'geneSet/GeneSet' should be a character vector with length > 0, without NA or empty names!\n")
	}
	if(name=="gs.single") {
		if(!is.character(para) || length(para)!=1 || is.na(para) || para=="")
			stop("'geneSet/GeneSet' should be single character!\n")		
	}
	if(name=="gscs.names") {
		if(!is.character(para) || length(para)==0) 
			stop("'gscs' should be a character! \n")
	}
	if(name=="gsc.name") {
		if(!is.character(para) || length(para)!=1) 
			stop("'gsc' should be a single character! \n")
	}
	if(name=="keggGSCs") {
		if(!is.character(para) || length(para)==0)
			stop("'keggGSCs' should be a character!\n")
	}
	if(name=="goGSCs") {
		if(!is.character(para) || length(para)==0)
			stop("'goGSCs' should be a character!\n")
	}
	if(name=="genelist") {
		if(!(is.numeric(para) || is.integer(para)) || length(para)==0 || is.null(names(para)))
			stop("'geneList' should be a named numeric or integer vector with length > 0!\n")
		#if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)=="")) 
	}
	if(name=="universe") {
		if(!is.character(para) || any(is.na(para)) || any(para=="")) 
			stop("'universe' should be a character vector without any NA or empty values!\n")
	}
	if(name=="genelist.general") {
		if(is.matrix(para)) {
			if(is.null(rownames(para)) || any(is.na(rownames(para))) || any(rownames(para)=="")) 
				stop("geneList should be a matrix with rownames!\n")		
		} else {
			if(!(is.numeric(para) || is.integer(para)) || length(para)==0)
				stop("'geneList' should be a numeric or integer vector with length > 0!\n")
			if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)=="")) 
				stop("'geneList' should be a named numeric or integer vector!\n")
		}
	}
	if(name=="hits") {
		if(!is.character(para) || length(para)==0)
			stop("'hits' should be a character vector with length > 0!\n")
	}
	if(name=="gsca.para") {
		if(missing(para))
			stop("'para' should be provided as a list!\n")
		##default parameters
		para.default<-list(pValueCutoff = 0.05,pAdjustMethod = "BH", nPermutations = 1000, minGeneSetSize = 15,exponent = 1)
		##check if input parameters are supported
		if(length(setdiff(names(para),names(para.default)))>0) 
			stop("Some parameters in 'para' are not supported. Check the right format of para!\n")
		##fill out default parameters for non-specified ones
		para.unspecified<-setdiff(names(para.default),names(para))
		if(length(para.unspecified)>0)
			for(i in 1:length(para.unspecified)) {
				para[[para.unspecified[i]]]<-para.default[[para.unspecified[i]]]
			}
		##check data type in para
		if(!(is.integer(para$pValueCutoff) || is.numeric(para$pValueCutoff)) || length(para$pValueCutoff)!=1 || para$pValueCutoff>1)
			stop("'pValueCutoff' should be an integer or numeric value <=1!\n")
		if(!is.character(para$pAdjustMethod) || length(para$pAdjustMethod)!=1 || 
				!(para$pAdjustMethod %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
			stop("'pAdjustMethod' should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
		if(!(is.integer(para$nPermutations) || is.numeric(para$nPermutations)) || length(para$nPermutations)!=1 || para$nPermutations<1)
			stop("'nPermutations' should be an integer >=1 !\n'")
		if(!(is.integer(para$minGeneSetSize) || is.numeric(para$minGeneSetSize)) || length(para$minGeneSetSize)!=1 || para$minGeneSetSize<1)
			stop("'minGeneSetSize' should be an integer >=1 !\n'")
		if(!(is.integer(para$exponent) || is.numeric(para$exponent)) || length(para$pValueCutoff)!=1 || para$exponent<1)
			stop("'exponent' should be an integer or numeric value >=1 !\n")
		return(para)
	}
	if(name=="pAdjustMethod") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
			stop("'pAdjustMethod' should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
	}
	if(name=="pValueCutoff") {
		if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para>1 || para<0)
			stop("'pValueCutoff' should be an integer or numeric value <=1 and >=0!\n")
	}
	if(name=="nPermutations") {
		if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
			stop("'nPermutations' should be an integer >=1 !\n'")
	}
	if(name=="minGeneSetSize") {
		if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
			stop("'minGeneSetSize' should be an integer >=1 !\n'")
		
	}
	if(name=="exponent") {
		if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
			stop("'exponent' should be an integer or numeric value >=1 !\n")
	}
	if(name=="species") {
		if(!is.character(para) || length(para) != 1) 
			stop("'species' should be a character!\n")
		if(!(para %in% c("Dm","Hs","Rn","Mm","Ce"))) {
			stop("'species' does not match any of the names recognized by this function, please provide one of the following character strings: 'Dm' ('Drosophila_melanogaster'), 'Hs' ('Homo_sapiens'), 'Rn' ('Rattus_norvegicus'), 'Mm' ('Mus_musculus'), 'Ce' ('Caenorhabditis_elegans')")
		}	
	}
	if(name=="mam.species") {
		if(!is.character(para) || length(para) != 1) 
			stop("'species' should be a character!\n")
		if(!(para %in% c("Hs","Rn","Mm"))) {
			stop("'species' does not match any of the names recognized by this function, please provide one of the following character strings: 'Hs' ('Homo_sapiens'), 'Rn' ('Rattus_norvegicus'), 'Mm' ('Mus_musculus')")
		}	
	}
	if(name=="initialIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt","wormbase")))
			stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt','wormbase'! \n")
	}
	if(name=="finalIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt","wormbase")))
			stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt','wormbase'!\n")
	}	
	if(name=="dro.initialIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt")))
			stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt'! \n")
	}
	if(name=="dro.finalIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt")))
			stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt'!\n")
	}
	if(name=="cel.initialIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","wormbase")))
			stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','wormbase'! \n")
	}
	if(name=="cel.finalIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","wormbase")))
			stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','wormbase'!\n")
	}	
	if(name=="mam.initialIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank")))
			stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank'! \n")
	}
	if(name=="mam.finalIDs") {
		if(!is.character(para) || length(para)!=1 || 
				!(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank")))
			stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank'! \n")
		
	}
	if(name=="keepMultipleMappings") {
		if(!is.logical(para) || length(para)!=1)
			stop("keepMultipleMappings should be a logical value!\n")
	}
	if(name=="duplicateRemoverMethod") {
		if(!is.character(para) || length(para) != 1 || !(para %in% c("max","min","average","fold.change.average")))
			stop("'duplicateRemoverMethod' should be only one of the following character strings: 'max', 'min', 'average', 'fc.avg(fold change average)'")
	}
	if(name=="orderAbsValue") {
		if(!is.logical(para) || length(para)!=1) 
			stop("'orderAbsValue' should be a logical value!\n")
	}
	if(name=="verbose") {
		if(!is.logical(para) || length(para)!=1)
			stop("'verbose' should be a logical value!\n")
	}
	if(name=="filepath") {
		if(!is.character(para) || length(para)!=1)
			stop("'filepath' should be a character!\n")
	}
	if(name=="dataDirectory") {
		if(!is.character(para) || length(para)!=1)
			stop("'dataDirectory' should be a character!\n")
	}
	if(name=="filename") {
		if(!is.character(para) || length(para)!=1)
			stop("'filename' should be a character!\n")
	}
	if(name=="ntop") {
		if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<=0)
			stop("'ntop' should be a integer or numeric value >0 ! \n")
	}
	if(name=="allSig") {
		if(!is.logical(para) || length(para)!=1)
			stop("'allSig' should be a logical value!\n")
	}
	if(name=="gsNameType") {
		if(!is.character(para) || length(para)!=1 || !(para%in% c("id","term","none")))
			stop("'gsNameType' should be a single character value: 'id', 'term' or 'none'!\n")		
	}
	if(name=="displayEdgeLabel") {
		if(!is.logical(para) || length(para)!=1)
			stop("'displayEdgeLabel' should be a logical value!\n")		
	}
	if(name=="layout") {
		if(!is.character(para) || length(para)!=1 || !(para%in%c("layout.fruchterman.reingold", "layout.spring", "layout.circle", "layout.kamada.kawai")))
			stop("'layout' must be one of 'layout.fruchterman.reingold', 'layout.spring', 'layout.circle' and 'layout.kamada.kawai'!\n")
	}
	if(name=="resultName") {
		if(!is.character(para) || length(para)!=1)
			stop("'resultName' should be a character!\n")
	}
	if(name=="pvalues") {
		if(!is.numeric(para) || length(para)==0 || is.null(names(para)))
			stop("'pvalues' should be a named numeric vector with length > 0!\n")
	}
	if(name=="phenotypes") {
		if(!(is.numeric(para) || is.integer(para)) || length(para)==0 || is.null(names(para)))
			stop("'phenotypes/phenotypeVector' should be a named numeric vector with length > 0!\n")
		#if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)==""))
	}
	if(name=="interactome") {
		if(!is(para,"graphNEL") && (numNodes(para)==0 || numEdges(para)==0))
			stop("Input 'interactome/graph' should be a graphNEL object with node and edge No > 0!\n")
	}
	if(name=="fdr") {
		if(!is.numeric(para) || para>1) 
			stop("'fdr' should be <=1 ! \n")
	}
	if(name=="interactionMatrix") {
		#If a data matrix is specified, check that it contains the right columns
		if(!is.matrix(para)) 
			stop("'interactionMatrix' should be a matrix")
		if(!all(c("InteractionType","InteractorA","InteractorB") %in% colnames(para))) 
			stop("'interactionMatrix' should contain the following named columns: 'InteractionType','InteractorA','InteractorB'")
	}
	if(name=="link") {
		if(!is.character(para) || length(para)!=1)
			stop("'link' should be a character!\n")
	}
	if(name=="reportDir") {
		if(!is.character(para) || length(para)!=1)
			stop("'reportDir' should be a character!\n")
	}
	if(name=="genetic") {
		if(!is.logical(para) || length(para)!=1)
			stop("'genetic' should be a logical value!\n")
	}
	if(name=="what.nwa") {
		if(!any(para %in% c("ALL","Pval","Phenotype","Interactome","Para","Result")) || !is.character(para))
			stop("Wrong what input! Please input \"ALL\"(all summary information), \"Pval\"(p-values), \"Phenotype\", \"Interactome\", \"Para\"(parameters for analysis) and \"Result\"\n")
		
	}
	if(name=="what.gsca") {
		if(!is.character(para) || !any(para %in% c("ALL","GSC","GeneList","Hits","Para","Result")))
			stop("Wrong what input! Please input \"ALL\"(all summary information), \"GSC\"(gene set collection), \"GeneList\", \"Hits\", \"Para\"(parameters for analysis) and \"Result\"\n")
		
	}
	if(name=="experimentName") {
		if(!is.character(para) || length(para)!=1)
			stop("'experimentName' should be a character!\n ")
	}
	if(name=="ontologies") {
		if(!is.character(para) || length(para)==0 || !all(para %in% c("BP","MF","CC"))) 
			stop("'ontologies' should be a character vector containing any non redundant combination of 'BP','MF','CC'!\n")
	}
	if(name=="output") {
		if(!is.character(para) || length(para)!=1 || !all(para %in% c("png","pdf")))
			stop("'output' should be 'png' or 'pdf'!\n")
	}
	if(name=="gseaScore.mode") {
		if(!is.character(para) || length(para)!=1 || !(para %in% c("graph", "score"))) {
			stop("'mode' should be 'graph' or 'score'!\n")
		}
	}
	if(name=="cutoffHitsEnrichment") {
		if(length(para) != 1 || (!is.integer(para) && !is.numeric(para))) 
			stop("'cutoffHitsEnrichment' should be a single integer!\n ")
	}
	if(name=="doGSOA" || name=="doGSEA") {
		if(length(para) != 1 || !is.logical(para))
			stop("'doGSOA' and 'doGSEA' should be a single logical value!\n ")
	}
}




















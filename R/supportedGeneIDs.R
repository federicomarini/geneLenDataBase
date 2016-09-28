supportedGeneIDs=function(){
	base=unfactor(GenomicFeatures:::supportedUCSCtables())
	#Having the db entries as row names gets confusing, so fix that
 	base$db=rownames(base)
	base=base[,c(ncol(base),1:(ncol(base)-1))]
	rownames(base)=1:nrow(base)
	#Old version of GenomicFeatures
	#txname2gene_mapinfo=GenomicFeatures:::.UCSC_TXNAME2GENEID_MAPINFO
	#Current version
	txname2gene_mapinfo=GenomicFeatures:::.UCSC_TXNAME2GENEID_MAPDEFS
	base$GeneID=""
	#Old version
	#tmp=unlist(sapply(txname2gene_mapinfo[base$db],function(u){u[4]}))
	tmp=unlist(lapply(txname2gene_mapinfo[base$db],function(u){u[[2]]}))
	base$GeneID[match(names(tmp),base$db)]=as.character(tmp)
	#Add in gene symbol
	base[nrow(base)+1,]=c("geneSymbol",'refGene','refFlat',"Gene Symbol","")
	base$AvailableGenomes=""
	repo=grep(".*\\..*\\.LENGTH",as.data.frame(data(package="geneLenDataBase")$results,stringsAsFactors=FALSE)$Item,ignore.case=TRUE,value=TRUE)
	repo=matrix(unlist(strsplit(repo,"\\.")),ncol=3,byrow=TRUE)
	valid_genomes=sapply(split(repo[,1],repo[,2]),paste,collapse=",")
	valid_genomes=valid_genomes[which(names(valid_genomes)%in%base$db)]
	base$AvailableGenomes[match(names(valid_genomes),base$db)]=as.character(valid_genomes)
  
  avail = base[base$db == "knownGene" | base$db == "vegaGene" | base$db == "geneSymbol" | base$db == "ensGene",]
  avail
}

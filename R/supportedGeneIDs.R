supportedGeneIDs=function(){
	base=unfactor(GenomicFeatures:::supportedUCSCtables())

	#Current version
	txname2gene_mapinfo=GenomicFeatures:::.UCSC_TXNAME2GENEID_MAPDEFS
	base$GeneID=""

	#Old version
	#tmp=unlist(lapply(txname2gene_mapinfo[base$db],function(u){u[[2]]}))
	#base$GeneID[match(names(tmp),base$db)]=as.character(tmp)

	#NEW Ant 28/09/16
	tmp=unlist(lapply(txname2gene_mapinfo[base$tablename],function(u){u[[2]]}))
	base$GeneID = sapply(base$tablename,function(u){ifelse(u %in% names(tmp),tmp[u],"")})	
	base$GeneID[match(names(tmp),base$tablename)]=as.character(tmp)

	#Add in gene symbol
	base[nrow(base)+1,]=c("geneSymbol",'refGene','refFlat',"Gene Symbol")
	base$AvailableGenomes=""
	repo=grep(".*\\..*\\.LENGTH",as.data.frame(data(package="geneLenDataBase")$results,stringsAsFactors=FALSE)$Item,ignore.case=TRUE,value=TRUE)
	repo=matrix(unlist(strsplit(repo,"\\.")),ncol=3,byrow=TRUE)
	valid_genomes=sapply(split(repo[,1],repo[,2]),paste,collapse=",")
	valid_genomes=valid_genomes[which(names(valid_genomes)%in%base$db)]
	base$AvailableGenomes[match(names(valid_genomes),base$db)]=as.character(valid_genomes)
  
  avail = base[base$db == "knownGene" | base$db == "vegaGene" | base$db == "geneSymbol" | base$db == "ensGene",]
  avail
}

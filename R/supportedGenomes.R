supportedGenomes=function(){
	base=unfactor(ucscGenomes())
	base$AvailableGeneIDs=""
	repo=grep(".*\\..*\\.LENGTH",as.data.frame(data(package="geneLenDataBase")$results,stringsAsFactors=FALSE)$Item,ignore.case=TRUE,value=TRUE)
	repo=matrix(unlist(strsplit(repo,"\\.")),ncol=3,byrow=TRUE)
	valid_ids=sapply(split(repo[,2],repo[,1]),paste,collapse=",")
	valid_ids=valid_ids[which(names(valid_ids)%in%base$db)]
	base$AvailableGeneIDs[match(names(valid_ids),base$db)]=as.character(valid_ids)
	avail = base[base$AvailableGeneIDs != "",]
	avail
}

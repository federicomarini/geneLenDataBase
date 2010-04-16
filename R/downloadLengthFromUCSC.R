downloadLengthFromUCSC=function(genome,id){
		table_name=NULL
		#We have to handle geneSymbol specially
		if(id=="geneSymbol"){
			table_name='refFlat'
			id='refGene'
		}
		#Check that genome/id are in UCSC listing
		if(!genome%in%supportedGenomes()[,'db']){
			stop("Length lookup is not supported for the genome ",genome,".  You will have to obtain bias data manually.\n  Run supportedGenomes() for a list of supported genomes.")
		}
		if(!id%in%supportedGeneIDs()[,'db']){
			stop("Length lookup is not supported for the gene identifier ",id,".  You will have to obtain bias data manually.\n  Run supportedGeneIDs() for a list of supported gene identifier formats.")
		}
		session=browserSession()
		genome(session)=genome
		#track=supportedUCSCtables()[id,"track"]
		available_tracks=trackNames(session)
		if(!id%in%as.character(available_tracks)){
			tmp=supportedGeneIDs()
			tmp=tmp[tmp$db%in%available_tracks,]
			#We were actually looking for the gene Symbol guy...
			if(!is.null(table_name)){id="geneSymbol"}
			message("The gene identifier ",id," is unavailable for the genome ",genome,".  Please obtain bias data manually or use on of the following gene identifiers.")
			print(tmp)
			stop("Unsupported gene identifier.")
		}
		query=ucscTableQuery(session,id,table=table_name,names=NULL)
		#This table contais all the pertenant data
		data=getTable(query)
		#We need to map transcripts back to some kind of gene ID
		mapdef=GenomicFeatures:::.UCSC_TXNAME2GENEID_MAPDEFS[[id]]
		if(!is.null(table_name)){
			#Gene symbol is special...
			gene_id_type="Gene Symbol"
			gene_id=unfactor(data$geneName)
			tx_name=unfactor(data$name)
		}else if(is.null(mapdef)){
			#OT OH!  Try and guess...
		        gene_id_type="unknown gene ids"
			gene_id=unfactor(data$name2)
			tx_name=unfactor(data$name)
		}else{
			if (length(mapdef$L2Rchain)!=1L)
				stop("cannot extract the transcript-to-gene mapping from ",
				"the UCSC database when id='", id, "', sorry!")
			L2Rlink1=mapdef$L2Rchain[[1L]]
			tablename2=L2Rlink1[["tablename"]]
			a=tryCatch({
				query2=ucscTableQuery(session,id,table=tablename2)
				ucsc_genetable=getTable(query2)
				tx_name=ucsc_genetable[[L2Rlink1[["Lcolname"]]]]
				gene_id=ucsc_genetable[[L2Rlink1[["Rcolname"]]]]
		        	if(is.null(tx_name) | is.null(gene_id)){
					stop("expected cols \"", L2Rlink1[["Lcolname"]], "\" or/and \"", L2Rlink1[["Rcolname"]], "\" not found in table ", tablename2)
				}
			        if(!is.character(tx_name)){tx_name <- as.character(tx_name)}
			        if(!is.character(gene_id)){gene_id <- as.character(gene_id)}
		        	gene_id_type <- mapdef$gene_id_type
				list(gene_id_type,gene_id,tx_name)
			},error=function(ex){
				gene_id_type="unknown gene ids"
				gene_id=unfactor(data$name2)
				tx_name=unfactor(data$name)
				list(gene_id_type,gene_id,tx_name)
			})
			gene_id_type=a[[1]]
			gene_id=a[[2]]
			tx_name=a[[3]]
			rm(a)
			message("Using gene identifier ",gene_id_type)
		}
		if(is.null(gene_id)){
			#There was no gene information, failure
			gene_id=rep(NA,length(tx_name))
		}
		data$GID=gene_id[match(unfactor(data$name),tx_name)]		
		length_data=data.frame(Gene=data$GID,Transcript=unfactor(data$name),Length=transcriptWidths(unfactor(data$exonStarts),unfactor(data$exonEnds)),stringsAsFactors=FALSE)
		#If it is possible, it would be nice to have a routine which added the downloaded data to the pool of locally available databases
		length_data
}

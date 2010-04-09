\name{bosTau3.refGene.LENGTH}
\title{Transcript length data for the organism bosTau}
\description{bosTau3.refGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the refGene table.

The data file was made by calling downloadLengthFromUCSC(bosTau3, refGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(bosTau3.refGene.LENGTH)
head(bosTau3.refGene.LENGTH)
}
\keyword{datasets}

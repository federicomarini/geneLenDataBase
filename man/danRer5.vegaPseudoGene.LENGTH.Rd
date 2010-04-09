\name{danRer5.vegaPseudoGene.LENGTH}
\title{Transcript length data for the organism danRer}
\description{danRer5.vegaPseudoGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the vegaPseudoGene table.

The data file was made by calling downloadLengthFromUCSC(danRer5, vegaPseudoGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(danRer5.vegaPseudoGene.LENGTH)
head(danRer5.vegaPseudoGene.LENGTH)
}
\keyword{datasets}

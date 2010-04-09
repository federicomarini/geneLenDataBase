\name{cavPor3.nscanGene.LENGTH}
\title{Transcript length data for the organism cavPor}
\description{cavPor3.nscanGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the nscanGene table.

The data file was made by calling downloadLengthFromUCSC(cavPor3, nscanGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(cavPor3.nscanGene.LENGTH)
head(cavPor3.nscanGene.LENGTH)
}
\keyword{datasets}

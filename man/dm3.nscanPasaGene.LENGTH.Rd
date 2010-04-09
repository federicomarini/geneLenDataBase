\name{dm3.nscanPasaGene.LENGTH}
\title{Transcript length data for the organism dm}
\description{dm3.nscanPasaGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the nscanPasaGene table.

The data file was made by calling downloadLengthFromUCSC(dm3, nscanPasaGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(dm3.nscanPasaGene.LENGTH)
head(dm3.nscanPasaGene.LENGTH)
}
\keyword{datasets}

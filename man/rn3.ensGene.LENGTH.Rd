\name{rn3.ensGene.LENGTH}
\docType{data}
\alias{rn3.ensGene.LENGTH}
\title{Transcript length data for the organism rn}
\description{rn3.ensGene.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the ensGene table.

The data file was made by calling downloadLengthFromUCSC(rn3, ensGene) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(rn3.ensGene.LENGTH)
head(rn3.ensGene.LENGTH)
}
\keyword{datasets}

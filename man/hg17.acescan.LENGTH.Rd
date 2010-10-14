\name{hg17.acescan.LENGTH}
\docType{data}
\alias{hg17.acescan.LENGTH}
\title{Transcript length data for the organism hg}
\description{hg17.acescan.LENGTH is an R object which maps transcripts to the length (in bp) of their mature mRNA transcripts.  Where available, it will also provide the mapping between a gene ID and its associated transcripts.  The data is obtained from the UCSC table browser (http://genome.ucsc.edu/cgi-bin/hgTables) using the acescan table.

The data file was made by calling downloadLengthFromUCSC(hg17, acescan) on the date on which the package was last updated.}
\seealso{
\code{\link{downloadLengthFromUCSC}}}
\examples{
data(hg17.acescan.LENGTH)
head(hg17.acescan.LENGTH)
}
\keyword{datasets}

\name{supportedGenomes}
\Rdversion{1.1}
\alias{supportedGenomes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Supported Genomes
}
\description{
Lists supported genomes
}
\usage{
supportedGenomes()
}
\details{
Uses the \code{ucscGenomes()} function from the \code{rtracklayer} package to obtain a list of genomes available from the UCSC genome browser.  The \code{db} column lists genomes as they are provided to the \code{genome} arguement of various functions.

The final column, headed \code{AvailableGeneIDs} lists the gene ID formats for which there is a local copy of the length information avaible for the genome listed in the \code{geneLenDataBase} package.
}
\value{
A data.frame containing supported genomes.
}
\author{
Matthew D. Young \email{myoung@wehi.edu.au}
}
\examples{
supportedGenomes()
}

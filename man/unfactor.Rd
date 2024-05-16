\name{unfactor}
\Rdversion{1.1}
\alias{unfactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Purge factors
}
\description{
Removes all factors from a variable in a sensible way.
}
\usage{
unfactor(var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
  The variable from which you want the factors removed.
}
}
\details{
As factors are their own type, to remove factors we must convert each level into another type.  This is currently done using "typeless" behaviour: a factor is converted to a numeric vector if this can be done without inducing NAs, otherwise it is coerced using as.character. Currently supported types are: \code{factor}, \code{data.frame} and \code{list}.
}
\value{
The variable with all factors converted to characters or numbers (see \code{details}).
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Matthew D. Young \email{myoung@wehi.edu.au}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
#A named factor
x <- factor(sample(1:6, 100, replace=TRUE))
names(x) <- paste("Roll.No", 1:100, sep='.')
x
unfactor(x)

#A data.frame
x <- data.frame(player <- c("Alice", "Bob", "Mary", "Fred"), score <- factor(c(9, 7, 8, 9)), stringsAsFactors=TRUE)
x$player
x$score
y <- unfactor(x)
y$player
y$score

}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

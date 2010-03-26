\name{ordered(ListClustering)}
\Rdversion{1.1}
\alias{ordered}
\alias{ordered,ListClustering}
\alias{ordered,ListClustering-method}
\title{
  ~ Function: ordered(ListClustering) ~
}
\description{
  Sort the \code{\linkS4class{Clustering}} of a
  \code{\linkS4class{ListClustering}} according to a quality criterion.
  (the function \code{ordered} for \code{Partition} is describe
  \link[=ordered,Partition-method]{here}).
}

\usage{
ordered(x,...)
}

\arguments{
  \item{x}{[ListClustering]: Object whose \code{Clustering} should be sort.}
  \item{\dots}{Note used, for S4 compatibility only.}
}

\details{
  Sort the \code{Clustering} of a \code{ListClustering} for each
  list (sort the 'c2' list, the 'c3' list,...) according to a quality criterion.
  The criterion used to sort is the one in the field \code{criterionActif}. The
  increasing or decreasing option is selected according to the
  criterion using the constant \code{\link{CRITERION_MIN_OR_MAX}}.
  \code{Clustering} that does not have a value for the selected
  criterion are put last.
}
\value{
  None (this function change internaly the field of an object, it does not return any values.)
}
\references{
}
\author{
}
\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
\keyword{methods}

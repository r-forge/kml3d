\name{ordered(Partition)}
\Rdversion{1.1}
\alias{ordered,Partition}
\alias{ordered,Partition-method}
%\alias{partition,missing,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~ Function: ordered for Partition ~ }
\description{
Given a \code{Partition}, \code{ordered} order its clusters letters
according to the effectif of each clusters. It is meanly a "label
changing". (The function \code{ordered} for \code{ListClustering}
is describe \link[=ordered,ListClustering-method]{here}).
}

\usage{
\S4method{ordered}{Partition}(x,keepEmpty=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{[Partition]}: Partition to be ordered.}
  \item{keepEmpty}{\code{[logical]}: If
    \code{TRUE}, some clusters may be empty ; is \code{FALSE}, empty
    clusters are removed.}
  \item{\dots}{Not used, for S4 compatibility only.}
}
\details{
Given a \code{Partition}, \code{ordered} order its clusters letters
according to the effectif of each clusters. So A will be the cluster
with be biggest frequency, then B, then C...
}
\value{
  An object of class \code{\linkS4class{Partition}}.
}

\seealso{\linkS4class{Partition}}


\examples{
### Small partition
smallPart <- partition(clusters=c("A","D","A","C","C","C"),nbClusters=4)


### Same partition but ordered, without the empty cluster
ordered(smallPart)

### Ordered with the empty cluster
ordered(smallPart,empty=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{cluster}  % Clustering
\keyword{methods}

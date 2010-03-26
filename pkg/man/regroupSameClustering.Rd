\name{regroupSameClustering}
\Rdversion{1.1}
\alias{regroupSameClustering}

\title{
 ~ Function: regroupSameClustering ~
}
\description{
Remove duplicate \code{\linkS4class{Clustering}} present in a \code{\linkS4class{ListClustering}}.
}
\usage{
regroupSameClustering(object)
}

\arguments{
  \item{object}{\code{[ListClustering]}: List of the \code{Clustering}
    that should be simplified.
  }
}

\details{
  A clusterizing algorithm can find a \code{Clustering} several time. It
  is store several time in object\code{ListClustering}, encombering the memory.
  This function remove the duplicate \code{\linkS4class{Clustering}}.
}

\value{
}
\references{
}
\author{
}
\note{
}


\seealso{
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\name{expandParLongData}
\alias{expandParLongData}
\alias{expandParLongData,ParLongData,Partition}
\alias{expandParLongData,ParLongData,Partition-method}
\alias{expandParLongData,ParLongData,numeric}
\alias{expandParLongData,ParLongData,numeric-method}

\title{
~ Function: expandParLongData ~
}
\description{
  Change the values of an object
  \code{\linkS4class{ParLongData}} to make them fit with a
  \code{\linkS4class{Partition}} or with a number of clusters.
}
\usage{
expandParLongData(xParLongData, y)
}

\arguments{
  \item{xParLongData}{\code{[ParLongData]}: The object to expand.}
  \item{y}{\code{[Partition]} or \code{[numeric]}: see detail.}
}
\details{
  \code{\linkS4class{ParLongData}} object can hold values that are easy
  to specify (like \code{col="clusters"} or \code{pch="symbol"}) but that can not
  be directly used by functions \link[=plot,LongData-method]{plot} and
  \link[=plot3d,LongData-method]{plot3d}. This function modify theses values to
  make them fit with \code{plot} and \code{plot3d} expectations.

  The field \code{col} and \code{pch} are the ones concern by this
  function.

  If \code{y} is a \code{Partition}, \code{col} and \code{pch} are
  extanded to fit with the number of individual. If \code{y} is a number of
  clusters, \code{col} and \code{pch} are extanded to fit with the
  number of clusters.

  If \code{col='clusters'}, a color is affected to each clusters. Then
  the field \code{col} receive a vector of color such that each
  individual (if \code{y} is a \code{Partition}) or each clusters (if
  \code{y} is a number of clusters) get its corresponding color.

  If \code{pch='letters'}, a letters is affected to each clusters. Then
  the field \code{pch} receive a vector of letters such that each
  individual (if \code{y} is a \code{Partition}) or each clusters (if
  \code{y} is a number of clusters) get its corresponding letters.

  Same think if \code{pch='symbols'}.
}
\value{
An object of class \code{\linkS4class{ParLongData}}
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

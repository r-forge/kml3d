\name{as.longData}
\alias{as.longData}
\alias{as.longData.data.frame}
\alias{as.longData.array}
\title{ ~ Function: as.longData ~}
\description{
\code{as.longData} turns a \code{data.frame} or a \code{array} into an
object of class \code{\linkS4class{LongData}}.
}
\usage{
as.longData(data,idAll,varNames,time,timeDataFrame,maxNA=length(time)-2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{[data.frame]} or \code{[matrix]}: structure containning the longitudinal data.}
  \item{idAll}{\code{[vector(character)]}: single identifier for each trajectory (ie each individual).
    The default \code{idAll} is the first column for a \code{data.frame},
    the vector \code{1:dim(data)[1]} for an \code{array}.}
  \item{varNames}{\code{[vector(numeric)]}: names of the variable.
    The default \code{varNames} is \code{c(V1,V2,V3,...]} for an \code{array}.
    the names of the list provide to \code{timeDataFrame} for a \code{data.frame}.}
  \item{time}{\code{[vector(numeric)]}: column number in which longitudinal data can be found.
    The default \code{timeCol} is \code{1:dim(data)[2]} for an \code{array},
    the length of the first argument of \code{timeDataFrame} for a \code{data.frame}.
    There is no default value for \code{data.frame}.}
  \item{timeDataFrame}{\code{[list(character=numeric,...)]}: (for \code{data.frame} only)
    For each variable, \code{timeDataFrame} provide the column numbers that contain the trajectories.
    See example for details.}
  \item{maxNA}{\code{[numeric]} or \code{[vector(numeric)]}:
    Define, for each variable, the maximum number that a trajectories shall contain.
    Individual whose trajectories contain more missing value than
    \code{maxNA} are exclude from \code{traj} and will no be use in
    the analysis. When \code{maxNA} is a single number, it is
    used for all the variables.}
}

\details{
  \code{as.longData} turns a \code{data.frame} or a \code{matrix} into an
  object of class \code{\linkS4class{LongData}}.
}
\value{
  An object of class \code{\linkS4class{LongData}}.
}

\seealso{\link{longData}, \linkS4class{LongData}}

\examples{
#################
### data.frame
df <- data.frame(id=11:12,Va11=c(1,4),Va15=c(2,5),Pe11=c(2,3),Pe15=c(3,2))
as.longData(df,timeDataFrame=list(Va=c(2,3),Pe=c(4,5)))

### Some time can be missing (like V12 in the following):
df <- data.frame(id=11:12,Va11=c(1,4),Pe11=c(2,5),Pe12=c(3,5),Va13=c(2,3),Pe13=c(1,3))
as.longData(df,time=c(11,12,13),timeDataFrame=list(Va=c(2,NA,5),Pe=c(3,4,6)))

#################
### array
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
id2=1:120
time=c(1,2,3,4,8,12,16,20)

tra2 <- array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(120*8*2,0,3),dim=c(120,8,2))
ld2 <- as.longData(tra2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{package}
\keyword{ts}       % Time Series
\keyword{methods}



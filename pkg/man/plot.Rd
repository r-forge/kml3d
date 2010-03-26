\name{plot,LongData}
\Rdversion{1.1}
\alias{plot}
\alias{plot,LongData-method}
\alias{plot,LongData,ANY-method}
\alias{plot,LongData,missing-method}

\title{ ~ function: plot for LongData ~ }
\description{
  \code{plot} the a \code{\linkS4class{LongData}} object.
}

\usage{
plot(x,y,...)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containing the trajectories to plot.}
  \item{y}{Not used, for S4 compatibility only.}
  \item{\dots}{Graphical parameters to be passed to methods. Specificly :

    \itemize{
      \item{paramTraj}{\code{[parLongData]}: Set the graphical parameters
	used to plot the \code{LongData}. See \code{\linkS4class{ParLongData}}
	for details.}
      \item{windowsCut}{\code{[couple(numeric)]}: Set the \code{mfrow}
	parameters. If \code{nbVar} is the number of variable in the
	\code{LongData}, then the default value is \code{c(nbCol,nbLine)} where
	\code{nbCol <- ceiling(sqrt(nbVar))} and \code{nbLine <- ceiling(nbVar/nbCol)}.}
      \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
	be time consumming. This parameters fixe the maximum numbre of
	trajectories (randomly chosen) that will be drawn.}
    }
  }
}
\details{
  \code{plot} each variable of a \code{\linkS4class{LongData}}.
}

\seealso{\code{\linkS4class{LongData}}}

\examples{
##################
### Real example on array

time=c(1,2,3,4,8,12,16,20)
id3=1:12000
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
ld3 <- as.longData(array(cbind(outer(id3,time,f),outer(id3,time,g))+rnorm(12000*8*2,0,3),dim=c(12000,8,2)))
par(ask=TRUE)

### Basic plot
plot(ld3)

### Change the windows orientation
plot(ld3,windowsCut=c(2,1))

### Change some parameters
plot(ld3,paramTraj=parTraj(col="red"))

### Plot only 1000 trajectories
plot(ld3,nbSample=1000)

### Plot all the trajectories
plot(ld3,nbSample=Inf)
par(ask=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{ts}
\keyword{aplot}
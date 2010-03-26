\name{plot3d,LongData}
\Rdversion{1.1}
\alias{plot3d}
\alias{plot3d,LongData-method}
\alias{plot3d,LongData,Partition-method}
\alias{plot3d,LongData,missing-method}

\title{ ~ function: plot for LongData ~ }
\description{
  Plot two variables of a \code{\linkS4class{LongData}} object in 3D.
}

\usage{
plot3d(x,y,...)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containing the trajectories to plot.}
  \item{y}{\code{[Parition]}: Object containing the trajectories to plot.}
  \item{\dots}{Graphical parameters to be passed to methods. Specificly :

    \itemize{
      \item{paramTraj}{\code{[parLongData]}: Set the graphical parameters
	used to plot the \code{LongData}. See \code{\linkS4class{ParLongData}}
	for details.}
      \item{var1}{\code{[numeric]} or \code{[character]}: either the
	number or the name of the first variable to display. 1 by default.}
      \item{var2}{\code{[numeric]} or \code{[character]}: either the
	number or the name of the second variable to display. 2 by default.}
      \item{nbSample}{\code{[numeric]}: Graphical display of huge sample can
	be time consumming. This parameters fixe the maximum numbre of
	trajectories (randomly chosen) that will be drawn.}
    }
  }
}
\details{
  Plot two variables of a \code{\linkS4class{LongData}} object in 3D. It
  use the \code{\link[rgl:rgl]{rgl}} library. The user can make the
  graphical representation turn using the mouse.
}

\seealso{\code{\linkS4class{LongData}}}

\examples{
##################
### Real example on array

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
h <- function(id,t)(id\%\%4-0.5)*(20-t)
ld4 <- as.longData(array(cbind(outer(id2,time,f),outer(id2,time,g),outer(id2,time,h))+rnorm(120*8*3,0,3),dim=c(120,8,3)))

### Basic plot
plot(ld4)

### plot3d, variable 1 and 2
plot3d(ld4)

### plot3d, variable 1 and 3
plot3d(ld4,var2=3)
plot3d(ld4,paramTraj=parTraj(col="red"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{ts}
\keyword{aplot}
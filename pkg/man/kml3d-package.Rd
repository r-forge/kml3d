\name{kml3d-package}
\alias{kml3d-package}
\docType{package}
\title{~ Overview: K-means for joint Longitudinal data ~}

\description{
  \code{KmL} is a new implematation of k-means for longitudinal data (or trajectories).
  Here is an overview of the package.
  % For the description of the algorithm, see \code{\link{kml}}.
}
\details{
  \tabular{ll}{
    Package: \tab kml3d\cr
    Type: \tab Package\cr
    Version: \tab 0.1\cr
    Date: \tab 2010-03-01\cr
    License: \tab GPL (>= 2)\cr
    Lazyload: \tab yes\cr
    Depends: \tab methods,rgl\cr
    URL:\tab http://www.r-project.org\cr
    URL:\tab http://christophe.genolini.free.fr/kml\cr
  }
}


\section{Overview}{
  To clusterize data, \code{KmL} go through three steps, each of which
  is associated to some functions:

  \enumerate{
    \item Data preparation

    \item Building "optimal" clusterization.

    \item Exporting results
  }
}

\section{How to get help?}{
  For those who are not familiar with S4 programming:
  In S4 programming, each function can be adapted for some specific
  arguments.
  \decription{
    \item{To get help on a function (for example \code{plot}), use:
      \code{?(plot)}.}
    \item{To get help on a function adapted to its argument (for example \code{plot}
      on argument \plot{LongData}), used: \code{?"plot,LongData"}.}
  }
}


\references{Article "KmL: K-means for Longitudinal Data", in
  Computational Statistics (accepted on 11-11-2009) \cr
  Web site: \url{http://christophe.genolini.free.fr/kml}
}
\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : \email{genolini@u-paris10.fr}
}

\section{English translation}{
  Raphael Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}

\seealso{
  Overview: \code{\link{kml3d-package}} \cr
}

\examples{
### 1. Data Preparation
#myCld <- as.clusterizLongData(generateArtificialLongData())

### 2. Building "optimal" clusterization (with only 3 redrawings)
#kml(myCld,nbRedrawing=3,print.cal=TRUE,print.traj=TRUE)

### 3. Exporting results
#try(choice(myCld))
}

\keyword{package}  % Package Summaries
\keyword{dplot}    % Computations Related to Plotting
\keyword{iplot}    % Interacting with Plots
\keyword{chron}    % Dates and Times
\keyword{spatial}  % Spatial Statistics ['spatial' package]
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{nonparametric} % Nonparametric Statistics [w/o 'smooth']
\keyword{ts}       % Time Series
\keyword{robust}   % Robust/Resistant Techniques
\keyword{models}   % Statistical Models




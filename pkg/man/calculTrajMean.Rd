\name{calculTrajMean}
\Rdversion{1.1}
\alias{calculTrajMean}
\title{
~ Function: calculTrajMean ~
}
\description{
Given some longitudinal data and a \code{\linkS4class{Partition}},
  \code{calculTrajMean} computes the mean trajectories of each cluster.
}
\usage{
calculTrajMean(traj, part)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{[array]: longitudinal data.}
  \item{part}{[vector(character)]: affectation of each individual.}
}
\details{
  EM algorithm (like k-means) alternates between two phases : Esperance and
  Maximisation. During Esperance, the mean of each cluster is
  evaluated. This is what \code{calculTrajMean} does.

  Note that \code{calculTrajMean} does not work with
  \code{\linkS4class{ClusterizLongData}} object but with a matrix.

  \code{affectIndiv} used with \code{\link{calculTrajMean}} simulates one k-means step.

%  This function is programmed in C, it is expected to be fast.
}

\value{
  An array of dimention K x T x V with K number of groups, T number of
  time mesurement and V number of variable.
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
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}






\examples{
#######################
### calculTrajMean

### Some LongitudinalData
#traj <- as.cld(gald())["traj"]

### A partition
#part <- partition(floor(runif(200,1,5)),4)

### Clusters center
#calculTrajMean(traj,part)
}


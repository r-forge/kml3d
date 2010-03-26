\name{clustering}
\Rdversion{1.1}
\alias{clustering}

\title{
~ Function: clustering ~
}
\description{
  \code{clustering} is the constructor of the class \code{\linkS4class{Clustering}}.
}
\usage{
clustering(xLongData, yPartition, convergenceTime = 0, multiplicity = 1,
criterionNames = c("calinski", "test"), criterionValues = numeric(),
algorithm = c(algo = "kmeans", startCond = "", imputation = "LI-Bissectrice"))
}
\arguments{
  \item{xLongData}{[LongData]: longitudinal data on which the
    clusterization has been run.}
  \item{yPartition}{[Partition]: object that will be turn into a \code{Clusterization}}
  \item{convergenceTime}{[numeric]: if the clusterization has been
    obtained through an algorithm, save the number of steps of this algorithm
    before convergence.}
  \item{multiplicity}{[numeric]: if the clusterization has been
    obtained several time, this variable saves the number of time that
    this particular \code{Clustering} is obtained.}
  \item{criterionNames}{[vector(character)]: criterions used to evaluate the
    quality of the partitioning.}
  \item{criterionValues}{[vector(numeric)]: values of the criterion used to
    evaluate the quality of the partitioning.}
  \item{algorithm}{[vector3(character)]: This variable hold
    informations about the algorithm used to get the
    \code{Clustering}. It has three value. The first (named
    \code{algo}) is the algorithm used ; the second
    (named \code{startCond}, see \link{toto}) is the starting condition used for the
    algorithm ; the thrid (named \code{imputation}, see \link{imputation}) is the name of the
    imputation method used in case of missing value. Example:
    \code{algorithm=
      c(algo="kmeans",startCond="randomK",imputation="LI-Bissectrice")}.}
}

\details{
  In \code{KmL}, strickly speaking, a \code{\link[longitudinalData:Partition-class]{Partition}} is just a sequence of
  letters (independent of any trajectories) ; a \code{Clustering} is
  a \code{Partition} associated with a set of trajectories
  with some additional
  information like qualities criterion, size of the clusters, algorithm
  used to get the \code{Clustering}...
}

\value{
  Object of class \code{\linkS4class{Clustering}}.
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
### Creation of a partition
#part <- partition(rep(c(1,2),4),2)

### Some trajectories
#traj1 <- gald(nbEachClusters=2)

### Tranformation of part into a Clusterization
#clusterization(part,traj1)
# Calinski criterion is arround 0.50...



### Some other trajectories
#traj2 <- gald(nbEachClusters=4, functionClusters=list(function(t){5-t},function(t){5+t}))

### Tranformation of part into a Clusterization
#clusterization(part,traj2)
# Calinski criterion is arround 0.15...

# part is probably a good partition for traj1, but not for traj2...
}

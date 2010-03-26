\name{Clustering-class}
\docType{class}
\alias{Clustering}
\alias{Clustering-class}
\alias{[,Clustering-method}
\alias{[<-,Clustering-method}
\alias{show,Clustering-method}

\title{~ Class: Clustering ~}
\description{
  An object of class \code{Clustering} is a partition of a population
  into subgroups.  The object also contains some information like the
  percentage of trajectories in each group or some qualities criterion.
}

\section{Objects from the Class}{
  Objects are mainly intend to be created by some clusterization methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...)
}

\section{Slots}{
  \describe{
    \item{\code{nbClusters}}{[numeric]: number of groups, between 1 and 26}
    \item{\code{clusters}}{[vector(factor)]: vector containing the affectation
      groups of each individual. Should be upper-case letters.
      Note that a group might be empty.}
    \item{\code{percentEachCluster}}{[vector(numeric)]: percentage of
      trajectories contained in each group.}
    \item{\code{convergenceTime}}{[numeric]:  if the
      \code{Clustering} is obtained by running an algorithm, this
      variable saves the number of iteration
      necessary to converge.}
    \item{\code{multiplicity}}{[numeric]: using an algorithm, this
      variable saves the number of time that this particular
      \code{Clustering} is obtained.}
    \item{\code{criterionNames}}{[vector(character)]: Names of the quality
      criterions used to evaluate the quality of the \code{Clustering}.}
    \item{\code{criterionValues}}{[vector(numeric)]: Value of the quality
      criterions used to evaluate the quality of the \code{Clustering}.}
    \item{\code{algorithm}}{[vector3(character)]: This variable hold
      informations about the algorithm used to get the
      \code{Clustering}. It has three value. The first (named
      \code{algo}) is the algorithm used ; the second
      (named \code{startCond}, see \link{toto}) is the starting condition used for the
      algorithm ; the thrid (named \code{imputation}, see \link{imputation}) is the name of the
      imputation method used in case of missing value. Example:
      \code{algorithm= c(algo="kmeans",startCond="randomAll",imputation="LI-Bissectrice")}.}
  }
}


\section{Construction}{
  Class \code{Clustering} objects are mainly constructed by some clustering methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...) Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{clustering}}.
}


\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field
      \code{xxx}. 'xxx' can also be the name of a criterion. Then its value
      is returned.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to
      \code{value}. Note that only fields
      'convergenceTime','mulitiplicity','criterionNames' and
      'criterionValues' can be changed (the other field are set at the
      contruction and can not be change latters.)}
  }
}


\references{Article submited\cr
}
\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}


\seealso{
  Overview: \code{\link{kml3d-package}} \cr
  Classes: \code{\linkS4class{LongData}} \cr
  Methods: \code{\link{partition}}\cr
}



\examples{
### Building Clustering
#part <- partition(rep(c("A","B","A"),time=10),nbClusters=2)
#traj1 <- array(rnorm(10*5*2),dim=c(10,5,2))
#myTraj <- longData(traj=traj1)

#clus <- clustering(myTraj,part)

### Get
#clus["clusters"]
#clus["percentEachClusters"]

### Set
#clus["multiplicity"]<-2
}


\keyword{classes}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series

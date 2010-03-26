\name{ListClustering-class}
\docType{class}
\alias{ListClustering}
\alias{ListClustering-class}
\alias{[,ListClustering-method}
\alias{[<-,ListClustering-method}
\alias{show,ListClustering-method}
\alias{listClustering}
\alias{listClustering-method}

\title{~ Class: ListClustering ~}
\description{
  An object of class \code{ListClustering} contain several liste of
  \code{Clustering} sorted by cluster numbers.
}

\section{Objects from the Class}{
  Objects are mainly design to store the numerous \code{Clustering} found
  by \code{\link{kml}}.
}

\section{Slots}{
  \describe{
    \item{\code{criterionActif}}{[character]: Store the criterion name
      that will be used by fonctions that need a single criterion
      (like \link[=plot,ListClustering-method]{plot} or \link{ordered}).}
    \item{\code{criterionPossibles}}{[vector(character)]: Names of all
      the criterion used for \code{Clustering}. Note that all of them
      might not be available for all the \code{Clustering}.}
    \item{\code{initializationMethod}}{[vector(chararcter)]: list all
      the initialization method that has allready been used to find some
      \code{Clustering}
      (usefull to not run several time a deterministic method).}
    \item{\code{sorted}}{\code{[logical]}: are the \code{Clustering}
      curently hold in the object sorted in decreasing (according to
      \code{criterionActif}) order ?}
    \item{\code{c2}}{[list(Clustering)]: list of
      \code{Clustering} with 2 clusters.}
    \item{\code{c3}}{[list(Clustering)]: list of
      \code{Clustering} with 3 clusters.}
    \item{\code{c26}}{[list(Clustering)]: list of
      \code{Clustering} with 26 clusters.}
  }
}


\section{Construction}{
  Class \code{ListClustering} objects are mainly constructed by
  \code{\link{kml}}.
  Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{listClustering}} that does
  create an empty object.
}


\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field
      \code{xxx}. If 'xxx' is 'criterionValues', only the list of the
      value corresponding to \code{criterionActif} is returned.}
    \item{\code{object['xxx',criterionName]}}{Get the criterion values
      of the criterion named \code{criterionName}.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to
      \code{value}. Note that fields 'c2','c3',...,'c26' can not be set
      that way ; use object['add']<-value instead.}
    \item{\code{object['add']<-Clustering}}{Add a Clustering object with
      i clusters to the field 'ci'. 'i' has not to be gived by the user,
      the Clustering is automaticly send in the correct list.}
    \item{\code{object['clear']<-ci}}{Clear the list 'ci'.}
    \item{\code{listClustering}}{Constructor. Build an empty object.}
    \item{\code{getCriterion}}{aze}
    \item{\code{sortClustering}}{aze}
    \item{\code{plotCriterion}}{aze}
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
#cat(con)
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

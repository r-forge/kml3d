\name{plot(ListCLustering)}
\Rdversion{1.1}
\alias{plot,ListClustering}
\alias{plot,ListClustering-method}
\alias{plot,ListClustering,ANY}
\alias{plot,ListClustering,ANY-method}
\title{
~ Function: plot for ListClustering ~
}
\description{
  This function graphically displays the quality criterion of all the
  \code{\linkS4class{Clustering}} of a \code{\linkS4class{ListClustering}} object.
}
\usage{
\S4method{plot}{ListClustering}(x,  y, criterion=x['criterionActif'],nbCriterion = 100, standardized = TRUE,...)
}

\arguments{
  \item{x}{\code{[ListClustering]}: object whose quality criterion should be
    displayed.}
  \item{y}{Not used, for S4 compatibility only.}
  \item{criterion}{\code{[character]} or \code{[vector(character)]}:
    name of the criterion(s) to plot (default= "calinksi").}
  \item{nbCriterion}{[numeric]: set the maximum number of criterion to
    display for a specific clusters number. When
    several criterions are plot, \code{nbCriterion} is set to 1 (default=100).}
  \item{standardized}{[logical]: some criterion sould be maximized, some
    other should be minimized ; some criterion take big values, some take
    small values. If \code{standardized} is \code{TRUE},
    all the criterion are scale to be in [0,1] and / or are
    inverted so the best possible value become 1 for all the criterion, and all of them should
    be maximized (default=TRUE).}
  \item{\dots}{Not used, for S4 compatibility only.}
}


\details{
  This function display graphically the quality criterion (probably to
  decide the best clusters' number). It can either display all the
  value find for a specific criterion ; this is useful to see the consistency of the result : is
  the best clusterization obtain several time or only one ? It can also
  display the values find by several criterions : this helps to compare
  the best cluster's number find by various ways.
}

\value{
  No value are return. A graph is printed.
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
#################
### Data generation
#dn <- as.cld(gald())

### Trying several clusters number and several starting condition
#kml(dn)

### Display the quality criterion, both way :
#par(mfrow=c(1,2))
#plotCriterion(dn)
#plotCriterion(dn,allCrit=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

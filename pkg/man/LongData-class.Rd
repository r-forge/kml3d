\name{LongData-class}
\docType{class}
\alias{LongData}
\alias{LongData-class}
\alias{[,LongData-method}
\alias{[<-,LongData-method}
\alias{show,LongData-method}
%\alias{addClusterization,LongData-method}
%\alias{addPartition,LongData-method}
%\alias{choice,LongData-method}
%\alias{getCalinskiCriterion,LongData-method}
%\alias{getClusterizList,LongData,missing,missing-method}
%\alias{getClusterizList,LongData,numeric,missing-method}
%\alias{getClusterizList,LongData,numeric,numeric-method}
%\alias{KmL,LongData-method}
%\alias{plot,LongData,ANY-method}
%\alias{print,LongData-method}

\title{~ Class: LongData ~}
\description{\code{LongData} is an objet containing the longitudinal
  data (the individual trajectories) and some associate value (like time, individual
  identifiant,...)
}

%\section{Objects from the Class}{
%  Object \code{LongData} can be created either directly by calling
%  the fonction \code{\link{longData}} or form an existing structure
%  (\code{data.frame} or \code{matrix}) by using
%  \code{\link{as.longData}}.
%}

\section{Slots}{
  \describe{
    \item{\code{idAll}}{\code{[vector(character)]}: Single identifier
      for each of the longData (each individual). Usefull for exporting clusters.}
    \item{\code{idFewNA}}{\code{[vector(character)]}: Restriction of
      \code{idAll} to the trajectories that does not have 'too many' missing
      value. See \code{maxNA} for details.}
    \item{\code{time}}{\code{[numeric]}: Time at which measures are made.}
    \item{\code{varNames}}{\code{[vector(character)]}: Names of the variable measured.}
    \item{\code{traj}}{\code{[array(numeric)]}: Contains
      the longitudianl data. Each horizontal plan (first dimension) corresponds to the trajectories of an
      individual. Vertical plans (second dimension) refer to the time at which measures
      are made. Transversal plans (the third dimension) are for variables.}
    \item{\code{dimTraj}}{\code{[vector3(numeric)]}: size of the array
      \code{traj} (ie \code{c(length(idFewNA),length(time),length(varNames))}).}
    \item{\code{maxNA}}{\code{[numeric]} or \code{[vector(numeric)]}:
      Individual whose trajectories contain more missing value than
      \code{maxNA} are exclude from \code{traj} and will no be use in
      the analysis. Their identifier is preserved in \code{idAll} but
      not in \code{idFewNA}. When \code{maxNA} is a single number, it is
      used for all the variables.}
    \item{\code{reverse}}{\code{[matrix(numeric)]}: contain the
      mean (first line) and the standard deviation (second line) used to
      normalize the data. Usefull to restaure the original data after a
      scaling operation.}
  }
}

\section{Construction}{
  Object \code{LongData} can be created by calling
  the fonction \code{\link{longData}} (build from
  scratch). \code{as.longData} can also
  turn a \code{\link[=as.longData.data.frame]{data.frame}} or a
  \code{\link[=as.longData.array]{array}} into a \code{LongData}.
}


\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field \code{xxx}.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to \code{value}.}
    \item{\code{show}}{Display a short version of the object.}
    \item{\code{print}}{Display the full version of the object.}
    \item{\code{\link[=plot]{plot}}}{Display the LongData, one
      graph for each variable.}
    \item{\code{\link[=plot3d]{plot3d}}}{Display two
      variables of the LongData in 3D.}
    \item{\code{\link{imputation}}}{Impute the
      trajectories.}
    \item{\code{\link{scale}}}{Scale the trajectories.
      Using the default value normalize each variable.}
%    \item{\code{\link{restaureRealData}}}{Reverse any scaling and
%      restaure the originale trajectories.}
  }
}

\references{Article submited\cr}


\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}

\seealso{
}


\examples{
### Building longData
traj1 <- array(c(1,2,3,1,4, 3,6,1,8,10, 1,2,1,3,2, 4,2,5,6,3, 4,3,4,4,4, 7,6,5,5,4),
            dim=c(3,5,2))

myTraj <- longData(
    traj=traj1,
    idAll=as.character(c(100,102,103)),
    time=c(1,2,4,8,15),
    varNames=c("P","A"),
    maxNA=3
    )

### Get
myTraj['varNames']

### Building from an array
#as.longData(tr1)

### Building from a data.frame
#df2 <- data.frame(id=1:3, w1=c(1,2,1), w2=c(3,1,2), s1=c(2,3,4), s2=c(3,3,4))
#as.longData(df2,time=1:2,timeDataFrame=list(W=c(2,3),S=c(4,5)))
#myTraj['varNames']
}

\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series

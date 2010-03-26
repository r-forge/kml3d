\name{calculCriterion}
\Rdversion{1.1}
\alias{calculCriterion}
\alias{calculCriterion,matrix,Partition-method}
\alias{calculCriterion,LongData,Partition-method}

\title{
 ~ Function: calculCriterion ~
}
\description{
Given a \code{\linkS4class{LongData}} and a
\code{\linkS4class{Partition}}, the fonction \code{calculCriterion}
 calculate some qualities criterion.
}
\usage{
calculCriterion(traj, part, imputationMethod = "LI-Bissectrice", criterionNames = c("calinski"))
}
\arguments{
  \item{traj}{\code{[array]}: object containning the trajectories.}
  \item{part}{\code{[vector(character)]}: partition of the trajectories.}
%  \item{xLongData}{\code{[LongData]}: object on which criterions are calculate.}
%  \item{yPartition}{\code{[Paritition]}: partition of the \code{LongData}}
  \item{imputationMethod}{\code{[character]}: if some value are missing in the
    \code{LongData}, it is necessary to impute them. The function
    \code{calculCriterion} call the function \code{\link{imputation}} using
    the method \code{imputationMethod}.}
  \item{criterionNames}{\code{[vector(character)]}: Names of the
    criterions that should be calculate.}
}

\details{
  Given a \code{\linkS4class{LongData}} and a
  \code{\linkS4class{Partition}}, the fonction \code{calculCriterion} calculate
  some qualities criterion.

%  If some individual have no clusters (ie if \code{Partition} has some
%  missing values), the corresponding trajectories are exclude from the
%  calculation.

%Note that if there is an empty cluster or an empty longData, most of
%the criterions are anavailable.

  Available criterions include : "Calinski & Harabatz"
}

\value{A list:
  \itemize{
    \item{calinski}{\code{[numeric]}: Calinski and Harabatz criterion c(k)=Trace(B)/Trace(W)*(n-k)/(k-1)}
  }
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}},
  \code{\link{imputation}}.
}

\examples{
##################
### Preparation of some artificial data
#par(ask=TRUE)
#traj <- gald()

### Correct partition
#part1 <- partition(rep(1:4,each=50),4)
#(cr1 <- criterion(traj,part1))
#plot(traj,part1,main=paste("Calinski =",formatC(cr1[["calinski"]])))


### Random partition
#part2 <- partition(floor(runif(200,1,5)),4)
#(cr2 <- criterion(traj,part2))
#plot(traj,part2,main=paste("Calinski =",formatC(cr2[["calinski"]])))

### Partition with 3 clusters instead of 4
#part3 <- partition(rep(c(1,2,3,3),each=50),3)
#(cr3 <- criterion(traj,part3))
#plot(traj,part3,main=paste("Calinski =",formatC(cr3[["calinski"]])))

### Comparisons of the Partition
#cr1["calinski"]
#cr2["calinski"]
#cr3["calinski"]
#par(ask=FALSE)
}

\keyword{package}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{methods}

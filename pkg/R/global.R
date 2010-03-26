#.onLoad <- function(libname, pkgname){
#    con <- 3
#}


setGeneric("longData",function(traj,idAll,time,varNames,maxNA=length(time)-2){standardGeneric("longData")})
#setGeneric("as.longData",function(data,...){standardGeneric("as.longData")})
setGeneric("imputation",function(object,method,imputRange,...){standardGeneric("imputation")})
setGeneric("restaureRealData",function(object){standardGeneric("restaureRealData")})
setGeneric("partition",function(clusters,nbClusters){standardGeneric("partition")})
setGeneric("expandParLongData",function(xParLongData,y){standardGeneric("expandParLongData")})
setGeneric("plot3d",function(x,y,...){standardGeneric("plot3d")})

setGeneric("clusterLongData",function(traj,idAll,time,varNames,maxNA=length(time)-2){standardGeneric("clusterLongData")})

#setGeneric("clusterizLongData",function(traj,id,time,varName="V",trajMinSize=1){standardGeneric("clusterizLongData")})
#setGeneric("as.clusterizLongData",function(data,...){standardGeneric("as.clusterizLongData")})
#setGeneric("as.longData",function(data,...){standardGeneric("as.longData")})
#setGeneric("plotAllCriterion",function(x,method="linearInterpolation"){standardGeneric("plotAllCriterion")})
#setGeneric("plotCriterion",function(x,criterion="calinski",nbCriterion=100,allCrit=FALSE){standardGeneric("plotCriterion")})
#setGeneric("plotAll",function(x,y,...){standardGeneric("plotAll")})
#setGeneric("plot",function(x,y,...){standardGeneric("plot")})
#
setGeneric("partitionInitialise",function(nbClusters,lengthPart,method="randomK",matrixDist){standardGeneric("partitionInitialise")})
#setGeneric("kml",function(Object,nbClusters=2:6,nbRedrawing=20,saveFreq=100,maxIt=200,trajMinSize=2,
#    print.cal=FALSE,print.traj=FALSE,imputationMethod="copyMean",
#    distance="euclidean",power=2,centerMethod=meanNA,startingCond="allMethods",distanceStartingCond="euclidean",...
#){standardGeneric("kml")})
#
#setGeneric("exportClusterization",function(object,y,typeGraph="bmp",
#    col="clusters",type="l",
#    col.mean="clusters",type.mean="b",main="",cex=1,
#    pch.mean="letters",pch.time=NA,...#lty=1,legends=TRUE,...
#)standardGeneric("exportClusterization"))
#setGeneric("choice",function(Object,typeGraph="bmp",...){standardGeneric("choice")})
#
#setGeneric("scale2",function(x,center,scale){standardGeneric("scale2")})

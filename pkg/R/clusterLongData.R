### clusterization est une partition associé a une longData, ou une clusterizLongData.
### cet objet ne devrait pouvoir exister que dans un cld


cat("####################################################################
######################### Class ClustLongData ######################
############################## Création ############################
####################################################################\n")

.ClusterLongData.validity <- function(object){
    validObject(as(object,"LongData"))
    validObject(as(object,"ListClustering"))
    return(TRUE)
}
cat("### Definition ###\n")
# id       : identifiant of the individual (or lines).
# time     : real time
# varNames : nom of the variable (single now, several in the futur)
# value    : array of the trajectories. Dim 1 is individual, 2 is time, 3 is variable(s)
setClass(
    Class="ClusterLongData",
    contains=c("LongData","ListClustering"),
    validity=.ClusterLongData.validity
)

setMethod("clusterLongData",signature=c("missing","missing","missing","missing","missing"),
    function(traj,idAll,time,varNames,maxNA=length(time)-2){new("ClusterLongData")}
)

.ClusterLongData.constructor <- function(traj,idAll,time,varNames,maxNA=length(time)-2){
#    if(class(traj)!="LongData"){
    traj <- longData(traj=traj,idAll=idAll,time=time,varNames=varNames,maxNA=maxNA)
 #   }else{}
    traj <- as(traj,"ClusterLongData")
    traj["criterionActif"] <- "calinski"
    return(traj)
}
setMethod("clusterLongData",signature=c("ANY","ANY","ANY","ANY","ANY"),.ClusterLongData.constructor)
cld <- clusterLongData

as.cld <- as.clusterLongData <- function(data,idAll,time,timeDataFrame,varNames,maxNA=length(time)-2){
    if(class(data)!="LongData"){
        data <- as.longData(data=data,idAll=idAll,time=time,timeDataFrame=timeDataFrame,varNames=varNames,maxNA=maxNA)
    }else{}
    return(as(data,"ClusterLongData"))
}

as.cld <- as.clusterLongData <- function(data,...){
    if(class(data)!="LongData"){
        data <- as.longData(data=data,...)
    }else{}
    cLongData <- as(data,"ClusterLongData")
    cLongData["criterionActif"] <- "calinski"
    return(cLongData)
}


.ClusterLongData.show <- function(object){
    show(as(object,"LongData"))
    cat("/n")
    show(as(object,"ListClustering"))
}

setMethod("show","ClusterLongData",
    definition=function(object){
        cat("   ~~~ Class: ClusterLongData ~~~")
        cat("\n      ~ Sub-Class: LongData ~ ")
        .LongData.show(as(object,"LongData"))
        cat("\n    ~ Sub-Class: ListClustering ~ ")
        .ListClustering.show(as(object,"ListClustering"))
    }
)

cat("### Getteur ###\n")
setMethod("[","ClusterLongData",
    function(x,i,j,drop){
        if(i%in%c("criterionActif","criterionValues",CLUSTER_NAMES,2:26)){
            x <- as(x,"ListClustering")
        }else{
            x <- as(x,"LongData")
        }
    return(x[i,j])
    }
)

cat("### Setteur ###\n")
### Héritage direct de ListClustering puisque set n'est pas défini pour LongData


cat("\n####################################################################
######################### Class ClustLongData ######################
############################### Autres #############################
####################################################################\n")

### On a un cld et un num, on plot le longData et la Partition qui va avec.
.plot.clusterLongData.num <- function(x,y,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200){
    if(length(y)==1){y<-c(y,1)}else{}
    yPartition <- x[y[1]][[y[2]]]
    plot(x=as(x,"LongData"),y=yPartition,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
    return(invisible())
}
#setMethod("plot",signature=c("ClusterLongData","ANY"),.clusterLongData.num.plot)


### Si y est manquant :
###  - soit il est calculable et on le calcul puis on appelle plot.ClusterLongData
###  - soit il n'est pas calculable et on appelle plot.LongData.num
.plot.clusterLongData.missingY <- function(x,y,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200){
    if(all(is.tna(x["criterionValues"]))){
        plot(x=as(x,"LongData"),paramTraj=paramTraj,paramWindows=paramWindows,nbSample=nbSample)
        return(invisible())
    }else{
        allCrit <- sapply(x["criterionValues"] , function(x){result <- x[[1]];names(result)<-NULL;result})
        y <- as.integer(substr(names(which.max(allCrit)),2,3))
    }
    .plot.clusterLongData.num(x,y,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
    return(invisible())
}
#setMethod("plot",signature=c("ClusterLongData","missing"),.clusterLongData.plot)


.plotAll <- function(x,y,paramTraj=parTraj(),paramMean=parMean(),paramWindows=windowsCut(x['nbVar']),nbSample=200,toPlot=c("both"),nbCriterion=100){
    switch(EXP=toPlot,
           "both"={
               listScreen <- split.screen(matrix(c(0,0.3,0.3,1,0,0,1,1),2))
               screen(listScreen[1])
               plot(as(cld3,"ListClustering"),paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample,nbCriterion=nbCriterion)
               screen(listScreen[2])
               if(missing(y)){
                   .plot.clusterLongData.missingY(cld3,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
               }else{
                   .plot.clusterLongData.num(cld3,y,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
               }
           },
           "traj"={
               if(missing(y)){
                   .plot.clusterLongData.missingY(cld3,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
               }else{
                   .plot.clusterLongData.num(cld3,y,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
               }
           },
           "criterion"={
               plot(as(cld3,"listClustering"),y=y,paramTraj=paramTraj,paramMean=paramMean,paramWindows=paramWindows,nbSample=nbSample)
           }
    )
    close.screen(listScreen)
    return(invisible())
}
setMethod("plot",signature=c("ClusterLongData","missing"),.plotAll)
setMethod("plot",signature=c("ClusterLongData","ANY"),.plotAll)

cat("\n--------------------------------------------------------------------
------------------------- Class ClustLongData ----------------------
------------------------------- Autres -----------------------------
--------------------------------------------------------------------\n")

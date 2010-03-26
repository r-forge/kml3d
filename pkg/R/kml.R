.partitionInitialise <- function(nbClusters,lengthPart,method="randomK",matrixDist){
    switch(method,
        "randomK"={
            part <- rep(NA,lengthPart)
            seeds <- sample(lengthPart,nbClusters)
            part[seeds] <- 1:nbClusters
        },
        "randomAll"={
            part <- floor(runif(lengthPart,1,nbClusters+1))       # Chaque individu recoit une affectation
            seeds <- sample(lengthPart,nbClusters)                # Puis on choisit k individus pour éviter les clusters vides.
            part[seeds] <- 1:nbClusters
        },
        "maxDist"={
            part <- rep(NA,lengthPart)
            seeds <- which(matrixDist==max(matrixDist,na.rm=TRUE),arr.ind=TRUE)[1,]
            while(length(seeds)<nbClusters){
                matrixDist[,seeds] <- 0
	        seeds <- c(seeds,
                    which.max(
                        apply(matrixDist[seeds,],2,min)
                    )[1]
                )
            }
            part[seeds] <- 1:nbClusters
        },
        stop("[PartitionInitialize] invalid initialization methods")
    )
    return(partition(clusters=part,nbClusters=nbClusters))
}
setMethod("partitionInitialise",signature=c("numeric","numeric"),.partitionInitialise)


dist3d <- function(x,y){
    return(dist(rbind(c(x),c(y))))
#    return(sqrt(sum((c(x)-c(y))^2)))
}

### ATTENTION : calculCenterGeneralized travaillait avec traj + Partition ;
### Maintenant, il travaille avec traj + part.
calculCenterGeneralized <- calculTrajMean <- function(traj,part,centerMethod=meanNA){
    trajMean <- apply(traj, c(2,3), tapply, part, centerMethod)
    return(trajMean)
}

### CalculMean : même chose mais en C.


### On suppose que si un centre est NA, il est en dernière ligne de clustersCenter
affectIndivGeneralized <- function(traj,clustersCenter,distance=dist3d){
#    if (distance %in% METHODS){distanceFun <- ,method=distance))}}else{distanceFun <- distance}
    nbId <- nrow(traj)
    clusterAffectation <- rep(1,nbId)
    distActuel <- apply(traj,1,function(x){distance(x,clustersCenter[1,,])})
    ##   print(distActuel)
    for(iNbClusters in 2:nrow(clustersCenter)){
        distToMean <- apply(traj,1,function(x){distance(x,clustersCenter[iNbClusters,,])})
 #       print(distToMean)
        cond <- distToMean<distActuel
        cond[is.na(cond)] <- FALSE # Car si cond==NA, c'est que distToMean==NA et donc on ne change pas l'affectation.
        clusterAffectation <- ifelse(cond,rep(iNbClusters,nbId),clusterAffectation)
        distActuel <- ifelse(distToMean<distActuel,distToMean,distActuel)
    }
    return(partition(clusterAffectation,nrow(clustersCenter)))
}

### affectIndiv : même chose, mais en C
trajKmlSlow <- function(traj,clusterAffectation,maxIt=200,print.traj=TRUE,
                        distance=dist3d,centerMethod=meanNA
                        ){
#    if (distance %in% METHODS){distanceFun <- function(x,y){return(dist(t(cbind(x,y)),method=distance))}}else{distanceFun <- distance}
 #   print(distanceFun)
    exClusterAffectation <- partition()
    if(print.traj){
        plot(as.longData(traj),clusterAffectation)
    }else{}
    for(iterations in 1:maxIt){
        clustersCenter <- calculCenterGeneralized(traj=traj,part=clusterAffectation['clusters'],centerMethod=centerMethod)
        clusterAffectation <- affectIndivGeneralized(traj=traj,clustersCenter=clustersCenter,distance=distance)
        if(identical(clusterAffectation,exClusterAffectation)){
            return(list(clusterAffectation=clusterAffectation,convergenceTime=iterations))
        }else{
            exClusterAffectation <- clusterAffectation
        }
        if(print.traj){
            plot(as.longData(traj),clusterAffectation)
        }else{}
    }
    return(list=c(clusterAffectation=clusterAffectation,convergenceTime=Inf))
}
ppp <- partitionInitialise(4,193)
trajKmlSlow(ld3['traj'],ppp)

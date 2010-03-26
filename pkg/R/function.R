cat("\n####################################################################
############################## Function ############################
####################################################################")

### Functions accepting NA
meanNA <- function(x){mean(x,na.rm=TRUE)}
medianNA <- function(x){median(x,na.rm=TRUE)}

sdNA   <- function(x){sd(c(x),na.rm=TRUE)}
rangeNA   <- function(x){range(x,na.rm=TRUE)}

which.minNA <- function(x){
  y <- which.min(x)
  if(length(y)==0){y<-NA}
  return(y)
}

### TRUE for Truly NA : false for NaN
is.tna <- function(x){return(is.na(x)&!is.nan(x))}


### Printing long line shortening them
catShort <- function(x){
    if(length(x)<=10){
        cat(x)
    }else{
        cat(x[1:10],"...")
    }
}

NAtrunc <- function(x) x[1:max(which(!is.na(x)))]

MAX_CLUSTERS <- 26
CLUSTER_NAMES <- paste("c",2:MAX_CLUSTERS,sep="")
CRITERION_MIN_OR_MAX<- c(calinski=1,test=1,test2=-1)
# METHODS <- c("manhattan", "euclidean", "minkowski", "maximum", "canberra", "binary")


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++ Fin Function ++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")



if(getwd()=="C:/Documents and Settings/Christophe/Mes documents"){
    setwd("C:/Documents and Settings/Christophe/Mes documents/articles/kml3D/package/tests")
}else{}
if(getwd()=="C:/Documents and Settings/Administrator/My Documents"){
    setwd("C:/Documents and Settings/Administrator/Desktop/Mes Documents/articles/kml3D/package/tests")
}else{}

library(codetools)
library(rgl)
library(clv)

cleanProg <- function(realResult,theoResult="",result=TRUE,tolerance=0){
  functionNames <- strsplit(deparse(substitute(realResult)),"\\(")[[1]][1]
  if(identical(theoResult,"")==FALSE){
    if( isTRUE(all.equal( realResult , theoResult ))!=result ){
      cat("WARNING(PreTest2) in    ",functionNames,":",deparse(substitute(realResult)), " == ",theoResult," is not ",result,"\a\n\a")
    }
  }else{}
  if(length(findGlobals(get(functionNames),FALSE)$variables)  > tolerance){
    cat("WARNIGS(detectGlobal) in ",functionNames,": These are the globals:",findGlobals(get(functionNames),FALSE)$variables,"\a\n")
  }else{}
}

source("../R/global.r")
source("testFunction.r")
source("testPartition.r")
source("testParLongData.r")
source("testParWindows.r")
#source("testLongData.data.r")
source("testLongData.r")
source("testClustering.data.r")
#source("testClustering.r")
source("testListClustering.r")
source("testClusterLongData.data.r")

#--------------------------------------------
#' @title Utility to initialize a gRain CPT.
#'
#' @description A function to create and initialize a gRain CPT. Handy when 
#' building a large network by hand in R.
#' 
#' @details This utility will format a vpar.list and input levels for the node (child.node.levels)
#' into a gRain CPT with the cptable function.  For the definition of vpar.list cf. help page 
#' of cptable (gRain).
#'
#' @param vpar.list a list of the child node and its parents.
#' @param child.node.levels input levels for the node.
#' @param printQ switch to print the CPT created.
#' @return A \code{cptable} object (a list).
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
initialize.gRainCPT<-function(vpar.list, child.node.levels=NULL, printQ=FALSE) {
  
  #Through error if no states are given for child node:
  if(is.null(child.node.levels)){
    print("Specify child node levels!")
    return(0)
  }
  
  #Check to see if this is going to be a prior node
  if(length(vpar.list)==1){
    prior.nodeQ<-TRUE
  } else {
    prior.nodeQ<-FALSE
  }
  
  #Set up node names to send to cptable in gRain:    
  if(prior.nodeQ==FALSE){
    
    pa.node.names<-sapply(2:length(vpar.list),function(x){vpar.list[[x]]$vpa[1]})
    
    ch.node.name<-vpar.list[[1]]
    node.names<-c(ch.node.name, pa.node.names)
  } else {
    ch.node.name<-vpar.list[[1]]
    node.names<-c(ch.node.name)
  }
  
  #Compute the number if 1s required to fill the CPT:
  num.probs<-length(child.node.levels)
  if(prior.nodeQ==FALSE){
    for(i in 2:length(vpar.list)){
      num.probs<- num.probs * length(vpar.list[[i]]$levels)
    }
  }
  
  #Get the CPT:
  node.cpt<-cptable(node.names, values=rep(1,num.probs), levels=child.node.levels, normalize=F)
  
  #Print the CPT if desired:
  if(printQ==TRUE){
    all.node.list<-rep(list(NULL), length(vpar.list))
    
    all.node.list[[1]]<-node.cpt
    if(prior.nodeQ==FALSE){
      for(i in 2:length(vpar.list)){
        all.node.list[[i]]<-vpar.list[[i]]
        #Below for fooling compileCPT into thinking the the parent nodes have no parents. 
        all.node.list[[i]]$vpa<-all.node.list[[i]]$vpa[1]   
        all.node.list[[i]]$values<-rep(1,length(all.node.list[[i]]$levels  ) )
      }
    }
    print("-----------------")
    print(paste("Initialized node:  ",vpar.list[[1]]))
    print("-----------------")
    print(gRain2HuginCPT(compileCPT( all.node.list)[[1]], prior.nodeQ))
  }
  
  return(node.cpt)  
}
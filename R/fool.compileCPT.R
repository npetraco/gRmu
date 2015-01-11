#--------------------------------------------
#' @title Utility to print out a gRain CPT from it's network
#'
#' @description A handy function to help print out gRain CPTs
#' 
#' @details This utility will compile a CPT into the necessary
#' parray (gRbase) form needed for gRain2HuginCPT to print it out. The trick is that the 
#' parent nodes and node names need to be included.
#' EG: fool.compileCPT(list(bg.seeds, garnet, loc), c("bg.seeds","garnet", "loc"))[[1]]
#' spits out the parray for bg.seeds. bg.seeds has parent nodes garnet and loc.
#'
#' @param vpar.list a list of the child node and its parents.
#' @param node.names string versions of the names for the nodes listed in vpar.list.
#' @return a \code{parray} 
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
fool.compileCPT<-function(vpar.list, node.names){
  
  all.node.list<-rep(list(NULL), length(vpar.list))
  
  all.node.list[[1]]<-vpar.list[[1]]
  all.node.list[[1]]$vpa<-node.names
  
  for(i in 2:length(vpar.list)){
    all.node.list[[i]]<-vpar.list[[i]]
    #Below for fooling compileCPT into thinking the the parent nodes have no parents. 
    all.node.list[[i]]$vpa <- node.names[i]
    all.node.list[[i]]$values<-rep(1,length(all.node.list[[i]]$levels  ) )    
  }
  
  cl<-compileCPT(all.node.list)
  
  return(cl)
  
}
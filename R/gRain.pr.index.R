#--------------------------------------------
#' @title Get the index of a specified state probability
#' in a gRain CPT.
#'
#' @description Get the index of a specified state probability
#' in a gRain CPT.
#' 
#' @details nslist is the variable specification list. They can
#' be specified in any order.
#' NOTE: A gRain.CPT is just an array or parray
#'
#' @param gRain.CPT
#' @param prior.nodeQ=NULL 
#' @param nslist=NULL
#' @return an array of row indices 
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
gRain.pr.index<-function(gRain.CPT, prior.nodeQ=NULL, nslist=NULL){
  
  
  if(is.null(prior.nodeQ)){
    print("******SPECIFY IF THIS IS A PRIOR NODE!!!!")
    return(0)
  }
  
  if(prior.nodeQ==TRUE){
    row.idx <- which(names(gRain.CPT) == nslist[[1]])
    if(length(row.idx) == 0) {
      stop("Probability not found in node table!")
    }
  }
  
  if(prior.nodeQ==FALSE){
    
    #Convert the CPT into Hugin form (linear)
    hug.cpt <- as.data.frame(ftable(gRain.CPT))
    node.name <- names(attributes( gRain.CPT )$dimnames)[1]    
    parent.node.names <- names(attributes( gRain.CPT )$dimnames)[-1]
    
    #Pick out the row number of the Hugin CPT which contains the the requested probability, according to nslist
    row.idx.list <- rep(list(NULL),length(nslist))    
    for(i in 1:length(nslist)){  
      node.idx <- which(colnames(hug.cpt) == names(nslist)[i] )
      row.idx.list[[i]] <- which(hug.cpt[,node.idx] == nslist[[i]] )
      
    }
    row.idx <- Reduce('intersect', row.idx.list)
    
    #     if(length(row.idx) == 0) {                #Not needed. Case should kill the function on line: row.idx.list[[i]] <- which(hug.cpt[,node.idx] == nslist[[i]] )
    #       stop("Probability not found in CPT!")
    #     }
    
    
  }
  
  return(row.idx)
  
}

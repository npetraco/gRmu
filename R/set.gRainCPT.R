#--------------------------------------------
#' @title Higher level utility function to set probabilities (beliefs) in an
#' initialized or existing gRain CPT
#'
#' @description Set beliefs in a gRain CPT. Handy for sensitivity analysis. 
#' 
#' @details Set beliefs (probabilities) in a gRain CPT. Handy for sensitivity analysis. 
#' Function is really just a wrapper for boilerplate code to input beliefs into an 
#' existing \code{cptable}. NOTE: The \code{complement.prob.vecQ} flag should only be switch on if the
#' node is BINARY.
#'
#' @param node.cpt a gRain CPT
#' @param prob.vec a vector of beliefs
#' @param complement.prob.vecQ switch to interlace the complement of \code{prob.vec} if required.
#' @return A \code{cptable} object (a list).
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
set.gRainCPT<-function(node.cpt, prob.vec, complement.prob.vecQ=FALSE){
  
  total.prob.vec<-prob.vec
  if(complement.prob.vecQ==TRUE){
    #Complements to the prob.vec
    total.prob.vec<-merge.probs(prob.vec)
  }
  
  #Set the probabilities in the node:
  new.node.cpt<-node.cpt
  new.node.cpt[] <- total.prob.vec #Should work the same for prior and conditional nodes.
    
  return(new.node.cpt)
  
}
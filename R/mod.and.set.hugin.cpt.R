#--------------------------------------------
#' Set beliefs (probabilities) of states for a Hugin CPT
#'
#' \code{mod.and.set.hugin.cpt} Set Prs of states for a Hugin CPT.
#'
#' Modify and set a CPT in a Hugin domain.
#'
#' @param hugin.domain RHugin loaded Hugin domain
#' @param node.name name of the node (CPT)
#' @param prob.vec probability vector to set
#' @param printQ switch to print the CPT
#' @return Nothing. 
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
mod.and.set.hugin.cpt<-function(hugin.domain, node.name, prob.vec, printQ=FALSE){
  
  #Get the CPT 
  cp.table<-get.table(hugin.domain,node=node.name)
  
  #Complements to the prob.vec
  prob.vec.complement<-1-prob.vec
  
  #Interlace the two probability vectors together. 
  total.prob.vec<-c(rbind(prob.vec,prob.vec.complement))
  #print(total.prob.vec)
  
  #Set the table back in Hugin
  cp.table[["Freq"]]<-total.prob.vec
  set.table(hugin.domain, node.name, cp.table)
  
  if(printQ==TRUE){
    print(paste("Changed CPT:",node.name))
    get.table(hugin.domain,node=node.name)
  }
  
}
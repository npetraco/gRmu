#--------------------------------------------
#' @title Merge (interlace) probabilities (beliefs) with their complements
#'
#' @description Merge (interlace) probabilities with their complements to make a
#' (gRain or Hugin) ordered prob vector for a CPT. 
#' 
#' @details The function is handy for sensitivity analysis.
#' NOTE: ONLY FOR BINARY NODES!
#'
#' @param prob.vec vector of probabilites to complement and merge
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
merge.probs<-function(prob.vec){
  #Complements to the prob.vec
  prob.vec.complement<-1-prob.vec
  
  #Interlace the two probability vectors together. 
  total.prob.vec<-c(rbind(prob.vec,prob.vec.complement))
  
  return(total.prob.vec)
  
}
#--------------------------------------------
#' @title Compute the coefs of the (multilinear) N-way 
#' sensitivity function
#'
#' @description Compute the coefs of the (multilinear) N-way
#' sensitivity function
#' 
#' @details none
#'
#' @param grain.domain 
#' @param nway.info
#' @param unique.params.list
#' @param param.functions.list
#' @param e.mat
#' @param h.mat
#' @return a vector
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
compute.sensitivity.function.coefs<-function(grain.domain, nway.info, unique.params.list, param.functions.list, e.mat, h.mat){
  
  n<-nrow(nway.info)
  print(paste("Computing coefs. for ",n,"-way sensitivity function.",sep=""))
  
  #pert.prob.mat<-make.perterbed.prob.mat(n)
  pert.prob.mat <- pert.prob.mat<-make.perterbed.prob.mat2(n)
  
  A.mat<-make.A(pert.prob.mat)
  y.vecs<-propagate.perterbed.probs(grain.domain, pert.prob.mat, nway.info, unique.params.list, param.functions.list, e.mat, h.mat)
  #print(y.vecs)
  
  c.vec<-solve(A.mat,y.vecs[,1])
  d.vec<-solve(A.mat,y.vecs[,2])
  
  #print(t(t(c.vec)))
  #print(t(t(d.vec)))
  
  coef.mat<-cbind(c.vec,d.vec)
  colnames(coef.mat)<-c("c","d")
  
  return(coef.mat)
  
}
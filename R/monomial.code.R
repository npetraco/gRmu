#--------------------------------------------
#' @title Coupe/Jensen code for a monomial in the multilinear function
#'
#' @description Coupe/Jensen code for a monomial in the multilinear function
#' 
#' @details none
#'
#' @param i
#' @return a string 
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
monomial.code<-function(i){
  #code.vec<-paste(rev(as.integer(intToBits(i-1))), collapse="")
  #print(code.vec)
  #code.vec<-paste(as.integer(intToBits(i-1)))
  return(cbind(paste("x",1:32,sep=""), code.vec))
}

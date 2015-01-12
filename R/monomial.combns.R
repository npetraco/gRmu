#--------------------------------------------
#' @title Canonically orderd indices of monomials excluding the constant term
#'
#' @description Canonically orderd indices of monomials excluding the constant term
#' 
#' @details none
#'
#' @param nn
#' @return a vector
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
monomial.combns<-function(nn){
  
  idx.list<-NULL
  if(nn>1){
    for(j in 2:nn){ #Get the combinations of elem indices
      idx.list<-c(idx.list, (combn(c(1:nn),j,simplify=F)) )
    }
  }
  
  idx.list<-c(sapply(1:nn,function(x){list(x)}), idx.list)
  
  return(idx.list)
}

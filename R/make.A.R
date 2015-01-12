#--------------------------------------------
#' @title Generate an A matrix from the perturbed prob mat
#'
#' @description Generate an A matrix from the perturbed prob mat
#' 
#' @details Need A for y=Ax
#'
#' @param pert.prob.mat
#' @return a matrix
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
make.A<-function(pert.prob.mat){
  
  n<-ncol(pert.prob.mat)
  Amat<-cbind(rep(1,2^n), pert.prob.mat)
  
  if(n>1){
    prod.probs.mat<-NULL
    for(i in 1:nrow(pert.prob.mat)){ #Loop over rows in pert.prob mat. Not the most efficient, but shouldn't be too bad as n should't go too high.
      row.prod.probs.vec<-NULL
      for(j in 2:n){ #Get the combinations of elem indices
        idx.mat<-t(combn(c(1:n),j,simplify=T))
        #print(idx.mat)
        for(k in 1:nrow(idx.mat)){ #Loop over the different combinations, multiplying the probs together.
          #print(prod(pert.prob.mat[i,c(idx.mat[k,])])) #xixj row if n=2, xixjxk if n=3, etc. 
          #Accum here
          row.prod.probs.vec<-c(row.prod.probs.vec, prod(pert.prob.mat[i,c(idx.mat[k,])]))
        }
      }
      #tack into mat here 
      prod.probs.mat<-rbind(prod.probs.mat, row.prod.probs.vec)
    }
    Amat<-cbind(Amat,prod.probs.mat)
  }
  rownames(Amat)<-NULL
  
  return(Amat)
}
#--------------------------------------------
#' @title Extract the lower triangle of elements in a square matrix 
#'
#' @description Extract the lower triangle of elements in a square matrix 
#' 
#' @details Handy to help check if any of the rows in the perturbed prob mat
#' are "to close" to each other 
#'
#' @param sqmat
#' @return an array
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
extract.lower.triangle<-function(sqmat)
{
  
  lt.vec<-NULL
  for(i in 1:dim(sqmat)[1])
  {
    for(j in 1:dim(sqmat)[2])
    {
      if(i<j)
      {
        lt.vec<-c(lt.vec,sqmat[i,j])
      }  
    }	
  }
  
  return(lt.vec)
}
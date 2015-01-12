#--------------------------------------------
#' @title Compute local sensitivity and vertex proximities
#' of parameters in a one-way sensitivity analysis
#'
#' @description Compute local sensitivity and vertex proximities
#' of parameters in a one-way sensitivity analysis
#' 
#' @details Cf. van der Gaag, Renooij and Coupe
#'
#' @param x0 
#' @param one.waycoef.mat
#' @return an array
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
one.way.sensitivity.metrics<-function(x0, one.waycoef.mat){
  
  a <- one.waycoef.mat[1,1]
  b <- one.waycoef.mat[2,1]
  c <- one.waycoef.mat[1,2]
  d <- one.waycoef.mat[2,2]
  
  s <- -1*d/c
  t <- a/c
  r <- (b/c) + (s*t)
  
  smets <- c(NA,NA)
  
  #local sensitivity = |f'(x0)|
  sens.val <- (a*d - b*c)/((c*x0 + d)^2)
  smets[1] <- abs(sens.val)
  
  #vertex proximity = dist bet x0 and |f'(x)|==1
  if(s<0){
    vex.val <- s + sqrt(abs(r))
    smets[2] <- abs(vex.val - x0)
  }
  if(s>1){
    vex.val <- s - sqrt(abs(r))
    smets[2] <- abs(vex.val - x0)
  }
  if(smets[1]==0){
    smets[2] <- 1e6 #Infinity for all intents and purposes
  }
  
  return(smets)
  
}
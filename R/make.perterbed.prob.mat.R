#--------------------------------------------
#' @title Generate a martix of perturbed probs for doing
#' n-way senstivity analysis
#'
#' @description Generate a martix of perturbed probs for doing
#' n-way senstivity analysis
#' 
#' @details NOTE: here n == num.states
#'
#' @param num.states
#' @return a vector
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
make.perterbed.prob.mat<-function(num.states){
  
  #Kill of n>7. permn needs to be replaced with something else bec it takes too long....
  if(num.states>7){
    print("Won't work for n>7 right now...")
    return(0)
  }
  
  perms.mat<-NULL
  for(i in 0:num.states){
    #Gen a vector of 1s and 2s to permute
    tmp <- c(rep(1,num.states-i),rep(2,i)) 
    
    #Permute and make into a matrix
    perms <- unique(permn(tmp))
    perms.sub.mat <- t(array(unlist(perms), c(num.states,length(perms))))
    perms.mat <- rbind(perms.mat,perms.sub.mat)
    
  }
  if(num.states==1){
    perms.mat<-t(t(perms.mat[-1]))
  }
  #print(perms.mat)
  
  var.choices<-seq(0.01,0.99,0.01)
  var.choice.mat<-NULL
  for(i in 1:num.states){
    #choose 2 values for each variable
    var.choice.mat<-rbind(var.choice.mat,sample(var.choices,2,replace=FALSE))
  }
  #print(var.choice.mat)
  
  xm<-NULL
  for(i in 1:num.states){
    colm<-sapply(1:(2^num.states),function(x){ var.choice.mat[i,perms.mat[x,i] ] })
    xm<-cbind(xm,colm)
  }
  
  return(xm)
  
}
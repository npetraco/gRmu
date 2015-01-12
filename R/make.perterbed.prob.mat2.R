#--------------------------------------------
#' @title Make perterbed probability matrix for sensitivity analysis 
#'
#' @description Make perterbed probability matrix for sensitivity analysis
#' 
#' @details none
#'
#' @param num.states
#' @return a matrix
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
make.perterbed.prob.mat2<-function(num.states){
  
  pm<-matrix(sample(seq(0.01,0.99,0.01),num.states*2^num.states,replace=T), nrow=2^num.states, ncol=num.states)
  
  if(num.states>1){
    corr.vals<-extract.lower.triangle(cor(pm))
    #print(corr.vals)
    
    corr.ok.flg<-FALSE
    while(corr.ok.flg==FALSE) {
      if(sum(corr.vals>0.95) > 0) {
        print("Uh-oh. At least two prob. rows too close. Try again....")
        pm<-matrix(sample(seq(0.01,0.99,0.01),num.states*2^num.states), nrow=2^num.states, ncol=num.states)
        corr.vals<-extract.lower.triangle(cor(pm))
      } else {
        corr.ok.flg<-TRUE
      } 
    }
  }
  
  return(pm)
}
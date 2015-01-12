#--------------------------------------------
#' @title Henrion style Noisy-OR gate
#'
#' @description Henrion style Noisy-OR gate
#' 
#' @details Handy for nodes with more than two parents. 
#' NOTE: This ONLY generates the noisy-or probs for the table, NOT the table itself
#' NOTE ALSO: For binary nodes with binary parents only
#'
#' @param gRain.CPT 
#' @param cond.parent.yes.probs.vec
#' @param leak.prob
#' @param complementQ
#' @return an array of probabilities
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
noisy.or.probs<-function(gRain.CPT, cond.parent.yes.probs.vec, leak.prob, complementQ=TRUE){
  
  #These are the "yeses" or "trues" of the parent nodes.
  parent.yes.levels<-data.frame(attributes(gRain.CPT)$dimnames, stringsAsFactors=F)[1,-1]
  #print(parent.yes.levels)
  
  #Convert the CPT over to Hugin format for an easier read
  hug.cpt<-gRain2HuginCPT(gRain.CPT, prior.node=F)
  #Drop the first and last columns
  hug.cpt<-hug.cpt[,-c(1,ncol(hug.cpt))]
  
  #Get the "yes" indices of the CPT: 1,3,5,7,....
  top.idxs<-seq(1, nrow(hug.cpt), 2)
  
  prob.vec<-rep(-1,length(top.idxs))
  count<-1
  for(i in top.idxs){
    #Compare to see if any of the causes are "on" for the row in the CPT
    yes.idxs<-which((parent.yes.levels == hug.cpt[i,])==TRUE ) 
    
    if(length(yes.idxs)==0){  #If none of the causes are "on" fill in the leak probability
      prob.vec[count] <- leak.prob
      count <- count + 1
    } else {                  #Otherwise use Henrion's formula for combining the ps of those causes that are "on"
      prob.vec[count] <- 1 - ( (1-leak.prob) * prod((1-cond.parent.yes.probs.vec[yes.idxs])/(1-leak.prob)) )
      count <- count + 1
    }
    
  }
  
  if(complementQ==TRUE){
    prob.vec.total<-merge.probs(prob.vec)
  } else {
    prob.vec.total<-prob.vec
  }
  
  
  return(prob.vec.total)
  
  
  
}

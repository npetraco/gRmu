#--------------------------------------------
#' @title Compute necessary probabilities to solve for 
#' sensitivity function coefs.
#'
#' @description Compute necessary probabilities to solve for 
#' sensitivity function coefs.
#' 
#' @details Cf. Coupe, Jensen et al N-way Sensititvity analysis
#'
#' @param grain.domain 
#' @param pert.prob.mat 
#' @param nway.info 
#' @param unique.params.list 
#' @param param.functions.list
#' @param e.mat 
#' @param h.mat
#' @return an array
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
propagate.perterbed.probs<-function(grain.domain, pert.prob.mat, nway.info, unique.params.list, param.functions.list, e.mat, h.mat){
  
  probs.mat<-array(-1,c(nrow(pert.prob.mat),2)) #This will store Pr(h,e) and Pr(e) need for the sensitivity analysis
  for(i in 1:nrow(pert.prob.mat)){ #loop over the 2^n rows of the perturbed probs
    #for(i in 1:1){
    
    #Grab a row of perturbing probabilities:
    prow<-pert.prob.mat[i,]
    #    print("===========================")
    #    print(paste("Perturbation",i, "of",nrow(pert.prob.mat)))
    #print("perturbing probabilities:")
    #print(prow)
    
    #Reset the network to perturb with a new row of probabilities:
    grain.domain.perterbed<-grain.domain
    
    for(j in 1:length(prow)){ #for a row, substitute each of the pert probs into their proper CPT, taking into account any other changes of other dependent probs in the CPT that ned to be changed.
      
      #Get the info for the node getting the substitution
      node.name<-nway.info[j,1]
      node.state.idx<-as.numeric(nway.info[j,2])
      #print(paste("  Node:",node.name,"State#:",node.state.idx, "Substuting:",prow[j]))
      
      #Get the indices of the unique params in the node getting the substution:
      unique.params.idxs <- unique.params.list[[node.name]]
      
      #Get the vector of original unique parameters for the node
      orig.unique.node.params <- array(grain.domain.perterbed$cptlist[[ node.name ]])[unique.params.idxs]      
      pvec <- orig.unique.node.params
      
      #Which of the unique params is getting substituted:
      chg.idx <- which(unique.params.idxs==node.state.idx)
      #print(paste("    This is unique params index#:",chg.idx))
      
      #Do the substitution
      pvec[chg.idx] <- prow[j]      
      total.prob.vec <- param.functions.list[[node.name]](pvec)
      
      #print(paste("    The node probabilities were:"))
      #print(array(grain.domain$cptlist[[ node.name ]]))
      #print(paste("    The node probabilities will now be:"))
      #print(total.prob.vec)
      
      grain.domain.perterbed$cptlist[[ node.name ]][] <- total.prob.vec
      #print(paste("    Changed", node.name, "param", node.state.idx, ". See:"))
      #print(grain.domain.perterbed$cptlist[[ node.name ]])
      #print("++++")
    }
    
    #print("These will be the evidence nodes:")
    #print(e.mat[,1])
    #print("These will be the states of the evidence nodes:")
    #print(e.mat[,2])
    
    #grain.domain.perterbed.propagated.instantiated<-setEvidence(grain.domain.perterbed.propagated, nodes=e.mat[,1], states=e.mat[,2])
    grain.domain.perterbed.instantiated.e<-setEvidence(grain.domain.perterbed, nodes=e.mat[,1], states=e.mat[,2])
    grain.domain.perterbed.instantiated.he<-setEvidence(grain.domain.perterbed, nodes=c(h.mat[,1],e.mat[,1]), states=c(h.mat[,2],e.mat[,2]))
    #print("Evidence set")
    
    p.e<-pEvidence(grain.domain.perterbed.instantiated.e)
    p.he<-pEvidence(grain.domain.perterbed.instantiated.he)
    probs.mat[i,]<-c(p.he,p.e)
    #    print(paste("P(h,e):",p.he))
    #    print(paste("P(e):",p.e))
    #    print(paste("P(h|e):",p.he/p.e))    
    #    print("++++++++++++++++++++++++++++++++++++++++++++")
    
  }
  
  colnames(probs.mat)<-c("Pr(h,e)","Pr(e)")
  
  return(probs.mat)
  
}
#--------------------------------------------
#' @title Convert a gRain format CPT to a pretty Hugin format CPT
#'
#' @description Convert a gRain format CPT to a Hugin format CPT, but with nicely
#' formatted strings indicating the conditional probability state (row of the Hugin CPT).
#' 
#' @details #Convert a gRain format CPT to a Hugin format CPT.
#' gRain format is a (multi) table. Hugin format
#' is a case-frequency data.frame. This function nicely formats the rows.
#'
#' @param gRain.CPT A gRain formatted CPT
#' @param prior.nodeQ switch to indicate if CPT is a prior node.
#' @return A row-wise CPT with nicely formatted node-state strings.
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
gRain2PrettyCPT<-function(gRain.CPT, prior.nodeQ=NULL){
  
  if(is.null(prior.nodeQ)){
    print("******SPECIFY IF THIS IS A PRIOR NODE!!!!")
    return(0)
  }
  
  if(prior.nodeQ==TRUE){
    hug.cpt<-as.data.frame(gRain.CPT)
    
    #names(attributes( hug.cpt )$dimnames) <- names(attributes( gRain.CPT )$dimnames)
    #names(attributes( hug.cpt )$names) <- names(attributes( gRain.CPT )$dimnames)
    names(hug.cpt) <- names(attributes( gRain.CPT )$dimnames)
    node.name <- names(hug.cpt)
    #print(attributes(hug.cpt))
    
    #colnames(hug.cpt)[1]<-paste("Probability Value")
    #print(node.name)
    pr.sentence.strings <- paste("Pr(",node.name," = ", rownames(hug.cpt),")", sep="")
    #print(pr.sentence.strings)
    #print(hug.cpt)
    pretty.cpt <- data.frame(pr.sentence.strings,hug.cpt[,1])
    colnames(pretty.cpt)<-c("Event","Probability")
    rownames(pretty.cpt) <- 1:dim(pretty.cpt)[1]
    return(pretty.cpt)
    #return(hug.cpt)
  }
  
  if(prior.nodeQ==FALSE){
    #print(as.data.frame(ftable(gRain.CPT)))
    
    hug.cpt <- as.data.frame(ftable(gRain.CPT))
    node.name <- names(attributes( gRain.CPT )$dimnames)[1]
    
    parent.node.names <- names(attributes( gRain.CPT )$dimnames)[-1]
    
    pr.sentence.strings <- paste("Pr(",node.name," = ", hug.cpt[,1]," | ", sep="")
    #print(pr.sentence.strings)
    
    #Loop over each row, paste tog given statements, paste predicate into givens string
    #for(i in 1:dim(hug.cpt)[1]){
    #  #print(paste(hug.cpt[i, 2:(dim(hug.cpt)[2] - 1) ]))
    #}
    #Loop over CPT givens (parent node levels) columns
    
    srows <- NULL
    for(i in 2:(dim(hug.cpt)[2] - 1)){
      #If not end ,
      if(i !=(dim(hug.cpt)[2] - 1)) {
        srows <- cbind(srows, paste(parent.node.names[i-1], "=", hug.cpt[,i]), ", ")
      } else {
        srows <- cbind(srows, paste(parent.node.names[i-1], "=", hug.cpt[,i]))
      }
    }
    
    smat <- cbind(pr.sentence.strings, srows, rep(")", nrow(srows) ) )
    
    #print(dim(smat))
    pretty.cpt<-array("",c(nrow(smat),1))
    for(i in 1:nrow(smat)){
      
      tmp <- paste(as.vector(smat[i,]), sep="",collapse="")
      
      #tmp <- paste(as.vector(smat[i,]), " = ", sep="",collapse="")
      #tmp <- paste(tmp, " = ",hug.cpt[i,ncol(hug.cpt)])
      #print(tmp)
      pretty.cpt[i,] <- tmp
      
    }
    
    pretty.cpt <- data.frame(pretty.cpt, hug.cpt[,ncol(hug.cpt)])
    colnames(pretty.cpt) <- c("Event","Probability")
    
    return(pretty.cpt)
    
    #return(as.data.frame(ftable(gRain.CPT)))
  }
  
}
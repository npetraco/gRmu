#--------------------------------------------
#' @title Write a CPT to file in Hugin order
#'
#' @description Write a CPT to file in Hugin order
#' 
#' @details none
#'
#' @param grain.cpt 
#' @param prior.node 
#' @param fpath 
#' @param remove.complimentQ 
#' @param freq.col  
#' @param row.numsQ
#' @return nothing
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
write.cpt<-function(grain.cpt, prior.node, fpath, remove.complimentQ=F, freq.col="last", row.numsQ=T){
  
  #Complete CPT
  hug.cpt<-gRain2HuginCPT(grain.cpt, prior.node)
  
  if(remove.complimentQ==T){ #Keep only the uniquely determined rows? 
    row.idxs <- seq(1,nrow(hug.cpt),2)
    hug.cpt <- hug.cpt[row.idxs,]
  }
  
  if(freq.col=="first"){ #Put the beliefs column first?
    hug.cpt <- cbind(hug.cpt[,"Freq"],hug.cpt[,-ncol(hug.cpt)])
    colnames(hug.cpt)[1]<-"Freq"
  }
  
  if(row.numsQ==T){ #Write the row numbers into the file?
    write.csv(hug.cpt, fpath) 
  } else {
    write.csv(hug.cpt, fpath, row.names=F) 
  }
  
  print(hug.cpt)
  print(paste("^------Wrote CPT"))
  
}
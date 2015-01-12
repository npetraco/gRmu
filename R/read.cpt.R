#--------------------------------------------
#' @title Read in a CPT to and convert to Hugin order if not already done so
#'
#' @description Read in a CPT to and convert to Hugin order if not 
#' already done so
#' 
#' @details NOTE, If coplements were not originally in the file, they WILL NOT 
#' BE replaced. One has to do that manually
#'
#' @param fpath
#' 
#' @return an array
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
read.cpt<-function(fpath){
  
  raw.cpt<-read.csv(fpath,stringsAsFactors=F)
  #print(raw.cpt)
  col.nms<-colnames(raw.cpt)
  if( col.nms[1]=="X") {
    cpt<-raw.cpt[,-1]
    #rownames(cpt)<-raw.cpt[,1]
  } else {
    cpt<-raw.cpt
  }
  
  col.nms<-colnames(cpt)
  freq.col.idx<-which(col.nms=="Freq")
  #print(freq.col.idx)
  
  cpt<-cbind(cpt[,-freq.col.idx], cpt[,freq.col.idx])
  colnames(cpt)[ncol(cpt)]<-"Freq"
  
  return(cpt)
}

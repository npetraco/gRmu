#--------------------------------------------
#' @title Convert a gRain format CPT to a Hugin format CPT
#'
#' @description Convert a gRain format CPT to a Hugin format CPT
#' 
#' @details #Convert a gRain format CPT to a Hugin format CPT.
#' gRain format is a (multi) table. Hugin format
#' is a case-frequency data.frame.
#' NOTE: A gRain.CPT is just an array or parray. See gRbase documentation.


#'
#' @param gRain.CPT A gRain formatted CPT
#' @param prior.nodeQ switch to indicate if CPT is a prior node.
#' @return A Hugin format CPT.
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
gRain2HuginCPT<-function(gRain.CPT, prior.nodeQ=NULL){
  
  if(is.null(prior.nodeQ)){
    print("******SPECIFY IF THIS IS A PRIOR NODE!!!!")
    return(0)
  }
  
  if(prior.nodeQ==TRUE){
    hug.cpt<-as.data.frame(gRain.CPT)
    
    #names(attributes( hug.cpt )$dimnames) <- names(attributes( gRain.CPT )$dimnames)
    #names(attributes( hug.cpt )$names) <- names(attributes( gRain.CPT )$dimnames)
    names(hug.cpt) <- names(attributes( gRain.CPT )$dimnames)
    #print(attributes(hug.cpt))
    
    colnames(hug.cpt)[1]<-paste("Freq", names(hug.cpt))
    return(hug.cpt)
  }
  
  if(prior.nodeQ==FALSE){
    return(as.data.frame(ftable(gRain.CPT)))
  }
  
}
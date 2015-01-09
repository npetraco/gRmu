#--------------------------------------------
#' @title Test a gRain CPT to see if is a prior node
#'
#' @description Test a gRain CPT to see if is a prior node
#' 
#' @details The function is an internal routine, but may be handy
#' for some applications.
#'
#' @param gRain.CPT A gRain formatted CPT
#' @return TRUE or FALSE
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
gRain.prior.nodeQ<-function(gRain.CPT){
  
  table.depth <- length(dimnames(gRain.CPT))
  if(table.depth == 1){
    priorQ <- TRUE
  } else if(table.depth>1) {
    priorQ <- FALSE
  } else {
    priorQ <- NULL
  }
  
  return(priorQ)
}
#--------------------------------------------
#' @title Get gRain CPT out of a gRain domain (network)
#'
#' @description Handy wrapper to grab a gRain CPT by name out of a gRain network.
#' 
#' @details Handy wrapper to grab a gRain CPT by name (as a string) out of a gRain network.
#' 
#' @param gRain.domain a gRain network
#' @param node.name the name of the node as a character string
#' @return a gRain \code{cptable}
#'
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
get.gRainCPT<-function(gRain.domain, node.name) {
  
  node.idx <- which(nodeNames(gRain.domain) == node.name)
  node.table <- gRain.domain$cptlist[[node.idx]]
  #print(node.table)
  
  return(node.table)
}
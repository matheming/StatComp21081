#' @title A function generates an index matrix
#' @description A function generates an index matrix
#' @param s the dimension of the matrix, \code{s} shouldn't be large
#' @return a \code{2^s*s} index matrix filled with 0 and 1
#' @examples 
#' \dontrun{
#' Index <- ff2n(3)
#' print(Index)
#' }
#' @export

ff2n<-function(s){
  if(s==1){
    return(matrix(c(0,1),2,1))
  }
  else{
    return(cbind(c(rep(0,2^(s-1)),rep(1,2^(s-1))),rbind(diag(1,2^(s-1)),diag(1,2^(s-1)))%*%ff2n(s-1)))
  }
}
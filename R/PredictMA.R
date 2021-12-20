#' @title A prediction function after model averaging
#' @name PredictMA
#' @description Predicting with new data
#' @param Data New data contain explanatory variables in whole model
#' @param MA Result of function `ModelAve()`
#' @param ynew Whether a new \code{y} is in \code{Data} or is given as a vector. If `ynew=TRUE`, \code{Data} should contain the explanatory vector,
#' and if `ynew` is a vector with same size as \code{Data}, if `ynew=FALSE`, there is no new \code{y}.
#' @return Prediction of data
#' @examples 
#' \dontrun{
#' n <- 15
#' p <-3
#' X <- matrix(rnorm(n*p), n, p)
#' beta <- c(1, 2, 3)
#' y <- X %*% beta+rnorm(n)
#' y<-data.frame(y)
#' Data <- cbind(y, data.frame(X))
#' subset <- matrix(c(1,0,0,0,1,0),ncol=p,byrow = TRUE)
#' get_MA_nest<-ModelAve(y~., Data[1:10,], method = "MMA", nested = TRUE)
#' PredictMA(Data[11:15,],get_MA_nest,ynew = TRUE)
#' }
#' @export
PredictMA<-function(Data,MA,ynew=FALSE){
  n<-nrow(Data)
  Formula<-MA$model
  name1<-as.character(Formula)
  name2<-gsub('[ ]','',name1[3])
  name3<-unlist(strsplit(name2,split = "[+]"))
  Eloc<-match(name3,colnames(Data))
  if(is.na(sum(Eloc))){
    stop("Data doesn't contain explanatory variable!")
  }
  else{
    Data2<-Data[,Eloc]
  }
  yfore<-as.matrix(cbind(seq(1,n),Data2))%*%MA$betahat
  
  if(is.numeric(ynew)==TRUE){
    y<-as.matrix(ynew)
    if(length(y)!=n) stop("Length of y doesn't match data.")
  }
  if(is.numeric(ynew)==FALSE){
    if(ynew==TRUE){
      Rloc<-match(name1[2],colnames(Data))
      if(is.na(sum(Rloc))==TRUE) stop("Data doesn't contain explanatory variable!")
      
      y<-as.matrix(Data[,Rloc])
    }
    if(ynew==FALSE) y<-NULL}
  
  if(is.null(y)) R2<-NULL
  if(!is.null(y)) R2<-sum((yfore-y)^2)
  
  return(list(yfore=yfore,yorigin=y,rsquare=R2))
}
#' @title A model averaging function
#' @name ModelAve
#' @description Model averaging function including MMA and JMA
#' @param Formula formula like `y~.` or `y~X1+X2` which mean whole model
#' @param Data Data set including response variable and explanatory variables, the first column is response variable
#' @param nested Whether using nested model, `default=FALSE`
#' @param method One of "MMA" and "JMA"
#' @param Subset An index matrix of n*p with 0-1 element, \code{n} is model number and \code{p} is whole explanatory variables number
#' @param equalweight Whether use equal weight, `default=FALSE`
#' @return The optimal weight, the coefficients of all models
#' @examples 
#' \dontrun{
#' ## simulation data
#' n <- 10
#' p <-3
#' X <- data.frame(matrix(rnorm(n*p), n, p))
#' beta <- c(1, 2, 3)
#' y <- X %*% beta
#' Data <- cbind(y, X)
#' subset <- matrix(c(1,0,0,0,1,0),ncol=p)
#' MA(y~., Data, method="MMA", Subset = subset)
#' }
#' @import stats 
#' @importFrom quadprog solve.QP
#' @useDynLib StatComp21081
#' @export
ModelAve<-function(Formula, Data, nested=FALSE, method, Subset=NULL, equalweight=FALSE){
  
  Data<-data.frame(Data)
  ## change order
  Formulas<-as.character(Formula)
  Rloc<-match(Formulas[2],colnames(Data))
  if(is.na(Rloc)) {stop("Data doesn't contain variable1!")}
  else{
    y<-Data[,Rloc]
    Data1<-Data[,-Rloc]
  }
  
  if(Formulas[3]=="."){
    Data2<-Data1
  }
  else{
    name2<-gsub('[ ]','',Formulas[3])
    name1<-unlist(strsplit(name2,split = "[+]"))
    Eloc<-match(name1,colnames(Data1))
    if(is.na(sum(Eloc))){
      stop("Data doesn't contain variable2!")
    }
    else{
      Data2<-Data1[,Eloc]
    }
  }
  Data<-cbind(y,Data2)
  p<-ncol(Data)-1
  n<-nrow(Data)
  Data2<-as.matrix(Data2)
  
  if(nested==FALSE) {
    if(is.null(Subset)) Index<-ff2nC(p)[-1,]
    else Index<-Subset
  }
  if(nested==TRUE){
    Index<-matrix(1,p,p)
    Index[!lower.tri(Index,diag = TRUE)]<-0
  }
  n_index<-nrow(Index)
  
  bsingles<-matrix(NA,n,n_index)
  bfsingles<-matrix(NA,n,n_index)
  hatm<-matrix(NA,n,n_index)
  bbeta <- matrix(0,p+1,n_index)
  if(method=="JMA") ee <- matrix(NA,n,n_index)
  
  for (i in 1:n_index) {
    ind<-Index[i,]
    ind<-which(ind==1,arr.ind = T)+1
    Result_lm<-lm(y~.,data = Data[,c(1,ind)],x=TRUE,y=TRUE)
    Beta<-Result_lm$coefficients
    bbeta[c(1,ind),i]<-Beta
    RMAT<-as.matrix(Result_lm$x)
    bsingles[,i]<-Result_lm$fitted.values
    hatm[,i]<-diag(RMAT%*%solve(t(RMAT)%*%RMAT)%*%t(RMAT))
    Sigma<-sum((Result_lm$residuals)^2)/Result_lm$df.residual
    if(method=="JMA"){
      ei<-Result_lm$residuals
      ee[,i]<-ei/(1-hatm[,i])
    }
  }
  
  if(method=="MMA") ee<-matrix(Data[,1],n,n_index)-bsingles
  
  
  #  model_cnt<-apply(Index, 1,sum)
  model_cnt <- rowSums(Index)
  feature<-list(Sigma=Sigma,hatm=hatm,model_cnt=model_cnt)
  
  a1<-t(ee)%*%ee
  if (qr(a1)$rank<ncol(ee)) a1 <- a1 + diag(n_index)*1e-10# singular matrix
  if (method == "MMA") a2 <- matrix(c(-feature$Sigma*model_cnt),n_index,1)  
  if (method == "JMA") a2 <- matrix(0,nrow=n_index,ncol=1)
  a3 <- t(rbind(matrix(1,nrow=1,ncol=n_index),diag(n_index),-diag(n_index)))
  a4 <- rbind(1,matrix(0,nrow=n_index,ncol=1),matrix(-1,nrow=n_index,ncol=1))
  
  if(equalweight==TRUE) w<-matrix(1,nrow=n_index,ncol=1)/n_index
  if(equalweight==FALSE){
    w0 <- matrix(1,nrow=n_index,ncol=1)/n_index 
    QP <- solve.QP(a1,a2,a3,a4,1)
    w <- QP$solution
    w <- as.matrix(w)
    w <- w*(w>0)
    w <- w/sum(w0)
  }
  betahat <- bbeta %*% w
  ybar <- mean(y)
  yhat <- cbind(c(1,n),Data2) %*% betahat
  ehat <- y-yhat
  r2 <- sum((yhat-ybar)^2)/sum((y-ybar)^2)
  if (method == "MMA") cn=(t(w) %*% a1 %*% w - 2*t(a2) %*% w)/n
  if (method == "JMA") cn=(t(w) %*% a1 %*% w)/n
  
  return(list(betahat=betahat,w=w,yhat=yhat,ehat=ehat,r2=r2,cn=cn))
}
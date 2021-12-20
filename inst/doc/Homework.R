## -----------------------------------------------------------------------------
Height<-c(152,166,170,178,183)
Weight<-50+(Height-150)*0.6
Weight

## -----------------------------------------------------------------------------
library(knitr)
Weight_height_table<-data.frame(Height,Weight,row.names = c("A","B","C","D","E"))
knitr::kable(t(Weight_height_table),format = "html",caption = "Table 1: height-weight table")

## -----------------------------------------------------------------------------
library(graphics)
plot(Weight_height_table,type = "p",pch=16,col="blue",xlab = "Height(cm)",ylab = "Weight(kg)")
abline(a=-40,b=0.6,col="blue")
text(Weight_height_table,labels =c("A","B","C","D","E"),adj = c(0,1))

## ----echo=FALSE---------------------------------------------------------------
sigma<-c(1:6)#sigma=1,2,3,4,5,6
n<-1000#sample size
u<-runif(n)#generate n samples from uniform distribution
par(mfrow=c(2,3))
hist(sqrt(-2*sigma[1]^2*log(1-u)),breaks = 10,main = "sigma=1",xlab = "x",border = "white")# histogram 
abline(v=sigma[1],col="red")#red line is the theoretical mode sigma
hist(sqrt(-2*sigma[2]^2*log(1-u)),breaks = 10,main = "sigma=2",xlab = "x",border = "white")
abline(v=sigma[2],col="red")
hist(sqrt(-2*sigma[3]^2*log(1-u)),breaks = 10,main = "sigma=3",xlab = "x",border = "white")
abline(v=sigma[3],col="red")
hist(sqrt(-2*sigma[4]^2*log(1-u)),breaks = 10,main = "sigma=4",xlab = "x",border = "white")
abline(v=sigma[4],col="red")
hist(sqrt(-2*sigma[5]^2*log(1-u)),breaks = 10,main = "sigma=5",xlab = "x",border = "white")
abline(v=sigma[5],col="red")
hist(sqrt(-2*sigma[6]^2*log(1-u)),breaks = 10,main = "sigma=6",xlab = "x",border = "white")
abline(v=sigma[6],col="red")

## ----echo=FALSE---------------------------------------------------------------
n<-1000
set.seed(111)
X1<-rnorm(n)#generate n N(0,1) distribution samples
X2<-rnorm(n,3,1)#generate n N(3,1) distribution samples
p1<-0.75
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1,1-p1))#generate weight with probabilities p1 and p2
Z<-r*X1+(1-r)*X2#generate samples from mixture
# histogram of the sample with density
hist(Z,freq = FALSE,ylim = c(0,0.3),border = "white",col = "purple",main = "histogram of the sample with density")
lines(density(Z),col="red")

## ----echo=FALSE---------------------------------------------------------------
p1<-seq(from=0.1,to=0.9,by=0.1)# set p1
# plot the histogram of mixture samples with different probabilities p1
#par(mfrow=c(3,3))
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[1],1-p1[1]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.1",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[2],1-p1[2]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.2",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[3],1-p1[3]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.3",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[4],1-p1[4]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.4",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[5],1-p1[5]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.5",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[6],1-p1[6]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.6",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[7],1-p1[7]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.7",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[8],1-p1[8]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.8",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
r<-sample(c(1,0),n,replace = TRUE,prob = c(p1[9],1-p1[9]))
hist(r*X1+(1-r)*X2,freq = FALSE,ylim = c(0,0.4),border = "white",col = "purple",main = "p1=0.9",xlab = "Z")
lines(density(r*X1+(1-r)*X2),col="red")
#mtext("Histograms of Different Mixture Distribution with Different Probabilities",side = 3, line = 0, outer = T)

## ----echo=FALSE---------------------------------------------------------------
#f is a function with some parameters that generate n samples of X(t)
#sample size n, Piosson parameter lambda, time t0, Gamma distribution shape=alpha, scale=beta
f<-function(n,lambda,t0,alpha,beta){
upper <- 100
pp <-rg<- numeric(n)
for (i in 1:n) {
N <- rpois(1, lambda * upper)
Un <- runif(N, 0, upper) #unordered arrival times
Sn <- sort(Un) #arrival times
n <- min(which(Sn > t0)) #arrivals+1 in [0, t0]
pp[i] <- n - 1 #arrivals in [0, t0]
rg[i]<-rgamma(1,shape = pp[i]*alpha,rate = beta)
}
return(rg)
}

## ----echo=FALSE---------------------------------------------------------------
t<-10
n<-500
# first change lambda with alpha=beta=2
Lambda<-c(1,2,5,10)#lambda
Alpha<-Beta<-2
r1<-v1<-numeric(4)
for(i in 1:4){
  r1[i]<-(mean(f(n,lambda = Lambda[i],t,Alpha,Beta))-t*Lambda[i])/(t*Lambda[i])
  v1[i]<-var(f(n,lambda = Lambda[i],t,Alpha,Beta))/(t*Lambda[i]*(Alpha+Alpha^2)/(Beta^2))-1
}
# second change shape=alpha with beta=2 and lambda=2
Alpha<-c(1,2,5,10)
Beta<-Lambda<-2
r2<-v2<-numeric(4)
for(i in 1:4){
  r2[i]<-(mean(f(n,lambda = Lambda,t,Alpha[i],Beta))-t*Lambda*Alpha[i]/Beta)/(t*Lambda*Alpha[i]/Beta)
  v2[i]<-var(f(n,lambda = Lambda,t,Alpha[i],Beta))/(t*Lambda*(Alpha[i]+Alpha[i]^2)/(Beta^2))-1
}
# last change beta with alpha=2 and lambda=2
Beta<-c(1,2,5,10)
Alpha<-Lambda<-2
r3<-v3<-numeric(4)
for(i in 1:4){
  r3[i]<-(mean(f(n,lambda = Lambda,t,Alpha,Beta[i]))-t*Lambda*Alpha/Beta[i])/(t*Lambda*Alpha/Beta[i])
  v3[i]<-var(f(n,lambda = Lambda,t,Alpha,Beta[i]))/(t*Lambda*(Alpha+Alpha^2)/(Beta[i]^2))-1
}

## -----------------------------------------------------------------------------
Beta.est<-function(x){
  if(x>1||x<0)
    print("Error input!")
  else {
    u<-runif(1000,min = 0,max = x)
    return(mean(30*u^2*(1-u)^2*x))
  }
}
x<-seq(0.1,0.9,by=0.1)
Mat<-matrix(0,9,3)
colnames(Mat)<-c("x","estimation","difference")
# Compare the estimates with the values returned by the pbeta function
for (i in 1:9) {
  Mat[i,1]<-x[i]
  Mat[i,2]<-Beta.est(x[i])
  Mat[i,3]<-Beta.est(x[i])-pbeta(x[i],3,3)
}
Mat

## -----------------------------------------------------------------------------
Gen_Ray_ant<-function(sigma=sigma,n=n,antithetic = TRUE){
  if(n%%2!=0 && antithetic)
    print("Error size!")
  else if(!antithetic) {
      u<-runif(n/2)
      v<-runif(n/2)
      return(list(X1=sqrt(-2*sigma^2*log(1-u)),X2=sqrt(-2*sigma^2*log(1-v))))
  } else {
      u<-runif(n/2)
      return(list(X1=sqrt(-2*sigma^2*log(1-u)),X2=sqrt(-2*sigma^2*log(u))))
        }
}

## -----------------------------------------------------------------------------
library(scales)
#antithetic variable
set.seed(2001)
Result_ant<-Gen_Ray_ant(sigma = 2,n=500,antithetic = TRUE)
X1_ant<-Result_ant$X1
X2_ant<-Result_ant$X2
print(var_ant<-var((X1_ant+X2_ant)/2))
#non-antithetic
set.seed(2002)
Result_non<-Gen_Ray_ant(sigma = 2,n=500,antithetic = FALSE)
X1_non<-Result_non$X1
X2_non<-Result_non$X2
print(var_non<-var((X1_non+X2_non)/2))
# compute percent reduction
print((var_non-var_ant)/var_non)

## -----------------------------------------------------------------------------
set.seed(10086)
u<-runif(1000)
theta.hat<-sqrt(1-2*log(1-u))/sqrt(2*pi*exp(1))
theta.est<-mean(theta.hat)

## ----eval=FALSE---------------------------------------------------------------
#  n<-20
#  alpha<-.05
#  UCL<-U<-V<-numeric(1000)
#  for (i in 1:1000){
#    x<-rchisq(n,2)
#    U[i]<-mean(x)+sd(x)/sqrt(n)*qt(alpha/2,df=n-1)
#    V[i]<-mean(x)-sd(x)/sqrt(n)*qt(alpha/2,df=n-1)
#    UCL[i]<-mean(x)-sd(x)/sqrt(n)*qt(alpha,df=n-1)
#  }

## -----------------------------------------------------------------------------
n<-200
mu0<-1
m<-500
Ind<-numeric(m)
for (i in 1:m) {
  x<-rchisq(n,df=1)
  Ind[i]<-t.test(x, mu = mu0)$p.value
}

## -----------------------------------------------------------------------------
l<-20
Ind<-numeric(m)
for (i in 1:m) {
  x<-rchisq(l,df=1)
  Ind[i]<-t.test(x, mu = mu0)$p.value
}

## -----------------------------------------------------------------------------
Ind<-numeric(m)
for (i in 1:m) {
  x<-runif(n,min = 0,max = 2)
  Ind[i]<-t.test(x, mu = mu0)$p.value
}

## -----------------------------------------------------------------------------
Ind<-numeric(m)
for (i in 1:m) {
  x<-runif(l,min = 0,max = 2)
  Ind[i]<-t.test(x, mu = mu0)$p.value
}

## -----------------------------------------------------------------------------
Ind<-numeric(m)
for (i in 1:m) {
  x<-rexp(n,rate = 1)
  Ind[i]<-t.test(x, mu = mu0)$p.value
}

## -----------------------------------------------------------------------------
Ind<-numeric(m)
for (i in 1:m) {
  x<-rexp(l,rate = 1)
  Ind[i]<-t.test(x, mu = mu0)$p.value
}

## -----------------------------------------------------------------------------
n<-c(10,20,30,50,100,500)#sample size
d<-2#dimension
cv<-qchisq(0.95,d*(d+1)*(d+2)/6)
rmvn.eigen <- function(n, mu, Sigma) {
# generate random vectors from MVN(mu, Sigma)
# dimension is inferred from mu and Sigma
d <- length(mu)
ev <- eigen(Sigma, symmetric = TRUE)
lambda <- ev$values
V <- ev$vectors
C <- V %*% diag(sqrt(lambda)) %*% t(V)
Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
X <- Z %*% C + matrix(mu, n, d, byrow = TRUE)
X
}
mu<-c(0,1)
Sigma <- matrix(c(1, .8, .8, 2), nrow = 2, ncol = 2)
#a function to calculate the multivariate skewness statistic
mulss<-function(x){
  mu.bar<-apply(x, 2,mean)
  num<-nrow(x)
  cov.hat.inv<-solve((num-1)/num*cov(x))
  x.t<-t(x)-mu.bar
  x.mat<-t(x.t)%*%cov.hat.inv%*%x.t
  return(sum(x.mat^3)/(num^2))
}
p.reject<-numeric(length(n))#to store sim. results
m<-1e3#num. repl. each sim.
for (i in 1:length(n)) {
  mdtests<-numeric(m)
  for (j in 1:m) {
    x<-rmvn.eigen(n[i],mu,Sigma)
    mdtests[j] <- as.integer(n[i]*mulss(x)/6 >= cv )
  }
  p.reject[i]<-mean(mdtests)
}
p.reject

## -----------------------------------------------------------------------------
alpha <- .1
n <- 40
m <- 200
Sigma1<-matrix(c(1,0.8,0.8,2),2,2)
Sigma2<-matrix(c(100,80,80,200),2,2)
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test
cv <- qchisq(1-alpha,d*(d+1)*(d+2)/6)
for (j in 1:N) { #for each epsilon
e <- epsilon[j]
mdtests <- numeric(m)
for (i in 1:m) { #for each replicate
index<- sample(c(1,2), replace = TRUE, size = n, prob = c(1-e, e))
x<-matrix(0,n,d)
for (k in 1:n) {
  if(index[k]==1) x[k,]=rmvn.eigen(1,c(0,0),Sigma1)
  if(index[k]==2) x[k,]=rmvn.eigen(1,c(0,0),Sigma2)
}
mdtests[i] <- as.integer(n*mulss(x)/6> cv)
}
pwr[j] <- mean(mdtests)
}
#plot power vs epsilon
plot(epsilon, pwr, type = "b", xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
data(scor,package="bootstrap")
n<-nrow(scor)
p<-ncol(scor)
Cova_scor<-(n-1)/n*cov(scor)#MLE
eigen_value_scor<-eigen(Cova_scor)
eigen_value_scor<-eigen_value_scor$values
Theta<-eigen_value_scor[1]/sum(eigen_value_scor)
print(Theta)
#Bootstrap
B<-500
R<-numeric(B)
for (b in 1:B) {
  i<-sample(1:n,size = n,replace = TRUE)
  Scor<-scor[i,]
  eign<-eigen((n-1)/n*cov(Scor))$values
  R[b]<-eign[1]/sum(eign)
}
print(se.R<-sd(R))
bias.Boot<-mean(R)-Theta
print(bias.Boot)

## -----------------------------------------------------------------------------
Theta.jack<-numeric(n)
for (i in 1:n) {
  Scor_i<-scor
  Cova<-cov(Scor_i[-i,])
  eig<-eigen((n-2)/(n-1)*Cova)$values
  Theta.jack[i]<-eig[1]/sum(eig)
}
bias<-(n-1)*(mean(Theta.jack)-Theta)
print(bias)
se.jack<-sqrt((n-1)*mean((Theta.jack-mean(Theta.jack))^2))
print(se.jack)

## -----------------------------------------------------------------------------
# 95% percentile CI
alpha<-c(.025,.975)
print(quantile(R,alpha,type=6))
# BCa
Z0<-qnorm(mean(as.integer(R<Theta)))
Zalpha<-qnorm(alpha)
L<-mean(Theta.jack)-Theta.jack
a<-sum(L^3)/(6*sum(L^2)^1.5)
adj.alpha <- pnorm(Z0 + (Z0+Zalpha)/(1-a*(Z0+Zalpha)))# adjusted alpha
adj.alpha
BCa<-quantile(R,adj.alpha,type = 6)

## -----------------------------------------------------------------------------
library(boot)
size<-1e2 #sample size
# sample skewness
sk <- function(x,i) {
  xbar <- mean(x[i])
  m3 <- mean((x[i] - xbar)^3)
  m2 <- mean((x[i] - xbar)^2)
  return( m3 / m2^1.5 )
}
# empirical CP
CPnm.norm<-CPnm.perc<-CPnm.basic<-numeric(1e2)
for (i in 1:1e2) {
# normal population
nmdata<-rnorm(size)
boot.nm<-boot(nmdata,statistic = sk,R=1e2)
bootnm.ci<-boot.ci(boot.nm,type = c("norm","basic","perc"))
CPnm.norm[i]<-as.integer(boot.nm$t0>bootnm.ci$normal[2]&boot.nm$t0<bootnm.ci$normal[3])
CPnm.perc[i]<-as.integer(boot.nm$t0>bootnm.ci$percent[4]&boot.nm$t0<bootnm.ci$percent[5])
CPnm.basic[i]<-as.integer(boot.nm$t0>bootnm.ci$basic[4]&boot.nm$t0<bootnm.ci$basic[5])
}
print(c(mean(CPnm.norm),mean(CPnm.basic),mean(CPnm.perc)))
# chi distribution
# empirical CP
CPchi.norm<-CPchi.perc<-CPchi.basic<-numeric(1e2)
for (i in 1:1e2) {
  datachi<-rchisq(size,5)
  # chi population
  bootchi<-boot(datachi,statistic = sk,R=1e2)
  bootchi.ci<-boot.ci(bootchi,type =  c("norm","basic","perc"))
  CPchi.norm[i]<-as.integer(bootchi$t0>bootchi.ci$normal[2]&bootchi$t0<bootchi.ci$normal[3])
  CPchi.perc[i]<-as.integer(bootchi$t0>bootchi.ci$percent[4]&bootchi$t0<bootchi.ci$percent[5])
  CPchi.basic[i]<-as.integer(bootchi$t0>bootchi.ci$basic[4]&bootchi$t0<bootchi.ci$basic[5])
}
print(c(mean(CPchi.norm),mean(CPchi.basic),mean(CPchi.perc)))

## -----------------------------------------------------------------------------
## generate two group sample randomly
n1<-20
n<-2*n1
K<-1:n
R<-499
x1<-runif(n1)
x2<-rexp(n1)
z<-c(x1,x2)
## calculate original Spearman rank correlation test statistic and p-value
t0<-cor(x1,x2,method = "spearman")
p0<-cor.test(x1,x2,method="spearman")$p.value
reps<-numeric(R)
set.seed(20211104)
for (i in 1:R) {
  k <- sample(K, size =n1, replace = FALSE)
  y1 <- z[k] 
  y2 <- z[-k]
  reps[i]<-cor(y1,y2,method = "spearman")
}
p<-mean(abs(c(t0,reps))>abs(t0))# p-value calculated by permutation test
round(c(p,p0),4)

## ----warning=FALSE------------------------------------------------------------
library(energy)
library(Ball)
library(boot)
library(RANN)
m <-500
k<-3
p<-2
mu<-0.3
set.seed(12345)
n1<-50
n2<-100
R<-499
n <- n1+n2
## Tn function
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z)
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 <= n1)
  i2 <- sum(block2 > n1)
  (i1 + i2) / (k * n)
}
## the NN method
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
  sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

## -----------------------------------------------------------------------------
N<-c(n1,n1)
p.values <- matrix(NA,m,3)
for(i in 1:m){
x<-matrix(rnorm(n1*p,0,1.5),ncol=p)
y<-matrix(rnorm(n1*p,0,2),ncol = p)
#y <- cbind(rnorm(n2),rnorm(n2,mean=mu));
z <- rbind(x,y)
p.values[i,1] <- eqdist.nn(z,N,k)$p.value
p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,
seed=i*12345)$p.value
}
alpha <- 0.1
pow <- colMeans(p.values<alpha)
pow<-data.frame(pow)
rownames(pow)<-c("NN","Energy","Ball")
colnames(pow)<-c("power")
knitr::kable(t(pow))

## -----------------------------------------------------------------------------
N<-c(n1,n1)
p.values <- matrix(NA,m,3)
for(i in 1:m){
x<-matrix(rnorm(n1*p,0,1.5),ncol=p)
y<-matrix(rnorm(n1*p,mu,2),ncol = p)
#y <- cbind(rnorm(n2),rnorm(n2,mean=mu));
z <- rbind(x,y)
p.values[i,1] <- eqdist.nn(z,N,k)$p.value
p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,
seed=i*12345)$p.value
}
alpha <- 0.1
pow <- colMeans(p.values<alpha)
pow<-data.frame(pow)
rownames(pow)<-c("NN","Energy","Ball")
colnames(pow)<-c("power")
knitr::kable(t(pow))

## -----------------------------------------------------------------------------
fcauchy<-function(x,theta,eta){
  stopifnot(theta>0)
  f<-1/(theta*pi*(1+((x-eta)/theta)^2))
  return(f)
}

## -----------------------------------------------------------------------------
set.seed(10006)
N<-1000# 500 to be discarded
x<-numeric(N)
x[1]<-rnorm(1,0,2)
theta<-1
eta<-0
sd<-10
u<-runif(N)
k<-0
for (i in 2:N) {
  xt<-x[i-1]
  y<-rnorm(1,xt,sd)
  num<-fcauchy(y,theta,eta)*dnorm(xt,mean = y,sd=sd)
  den<-fcauchy(xt,theta,eta)*dnorm(y,mean = xt,sd=sd)
  if (u[i] <= num/den){
      x[i] <- y
  } else {
      x[i] <- xt
      k<-k+1
  }
}
x.new<-x[-(1:500)]# discard the first 500 samples
perc<-seq(.1,.9,.1)
Result<-data.frame(matrix(NA,2,9))
Result[1,]<-round(quantile(x.new,probs=perc),4)
Result[2,]<-round(qcauchy(perc),4)
colnames(Result)<-c("10%","20%","30%","40%","50%","60%","70%","80%","90%")
rownames(Result)<-c("MCMC","Cauchy")
knitr::kable(Result)

## -----------------------------------------------------------------------------
set.seed(13010)
Gelman.Rubin <- function(psi) {
    # psi[i,j] is the statistic psi(X[i,1:j])
    # for chain in i-th row of X
    if(is.vector(psi)==T) psi <- t(as.matrix(psi))
    n <- ncol(psi)
    k <- nrow(psi)
    
    psi.means <- rowMeans(psi)     #row means
    B <- n * var(psi.means)        #between variance est.
    psi.w <- apply(psi, 1, "var")  #within variances
    W <- mean(psi.w)               #within est.
    v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
    r.hat <- v.hat / W             #G-R statistic
    return(r.hat)
}
cauchy.chain<-function(theta,eta,sd,N,x1){
## sd is the sd of proposal function, N is size,
## x1 is the initial value
  
## return chains and psi
k<-length(x1)
x<-numeric(N)
chain<-matrix(NA,k,N)
chain[,1]<-x1
u<-matrix(runif(N*k),k,N)
psi<-matrix(NA,k,N)
psi[,1]<-x1
Count<-1
rhat <- rep(0,N)
for (i in 2:N) {
  for (j in 1:k) {
  xt<-chain[j,i-1]
  y<-rcauchy(1,location=xt)
  num<-fcauchy(y,theta,eta)*dcauchy(xt,location=y)
  den<-fcauchy(xt,theta,eta)*dcauchy(y,location=xt)
  if (u[j,i] <= num/den){
      chain[j,i] <- y
  } else {
      chain[j,i] <- xt
  }
  psi[j,i]<-mean(chain[j,1:i])
  }
  rhat[i]<-Gelman.Rubin(psi[,1:i])
  if(rhat[i]<=1.2) break;# when rhat <=1.2, stop running the chains
  Count<-i
  }
return(list(chain=chain[,1:Count],rhat=rhat[1:Count]))
}
x0<-c(-5,-2,2,5)
#x0<-c(-5,0,5)
theta<-1
eta<-0
sd<-2
n<-2000
b<-100 # burn-in length
result_CC<-cauchy.chain(theta,eta,sd,n,x0)
## result_CC contains the chains with different initial values and the rhat vector 
plot(result_CC$rhat[-(1:b)],type="l",ylab = "Rhat")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(11002)
a<-2
b<-2
n<-20
N<-1000
burn<-100
y<-x<-numeric(N)
x[1]<-x0<-0
y[1]<-y0<-0.2
for (i in 2:N) {
  yt<-y[i-1]
  xt<-x[i]<-rbinom(1,n,yt)
  y[i]<-rbeta(1,xt+a,n-xt+b)
}
chain<-cbind(x,y)
colMeans(chain)
plot(chain,type = "p",pch=20,main = "Point Plot without discard")
## discard the first 1000 samples
chain.new<-chain[-(1:burn),]
colMeans(chain.new)
plot(chain.new,type = "p",pch=20,main = "Point Plot with discard")

## -----------------------------------------------------------------------------
set.seed(12003)
bi.chain<-function(a,b,n,N,x0){
## a, b, n is parameters
## N is the biggest length of chains
## x0 is the initial values, x0 is a 2*k matrix
  k<-ncol(x0)
  x.chain<-matrix(NA,k,N)
  y.chain<-matrix(NA,k,N)
  x.chain[,1]<-x0[1,]
  y.chain[,1]<-x0[2,]

  u<-matrix(runif(N*k),k,N) 
  x.psi<-y.psi<-matrix(NA,k,N)
  x.psi[,1]<-x.chain[,1]
  y.psi[,1]<-y.chain[,1]
  Count<-1
  x.rhat<-y.rhat <- rep(0,N)
  
  for (i in 2:N) {
    for (j in 1:k) {
      yt<-y.chain[j,i-1]
      xt<-x.chain[j,i]<-rbinom(1,n,yt)
      y.chain[j,i]<-rbeta(1,xt+a,n-xt+b)
      x.psi[j,i]<-mean(x.chain[j,1:i])
      y.psi[j,i]<-mean(y.chain[j,1:i])
    }
    x.rhat[i]<-Gelman.Rubin(x.psi[,1:i])
    y.rhat[i]<-Gelman.Rubin(y.psi[,1:i])
    if(x.rhat[i]<=1.2&y.rhat[i]<=1.2) break;
    Count<-i
  }
  chain<-list(x.chain=x.chain[,1:Count],y.chain=y.chain[,1:Count])
  rhat<-list(x.rhat=x.rhat[1:Count],y.rhat=y.rhat[1:Count])
  return(list(chain=chain,rhat=rhat))
}
a<-2
b<-2
n<-20
N<-9000
burn<-100
x0<-matrix(c(0,0.2,10,0.2,10,0.8,20,0.5),2,4)
result_bi<-bi.chain(a,b,n,N,x0)
plot(result_bi$rhat$x.rhat[-(1:100)],type="l",ylab = "Rhat",col=2)
lines(result_bi$rhat$y.rhat[-(1:100)],col=3)
abline(h=1.2, lty=2)
legend("topright",legend = c("x.rhat","y.rhat"),lty = 1,col = c(2,3),title = "Rhat")

## -----------------------------------------------------------------------------
term.func<-function(k,a){
## a is a vector
  d<-length(a)
  l<-exp((2*k+2)*log(norm(a,type = "2"))-log(2*k+1)-log(2*k+2)-k*log(2)+lgamma((d+1)/2)+lgamma(k+1.5)-lgamma(k+d/2+1)-lgamma(k+1))
  b<-(-1)^k*l
  return(b)
}
term.func(2^5,c(1:(2^5)))
term.func(0,c(1,2))

## -----------------------------------------------------------------------------
sum.func<-function(a){
  dis<-.Machine$double.neg.eps # convergence distance
  Sum<-0
  Count<-0
  for (k in 0:1000) {
    Sum1<-term.func(k,a)
    if(abs(Sum1)>dis){
      Sum<-Sum+Sum1
      Count<-Count+1
    }else{break;}
  }
  if(Count==1000) print("The convergence is very slow or fails!")
  return(Sum)
}

## -----------------------------------------------------------------------------
a<-c(1,2)
R<-sum.func(a)

## -----------------------------------------------------------------------------
K<-c(4:25,100,500,1000)
Cka <- function(k,a) {
  sqrt(a^2*k/(k+1-a^2))
}
S.diff<-function(k,a){
  ## S_{k}(a)-S_{k-1}(a)
  pt(Cka(k-1,a),k-1)-pt(Cka(k,a),k)
}
#par(mfrow=c(3,3))
for (k in 1:length(K)) {
  N<-1000
  A<-seq(0.01,sqrt(K[k])-1e-3,0.01)
  N_A<-length(A)
  S<-rep(N_A)
  for (i in 1:N_A) {
    S[i]<-S.diff(K[k],A[i])
  }
  plot(A,y=S,type="l",xlim = c(0,A[N_A]),xlab = "a",ylab = "S.diff",main=paste0("k=",K[k]))
  abline(h=0,col=3)
}

## -----------------------------------------------------------------------------
Root<-data.frame(matrix(0,2,length(K)))
Root[1,]<-K
row.names(Root)<-c("k","root")
for (k in 1:length(K)) {
  Root[2,k]<-uniroot(function(a){S.diff(K[k],a)},lower = 1e-8,upper =min(sqrt(K[k])-1e-8,4))$root
}
knitr::kable(t(Root))

## -----------------------------------------------------------------------------
f<-function(k,a){
  lgamma((k+1)/2)-log(sqrt(pi*k))-lgamma(k/2)+log(integrate(function(u){(1+u^2/k)^(-(k+1)/2)},0,Cka(k,a))$value)-(lgamma(k/2)-log(sqrt(pi*(k-1)))-lgamma((k-1)/2)+log(integrate(function(u){(1+u^2/(k-1))^(-k/2)},0,Cka(k-1,a))$value))
}
Root.new<-data.frame(matrix(0,3,length(K)))
Root.new[1:2,]<-Root
for (k in 1:length(K)) {
  Root.new[3,k]<-uniroot(function(a){f(K[k],a)},upper =min(sqrt(K[k])-1e-3,3) ,lower =1e-8)$root
}
knitr::kable(t(Root.new),col.names = c("k","root_11.4","root_11.5"))

## -----------------------------------------------------------------------------
Y<-c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
yo<-c(0.54, 0.48, 0.33, 0.43, 0.91,0.21, 0.85)
tol<-.Machine$double.eps^0.25
N<-100
lambda1<-lambda0<-1
for (i in 1:N) {
  lambda2<-(3*lambda1+sum(Y))/10
  if(abs(lambda2-lambda1)<tol){
    break
  }
  lambda1<-lambda2
}
print(list(lambda=lambda2,iter=i))

## -----------------------------------------------------------------------------
mlogL<-function(lambda=1){
  return(-(-length(yo)*log(lambda)-(sum(yo)+3)/lambda))
}
library(stats4)
fit<-mle(mlogL)
print(fit@coef)

## ----eval=FALSE---------------------------------------------------------------
#  trims <- c(0, 0.1, 0.2, 0.5)
#  x <- rcauchy(100)
#  lapply(trims, function(trim) mean(x, trim = trim))
#  lapply(trims, mean, x = x)

## -----------------------------------------------------------------------------
## formulas in Ex3
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
rsq <- function(mod) summary(mod)$r.squared
Lm<-lapply(formulas,lm,data=mtcars)
R2<-lapply(Lm, rsq)
print(unlist(R2)) # R square

## -----------------------------------------------------------------------------
## bootstrap
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})

Lm2<-lapply(bootstraps, lm, formula=mpg~disp)
R2<-lapply(Lm2, rsq)
print(unlist(R2)) #R square

## -----------------------------------------------------------------------------
vapply(mtcars, sd, numeric(1))

## -----------------------------------------------------------------------------
mkdata<-data.frame(matrix(NA,5,3))
mkdata[,1]<-c("2","2","3","5","4")
mkdata[,2]<-rnorm(5)
mkdata[,3]<-as.factor(sample(c(0,1),5,replace = T))
colnames(mkdata)<-c("character","numeric","factor")
mkdata<-data.frame(vapply(mkdata, as.numeric ,numeric(5)))
vapply(mkdata,sd,numeric(1))

## ---- eval=FALSE--------------------------------------------------------------
#  library(parallel)
#  ## By default mc.cores=2
#  mcsapply<-function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, mc.preschedule = TRUE,
#   mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
#    mc.cleanup = TRUE, mc.allow.recursive = TRUE, affinity.list = NULL )
#  {
#    ## use mclapply to calculate answer with list structure
#    answer <- mclapply(X = X, FUN = FUN, ...,mc.preschedule = mc.preschedule,
#  mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores,
#    mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive, affinity.list = affinity.list)
#    if (USE.NAMES && is.character(X) && is.null(names(answer)))
#      names(answer) <- X
#    if (!isFALSE(simplify) && length(answer))
#      simplify2array(answer, higher = (simplify == "array")) # translate structure to vector
#    else answer
#  }
#  ## use data mtcars to check function
#  mcsapply(mtcars,mean)

## ----eval=FALSE---------------------------------------------------------------
#  vapply<-function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
#  {
#    FUN <- match.fun(FUN)
#    if (!is.vector(X) || is.object(X))
#      X <- as.list(X)
#    .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
#  }

## -----------------------------------------------------------------------------
library(Rcpp)
cppFunction('NumericMatrix gibbsC(int a, int b, int n, int N, int thin) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0.2;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      x = rbinom(1, n, y)[0];
      y = rbeta(1, x + a, n - x + b)[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}')

## -----------------------------------------------------------------------------
#cpp_dir<-'E:/Learn/Statistical Computing/Homework/A-21081-2021-12-02/'
#sourceCpp(paste0(cpp_dir,'gibbsC.cpp'))
a<-b<-2
n<-20
thin<-10
N<-2000
resultC<-gibbsC(a = a,b = b, n = n,N = N,thin = thin)
burn<-1000
## show last 20 samples
resultC[(N-20+1):N,]

## -----------------------------------------------------------------------------
## gibbsR
gibbsR <- function(a, b, n, N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- 0
  y <- 0.2
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rbinom(1, n, y)
      y <- rbeta(1, x+a, n-x+b)
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

## -----------------------------------------------------------------------------
#source(paste0(cpp_dir,'gibbsR.R'))
## generate samples
resultR<-gibbsR(a,b,n,N=N,thin = thin)
## burn first 100 samples
resultR<-resultR[(burn+1):N,]
resultC<-resultC[(burn+1):N,]
## plot 
ap<-ppoints(100)
xC<-quantile(resultC[,1],ap)
xR<-quantile(resultR[,1],ap)
qqplot(x=xR,y=xC,xlab = "R",ylab = "Rcpp",
       main="Comparation of x generated by R and Rcpp ")
abline(0,1,col="red")
yC<-quantile(resultC[,2],ap)
yR<-quantile(resultR[,2],ap)
qqplot(x=yR,y=yC,xlab = "R",ylab = "Rcpp",
       main="Comparation of y generated by R and Rcpp ")
abline(0,1,col="red")

## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(gibbR=gibbsR(a,b,n,N,thin),
gibbC=gibbsC(a,b,n,N,thin))
summary(ts)[,c(1,3,5,6)]


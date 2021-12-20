## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = T)

## ----echo=TRUE----------------------------------------------------------------
library(tseries)

## -----------------------------------------------------------------------------
set.seed(1234)  
M<- 10000  
t<- runif(M,0,pi/6)  
est<-mean(cos(t))*(pi/6)  
tru<-sin(pi/6)-sin(0)  
print(c(est,tru))  

## -----------------------------------------------------------------------------
f<- function(x,y){z<- 1/(2*pi)*exp(-x^2-y^2)}
x<- y<- seq(-3,3,length=30)
z<- outer(x,y,f)
persp(x,y,z,theta=45,phi=30,expand=0.5,ltheta=-100,shade=0.2,col = "yellow",main='Figure 1')

## -----------------------------------------------------------------------------
library(datasets) 
library(xtable)
xtable(head(cars)) 

## -----------------------------------------------------------------------------
#install.packages("kableExtra")
library(kableExtra)
set.seed(1234)
a<- rnorm(100)
b<- rnorm(100)
tt<- summary(cbind(a,b))
kable(tt,align='c',caption = 'Table 1',
             booktabs = TRUE) %>% 
  kable_styling("responsive",full_width = F)%>% 
  row_spec(c(1,3,5), bold = T, color = "white", background = "blue")

## ----echo=TRUE----------------------------------------------------------------
library(tseries)
sigma<- 2
m<- 10000
U<- runif(m)
x<- sqrt(-2*sigma^2*log(U))
hist(x,prob=T,main=expression(f(x)))
y<- seq(min(x),max(x),length=m)
lines(y,y/(sigma^2)*exp(-y^2/(2*sigma^2)))

## -----------------------------------------------------------------------------
n2<- 1000
x1<- rnorm(n2,0,1)
x2<- rnorm(n2,3,1)
p<- 0.75
r<- sample(c(0,1),n2,prob=c(p,1-p),replace=T)
z<- r*x1+(1-r)*x2
hist(z,main='histogram of z (p=0.75)')

## -----------------------------------------------------------------------------
P<- seq(0,1,0.1)
#par(mfrow=c(3,4))
for (p in P){r<- sample(c(0,1),n2,prob=c(p,1-p),replace=T)
zp<- r*x1+(1-r)*x2
hist(zp,main=bquote('histogram of z :p='*.(p)))}

## -----------------------------------------------------------------------------
lamada<- 8 #the parameter of Poisson process
alpha<- 4  #the parameter1 of Gamma distribution
beta<- 3 #the parameter2 of Gamma distribution
n3<- 10000
t<- 10

N<- rpois(n3,lamada*t)
x10<- matrix(0,n3)
for (i in 1:n3){Y<- rgamma(N[i],alpha,beta)
x10[i]<- sum(Y)}

# theoretical values
Ex10<- lamada*t*alpha/beta
Vx10<- lamada*t*(alpha/beta^2+(alpha/beta)^2)

library(knitr)
library(kableExtra)
value=data.frame(statistics=c("sample mean","sample variance","population expect","population variance"),
             values=c(mean(x10),var(x10),Ex10,Vx10))
kable(value,align='l',full_width = T) 

## -----------------------------------------------------------------------------
set.seed(123)
m<- 10000
x<- seq(0.1,0.9,length=9)
betacdf<- numeric(length(x))
for (i in 1:length(x)){
  t<- runif(m,0,x[i])
  betacdf[i]<- x[i]*mean(30*t^2*(1-t)^2)
}
pbeta<- pbeta(x,3,3)
cbind(pbeta,betacdf)

## -----------------------------------------------------------------------------
set.seed(123)
# Use the function to calculate the integral value estimated by the two methods
ant <- function(x, sigma, R = 10000, antithetic = TRUE) {
  t1 <- runif(R/2,0,x)
  if (!antithetic) t2 <- runif(R/2) else t2 <- 1 - t1
  t <- c(t1, t2)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i] * t/sigma^2*exp(-t^2 / (2*sigma^2))
    cdf[i] <- mean(g)
  }
  cdf
}
m <- 1000
MC1 <- MC2 <- numeric(m)
x <- 1
sigma<-1
for (i in 1:m) {
  MC1[i] <- ant(x, sigma,R = 1000, anti = FALSE)
  MC2[i] <- ant(x, sigma,R = 1000)
}
mean(MC1) #Integral calculated by simple random number sample
mean(MC2) #Integral calculated by opposing a sample of random numbers
redu_var<- 100*(var(MC1)-var(MC2))/var(MC1) #Calculate the percentage of variance reduction using the opposite method
print(redu_var)

## -----------------------------------------------------------------------------
set.seed(123)
# Add the calculation process of the estimated integral directly in the loop
m<- 1000
n3<- 10000
sigma<- 1
eta_sim<-eta_ant<- numeric(m)
x<- 1 #Set the upper limit of points
for (i in 1:m){
t1<- runif(n3/2,0,x) 
t2<- runif(n3/2,0,x) 
t3<- x-t1 
u1<- c(t1,t2) 
u2<- c(t1,t3) 
eta_sim[i]<- mean(x*u1/sigma^2*exp(-u1^2/(2*sigma^2)))
eta_ant[i]<- mean(x*u2/sigma^2*exp(-u2^2/(2*sigma^2)))
}
mean(eta_sim) 
mean(eta_ant) 
redu_var<- 100*(var(eta_sim)-var(eta_ant))/var(eta_sim) 
print(redu_var)

## -----------------------------------------------------------------------------
set.seed(123)
m<- 10000
sigma<- 1
theta_hat<- numeric(2)
se<- numeric(2)
g<- function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)
}
# f1
x1<- rnorm(m) 
i<- c(which(x1<1)) 
x1[i]<- 0 
fg1<- x1^2
theta_hat[1]<- mean(fg1)
se[1]<- sd(fg1)
# f2
U<- runif(m, 0, 1)
x2<- sqrt(-2*log(1-U))*sigma 
j<- c(which(x2<1)) 
x2[j]<- 0 
fg2<- x2/sqrt(2*pi)
theta_hat[2]<- mean(fg2)
se[2]<- sd(fg2)
rbind(theta_hat,se)

## -----------------------------------------------------------------------------
set.seed(123)
m<- 10000
sigma<- 1
x4<- rnorm(m) 
i<- c(which(x4<1)) 
x4[i]<- 0 
g<- x4^2
theta_hat<- mean(g)
print(theta_hat)

## -----------------------------------------------------------------------------
m<- 1000
n<- 20
lcl<- rcl<- numeric(m)
for (i in 1:m){
chi<- rchisq(20,2)
lcl[i]<- mean(chi)-sd(chi)*qt(0.975,n-2)/sqrt(n)
rcl[i]<- mean(chi)+sd(chi)*qt(0.975,n-2)/sqrt(n)
}
mean(lcl<=2 & 2<=rcl)

## -----------------------------------------------------------------------------
##Chisq
set.seed(123)
m<- 10000
n<-1000
t<- numeric(m)
C<- replicate(m, expr = {x<- rchisq(n,1)
sqrt(n)*(mean(x)-1)/sd(x)})
pi1<- 2*(1-pt(abs(C),n-2))
mean(pi1<=0.05)

##unif
U<- replicate(m, expr = {y<- runif(n,0,2)
sqrt(n)*(mean(y)-1)/sd(y)})
pi2<- 2*(1-pt(abs(U),n-2))
mean(pi2<=0.05)

##exp
E<- replicate(m, expr = {z<- runif(n,0,2)
sqrt(n)*(mean(z)-1)/sd(z)})
pi3<- 2*(1-pt(abs(E),n-2))
mean(pi3<=0.05)

## ----echo=TRUE----------------------------------------------------------------
set.seed(1021)
library(MASS)
m<- 10000 
d<- 5 
n<- c(10,20,30,50,100,500) 
cvs<- 6*qchisq(0.95,d*(d+1)*(d+2)/6)/n 
p_reject<- numeric(length(n)) #Type-I-error
meanX<- matrix(0,d,1) 
sigmaX<- diag(rep(1,d)) 
b1<- numeric(m)
for (j in 1:length(n)){
Mnsktest<- numeric(m)

 for (i in 1:m){
   X<- mvrnorm(n[j],meanX,sigmaX)
   x<- scale(X) 
   b1[i]<- 1/(n[j])^2*sum((x %*% t(x))^3) #skewness test statistic
   Mnsktest[i]<- as.integer(b1[i] >= cvs[j])
 }
p_reject[j]<- mean(Mnsktest)
}
p_reject

## ----echo=TRUE----------------------------------------------------------------
# mixture distribution
library(MASS)
mix<- function(n,e,d,mu,sigma1,sigma10){
   X<- matrix(0,n,d)
   for (k in 1:n){
      s<- rbinom(1,size=1,prob=e)
      if (s) 
         X[k,]<- mvrnorm(1,mu,sigma10) else
         X[k,]<- mvrnorm(1,mu,sigma1)
   }
   return(X)
}

## ----echo=TRUE----------------------------------------------------------------
set.seed(1021)
library(MASS)
alpha<- 0.1
d<- 3
n<- 30
m<- 1000
b2<- numeric(m)
Mnsktest<- numeric(m)
eps<- c(seq(0,0.25,0.01),seq(0.25,1,0.05))
l<- length(eps)
pow<- numeric(l)
cvs<- 6*qchisq(0.9,d*(d+1)*(d+2)/6)/n
mu<- rep(0,d)
sigma1<- diag(1,d,d)
sigma10<- diag(100,d,d)
for (k in 1:l){
   ep<- eps[k]
   for (s in 1:m){
      X<- mix(n,ep,d,mu,sigma1,sigma10) 
      x<- scale(X)
   b2[s]<- 1/n^2*sum((x %*% t(x))^3)
   Mnsktest[s]<- as.integer(b2[s] >= cvs)
   }
   pow[k]<- mean(Mnsktest)
}
pow
paste("The power is the highest when e is about",eps[which.max(pow)])#Find the e corresponding to the maximum power
plot(eps,pow,type='b',xlab=bquote(eps),ylim=c(0,1),main="Power of demension 5")
abline(h=alpha,lty=3)
lines(eps,pow+sqrt(pow*(1-pow)/m),lty=3)
lines(eps,pow-sqrt(pow*(1-pow)/m),lty=3)


## ----echo=TRUE----------------------------------------------------------------
set.seed(1028)
library(bootstrap)
Sigma_hat<- var(scor) 
lambda_hat<- eigen(Sigma_hat) 
theta_hat<- max(lambda_hat$values)/sum(lambda_hat$values) 

B<- 500
n<- nrow(scor)
theta_b<- numeric(B)

# bootstrap steps
for (b in 1:B){
  i<- sample(1:n,size=n, replace=T)
  scor_b<- scor[i,1:5]
  Sigma_b<- var(scor_b)
  lambda_b<- eigen(Sigma_b)
  theta_b[b]<- max(lambda_b$values)/sum(lambda_b$values)
}

bias_b<- mean(theta_b)-theta_hat 
se_b<- sd(theta_b)
cat("the bias of bootstrap =", bias_b, "the standard error of bootstrap =", se_b)

## ----echo=TRUE----------------------------------------------------------------
set.seed(1028)
theta_j<- numeric(n)
for (i in 1:n){
  Sigma_j<- var(scor[-i,])
  lambda_j<- eigen(Sigma_j)
  theta_j[i]<- max(lambda_j$values)/sum(lambda_j$values)
}

bias_j<- (n-1)*(mean(theta_j)-theta_hat) 
se_j<- sqrt((n-1)*mean((theta_j-mean(theta_j))^2))
cat("the bias of jackknife =", bias_j, "the standard error of jackknife =", se_j)

## -----------------------------------------------------------------------------
set.seed(1028)
library(boot)
eta.interval<- function(x,i){
  #i<-sample(n,replace=T)
  lambda.star<- eigen(cor(x[i,]))
  theta.star<- max(lambda.star$values)/sum(lambda.star$values)
}

n<- ncol(scor)
m<-1000
sta<- boot(data=scor,statistic=eta.interval,R=999)
ci<- boot.ci(sta,type=c("perc","bca"))
print(ci)

## ----echo=TRUE----------------------------------------------------------------
set.seed(1028)
library(fBasics)
ske.interval<- function(x,i) skewness(x[i])
m<-1000
ci1.n<-ci1.p<- ci1.b<-ci2.n<-ci2.p<- ci2.b<- matrix(NA,m,2)
n<-100
for (k in 1:m){
x.norm<- rnorm(n)
sta.norm<- boot(data=x.norm,statistic=ske.interval,R=1000)
ci.norm<- boot.ci(sta.norm,type=c("norm","perc","basic"))
ci1.n[k,]<- ci.norm$norm[2:3]
ci1.p[k,]<- ci.norm$percent[4:5]
ci1.b[k,]<- ci.norm$basic[4:5]
}
cp1.n<- mean(ci1.n[,1]<= 0 & ci1.n[,2]>= 0)
cp1.p<- mean(ci1.p[,1]<= 0 & ci1.p[,2]>= 0) 
cp1.b<- mean(ci1.b[,1]<= 0 & ci1.b[,2]>= 0)
misl1.n<- sum(ci1.n[,1]>= 0)
misr1.n<- sum(ci1.n[,2]<= 0)
misl1.p<- sum(ci1.p[,1]>= 0)
misr1.p<- sum(ci1.p[,2]<= 0)
misl1.b<- sum(ci1.b[,1]>= 0)
misr1.b<- sum(ci1.b[,2]<= 0)

cat("norm=",cp1.n,"percentile=",cp1.p,"basic=",cp1.b)
cat("miss on the left=",cbind(misl1.n,misl1.p,misl1.b))
cat("miss on the right=",cbind(misr1.n,misr1.p,misr1.b))

ske.chisq<- sqrt(8/5) 
for (k in 1:m){
x.chisq<- rchisq(n,5)
sta.chisq<- boot(data=x.chisq,statistic=ske.interval,R=1000)
ci.chisq<- boot.ci(sta.chisq,type=c("norm","perc","basic"))
ci2.n[k,]<- ci.chisq$norm[2:3]
ci2.p[k,]<- ci.chisq$percent[4:5]
ci2.b[k,]<- ci.chisq$basic[4:5]
}
cp2.n<- mean(ci2.n[,1]<= ske.chisq & ci2.n[,2]>= ske.chisq)
cp2.p<- mean(ci2.p[,1]<= ske.chisq & ci2.p[,2]>= ske.chisq) 
cp2.b<- mean(ci2.b[,1]<= ske.chisq & ci2.b[,2]>= ske.chisq)
misl2.n<- sum(ci2.n[,1]>= ske.chisq)
misr2.n<- sum(ci2.n[,2]<= ske.chisq)
misl2.p<- sum(ci2.p[,1]>= ske.chisq)
misr2.p<- sum(ci2.p[,2]<= ske.chisq)
misl2.b<- sum(ci2.b[,1]>= ske.chisq)
misr2.b<- sum(ci2.b[,2]<= ske.chisq)

cat("norm=",cp2.n,"percentile=",cp2.p,"basic=",cp2.b)
cat("miss on the left=",cbind(misl2.n,misl2.p,misl2.b))
cat("miss on the right=",cbind(misr2.n,misr2.p,misr2.b))

## -----------------------------------------------------------------------------
set.seed(1109)
n<- 1000
X<- rnorm(n,1,4)
Y<- rnorm(n,1,4)
B<- 5000
spcor<- numeric(B)
Z<-c(X,Y)
N<- length(Z)
spcor0<- cor(X,Y,method='spearman') # spearman
p0<-cor.test(X,Y) 
# Shuffle the original sample order
for (b in 1:B){
  i<- sample(1:N,size=n,replace=F)
  Xp<- Z[i];Yp<-Z[-i]
  spcor[b]<- cor(Xp,Yp,method='spearman')
}
p<- mean(abs(c(spcor0,spcor))>=spcor0) 
round(c(p0$p.value,p),3)

## -----------------------------------------------------------------------------
# NN test function
library(MASS)
Tm <- function(h,i,N,k) {
  p <- N[1]; q <- N[2]; l <- p + q
  if(is.vector(h)) h <- data.frame(h,0);
  h <- h[i, ];
  nn <- nn2(data=h, k=k+1)
  qj1 <- nn$nn.idx[1:p,-1]
  qj2 <- nn$nn.idx[(p+1):l,-1]
  c1 <- sum(qj1 < p + .5); c2 <- sum(qj2 > p+.5)
  (c1 + c2) / (k * l)
}


eqdist.nn <- function(h,N,k){
  boot.obj <- boot(data=h,statistic=Tm,R=R, sim = "permutation", N = N,k=k)
  tt <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(tt>=tt[1])
  list(statistic=tt[1],p.value=p.value)
}


## -----------------------------------------------------------------------------
library(MASS)
library(RANN)
library(boot)
library(energy)
library(Ball)
B<- 100
k<- 3
R<- 999
p.values<- matrix(NA,B,3)
N<-n<- c(20,30)
mu<- c(0,1)
sigma<- c(1,4)

## -----------------------------------------------------------------------------
set.seed(1104)
mu1 <- matrix(mu[1],2,1)
sigma11 <- diag(sigma[1],2)
sigma12 <- diag(2*sigma,2)

for (b in 1:B){
x1<- mvrnorm(n[1],mu1,sigma11)
y1<- mvrnorm(n[2],mu1,sigma12)
z1<- rbind(x1,y1)
p.values[b,1] <- eqdist.nn(z1,N,k)$p.value
p.values[b,2] <- eqdist.etest(z1,N,R=R)$p.value
p.values[b,3] <- bd.test(x=x1,y=y1,num.permutations = R,seed=b*1104)$p.value
}

alpha<-0.05
pow<- colMeans(p.values<alpha)
cat('NN power=',pow[1],'Energy power=',pow[2],'Ball power=',pow[3])

## -----------------------------------------------------------------------------
# (2)
set.seed(1104)
mu21<- matrix(mu[1],2,1)
mu22<- matrix(2*mu,2,1)
sigma21<- diag(sigma,2)
sigma22<- diag(2*sigma,2)

for (b in 1:B){
x2<- mvrnorm(n[1],mu21,sigma21)
y2<- mvrnorm(n[2],mu22,sigma22)
z2<- rbind(x2,y2)
p.values[b,1] <- eqdist.nn(z2,N,k)$p.value
p.values[b,2] <- eqdist.etest(z2,N,R=R)$p.value
p.values[b,3] <- bd.test(x=x2,y=y2,num.permutations = R,seed=b*1104)$p.value
}

alpha<-0.05
pow<- colMeans(p.values<alpha)
cat('NN power=',pow[1],'Energy power=',pow[2],'Ball power=',pow[3])

## -----------------------------------------------------------------------------
set.seed(1104)
for (b in 1:B){
x31<- rt(n[1],1,2)
y31<- rt(n[2],2,4)
z31<- c(x31,y31)
p.values[b,1] <- eqdist.nn(z31,N,k)$p.value
p.values[b,2] <- eqdist.etest(z31,N,R=R)$p.value
p.values[b,3] <- bd.test(x=x31,y=y31,num.permutations = R,seed=b*1104)$p.value
}

alpha<-0.05
pow<- colMeans(p.values<alpha)
cat('NN power=',pow[1],'Energy power=',pow[2],'Ball power=',pow[3])

## -----------------------------------------------------------------------------
set.seed(1104)
e1<- 0.3
e2<- 0.5

for (b in 1:B){
n1<- sample(1:2,n[1],prob=c(e1,1-e1),replace=T)
n2<- sample(1:2,n[2],prob=c(e2,1-e2),replace=T)
x32<- mvrnorm(n[1],matrix(mu[1],2,1),diag(sigma[1],2))
y32<- mvrnorm(n[2],matrix(mu[2],2,1),diag(sigma,2))
z32<- rbind(x32,y32)
p.values[b,1] <- eqdist.nn(z32,N,k)$p.value
p.values[b,2] <- eqdist.etest(z32,N,R=R)$p.value
p.values[b,3] <- bd.test(x=x32,y=y32,num.permutations = R,seed=b*1104)$p.value
}

alpha<-0.05
pow<- colMeans(p.values<alpha)
cat('NN power=',pow[1],'Energy power=',pow[2],'Ball power=',pow[3])

## -----------------------------------------------------------------------------
set.seed(1104)
m1<- 5
m2<- 45
N4<- c(m1,m2)

for (b in 1:B){
x4<- mvrnorm(m1,matrix(mu[1],2,1),diag(sigma[1],2))
y4<- mvrnorm(m2,matrix(mu[2],2,1),diag(sigma[2],2))
z4<- rbind(x4,y4)
p.values[b,1] <- eqdist.nn(z4,N4,k)$p.value
p.values[b,2] <- eqdist.etest(z4,N4,R=R)$p.value
p.values[b,3] <- bd.test(x=x4,y=y4,num.permutations = R,seed=b*1104)$p.value
}

alpha<-0.05
pow<- colMeans(p.values<alpha)
cat('NN power=',pow[1],'Energy power=',pow[2],'Ball power=',pow[3])

## ----echo=T-------------------------------------------------------------------
    set.seed(1111)
    cauchy <- function(x,theta,eta) {
        return(1/(theta*pi*(1+((x-eta)/theta)^2))) # cauchy distribution pdf
}
    rep <- 10000
    theta <- 1
    eta<- 0
    sigma<- 0.5
    x <- numeric(rep)
    x[1] <- 1 
    k <- 0
    u <- runif(rep)

    for (i in 2:rep) {
        xt <- x[i-1]
        y<- rnorm(1,xt,sigma) # normal distribution as proposal distribution
        num <- cauchy(y,theta,eta)
        den <- cauchy(xt,theta,eta)
        if (u[i] <= num/den){
          x[i] <- y
        } else {
          x[i] <- xt
          k <- k+1     # reject y, repeat steps above
        }
          }
    
    index <- 9500:10000
    y1 <- x[index]
    plot(index, y1, type="l", main="Part of a chain generated by M-H sampler of a Cauchy distribution", ylab="x") 
  
    M<- 5 
    burn <- 1001      
    y <- x[burn:rep]
    QT <- qcauchy(0:9/10)  
    QC <- quantile(y, (0:9)/10) 
    QT[1]<-QC[1]<- -M
    QT[10]<-QC[10]<- M

## ----echo=T-------------------------------------------------------------------
    qqplot(QC, QT,main="QQ plot for a M-H chain")
    abline(0,1,col='blue',lwd=2)
    hist(y, breaks="scott", main="Histogram with target Cauchy density",ylim=c(0,0.35), freq=FALSE)
    z<- seq(min(y),max(y),length=1000)
    lines(z, cauchy(z,theta,eta),col='red')

## -----------------------------------------------------------------------------
chain<- 5000
burn<- 4000
n<- 20
a<- 2
b<- 2
Z<- matrix(0, chain, 2) 
Z[1,1]<- 1 # initial 
Z[1,2]<- 0.5

for (i in 2:chain) {
Y <- Z[i-1, 2]
Z[i, 1] <- rbinom(1,n,Y)
X<- Z[i, 1]
Z[i, 2] <- rbeta(1,X+a,n-X+b)
}

b <- burn + 1
z <- Z[b:chain, ]

plot(z[,1],type='l',col=1,lwd=2,xlab='Index',ylab='Random numbers')
lines(z[,2],col=2,lwd=2)
legend('bottomright',c(expression(x),expression(y)),col=1:2,lwd=2)

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
        # psi[i,j] is the statistic psi(X[i,1:j])
        # for chain in i-th row of X
        psi <- as.matrix(psi)
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

## -----------------------------------------------------------------------------
set.seed(0)
cauchy <- function(x,theta,eta) {
        return(1/(theta*pi*(1+((x-eta)/theta)^2))) 
}

cauchy.chain <- function(theta, eta, chain, sigma, X1) {
        #generates a Metropolis chain for Cauchy(1,0)
        #with Normal(X[t], sigma) proposal distribution
        #and starting value X1
        x <- rep(0, chain)
        x[1] <- X1
        u <- runif(chain)

        for (i in 2:chain) {
            xt <- x[i-1]
            y<- rnorm(1,xt,sigma)
            r1 <- cauchy(y, theta, eta)
            r2 <- cauchy(xt, theta, eta)
            r <- r1 / r2
            if (u[i] <= r) x[i] <- y else
                 x[i] <- xt
            }
        
        return(x)
        }

    theta<- 1 #parameter of proposal distribution
    eta<- 0
    sigma<- 1
    k <- 4          #number of chains to generate
    chain <- 15000      #length of chains
    burn <- 1000       #burn-in length

    #choose overdispersed initial values
    x0 <- c(-3, -1, 1, 3)

    #generate the chains
   
    X <- matrix(0, nrow=k, ncol=chain)
    for (i in 1:k)
        X[i, ] <- cauchy.chain(theta,eta,chain,sigma,x0[i])

    #compute diagnostic statistics
    psi <- t(apply(X, 1, cumsum))
    for (i in 1:nrow(psi))
        psi[i,] <- psi[i,] / (1:ncol(psi))

    #plot psi for the four chains
    for (i in 1:k)
      if(i==1){
        plot((burn+1):chain,psi[i, (burn+1):chain],ylim=c(-1,1), type="l",
            xlab='Index', ylab=bquote(phi))
      }else{
        lines(psi[i, (burn+1):chain], col=i)
    }
    par(mfrow=c(1,1)) #restore default
    

## -----------------------------------------------------------------------------
#plot the sequence of R-hat statistics
    rhat <- rep(0, chain)
    for (j in (burn+1):chain)
        rhat[j] <- Gelman.Rubin(psi[,1:j])
    plot(rhat[(burn+1):chain], type="l", xlab="", ylab="R")
    abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
set.seed(1111)
library(coda)
 f.chain <- function(n,a,b,chain,x0,y0) {
        #Use the Gibbs sampler to generates a chain for f(x,y)
        #and starting value (x0,y0)
        Z <- matrix(0,chain,2)
        Z[1,1]<- x0
        Z[1,2]<- y0

        for (i in 2:chain) {
        Y <- Z[i-1, 2]
        Z[i, 1] <- rbinom(1,n,Y)
        X<- Z[i, 1]
        Z[i, 2] <- rbeta(1,X+a,n-X+b)
        }
        return(Z)
        }

    # parameter of proposal distribution
    n<- 20; a<- 2; b<- 2; x0<- sample(1:n,size=4,replace=T); y0<-runif(4); chain<- 20000
    k <- 4          # number of chains to generate
    burn <- 1000 # burn-in length
    
    z<- matrix(0,2*k,chain)
    
    # generate the chains
    for (s in 1:k){
      z[(2*s-1):(2*s),]<- f.chain(n,a,b,chain,x0[s],y0[s])
    }
    
  # For X:
  # compute diagnostic statistics
  psi <- t(apply(z[c(1,3,5,7),], 1, cumsum))
  for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
  # plot the sequence of R-hat statistics
  rhat <- rep(0, chain)
  for (j in (burn+1):chain)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
  plot(rhat[(burn+1):chain], main='covergence for X',type="l", xlab="", ylab="R")
  abline(h=1.2, lty=2)
    
  # For Y:
  # compute diagnostic statistics
  psi <- t(apply(z[c(2,4,6,8),], 1, cumsum))
  for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
  # plot the sequence of R-hat statistics
  rhat <- rep(0, chain)
  for (j in (burn+1):chain)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
  plot(rhat[(burn+1):chain], main='covergence for Y',type="l", xlab="", ylab="R")
  abline(h=1.2, lty=2)
    

## -----------------------------------------------------------------------------
# The original function calculates the value of the k-th term
kth<- function(a,d,k){
(-1)^k/(factorial(k)*2^k) * (sqrt(t(a) %*% a))^(2*k+2)/((2*k+1)*(2*k+2)) * (gamma((d+1)/2)*gamma(k+3/2))/(gamma(k+d/2+1))
}

# The transformed function calculates the value of the kth term (when k and d are large)
per.kth<- function(a,d,k){
    return(
(-1)^k/factorial(k) * exp((2*k+2)*log(sqrt(t(a) %*% a))-k*log(2)-log(2*k+1)-log(2*k+2)+lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1))
)
}

## -----------------------------------------------------------------------------
tol<- .Machine$double.eps^0.5 # Setting accuracy
sum.k<- function(a,d,M){
  ktherm<- numeric(M+1)
  k<- 0:M
  for (i in 1:M+1){
  ktherm[i]<- per.kth(a,d,k[i])
  if (ktherm[i]< tol){
    break
  }
  }
  return(sum(ktherm))
}

## -----------------------------------------------------------------------------
a<- matrix(c(1,2),2,1)
d<- 100
M1<- 1000
M2<- 2000
cat('repeat 1000 times，sum=',sum.k(a,d,M2),'repeat 2000 times，sum=',sum.k(a,d,M1))

## -----------------------------------------------------------------------------
k0<- c(4:25,100,500,1000)
Ak<- numeric(length(k0))

for (i in 1:length(k0)){
k<- k0[i]
Sk<- function(a){
 pt(sqrt(a^2*(k-1)/(k-a^2)),k-1)-pt(sqrt(a^2*k/(k+1-a^2)),k) # S_k(a)-S_{k-1}(a)
}
Ak[i]<- uniroot(Sk,c(1,2))$root
}

## -----------------------------------------------------------------------------
k0<-c(4:25,100,500,1000)
root<- numeric(length(k))

for (i in 1:length(k0)){

k<-k0[i]

f<- function(u,k){
  (1+u^2/k)^(-(k+1)/2)
}


int<-function(a,k){
return(integrate(f,lower=0,upper=sqrt(a^2*k/(k+1-a^2)),rel.tol=.Machine$double.eps^0.25,k=k)$value)
}


cha2<- function(a){
  1/sqrt(pi*(k-1))*exp(lgamma(k/2)-lgamma((k-1)/2))*int(a,k-1)-1/sqrt(pi*k)*exp(lgamma((k+1)/2)-lgamma(k/2))*int(a,k)
}

root[i]<- uniroot(cha2,c(0.01,2))$root
}

cbind(k0,root,Ak)

## -----------------------------------------------------------------------------
library(stats4)
iden<- function(x,tau){
  n<- sum(x==tau)
  n0<- length(x)-n
  sumx<- sum(x)
  return(n0)
  }
x.ob<- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
logMLE<- function(theta=1){
  return(iden(x.ob,1)*log(theta)+1/theta*sum(x.ob))
}
fit<- mle(logMLE)
lam.ob<-as.numeric(c(fit@coef)) 
lam.gs<- 1/iden(x.ob,1)*sum(x.ob) 

## -----------------------------------------------------------------------------
# Use the EM algorithm to iterate out the value of lambda
m<- 1000
lambda0<- 1
tol<- .Machine$double.eps^0.5
lam.EM<- lambda0+1

for (i in 1:m){
  lambda0<- (sum(x.ob)+3*lambda0)/10
    if ((abs(lambda0-lam.EM)/lam.EM)< tol) {
      break
      }
  lam.EM<- lambda0
}
cbind(lam.ob,lam.EM)

## -----------------------------------------------------------------------------
trims<- c(0,0.1,0.2,0.5)
x<- rcauchy(100)
lapply(trims,function(trim) mean(x,trim=trim))
lapply(trims,mean,x=x)

## -----------------------------------------------------------------------------
library(stats)
mpg<- mtcars$mpg
disp<- mtcars$disp
wt<- mtcars$wt

formulas<- list(
  mpg~disp,
  mpg~I(1/disp),
  mpg~disp+wt,
  mpg~I(1/disp)+wt
)

# loop
for (i in seq_along(formulas)){
  print(lm(as.formula(as.character(formulas[i]))))
}

# lapply
mylm<- lapply(formulas,lm)

# R^2
rsq<- function(mod) summary(mod)$r.squared
lapply(mylm,rsq)

## -----------------------------------------------------------------------------
bootstrap<- lapply(1:10,function(i){
  rows<- sample(1:nrow(mtcars),rep=TRUE)
  mtcars[rows, c(1,3)]
})

# loop
for (b in seq_along(bootstrap)){
  print(lm(mpg~disp,bootstrap[[b]]))
}

# lapply()
md<- lapply(bootstrap,lm)

# R^2
rsq<- function(mod) summary(mod)$r.squared
lapply(md,rsq)

## -----------------------------------------------------------------------------
# a) choose ‘trees’ data set
data<- as.data.frame(trees) # The trees data set is of type "matrix" "array" and needs to be converted
vapply(data,sd,FUN.VALUE=c(1))

# b) choose ‘iris’ data set. The last column 'Species' is of type Factor
vapply(iris[(vapply(iris,class,character(1))=='numeric')], sd,FUN.VALUE=c(1))


## ----eval=FALSE---------------------------------------------------------------
#  library(parallel)
#  boot_lm <- function(i) {
#    dat <- mtcars[sample(nrow(mtcars), rep = T), ]
#    summary(lm(mpg ~ wt + disp, data = dat))$r.square
#  }
#  n <- 1e3
#  system.time(sapply(1:n, boot_lm))
#  
#  cl <- makeCluster(getOption("cl.cores", 4))
#  system.time({
#  res <- parSapply(cl,1:n,boot_lm)
#  })
#  stopCluster(cl)

## ----eval=FALSE---------------------------------------------------------------
#  #include <Rcpp.h>
#  using namespace Rcpp;
#  
#  //' @title A Gibbs sampler using Rcpp
#  //' @description A Gibbs sampler using Rcpp
#  //' @param N the number of samples
#  //' @return a random sample (x,y)
#  //' @examples
#  //' \dontrun{
#  
#  //' }
#  //' @export
#  // [[Rcpp::export]]
#  NumericMatrix GibbsC(int N){
#    NumericMatrix mat(N,2);
#    double x=1,y=0.5; /*initial*/
#    double a=2,b=3;
#    int n=20;
#  
#    for (int i=0; i<N; i++){
#      x = rbinom(1,n,y)[0];
#      y = rbeta(1,x+a,n-x+b)[0];
#      mat(i,0) = x;
#      mat(i,1) = y;
#    }
#    return mat;
#  }

## ----eval=TRUE----------------------------------------------------------------
set.seed(1202)
# use R
chain<- function(N){
a<- 2
b<- 3
n<- 20
Z<- matrix(0, N, 2) 
Z[1,1]<- 1 # initial
Z[1,2]<- 0.5

for (i in 2:N) {
Y <- Z[i-1, 2]
Z[i, 1] <- rbinom(1,n,Y)
X<- Z[i, 1]
Z[i, 2] <- rbeta(1,X+a,n-X+b)
}
return(Z)
}

library(Rcpp)
library(StatComp21024) 

#qqplots of x and y
qqplot(chain(500)[,1],GibbsC(500)[,1],main='qqplot of x')
qqplot(chain(500)[,2],GibbsC(500)[,2],main='qqplot of y')

## ----eval=TRUE----------------------------------------------------------------
set.seed(1202)
library(microbenchmark)
library(StatComp21024) 
N<- 500
ts <- microbenchmark(chain1<- chain(N),chain2<- GibbsC(N))
summary(ts)


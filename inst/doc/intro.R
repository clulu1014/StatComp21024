## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  netA<- function(K,N){
#    A<- matrix(NaN,N,N)
#    block<- sample(1:K,size=N,replace = T,prob=rep(1/K,K))
#    for (i in 1:N){
#      for (j in 1:i){
#        if(block[i]==block[j])
#        { A[i,j]<-A[j,i]<- sample(c(0,1),size=1,replace = T,prob=c(0.2,0.8))  }
#        else
#        { A[i,j]<-A[j,i]<- sample(c(0,1),size=1,replace = T,prob=c(0.9,0.1)) }
#      }
#    }
#    diag(A)<-0
#    A<- A[which(rowSums(A)!=0),which(rowSums(A)!=0)]
#    return(A)
#  }

## ----eval=TRUE----------------------------------------------------------------
set.seed(1234)
library(StatComp21024)
K<- 3
N<- 10
A<- netA(K,N)
A

## ----eval=FALSE---------------------------------------------------------------
#  obR<- function(A,beta,T){
#    N<- nrow(A)
#    e<- matrix(rnorm(N*T,0,1),N,T)
#    W<- diag(1/rowSums(A)) %*% A
#    G<- beta[1]*W + beta[2]*diag(N)
#    Y<- matrix(0,N,T)
#    X<- list()
#    for (s in 1:T){
#      X[[s]]<- matrix(0,N,2)
#    }
#    Y[,1]<- rnorm(N,0,1)
#    for (i in 2:T){
#      Y[,i]<- G %*% Y[,i-1]+e[,i]
#    }
#    for (j in 1:T){
#      for (k in 1:N){
#        X[[j]][k,]<- rbind(W[k,] %*% Y[,j], Y[k,j])
#      }
#    }
#    return(list(Y=Y,X=X))
#  }

## ----eval=TRUE----------------------------------------------------------------
set.seed(1234)
library(StatComp21024)
T<- 100
K<- 3
N<- 10
beta<- c(0.2,-0.4)
A<- netA(K,N)
obR(A,beta,T)

## ----eval=FALSE---------------------------------------------------------------
#  betahatR<- function(Y,X){
#    Z<-V<- list()
#    SumZ<- matrix(0,2,2)
#    SumV<- matrix(0,2,1)
#    for (t in 2:ncol(Y)){
#      Z[[t-1]]<- t(X[[t-1]]) %*% X[[t-1]]
#      SumZ<- Z[[t-1]]+SumZ
#      V[[t-1]]<- t(X[[t-1]]) %*% Y[,t]
#      SumV<- V[[t-1]]+SumV
#    }
#    betahat<- solve(SumZ) %*% SumV
#    return(list(betahat=betahat,ni=solve(SumZ)))
#  }

## ----eval=TRUE----------------------------------------------------------------
set.seed(1234)
library(StatComp21024)
T<- c(10,50,100,1e4)
K<- 3
N<- 10
beta<- c(0.2,-0.4)
A<- netA(K,N)
res<- list()
hatbeta<- matrix(0,2,length(T))
for (t in 1:length(T)){
res[[t]]<- obR(A,beta,T[t])
hatbeta[,t]<- betahatR(res[[t]]$Y,res[[t]]$X)$betahat
}
rownames(hatbeta)<- c('beta1','beta2')
colnames(hatbeta)<- T
hatbeta

## ----eval=FALSE---------------------------------------------------------------
#  RMSE<- function(rep,K,N,beta,T){
#    hatbeta<-sigmahat<- CIl<- CIr<- matrix(0,2,rep)
#    eps<- numeric(T)
#    for (r in 1:rep){
#      A<- netA(K,N)
#      res<- obR(A,beta,T)
#      hatbeta[,r]<- betahatR(res$Y,res$X)$betahat
#      for (t in 1:T){
#        eps[t]<- t(res$Y[t]-res$X[[t]] %*% hatbeta[,r]) %*% (res$Y[t]-res$X[[t]] %*% hatbeta[,r])
#      }
#      sigmahat[,r]<- sqrt(diag(betahatR(res$Y,res$X)$ni*sum(eps)/(T*nrow(A))))
#      CIl[,r]<- hatbeta[,r]-qnorm(0.975)*sigmahat[,r]
#      CIr[,r]<- hatbeta[,r]+qnorm(0.975)*sigmahat[,r]
#    }
#    RMSE1<- (sum((hatbeta[1,]-beta[1])^2)/rep)^(1/2)
#    RMSE2<- (sum((hatbeta[2,]-beta[2])^2)/rep)^(1/2)
#    CPbeta1<- mean(CIl[1,]<=beta[1] & beta[1]<=CIr[1,])
#    CPbeta2<- mean(CIl[2,]<=beta[2] & beta[2]<=CIr[2,])
#    return(rbind(CPbeta1,CPbeta2,RMSE1,RMSE2))
#  }

## ----eval=TRUE----------------------------------------------------------------
set.seed(1234)
library(StatComp21024)
rep<- 1000
T<- 100
K<- 3
N<- 10
beta<- c(0.2,-0.4)
RMSE(rep,K,N,beta,T)


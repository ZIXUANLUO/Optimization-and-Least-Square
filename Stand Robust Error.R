n<-100
x1<-runif(n,0,1)
x2<-runif(n,0,2)
e<-rnorm(n,0,1)
ee<-rnorm(n,0,4)
e[x2>1]<-ee[x2>1]
y=2+4*x1+6*x2+e
plot(x1,y)
plot(x2,y)
result<-summary(lm(y~x1+x2))
xx<-cbind(1,x1,x2)
ehat<-y-xx%*%result$coefficients[,1]
sigma2.hat<-sum(ehat^2)/(n-3)
xx.inv<-solve(t(xx)%*%xx)
var.bhat<-sigma2.hat*xx.inv
se.bhat<-(diag(var.bhat))^.5
omega.hat<-matrix(0,n,n)
diag(omega.hat)<-ehat^2

var.bhat.robust<-xx.inv%*%t(xx)%*%omega.hat%*%xx%*%xx.inv
se.bhat.robust<-(diag(var.bhat.robust))^.5


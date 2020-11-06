rm(list=ls())
n<-50
t<-seq(1,50)
x<-50-2*t+rnorm(n,0,10)
plot(t,x)
y<-3*t+rnorm(n,0,10)
plot(t,y)
summary(lm(y~x+t)) 
#this is a spurious regression, x is not causing y, x and y are both related to time, but x and y are not related. 

yy<-2+3*x+rnorm(n,0,100)
summary(lm(yy~x))
#if change to yy~x+t, both x,t get less significant

yd<-yyd<-xd<-rep(0,n)
for (i in 2:n){
  yd[i]<-y[i]-y[i-1] # this is called first differencing
  yyd[i]<-yy[i]-yy[i-1]
  xd[i]<-x[i]-x[i-1]
}
#the first value for yd, yyd, xd are all 0, how to get rid off it? the folloing codes will get rid off it.
#
yd<-yd[-1]
yyd<-yyd[-1]
xd<-xd[-1]

summary(lm(yd~xd)) #not significant, why? 
summary(lm(yyd~xd)) #significant, why?



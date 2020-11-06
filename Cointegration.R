#co-integration 
rm(list=ls())
n<-500
t<-seq(1,50)
x<-50-2*t+rnorm(n,0,10)
y<-3*t+rnorm(n,0,10)
yy<-2+3*x+rnorm(n,0,10)
yd<-yyd<-xd<-rep(0,n)
for (i in 2:n){
  yd[i]<-y[i]-y[i-1] # this is called first differencing
  yyd[i]<-yy[i]-yy[i-1]
  xd[i]<-x[i]-x[i-1]
}

# test y for unit root

unit.root<-function(z){
  tt<-length(z)
  zd<-rep(0,tt)
  for (i in 2:tt) 
  {zd[i]<-z[i]-z[i-1]}
  zd<-zd[-1]
  zl<-z[1:(tt-1)]
  ur.result<-summary(lm(zd~zl))
  ur<-0 # no unit root
  if (ur.result$coefficients[2,3]>-2.91) 
  ur<-1 
  # look up the -2.91 from the Dickey Fuller T test table
  return(ur)
}
answer.x<-unit.root(x)
answer.y<-unit.root(y)
xy.result<-lm(y~x)
answer.xy<-unit.root(xy.result$residuals)
answer.yy<-unit.root(yy)


yys<-xs<-ys<-rep(0,n)
for (i in 2:n){
  ys[i]=2+ys[i-1]+rnorm(n,0,10) 
  xs[i]=3+xs[i-1]+rnorm(n,0,10)
  yys[i]=xs[i]+rnorm(1,0,10)
  # we are generating both as unit root.
}

ys<-ys[-1]
xs<-xs[-1]
yys<-yys[-1]

answer.x<-unit.root(xs)
answer.y<-unit.root(ys)
xy.result<-summary(lm(ys~xs))
summary<-xy.result
answer.xy<-unit.root(xy.result$residuals)


answer.yys<-unit.root(yys)
yys.xs.result<-summary(lm(yys~xs))
summary<-yys.xs.result
answer.yys.xs<-unit.root(yys.xs.result$residuals)


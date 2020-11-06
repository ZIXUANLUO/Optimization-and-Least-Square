rho<-0.9
T<-1000
y<-rep(0,100)
for (j in 2:T){
  y[j]<-rho*y[j-1]+rnorm(1,0,1) 
}
# this is a AR(1) and 

#if rho=1: it's called non-stationary; it randomly go all over the place. Time series are non-stationary, the mean doesn't stay the same -> Yt has a unit root. How to test unit root? Dickey-Fuller Test

#if rho<1: it's called mean stationary, the mean doesn't change.; it generally stays around the mean, in this case mean=0. 

plot(y)
#above is called a "RANDOM WALK"

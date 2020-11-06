n<-100
x1<-runif(n,0,1)
x2<-runif(n,0,2)
e<-rnorm(n,0,2)
y<-2-4*x1+6*x2+e
lm(y~x1+x2)
x<-cbind(1,x1,x2)
bhat.ls<-solve(t(x)%*%x)%*%t(x)%*%y
#fun.ls<-function(bhat){
  #sum((y-x%*%bhat)^2) # sum of least square
  #}

fun.ls<-function(bhat){
  sum(abs(y-x%*%bhat)^2)
}

b.start<-c(0,0,0) # this is a 3 dimensional space, because you have 3 betas
b1<-b.start+c(0.01,0,0) #adding a little number in the first dimension
b2<-b.start+c(0,0.01,0) #adding a little number in the second dimension
b3<-b.start+c(0,0,0.01) #adding a little number in the third dimension

der.1<-fun.ls(b1)-fun.ls(b.start)
der.2<-fun.ls(b2)-fun.ls(b.start)
der.3<-fun.ls(b3)-fun.ls(b.start)

b.start<-b.start+b1+b2+b3


#Alan 191017 continue on optimizer below:
fun.ls<-function(b){
  sum((y-x%*%b)^2)
}
tol<-0.1 # Tolerance is important in optimizer 
converge<-0
funct.eval<-100000000
iter.limit<-1000 # this is important in optimizer
b.start<-c(0,0,0)
ss<-0.1 #step size, reduce the step size, the result got much better 
iter<-0 #where iteration starts, it starts at 0 and meet iter.limit of 1000.

b.iter<-matrix(0,iter.limit+1,3)
f.iter<-rep(0,iter.limit+1)
while(converge<1){
  
der.1<-(fun.ls(b.start+c(ss,0,0))-fun.ls(b.start))/ss #evaluating at the first dimension
der.2<-(fun.ls(b.start+c(0,ss,0))-fun.ls(b.start))/ss #second dimension
der.3<-(fun.ls(b.start+c(0,0,ss))-fun.ls(b.start))/ss #third dimension

if (der.1<0) {d1<-2} else {d1<-1} #if der.1 is negative, to the power of d1=2, it's positive; if der.1 is negative, to the power of d1=1, it's negative; by doing this, we know the direction whether it's going to the positive or negative direction on the graph. 
if (der.2<0) {d2<-2} else {d2<-1} 
if (der.3<0) {d3<-2} else {d3<-1}

b.start.new<-b.start + c((-1)^d1*ss, (-1)^d2*ss, (-1)^d3*ss)

iter<-iter+1
f.iter[iter]<-fun.ls(b.start.new) #f.iter.new is a vactor
b.iter[iter,]<-b.start.new #b.iter.new is a matrix (b1, b2, b3)
if (abs(fun.ls(b.start.new)-fun.ls(b.start))<tol) converge<-1
if (iter>iter.limit) converge<-1
b.start<-b.start.new
}
#now let's change the tol (from 1 to 0.1) and ss (from 0.1 to 0.01) to a smaller number, we found it takes 
iter
plot(f.iter[1:iter])
plot(b.iter[1:iter,1]) #this shows the path of the first parameter
plot(b.iter[1:iter,2])
plot(b.iter[1:iter,3])

#Alan 191023 continue finishing on the optimizer 

der.us.2<-matrix(0,3,3) #have 3 by 3 matrix second derivatives. 

while{
k<-3
der.us.1<-c(der.1, der.2, der.3) #put all first derivatives into same dimension.

for (j in 1:k){
  for (jj in 1:k){
    b1<-b.start # following at looking at the derivative at one direction
    b1[j]<-b1[j]+ss
    b1[jj]<-b1[jj]+ss
    f1<-fun.ls(b1)
    
    b2<-b.start #this is looking at the derivative at the other direction,
    b2[j]<-b2[j]+ss
    b2[jj]<-b2[jj]-ss
    f2<-fun.ls(b2)
    
    b3<-b.start # looking at 1 negative and 1 positve direction
    b3[j]<-b3[j]-ss
    b3[jj]<-b3[jj]+ss
    f3<-fun.ls(b3)
    
    b4<-b.start # it is looking at both negative direction
    b4[j]<-b4[j]-ss
    b4[jj]<-b4[jj]-ss
    f4<-fun.ls(b4)
    
    der.us.2[j,jj]<-(f1-f2-f3+f4)/(4*ss^2) # this tells us the second derivatives,
  }
  }
    step.us<-as.vector(-solve(der.us.2)%*%der.us.1) # thers is a negative sign, this is also picked up along the hecksion along the diagnol. "solve" is taking the inverse of the second derivatives 
b.start.new<-b.start+step.us
iter<-iter+1
f.iter[iter]<-fun.ls(b.start.new) #f.iter.new is a vactor
b.iter[iter,]<-b.start.new #b.iter.new is a matrix (b1, b2, b3)
if (abs(fun.ls(b.start.new)-fun.ls(b.start))<tol) converge<-1
if (iter>iter.limit) converge<-1
}
iter
plot(f.iter[1:iter])
plot(b.iter[1:iter,1])

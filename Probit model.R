#Probit model - Young Liu 2019-10-09

set.seed(1234) # this means the class will get the same random numbers, so our results will be the same, easier for comparison.
n<-500
x1<-rnorm(n, mean=5, sd=4)
x2<-rnorm(n, mean=-4, sd=10)
x3<-rnorm(n, mean=23, sd=12)
e<-rnorm(n)

#below are our true parameters
b0<-5
b1<-4
b2<--4
b3<-8

ystar<-b0+b1*x1+b2*x2+b3*x3

y_temp<-(ystar-mean(ystar))/sd(ystar)

p<-pnorm(y_temp)

y<-ifelse(runif(n)<p, 1, 0)
y

mdat<- data.frame(y=y, x1=x1, x2=x2, x3=x3)
reg_probit<-glm(y~x1+x2+x3,data=mdat,
                family = binomial(link="probit")) # if change the link to logit, it will run logit regression. 

summary(reg_probit)

install.packages("stargazer")
require(stargazer)
stargazer(reg_probit,type="text")
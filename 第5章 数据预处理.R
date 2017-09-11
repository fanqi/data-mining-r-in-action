install.packages(lattice)       
install.packages(MASS)                                     
install.packages(nnet)                                      
library(lattice)                                            
library(MASS)                                        
library(nnet)   

install.packages(mice)
library(mice)
data(nhanes2)   
nrow(nhanes2);ncol(nhanes2)  
summary(nhanes2)
head(nhanes2)

sum(is.na(nhanes2))
sum( complete.cases(nhanes2))
md.pattern(nhanes2) 

imp=mice(nhanes2,m=4)
fit=with(imp,lm(chl~age+hyp+bmi))
pooled=pool(fit)                  
summary(pooled) 

sub=which(is.na(nhanes2[,4])==TRUE) 
dataTR=nhanes2[-sub,]    
dataTE=nhanes2[sub,] 
dataTE

lm=lm(chl~age,data=dataTR) 
nhanes2[sub,4]=round(predict(lm,dataTE))
head(nhanes2) 

y=rnorm(100) 
outlier(y)                                           
outlier(y,opposite=TRUE)                          
dim(y) <- c(20,5)  
outlier(y) 
outlier(y,opposite=TRUE)   
y=rnorm(10)  
outlier(y,logical=TRUE)  

x=rnorm(12)
x=sort(x) 
dim(x)=c(3,4)  
x[1,]=apply(x,1,mean)[1] 
x[2,]=apply(x,1,mean)[2]   
x[3,]=apply(x,1,mean)[3]  
x  

x=cbind(sample(c(1:50),10),sample(c(1:50),10))      
chisq.test(x) 

x=cbind(rnorm(10),rnorm(10))   
cor(x)
cov(x) 

x=cbind(sample(c(1:10),10,replace=T),rnorm(10),rnorm(10))
head(x)

y=unique(x[,1]) 
sub=rep(0,length(y))    
for(i in 1:length(y))  sub[i]=which(x[,1]==y[i])[1]
x=x[sub,]
head(x)

a=rnorm(5)
b=scale(a) 
b

x=matrix(rnorm(100*20),100,20) 
y=rnorm(100) 
fit1=glmnet(x,y)                                
b=coef(fit1,s=0.01)               
b 

predict(fit1,newx=x[1:10,],s=c(0.01,0.005))






















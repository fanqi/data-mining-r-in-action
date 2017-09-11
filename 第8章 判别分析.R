# data #
library(kknn)
data(miete)
head(miete)
dim(miete)
summary(miete)

library(sampling)
n=round(2/3*nrow(miete)/5)
n
sub_train=strata(miete,stratanames="nmkat",size=rep(n,5),method="srswor")
head(sub_train)
data_train=getdata(miete[,c(-1,-3,-12)],sub_train$ID_unit)  
data_test=getdata(miete[,c(-1,-3,-12)],-sub_train$ID_unit) 
dim(data_train);dim(data_test)
head(data_test)

# lda #
# lda(formula, data, ..., subset, na.action)
# lda(x,grouping,prior=proportions,tol=1.0e-4,method, CV = FALSE, nu, ...)
# lda(x,grouping, ...,subset,na.action)
install.packages("MASS")
library(MASS)
fit_lda1=lda(nmkat~.,data_train)
names(fit_lda1)
fit_lda1$prior
fit_lda1$counts
fit_lda1$means
fit_lda1$scaling
fit_lda1$lev
fit_lda1$svd
fit_lda1$N     
fit_lda1$call
fit_lda1$terms
fit_lda1$xlevels
fit_lda1

fit_lda2=lda(data_train[,-12],data_train[,12])
fit_lda2

plot(fit_lda1)
plot(fit_lda1,dimen=1)
plot(fit_lda1,dimen=2)

pre_lda1=predict(fit_lda1,data_test)
pre_lda1$class
pre_lda1$posterior

table(data_test$nmkat,pre_lda1$class)
error_lda1=sum(as.numeric(as.numeric(pre_lda1$class)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_lda1

# qda #
fit_qda=qda(data_train[,-12],data_train[,12])

# bayes #
# ## S3 method for class ’formula’
  NaiveBayes(formula, data, ..., subset, na.action = na.pass)
  ## Default S3 method:
  NaiveBayes(x, grouping, prior, usekernel = FALSE, fL = 0, ...)

install.packages("klaR")
library(klaR)

fit_Bayes1=NaiveBayes(nmkat~.,data_train)
names(fit_Bayes1)
fit_Bayes1$apriori
fit_Bayes1$tables
fit_Bayes1$levels
fit_Bayes1$call
fit_Bayes1$usekernel
fit_Bayes1$varnames

fit_Bayes2=NaiveBayes(data_train[,-12],data_train[,12])
fit_Bayes2

plot(fit_Bayes1,vars="wfl",n=50,col=c(1,"darkgrey",1,"darkgrey",1)) # 占地面积
plot(fit_Bayes1,vars="mvdauer",n=50,col=c(1,"darkgrey",1,"darkgrey",1)) # 租赁期
plot(fit_Bayes1,vars="nmqm",n=50,col=c(1,"darkgrey",1,"darkgrey",1)) # 每平方米净租金

pre_Bayes1=predict(fit_Bayes1,data_test)
pre_Bayes1

table(data_test$nmkat,pre_Bayes1$class)
error_Bayes1=sum(as.numeric(as.numeric(pre_Bayes1$class)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_Bayes1

# knn #
# knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

install.packages("class")
library(class)

fit_pre_knn=knn(data_train[,-12],data_test[,-12],cl=data_train[,12])
fit_pre_knn
table(data_test$nmkat,fit_pre_knn)
error_knn=sum(as.numeric(as.numeric(fit_pre_knn)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_knn

error_knn=rep(0,20)
for(i in 1:20)
{ fit_pre_knn=knn(data_train[,-12],data_test[,-12],cl=data_train[,12],k=i)
  error_knn[i]=sum(as.numeric(as.numeric(fit_pre_knn)!=as.numeric(data_test$nmkat)))/nrow(data_test)}
error_knn
plot(error_knn,type="l",xlab="K")

# kknn #
# kknn(formula = formula(train), train, test, na.action = na.omit(),
  k = 7, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
  contrasts = c(’unordered’ = "contr.dummy", ordered = "contr.ordinal"))

install.packages("kknn")
library(kknn)
fit_pre_kknn=kknn(nmkat~.,data_train,data_test[,-12])
fit_pre_kknn
summary(fit_pre_kknn)
fit=fitted(fit_pre_kknn)
fit

table(data_test$nmkat,fit)
error_kknn=sum(as.numeric(as.numeric(fit)!=as.numeric(data_test$nmkat)))/nrow(data_test)
error_kknn

error_kknn=rep(0,20)
for(i in 1:20)
{ fit_pre_kknn=kknn(nmkat~.,data_train,data_test[,-12],k=i)
  error_kknn[i]=sum(as.numeric(as.numeric(fitted(fit_pre_kknn))!=as.numeric(data_test$nmkat)))/nrow(data_test)}
error_kknn
plot(error_kknn,type="l",xlab="K")

sub=matrix(0,4,30)
for(i in 1:4)  sub[i,]=sample(which(miete$nmkat==i),30)
SUB=sample(which(miete$nmkat=="5"),200)
subb=matrix(0,5,20)
for(i in 1:5)  subb[i,]=sample(which(miete$nmkat==i),20)
data_train=miete[c(sub,SUB),c(-1,-3,-12)]  
data_test=miete[subb,c(-1,-3,-12)] 
dim(data_train);dim(data_test)

# 一个案例 #

setwd("D://book")
data=read.table("u.data.txt")
data=data[,-4]
names(data)=c("userid","itemid","rating")

# 使用函数 #
MovieLens_KNN(Userid=1,Itemid=61,n=50,K=10)

# 函数 #

Userid=1;Itemid=61;n=50;K=10

MovieLens_KNN=function(Userid,Itemid,n,K) {

sub=which(data$userid==Userid)
if(length(sub)>=n) sub_n=sample(sub,n)
if(length(sub)<n) sub_n=sample(sub,length(sub))
known_itemid=data$itemid[sub_n]
unknown_itemid=Itemid

unknown_sub=which(data$itemid==unknown_itemid)
user=data$userid[unknown_sub[-1]]

data_all=matrix(0,1+length(user),2+length(known_itemid))
data_all=data.frame(data_all)
names(data_all)=c("userid",paste("unknown_itemid_",Itemid),paste("itemid_",known_itemid,sep=""))
item=c(unknown_itemid,known_itemid)
data_all$userid=c(Userid,user)

for (i in 1:nrow(data_all))
{ 
  data_temp=data[which(data$userid==data_all$userid[i]),]
  for (j in 1:length(item))
  {  if(sum(as.numeric(data_temp$itemid==item[j]))!=0)
     {data_all[i,j+1]=data_temp$rating[which(data_temp$itemid==item[j])]}
  } }

data_test_x=data_all[1,c(-1,-2)]
data_test_y=data_all[1,2]
data_train_x=data_all[-1,c(-1,-2)]
data_train_y=data_all[-1,2]

fit=knn(data_train_x,data_test_x,cl=data_train_y,k=K)
list("data_all:"=data_all,"True Rating:"=data_test_y,"Predict Rating:"=fit,"User ID:"=Userid,"Item ID:"=Itemid)
}

user1=NULL
for(Item in 1:20) 
user1=c(user1,MovieLens_KNN(Userid=1,Itemid=Item,n=50,K=10)$`True Rating:`)
user1

which(user1==5)









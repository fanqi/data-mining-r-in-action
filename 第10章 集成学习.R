
# data #

setwd("D://book")
data=read.csv("bank.csv",header=TRUE,sep=";")
head(data)
dim(data)
summary(data)
sum(data$y=="yes"); sum(data$y=="no")
sub=sample(1:nrow(data),round(nrow(data)/4))
length(sub)
data_train=data[-sub,]
data_test=data[sub,]
dim(data_train);dim(data_test)

# Bagging #

install.packages("adabag")
install.packages("rpart")
library(adabag)
library(rpart)

bag=bagging(y~.,data_train,mfinal=5)
names(bag)
bag$formula
bag$trees[2]
bag$votes[105:115,]
bag$prob[105:115,]
bag$class[105:115]
bag$samples[105:115,]
bag$importance
barplot(bag$importance)

bag1=bagging(y~.,data_train,mfinal=5,control=rpart.control(maxdepth=3))
bag1$trees[2]

pre_bag=predict(bag,data_test)
names(pre_bag)
pre_bag$votes[1:10,]
pre_bag$prob[1:10,]
pre_bag$class[1:10]
pre_bag$confusion
pre_bag$error

sub_minor=which(data_test$y=="yes")
sub_major=which(data_test$y=="no")
length(sub_minor); length(sub_major)

err_bag=sum(pre_bag$class!=data_test$y)/nrow(data_test)
err_minor_bag=sum(pre_bag$class[sub_minor]!=data_test$y[sub_minor])/length(sub_minor)
err_major_bag=sum(pre_bag$class[sub_major]!=data_test$y[sub_major])/length(sub_major)
err_bag; err_minor_bag; err_major_bag

# Boosting #

boo=boosting(y~.,data_train,mfinal=5)
pre_boo=predict(boo,data_test)

err_boo=sum(pre_boo$class!=data_test$y)/nrow(data_test)
err_minor_boo=sum(pre_boo$class[sub_minor]!=data_test$y[sub_minor])/length(sub_minor)
err_major_boo=sum(pre_boo$class[sub_major]!=data_test$y[sub_major])/length(sub_major)
err_boo; err_minor_boo; err_major_boo


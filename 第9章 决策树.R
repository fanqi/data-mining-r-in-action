# start #
library(mvpart)
data(car.test.frame)
head(car.test.frame)

car.test.frame$Mileage=100*4.546/(1.6*car.test.frame$Mileage)
names(car.test.frame)=c("价格","产地","可靠性","油耗","类型","车重",
                        "发动机功率","净马力")
head(car.test.frame)

str(car.test.frame)
summary(car.test.frame)

Group_Mileage=matrix(0,60,1)
Group_Mileage[which(car.test.frame$"油耗">=11.6)]="A"
Group_Mileage[which(car.test.frame$"油耗"<=9)]="C"
Group_Mileage[which(Group_Mileage==0)]="B"
car.test.frame$"分组油耗"=Group_Mileage
car.test.frame[1:10,c(4,9)]

library(sampling)
a=round(1/4*sum(car.test.frame$"分组油耗"=="A"))
b=round(1/4*sum(car.test.frame$"分组油耗"=="B"))
c=round(1/4*sum(car.test.frame$"分组油耗"=="C"))
a;b;c
sub=strata(car.test.frame,stratanames="分组油耗",size=c(c,b,a),method="srswor")
sub
Train_Car=car.test.frame[-sub$ID_unit,]
Test_Car=car.test.frame[sub$ID_unit,]
nrow(Train_Car);nrow(Test_Car)

# CART #
library(rpart)
library(rpart.plot)
library(maptree)

formula_Car_Reg=油耗~价格+产地+可靠性+类型+车重+发动机功率+净马力
rp_Car_Reg=rpart(formula_Car_Reg,Train_Car,method="anova")
print(rp_Car_Reg)
printcp(rp_Car_Reg)
summary(rp_Car_Reg)

rp_Car_Reg1=rpart(formula_Car_Reg,Train_Car,method="anova",minsplit=10)
print(rp_Car_Reg1)
printcp(rp_Car_Reg1)

rp_Car_Reg2=rpart(formula_Car_Reg,Train_Car,method="anova",cp=0.1)
print(rp_Car_Reg2)
printcp(rp_Car_Reg2)

rp_Car_Reg3=prune.rpart(rp_Car_Reg,cp=0.1)
print(rp_Car_Reg3)
printcp(rp_Car_Reg3)

rp_Car_Reg4=rpart(formula_Car_Reg,Train_Car,method="anova",maxdepth=1)
print(rp_Car_Reg4)
printcp(rp_Car_Reg4)

rp_Car_Plot=rpart(formula_Car_Reg,Train_Car,method="anova",minsplit=10)
print(rp_Car_Plot)
rpart.plot(rp_Car_Plot)
rpart.plot(rp_Car_Plot,type=4)
rpart.plot(rp_Car_Plot,type=4,branch=1)
rpart.plot(rp_Car_Plot,type=4,fallen.leaves=TRUE)

draw.tree(rp_Car_Plot,col=rep(1,7),nodeinfo=TRUE)
plot(rp_Car_Plot,uniform=TRUE,main="plot: Regression Tree")
text(rp_Car_Plot,use.n=TRUE,all=TRUE)
post(rp_Car_Plot,file="",title.="post: Regression Tree") 

formula_Car_Cla=分组油耗~价格+产地+可靠性+类型+车重+发动机功率+净马力
rp_Car_Cla=rpart(formula_Car_Cla,Train_Car,method="class",minsplit=5)
print(rp_Car_Cla)
rpart.plot(rp_Car_Cla,type=4,fallen.leaves=TRUE)

pre_Car_Cla=predict(rp_Car_Cla,Test_Car,type="class")
pre_Car_Cla
table(Test_Car$分组油耗,pre_Car_Cla)
(p=sum(as.numeric(pre_Car_Cla!=Test_Car$分组油耗))/nrow(Test_Car))

# C4.5 #
library(RWeka)
names(Train_Car)=c("Price","Country","Reliability","Mileage",
                   "Type","Weight","Disp.","HP","Oil_Consumption") 
Train_Car$Oil_Consumption=as.factor(Train_Car$Oil_Consumption)
formula=Oil_Consumption~Price+Country+Reliability+Type+Weight+Disp.+HP

C45_0=J48(formula,Train_Car)
C45_0
summary(C45_0)

C45_1=J48(formula,Train_Car,control=Weka_control(M=3))
C45_1
plot(C45_1)




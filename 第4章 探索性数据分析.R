# 4.1 数据 #
install.packages(MASS)
library(MASS)
data(Insurance)
head(Insurance)
nrow(Insurance);ncol(Insurance)
dim(Insurance)

# 4.2 数字化探索 #
names(Insurance)
attributes(Insurance)
str(Insurance)
summary(Insurance)

install.packages(Hmisc)
library(Hmisc)
describe(Insurance[,1:3])
describe(Insurance[,4:5])

install.packages(fBasics)
library(fBasics)
basicStats(Insurance$Holders)

install.packages(timeDate)
library(timeDate)
skewness(Insurance[,4:5])
kurtosis(Insurance[,4:5])

install.packages(Matrix)
library(Matrix)
i=sample(1:10,10,replace=TRUE)
j=sample(1:10,10,replace=TRUE)
(A=sparseMatrix(i, j, x = 1))
loca=which(A==1, arr.ind=TRUE)
plot(loca,pch = 22)

for(i in 1:10)
{ row=sample(1:64,1)
  col=sample(1:5,1)
  Insurance[row,col]=NA
}
install.packages(mice)
library(mice)
md.pattern(Insurance)

library(rattle)
data(weather)

numerics=c(12:21)
cor_matrix=cor(weather[numerics],use="pairwise",method="pearson")
cor_matrix

library(ellipse)
plotcorr(cor_matrix,col=rep(c("white","black"),5))
plotcorr(cor_matrix,diag=T,type="lower",col=rep(c("white","black"),5))


# 4.3 可视化探索 #

# 直方图 MASS
hist(Insurance$Claims,main="Histogram of Freq of Insurance$Claims")

hist(Insurance$Claims,freq=FALSE,density=20,
     main="Histogram of Density of Insurance$Claims")
lines(density(Insurance$Claims))

str(hist(Insurance$Claims,breaks=20,labels = TRUE,
         col="black",border="white",
         main="Histogram of Insurance$Claims with 20 bars"))

# 累积分布图 Hmisc
Ecdf(Insurance$Claims,xlab="Claims",main="Cumulative Distribution of Claims")

data_plot=with(Insurance,
      rbind(data.frame(var1=Claims[Age=="<25"],var2="<25"),
            data.frame(var1=Claims[Age=="25-29"],var2="25-29"),
            data.frame(var1=Claims[Age=="30-35"],var2="30-35"),
            data.frame(var1=Claims[Age==">35"],var2=">35") )
       )
Ecdf(data_plot$var1,lty=2,group=data_plot$var2,label.curves=1:4,
      xlab="Claims", main="Cumulative Distribution of Claims by Age")
Ecdf(Insurance$Claims,add=TRUE)

# 箱型图
Claims_bp=boxplot(Insurance$Claims,main="Distribution of Claims")
Claims_bp$stats

points(x=1,y=mean(Insurance$Claims),pch=8)
Claims_points=as.matrix(Insurance$Claims[which(Insurance$Claims>102)],6,1)
Claims_text=rbind(Claims_bp$stats,mean(Insurance$Claims),Claims_points)
for(i in 1:length(Claims_text))
text(x=1.1,y=Claims_text[i,],labels=Claims_text[i,])

boxplot(var1~var2,data=data_plot,horizontal=TRUE,
        main="Distribution of Claims by Age",xlab="Claims",ylab="Age")

with(Insurance, 
  {
    boxplot(Holders ~ Age, boxwex=0.25, at=1:4+0.2,
            subset = Age == ">35")
    boxplot(Holders ~ Age, add = TRUE, boxwex=0.25, at=1:4+0.2,
            subset = Age == "30-35")
    boxplot(Holders ~ Age, add = TRUE, boxwex=0.25, at=1:4+0.2,
            subset = Age == "25-29")
    boxplot(Holders ~ Age, add = TRUE, boxwex=0.25, at=1:4+0.2,
            subset = Age == "<25") 
  } )
boxplot(var1~var2, data=data_plot, add = TRUE, boxwex=0.25, at=1:4 - 0.2, 
        col="lightgrey", main="Distribution of Claims&Holders by Age",
        xlab="Age", ylab="Claims&Holders")
legend(x="topleft", c("Claims", "Holders"), fill = c("lightgrey", "white"))

data_bp=list(data_plot$var1[which(data_plot$var2=="<25")],
             data_plot$var1[which(data_plot$var2=="25-29")],
             data_plot$var1[which(data_plot$var2=="30-35")],
             data_plot$var1[which(data_plot$var2==">35")])
bpplot(data_bp,name=c("<25","25-29","30-35",">35"),xlab="Age", ylab="Claims")

# 条形图

Claims_Age = with(Insurance,
                c( sum(Claims[which(Age=="<25")]), sum(Claims[which(Age=="25-29")]),
                   sum(Claims[which(Age=="30-35")]), sum(Claims[which(Age==">35")]) ) )
 
barplot(Claims_Age, names.arg=c("<25","25-29","30-35",">35"),density=rep(20,4),
        main="Distribution of Age by Claims", xlab="Age", ylab="Claims")

Holders_Age = with(Insurance,
                c( sum(Holders[which(Age=="<25")]), sum(Holders[which(Age=="25-29")]),
                   sum(Holders[which(Age=="30-35")]), sum(Holders[which(Age==">35")]) ) )
Holders_Age
data_bar = rbind(Claims_Age,Holders_Age)
data_bar
barplot(data_bar, names.arg=c("<25","25-29","30-35",">35"),beside=TRUE,
        main="Age Distribution by Claims and Holders",
        xlab="Age", ylab="Claims&Holders", col=c("black","darkgrey"))
legend(x="topleft", rownames(data_bar), fill = c("black","darkgrey"))

barplot(data_bar, names.arg=c("<25","25-29","30-35",">35"),
        main="Age Distribution by Claims and Holders",
       ylab="Claims&Holders", col=c("black","darkgrey"))
legend(x="topleft", rownames(data_bar), fill = c("black","darkgrey"))

# 点阵图
dotchart(data_bar,xlab="Claims&Holders", pch=1:2,
         main="Age Distribution by Claims and Holders")
legend(x=14000,y=15,"<25",bty="n")
legend(x=14000,y=11,"25-29",bty="n")
legend(x=14000,y=7,"30-35",bty="n")
legend(x=14000,y=3,">35",bty="n")

# 饼图
pie(Claims_Age,labels=c("<25","25-29","30-35",">35"),
    main="Pie Chart of Age by Claims",col=c("white","lightgray","darkgrey","black"))

percent = round(Claims_Age/sum(Claims_Age)*100)
label = paste(paste(c("<25","25-29","30-35",">35"),":"), percent,"%",sep="")
pie(Claims_Age,labels = label,  
    main="Pie Chart of Age by Claims",col=c("white","lightgray","darkgrey","black"))

install.packages(plotrix)
library(plotrix)
pie3D(Claims_Age,labels=c("<25","25-29","30-35",">35"),explode=0.05,
      main="3D Pie Chart of Age by Claims",labelcex=0.8,
      col=c("white","lightgray","darkgrey","black"))












































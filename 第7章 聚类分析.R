# data #
# http://www.uni-koeln.de/themen/statistik/data/cluster/
data=read.table("D://book//data.txt")
data=data[-17,]
countries=data[-51,]

names(countries)=c("country","birth","death")
var=countries$country
var=as.character(var)
for(i in 1:68) row.names(countries)[i]=var[i]

plot(countries$birth,countries$death)
C=which(countries$country=="CHINA")
T=which(countries$country=="TAIWAN")
H=which(countries$country=="HONG-KONG")
I=which(countries$country=="INDIA")
U=which(countries$country=="UNITED-STATES")
J=which(countries$country=="JAPAN")
M=which.max(countries$birth)
points(countries[c(C,T,H,I,U,J,M),-1],pch=16)
legend(countries$birth[C]-1.2,countries$death[C],"CHINA",bty="n",xjust=0,yjust=0.5,cex=0.8)
legend(countries$birth[T],countries$death[T],"TAIWAN",bty="n",xjust=0.5,cex=0.8)
legend(countries$birth[H],countries$death[H],"HONG-KONG",bty="n",xjust=0.5,cex=0.8)
legend(countries$birth[I]-1.2,countries$death[I],"INDIA",bty="n",xjust=0,yjust=0.5,cex=0.8)
legend(countries$birth[U],countries$death[U],"UNITED-STATES",bty="n",xjust=0.5,yjust=0,cex=0.8)
legend(countries$birth[J],countries$death[J],"JAPAN",bty="n",xjust=1,yjust=0.5,cex=0.8)
legend(countries$birth[M],countries$death[M],countries$country[M],bty="n",xjust=1,cex=0.8)

# k-means #
fit_km1=kmeans(countries[,-1],center=3)
print(fit_km1)
fit_km1$centers
fit_km1$totss;fit_km1$tot.withinss;fit_km1$betweenss
fit_km1$betweenss+fit_km1$tot.withinss

plot(countries[,-1],pch=(fit_km1$cluster-1))
points(fit_km1$centers,pch=8)
legend(fit_km1$centers[1,1],fit_km1$centers[1,2],"Center_1",bty="n",xjust=1,yjust=0,cex=0.8)
legend(fit_km1$centers[2,1]-2,fit_km1$centers[2,2],"Center_2",bty="n",xjust=0,yjust=0,cex=0.8)
legend(fit_km1$centers[3,1],fit_km1$centers[3,2],"Center_3",bty="n",xjust=0.5,cex=0.8)

for(i in 1:7)
{ var=c(C,T,H,I,U,J,M)
  points(countries[var[i],-1],pch=fit_km1$cluster[var[i]]+14) }
legend(countries$birth[C]-1.2,countries$death[C],"CHINA",bty="n",xjust=0,yjust=0.5,cex=0.8)
legend(countries$birth[T],countries$death[T],"TAIWAN",bty="n",xjust=0.5,cex=0.8)
legend(countries$birth[H],countries$death[H],"HONG-KONG",bty="n",xjust=0.5,cex=0.8)
legend(countries$birth[I]-1.1,countries$death[I],"INDIA",bty="n",xjust=0,yjust=0.5,cex=0.8)
legend(countries$birth[U],countries$death[U],"UNITED-STATES",bty="n",xjust=0.5,yjust=0,cex=0.8)
legend(countries$birth[J],countries$death[J],"JAPAN",bty="n",xjust=1,yjust=0.5,cex=0.8)
legend(countries$birth[M],countries$death[M],countries$country[M],bty="n",xjust=1,cex=0.8)

result=rep(0,67)
for(k in 1:67)
{
   fit_km=kmeans(countries[,-1],center=k)
   result[k]=fit_km$betweenss/fit_km$totss
}
plot(1:67,result,type="b",main="Choosing the Optimal Number of Cluster",
     xlab="number of cluster: 1 to 67",ylab="betweenss/totss")
points(10,result[10],pch=16)
legend(10,result[10],paste("(10,",sprintf("%.1f%%",result[10]*100),")",sep=""),bty="n",xjust=0.3,cex=0.8)

fit_km2=kmeans(countries[,-1],center=10)
cluster_CHINA=fit_km2$cluster[which(countries$country=="CHINA")]
which(fit_km2$cluster==cluster_CHINA)


# k-Medoids #
library(cluster)

fit_pam=pam(countries[,-1],3)
print(fit_pam)
head(fit_pam$data) 

fit_pam1=pam(countries[,-1],3,keep.data=FALSE)
fit_pam1$data
fit_pam2=pam(countries[,-1],3,cluster.only=TRUE)
print(fit_pam2)

which(fit_km$cluster!=fit_pam$cluster)

plot(countries[,-1],pch=(fit_pam$cluster-1))
c1=which(rownames(countries)==rownames(fit_pam$medoids)[1])
c2=which(rownames(countries)==rownames(fit_pam$medoids)[2])
c3=which(rownames(countries)==rownames(fit_pam$medoids)[3])
for(i in 1:3)
{ var=c(c1,c2,c3)
  points(countries[var[i],-1],pch=fit_pam$cluster[var[i]]+14) }
legend(fit_pam$medoids[1,1],fit_pam$medoids[1,2],paste("Center_1:",rownames(fit_pam$medoids)[1]),bty="n",xjust=0.5,yjust=0,cex=0.8)
legend(fit_pam$medoids[2,1]-1.2,fit_pam$medoids[2,2],paste("Center_2:",rownames(fit_pam$medoids)[2]),bty="n",xjust=0,yjust=0.5,cex=0.8)
legend(fit_pam$medoids[3,1],fit_pam$medoids[3,2]+3.5,paste("Center_3:",rownames(fit_pam$medoids)[3]),bty="n",xjust=0.5,yjust=0,cex=0.8)
points(countries[c(21,23,33),-1],pch=12)
legend(countries$birth[21],countries$death[21],"MONGOLIA",bty="n",xjust=0.5,yjust=0,cex=0.8)
legend(countries$birth[23]-1.2,countries$death[23],"SYRIA",bty="n",xjust=0,yjust=0.5,cex=0.8)
legend(countries$birth[33]-1.2,countries$death[33],"PANAMA",bty="n",xjust=0,yjust=0.5,cex=0.8)

# result=matrix(0,66,2)
# for(k in 2:67)
# {
#    fit_pam=pam(countries[,-1],k)
#    result[k-1,]=fit_pam$objective
# }
# plot(2:67,result[,1],type="l",main="Choosing the Optimal Number of Cluster",
#      xlab="number of cluster: 2 to 67",ylab="betweenss/totss")
# points(2:67,result[,2],type="l",col="red")
# points(10,result[10],pch=16)
# legend(10,result[10],paste("(10,",sprintf("%.1f%%",result[10]*100),")",sep=""),bty="n",xjust=0.3,cex=0.8)

# HC #
fit_hc=hclust(dist(countries[,-1]))
print(fit_hc)
plot(fit_hc)

group_k3=cutree(fit_hc,k=3)
group_k3
table(group_k3)

group_h18=cutree(fit_hc,h=18)
group_h18
table(group_h18)

sapply(unique(group_k3),function(g)countries$country[group_k3==g])

plot(fit_hc)
rect.hclust(fit_hc,k=4,border="light grey")
rect.hclust(fit_hc,k=3,border="dark grey")
rect.hclust(fit_hc,k=7,which=c(2,6),border="dark grey")

# DBSCAN #
library(fpc)
# dbscan(data,eps,MinPts=5,scale=FALSE,method=c("hybrid","raw","dist"),
#        seeds=TRUE,showplot=FALSE,countmode=NULL)

ds1=dbscan(countries[,-1],eps=1,MinPts=5)
ds2=dbscan(countries[,-1],eps=4,MinPts=5)
ds3=dbscan(countries[,-1],eps=4,MinPts=2)
ds4=dbscan(countries[,-1],eps=8,MinPts=2)
par(mfcol=c(2,2))
plot(ds1,countries[,-1],main="1: MinPts=5 eps=1")
plot(ds3,countries[,-1],main="3: MinPts=2 eps=4")
plot(ds2,countries[,-1],main="2: MinPts=5 eps=4")
plot(ds4,countries[,-1],main="4: MinPts=2 eps=8")

d=dist(countries[,-1])
max(d);min(d)
library(ggplot2)
interval=cut_interval(d,30)
table(interval)
which.max(table(interval))

for(i in 3:5)
{ for(j in 1:10)
  {  ds=dbscan(countries[,-1],eps=i,MinPts=j)
     print(ds)
  }
}

ds5=dbscan(countries[,-1],eps=3,MinPts=2)
ds6=dbscan(countries[,-1],eps=4,MinPts=5)
ds7=dbscan(countries[,-1],eps=5,MinPts=9)
par(mfcol=c(1,3))
plot(ds5,countries[,-1],main="1: MinPts=2 eps=3")
plot(ds6,countries[,-1],main="3: MinPts=5 eps=4")
plot(ds7,countries[,-1],main="2: MinPts=9 eps=5")


# EM #
library(mclust)
 
fit_EM=Mclust(countries[,-1])
summary(fit_EM)
summary(fit_EM,parameters=TRUE)
plot(fit_EM)

countries_BIC=mclustBIC(countries[,-1])
countries_BICsum=summary(countries_BIC,data=countries[,-1])
countries_BICsum

countries_BIC
plot(countries_BIC,G=1:7,col="black")

names(countries_BICsum)
mclust2Dplot(countries[,-1], classification=countries_BICsum$classification,parameters=countries_BICsum$parameters,col="black")

countries_Dens=densityMclust(countries[,-1])
plot(countries_Dens,countries[,-1],col="grey",nlevels=55)
plot(countries_Dens,type = "persp",col = grey(0.8))


































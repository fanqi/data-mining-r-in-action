install.packages ( "randomForest" )
library(randomForest)

###importance函数示例
set.seed(4)
data(mtcars)
mtcars.rf=randomForest(mpg~.,data=mtcars,ntree=1000,importance=TRUE)
importance(mtcars.rf)	
importance(mtcars.rf, type=1)

###MDSplot函数示例
set.seed(1)	# 设定产生随机数的初始值
data(iris)	# 调用数据集iris
iris.rf=randomForest(Species ~ ., iris, proximity=TRUE)# 基于数据集iris建立随机森林模型
MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))# 绘制图像

###rfImpute函数示例
data(iris)	# 调用数据集iris
iris.na=iris	# 生成需要进行处理的数据集
iris.na[75,2]=NA;iris.na[125,3]=NA;	  # 在第75号样本和第125号样本中设置缺失值
set.seed(111)				# 设置随机数生成器初始值
iris.imputed=rfImpute(Species ~ .,data=iris.na)	# 对数据集iris.na进行插值

###treesize函数示例
iris.rf<- randomForest(Species ~ ., iris)	# 利用数据iris构建相关随机森林模型
hist(treesize(iris.rf))			# 绘制相应的柱状图

###模型可视化示例
data(airquality)		# 调用数据集airquality
set.seed(131)			# 设置随机数生成器初始值
ozone.rf=randomForest(Ozone~.,data=airquality,mtry=3,importance=TRUE,na.action=na.omit)# 建立随机森林回归模型
plot(ozone.rf)			# 绘制相关图像


###实际案例
wine=read.table("d:\\wine.txt") # 本文默认数据以记事本格式存储于电脑D盘中
names(wine)=c("fixed acidity","volatile acidity","citric acid","residual sugar","chlorides","free sulfur dioxide","total sulfur dioxide","density","PH","sulphates","alcohol","quality")	# 为数据集wine各个变量命名
summary (wine)                # 获取wine数据集的概括信息


cha=0	# 设置中间变量对处理后的向量进行临时存储
for(i in 1:4898) # 针对每一个样本进行调整
{
	if(wine[i,12]>6)
	{
		cha[i]="good"	# 将品质大于6的样本品质定义为“good”
	}
	else if(wine[i,12]>5)
	{
		cha[i]="mid"	# 将品质大于5却不大于6的样本品质定义为“mid”
	}
	else
	{
		cha[i]="bad"	# 将品质不大于5的样本品质定义为“bad”
	}
}
wine[,12]=factor(cha)	# 将字符型变量转化为含有因子的变量并复制给数据集wine
summary(wine$quality)
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")

set.seed(71)			# 设置随机数生成器初始值
samp=sample(1:4898,3000)	# 从全部数据集中抽取3000个样本作为训练集
set.seed(111)			# 设置随机数生成器初始值
wine.rf=randomForest(quality~.,data=wine,importance=TRUE,proximity=TRUE,ntree=500,subset=samp)	 # 构建决策树为500棵的随机森林模型

x=wine[-samp,1:11]		# 利用构建模型剩下的样本作为测试集
pred=predict(wine.rf,x)		# 根据模型wine.rf对x数据进行预测
pred[sample(1:1898,8)]		# 随机挑选8个预测结果进行展示

###寻找模型最优节点变量数
n=ncol(wine)-1			# 计算数据集中自变量个数
rate=1				# 设置模型误判率向量初始值
for(i in 8:n)			# 依次逐个增加节点所选变量个数
{
	set.seed(222)		# 设置随机数生成器的初始值
	model=randomForest(quality~.,data=wine,mtry=i,importance=TRUE,ntree=1000)	# 建立随机森林模型
	rate[i]=mean(model$err.rate)				# 计算基于OOB数据的模型误判率均值
	print(model)		# 展示模型简要信息
}
rate			

set.seed(222)		# 设置随机数生成器初始值
model=randomForest(quality~.,data=wine,mtry=1,importance=TRUE,ntree=1000) # 构建随机森林模型
plot(model,col=1:1)
legend(800,0.215,"mid",cex=.9,bty="n")			#为图像添加图例
legend(800,0.28,"bad",cex=.9,bty="n")			#为图像添加图例
legend(800,0.37,"good",cex=.9,bty="n")			#为图像添加图例
legend(800,0.245,"total",cex=0.9,bty="n")		#为图像添加图例


set.seed(222)		# 设置随机数生成器初始值
model=randomForest(quality~.,data=wine,mtry=1,proximity=TRUE,importance=TRUE,ntree=400)# 建立随机森林模型
hist(treesize(model))						 # 展示随机森林模型中每颗决策树的节点数
importance(model)			 # 展示在随机森林模型中各个变量对模型预测能力的影响
MDSplot(model,wine$quality, palette=rep(1, 3), pch=as.numeric(wine$quality))		 # 展示数据集在二维的情况下个类别的具体分布情况
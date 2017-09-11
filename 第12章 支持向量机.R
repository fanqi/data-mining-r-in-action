install.packages("e1071")				# 下载安装e1071软件包
library(e1071)						# 加载e1071软件包

###第一种格式建立模型
data(iris)						# 获取数据集iris
model=svm(Species~.,data=iris)				# 建立svm模型
###第二种格式建立模型
x=iris[,-5]						# 提取iris数据中除第5列以外的数据作为特征变量
y=iris[,5]						# 提取iris数据中的第5列数据作为结果变量
model=svm(x,y,kernel ="radial",gamma =if(is.vector(x)) 1 else 1/ncol(x))   # 建立svm模型

###对模型进行预测
x=iris[,1:4]									  # 确认需要进行预测的样本特征矩阵
pred=predict(model,x)								# 根据模型model对x数据进行预测
pred[sample(1:150,8)]								 # 随机挑选8个预测结果进行展示
table(pred,y)								# 模型预测精度展示

###实际建模过程中完整操作
attach(iris)					# 将数据iris按列单独确认为向量
x=subset(iris,select=-Species)		# 确定特征变量为数据iris中除去Species的其他项
y=Species				# 确定结果变量为数据iris中的Species项
type=c("C-classification","nu-classification","one-classification")# 确定将要适用的分类方式
kernel=c("linear","polynomial","radial","sigmoid")				#确定将要适用的核函数
pred=array(0,dim=c(150,3,4))		#初始化预测结果矩阵的三维长度分别为150，3，4
accuracy=matrix(0,3,4)						#初始化模型精准度矩阵的两维分别为3，4
yy=as.integer(y)					#为方便模型精度计算，将结果变量数量化为1，2，3
for(i in 1:3)								#确认i影响的维度代表分类方式
{
	for(j in 1:4)							#确认j影响的维度代表核函数
	{
		pred[,i,j]=predict(svm(x,y,type=type[i],kernel=kernel[j]),x)   #对每一模型进行预测
		if(i>2)
		{
			accuracy[i,j]=sum(pred[,i,j]!=1)
		}
		else
		{
			accuracy[i,j]=sum(pred[,i,j]!=yy)
		}
	}
}
dimnames(accuracy)=list(type,kernel)					#确定模型精度变量的列名和行名
table(pred[,1,3],y)							# 模型预测精度展示

###模型可视化
plot(cmdscale(dist(iris[,-5])),col=c("lightgray","black","gray")[as.integer(iris[,5])],pch= c("o","+")[1:150 %in% model$index + 1])                # 绘制模型分类散点图
legend(2,-0.8,c("setosa","versicolor","virginica"),col=c("lightgray","black","gray"),lty=1)		# 标记图例

data(iris)										#读入数据iris
model=svm(Species~., data = iris)							#利用公式格式建立模型
plot(model,iris,Petal.Width~Petal.Length,fill=FALSE,symbolPalette=c("lightgray","black","grey"),svSymbol="+")
									#绘制模型类别关于花萼宽度和长度的分类情况
legend(1,2.5,c("setosa","versicolor","virginica"),col=c("lightgray","black","gray"),lty=1)		#标记图例

###模型进一步优化
wts=c(1,1,1)							# 确定模型各个类别的比重为1：1：1
names(wts)=c("setosa","versicolor","virginica")			#确定各个比重对应的类别
model1=svm(x,y,class.weights=wts)				#建立模型
wts=c(1,100,100)						# 确定模型各个类别的比重为1：100：100
names(wts)=c("setosa","versicolor","virginica")			#确定各个比重对应的类别
model2=svm(x,y,class.weights=wts)				#建立模型
pred2=predict(model2,x)						#根据模型进行预测
table(pred2,y)							#展示预测结果
wts=c(1,500,500)						# 确定模型各个类别的比重为1：500：500
names(wts)=c("setosa","versicolor","virginica")			#确定各个比重对应的类别
model3=svm(x,y,class.weights=wts)				#建立模型
pred3=predict(model3,x)						#根据模型进行预测
table(pred3,y)				 			#展示预测结果


###第一种建模格式
wine=read.table("d:\\wine.txt") 	# 本文默认数据以记事本格式存储于电脑D盘中
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")															# 为每一个变量命名
set.seed(71)
samp=sample(1:4898,3000) 		# 从总样本集中抽取3000个样本作为训练集
wine[samp,1:11]=scale01(wine[samp,])	# 对样本进行预处理
r=1/max(abs(wine[samp,1:11]))		# 确定参数rang的变化范围
set.seed(101)
model1=nnet(quality~.,data=wine,subset=samp,size=4,rang=r,decay=5e-4,maxit=200)												# 建立神经网络模型
###第二种建模格式
x=subset(wine,select=-quality)		# 提取wine数据中除quality列以外的数据作为自变量
y=wine[,12]				# 提取wine数据中的quality列数据作为响应变量
y=class.ind(y)				# 对响应变量进行预处理，将其变为类指标矩阵
set.seed(101)
model2=nnet(x,y,decay=5e-4,maxit=200,size=4,rang=r)   	# 建立神经网络模型

###针对第一种格式进行预测
x=wine[,1:11]				# 确认需要进行预测的样本特征矩阵
pred=predict(model1,x,type=”class”)	# 根据模型model1对xt数据进行预测
set.seed(110)
pred[sample(1:4898,8)]			# 随机挑选8个预测结果进行展示

###针对第二种格式进行预测
xt=wine[,1:11]				# 确认需要进行预测的样本特征矩阵
pred=predict(model2,xt)			# 根据模型model2对xt数据进行预测
dim(pred)				# 查看预测结果的维度
pred[sample(1:4898,4),]			# 随机挑选4个预测结果进行展示
name=c("bad","good","mid")		# 为三个类别确定名称
prednew=max.col(pred)			# 确定每行中最大值所在列
prednewn=name[prednew]			# 根据预测结果将其变为相对应的类别名称
set.seed(201)
prednewn[sample(1:4898,8)]		# 随机挑选8个预测结果进行展示
true=max.col(y)				# 确定真实值的每行中最大值所在列
table(true,prednewn)			# 模型预测精度展示


###nnet函数使用过程中特别注意
model1=nnet(x,y,rang=1/max(abs(x)),size=4,maxit=500,decay=5e-4)   # 建立模型model1
model2=nnet(x,y,rang=1/max(abs(x)),size=4,maxit=500,decay=5e-4)   # 建立模型model2
name=c("setosa","versicolor","virginica")			# 为三个类别确定名称
 pred1=name[max.col(predict(model1,x))]
# 利用第二种模型的预测方法对模型model1进行预测
pred2=name[max.col(predict(model2,x))]
# 利用第二种模型的预测方法对模型model2进行预测
 table(Species,pred1)						# 模型model1预测精度展示


###实际建模操作
###确定隐藏层节点数
wine=read.table("d:\\wine.txt") # 本文默认数据以记事本格式存储于电脑D盘中
names(wine)=c("fixed","volatile","citric","residual","chlorides","free","total","density","PH","sulphates","alcohol","quality")															# 为每一个变量命名
set.seed(71)
wine=wine[sample(1:4898,3000),]
nrow.wine=dim(wine)[1]

###原始数据归一化程序
scale01=function(x)
{
	ncol=dim(x)[2]-1
	nrow=dim(x)[1]
	new=matrix(0,nrow,ncol)
	for(i in 1:ncol)
	{
		max=max(x[,i])
		min=min(x[,i])
		for(j in 1:nrow)
		{
			new[j,i]=(x[j,i]-min)/(max-min)
		}
	}
	new
}
cha=0	# 设置中间变量对处理后的向量进行临时存储
for(i in 1: nrow.wine) # 针对每一个样本进行调整
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
set.seed(444)
samp=sample(1:nrow.wine, nrow.wine*0.7) 	# 从总样本集中抽取70%的样本作为训练集
wine[samp,1:11]=scale01(wine[samp,])		# 对训练集样本进行预处理
wine[-samp,1:11]=scale01(wine[-samp,])		# 对测试集样本进行预处理
r=1/max(abs(wine[samp,1:11]))			# 确定参数rang的变化范围
n=length(samp)
err1=0
err2=0
for(i in 1:17)
{	
	set.seed(111)
	model=nnet(quality~.,data=wine,maxit=400,rang=r,size=i,subset=samp,decay=5e-4)
	err1[i]=sum(predict(model,wine[samp,1:11],type='class')!=wine[samp,12])/n
	err2[i]=sum(predict(model,wine[-samp,1:11],type='class')!=wine[-samp,12])/(nrow.wine -n)
}
plot(1:17,err1,'l',col=1,lty=1,ylab="模型误判率",xlab="隐藏层节点个数",ylim=c(min(min(err1),min(err2)),max(max(err1),max(err2))))
lines(1:17,err2,col=1,lty=3)
points(1:17,err1,col=1,pch="+")
points(1:17,err2,col=1,pch="o")
legend(1,0.53,"测试集误判率",bty="n",cex=1.5)
legend(1,0.35,"训练集误判率",bty="n",cex=1.5)

###确定训练周期
err11=0
err12=0
for(i in 1:500)
{
	set.seed(111)	
	model=nnet(quality~.,data=wine,maxit=i,rang=r,size=3,subset=samp)
	err11[i]=sum(predict(model,wine[samp,1:11],type='class')!=wine[samp,12])/n
	err12[i]=sum(predict(model,wine[-samp,1:11],type='class')!=wine[-samp,12])/(nrow.wine-n)
}

plot(1:length(err11),err11,'l',ylab="模型误判率",xlab="训练周期",col=1,ylim=c(min(min(err11),min(err12)),max(max(err11),max(err12))))
lines(1:length(err11),err12,col=1,lty=3)
legend(250,0.47,"测试集误判率",bty="n",cex=1.2)
legend(250,0.425,"训练集误判率",bty="n",cex=1.2)

###最终模型
set.seed(111)
model=nnet(quality~.,data=wine,maxit=300,rang=r,size=3,subset=samp)
x=wine[-samp,1:11]				# 确认需要进行预测的样本特征矩阵
pred=predict(model,x,type="class")		# 根据模型model1对xt数据进行预测
table(wine[-samp,12],pred)
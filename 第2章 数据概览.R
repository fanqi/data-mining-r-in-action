x = c ( 1, 2, 3, 4 )                                
x                                                               
class ( x )                                             

x1 = as.integer ( x )                                   
class ( x1 )                                              # 显示向量x1的数据类型

x = c ( 1, 2, 3, 4 )                                  # 构造元素依次为1,2,3,4的向量x
x==2                                               # 判断向量x中等于2的元素
! ( x<2 )                                         # 判断向量x中大于等于2的元素
which ( x<2 )                                         # 选择向量x中小于2的元素
is.logical ( x )                                      # 判断向量x是否为逻辑性数据

y = c ( "I", "love", "R" )            # 构造元素依次为字符串“I”，“love”，“R”的向量y
y                                                                # 输出y的值
class ( y )                                                # 显示向量y的数据类型
length ( y )                                       # 显示向量y的维度，即元素个数
nchar ( y )                                     # 显示向量y中每个元素的字符个数
y=="R"                                            # 判断向量y中为“R”的元素

sex = factor ( c(1,1,0,0,1), levels=c(0,1), labels=c("male","female") )  # 设置因子型数据sex
sex                                                             # 输出sex的值
class ( sex )                                                # 显示sex的数据类型
sex1 = factor ( c(1,1,0,0,1), levels=c(0,1), labels=c("female","male") )      # 调换标签（labels）的取值，得到因子型数据sex1
sex1                                                           # 输出sex1的值
sex2 = factor (c(1,1,0,0,1), levels=c(1,0), labels=c("male","female") )  # 调换水平（levels）的取值，得到因子型数据sex2
sex2                                                          # 输出sex2的值
num = factor ( c("a","b","c","d") )                             # 设置因子型变量num
as.numeric ( num )                            # 将因子型数据num转换为数值型数据
num1 = factor ( c("b","a","d","c") )        # 调换num中元素顺序，构造因子型变量num1
as.numeric ( num1 )                          # 将因子型数据num1转换为数值型数据
num + 1                                          # 因子型数据不可进行数值运算
as.numeric ( num ) + 1                            # 转换为数值型数据后可参与运算

library ( MASS )                                  # 加载含有数据集的软件包MASS
data ( Insurance )                                          # 获取数据集Insurance

dim ( Insurance )                                             # 获取数据集的维度
dim ( Insurance[1:10, ] )                            # 获取数据集前10条数据的维度
dim ( Insurance[ ,2:4] )                 # 获取数据集仅含第2、3、4个变量部分的维度
dim ( Insurance ) [1]                     # 获取数据集维度向量的第一个元素，即行数
dim ( Insurance ) [2]                     # 获取数据集维度向量的第二个元素，即列数

vars = c ( "District", "Age" )     # 构造含有“District”和“Age”两个元素的字符向量vars
Insurance [ 20:25, vars ]                 # 筛选出District及Age变量的第20-25行数据
names ( Insurance )                                  # 输出Insurance数据集变量名
head ( names(Insurance), n=2 )                                # 仅输出前2个变量名
tail ( names(Insurance), n=2 )                                # 仅输出后2个变量名
head ( Insurance$Age )                              # 仅输出Age变量前若干条数据

class ( Insurance$District )                                # 显示District的变量类型
class ( Insurance$Age )                                      # 显示Age的变量类型
class ( Insurance$Holders )                                # 显示Holders的变量类型

levels ( Insurance$Age )                                # 显示Age变量的4个水平值
levels ( Insurance$Age) [1]                            # 显示Age变量的第1个水平值
levels ( Insurance$Age ) [1] = "young"        # 将Age变量的第1个水平值修改为“young”
head ( Insurance$Age )                          # 回看修改后Age变量前若干个取值

is.character ( Insurance$Age )                           # 判断Age是否为字符型变量
class ( Insurance$Claims )                                  # 显示Claims的变量类型
class ( as.numeric (Insurance$Claims) )         # 将Claims的数据类型强制转换为数值型


# 抽样技术 #
sub1=sample(nrow(Insurance),10,replace=T)
Insurance[sub1,]

sub2=sample(nrow(Insurance),10,replace=T,prob=c(rep(0,nrow(Insurance)-1),1))
Insurance[sub2,]

sub3=sample(nrow(Insurance),nrow(Insurance)+1)

sub4=strata(Insurance,stratanames="District",size=c(1,2,3,4),method="srswor")
sub4
getdata(Insurance,sub4)

sub5=strata(Insurance,stratanames="District",size=c(1,2,3,4),description=TRUE)
sub5
getdata(Insurance,sub5)

sub6=strata(Insurance,stratanames="District",size=c(1,2,3,4),method="systematic",pik=Insurance$Claims)
sub6
getdata(Insurance,sub6)

sub7=cluster(Insurance,clustername="District",size=2,method="srswor",description=TRUE)
sub7

sub8=mstage(Insurance, stage = c("stratified","stratified"), varnames=c("District","Group"), size=list(c(16,16,16,16),2),description=TRUE)
sub8
getdata(Insurance,sub8)

train_sub=sample(nrow(Insurance),3/4*nrow(Insurance))
train_data=Insurance[train_sub,]
test_data=Insurance[-train_sub,]
dim(train_data);dim(test_data)












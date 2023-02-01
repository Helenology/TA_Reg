#读取数据
a=read.csv("第3章.csv",header=T)
#查看前五行
a[c(1:5),]

#--------------该数据的行数（样本量），列数（字段个数），ST样本总数，以及占比情况--------------
#dim(a)表示查看数据的维度，第一个数字表示有多少行，第二个数字表示有多少列
#sum(a$ST)计算ST=1的样本数目
#mean(a$ST)计算ST=1的本占比
c(dim(a),sum(a$ST),mean(a$ST))

#-------------对自变量单独做描述行分析-------------
#对X变量ARA（应收账款占比）做直方图
hist(a$ARA,main="应收账款占比",xlab="ARA" ,ylab = "频数")

#对另外6个X变量做同样的操作，但是放在一个2*3的画板中
par(mfrow=c(2,3))
hist(a$ASSET,main="对数资产规模",xlab="ASSET",ylab = "频数")
hist(a$ATO,main="资产周转率",xlab="ATO",ylab = "频数")
hist(a$ROA,main="资产回报率",xlab="ROA",ylab = "频数")
hist(a$GROWTH,main="销售收入增长率",xlab="GROWTH",ylab = "频数")
hist(a$LEV,main="杠杆水平",xlab="LEV",ylab = "频数")
hist(a$SHARE,main="第一大股东持股比率",xlab="SHARE",ylab = "频数")


#-------------自变量和因变量关系的描述行分析-------------
# 将因变量ST纳入描述分析之中。根据ST取值（0或者1），将数据分成两组，然后用箱线图，对ARA做对比分析
par(mfrow=c(1,1))
boxplot(ARA~ST,ylab="应收账款占比",xlab="是否ST",data=a,names=c("非ST","ST"))

# 对其他6个X变量做同样的分析，但是放在一个2*3的画板中
par(mfrow=c(2,3))
boxplot(ASSET~ST,ylab="对数资产规模",xlab="是否ST",data=a,names=c("非ST","ST"))
boxplot(ATO~ST,ylab="资产周转率",xlab="是否ST",data=a,names=c("非ST","ST"))
boxplot(ROA~ST,ylab="资产回报率",xlab="是否ST",data=a,names=c("非ST","ST"))
boxplot(GROWTH~ST,ylab="销售收入增长率",xlab="是否ST",data=a,names=c("非ST","ST"))
boxplot(LEV~ST,ylab="杠杆水平",xlab="是否ST",data=a,names=c("非ST","ST"))
boxplot(SHARE~ST,ylab="第一大股东持股比率",xlab="是否ST",data=a,names=c("非ST","ST"))

#------- 对所有的变量（X和Y），计算基本的关键统计量-------------
# 计算每一列的样本量N
N=sapply(a,length)
# 计算每一列的样本均值
MU=sapply(a,mean) 
# 计算每一列的样本标准差SD
SD=sapply(a,sd)
# 计算每一列的最小值
MIN=sapply(a,min)
# 计算每一列的中位数
MED=sapply(a,median)
# 计算每一列的最大值
MAX=sapply(a,max)
# 按列合并
result=cbind(N,MU,SD,MIN,MED,MAX)
result


#-------------接下来用glm函数建立逻辑回归模型-------------
# 请注意设定family为binomial，这表示Y是一个0-1型因变量。同时，还要设定link=logit，表示这是一个逻辑回归
model.full=glm(ST~ARA+ASSET+ATO+GROWTH+LEV+ROA+SHARE,family=binomial(link=logit),data=a)
summary(model.full)

# 做一个关于该模型全局显著性的卡方检验。
# 统计量为：282.07-251.51=30.56，对应的应该是一个自由度为：683-676=7的卡方分布。
# 对应的P-值可以通过下面代码计算得到。
1-pchisq(30.56,df=7)


#------模型选择--------------
#计算模型的AIC的取值，和BIC取值
c(AIC(model.full),BIC(model.full))
# 自动寻找最优的AIC模型，默认k=2表示aic
model.aic=step(model.full,trace = F)
summary(model.aic)
#ss为样本的行数
ss=length(a[,1])
# 自动寻找最优的BIC模型，k=log(ss)表示bic
model.bic=step(model.full,trace = F,k=log(ss))
summary(model.bic)


#------画出相应的ROC曲线--------------
library(pROC)
#得出相应模型的预测值
pred.full=predict(model.full,a)
pred.aic=predict(model.aic,a)
pred.bic=predict(model.bic,a)
Y=a$ST
#如何对这三个模型计算AUC 
roc.full=roc(Y,pred.full)
roc.aic=roc(Y,pred.aic)
roc.bic=roc(Y,pred.bic)
c(roc.full$auc,roc.aic$auc,roc.bic$auc)
#画相应的roc曲线，放在一个1*3的画板中
par(mfrow=c(1,3))
plot(roc.full,main="全模型")
plot(roc.aic,main="AIC模型")
plot(roc.bic,main="BIC模型")

#-----在4:1的比例分配下（80%的训练数据，20%的验证数据），计算外样本AUC，重复实验100次-------
nsimu=100 # 重复实验的次数
p=0.8
ss0=round(ss*p) #训练数据集的样本数
AUC=as.data.frame(matrix(0,nsimu,3))
names(AUC)=c("全模型","AIC模型","BIC模型")
for(i in 1:nsimu){
  #每次随机试验，我们将原数据随机排序
  aa=a[order(runif(ss)),]
  #选取前80%的数据作为训练数据
  A0=aa[c(1:ss0),]
  #选取后20%的数据作为验证数据
  A1=aa[-c(1:ss0),]
  
  #利用训练数据建立3个不同的模型，在训练样本上获得三个模型的参数估计
  #模型1:全模型
  #模型2: AIC模型（ARA+GROWT+LEV）
  # 模型3: BIC模型（ARA）
  model.1=glm(ST~ARA+ASSET+ATO+GROWTH+LEV+ROA+SHARE,family=binomial(link=logit),data=A0)
  model.2=glm(ST~ARA+GROWTH+LEV,family=binomial(link=logit),data=A0)
  model.3=glm(ST~ARA,family=binomial(link=logit),data=A0)
  
  #在测试样本上计算各个模型的外样本AUC
  pred.1=predict(model.1,A1)
  pred.2=predict(model.2,A1)
  pred.3=predict(model.3,A1)
  Y=A1$ST
  auc.1=roc(Y,pred.1)$auc
  auc.2=roc(Y,pred.2)$auc
  auc.3=roc(Y,pred.3)$auc
  
  AUC[i,]=c(auc.1,auc.2,auc.3)
}

par(mfrow=c(1,1))
boxplot(AUC,main="外样本AUC对比")

#------------模型应用（利用AIC模型帮助投资者预判目标股票未来被ST的可能性）-----------------------
# 假设某企业销售收入增长率为20%，应收账款占比高达60%，资产债务比率也多达70%
newdata = data.frame(LEV = 0.7,ARA=0.6,GROWTH=0.2)
# 预测该企业未来被ST的风险，计算结果为52%.# 注意，predict函数得到的结果Z并非被ST的可能性，要进行exp"(Z)/{1+" exp"(Z)}" 变换之后得到的才是被ST的可能性估计值
exp(predict(model.aic,newdata))/(1+exp(predict(model.aic,newdata)))

#计算行业内被ST的风险的平均水平，计算结果为5%。该企业未来被ST的风险为52%，远高于行业平均水平5%
prediction = exp(predict(model.aic,a))/(1+exp(predict(model.aic,a))) 
summary(prediction)

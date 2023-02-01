#读取数据
a = read.csv("第1章.csv",header=T)
#将列名从左到右命名为"Y", "X1", "X2", "X3"
names(a) = c("Y", "X1", "X2", "X3")
#查看前五行
a[c(1:5),]

#--------利用hist函数对每个变量绘制直方图，观察一下数据分布的形状--------
par(mfrow = c(2,2))
hist(a$Y, xlab = "间接价值", ylab = "频数",main=NULL)
hist(a$X1, xlab = "通话时长（百分钟）", ylab = "频数",main=NULL)
hist(a$X2, xlab = "大网占比（%）", ylab = "频数",main=NULL)
hist(a$X3, xlab = "小网占比（%）", ylab = "频数",main=NULL)

#--------对a中的每一列指标计算各种描述统计量--------
N = sapply(a,length) # 计算每一列的样本量N
MU = sapply(a,mean) # 计算每一列的样本均值MU 
SD = sapply(a,sd)  # 计算每一列的样本标准差SD 
MIN = sapply(a,min) # 计算每一列的最小值MIN
MED = sapply(a,median) # 计算每一列的中位数MED
MAX = sapply(a,max) # 计算每一列的最大值MAX
result = cbind(N,MU,SD,MIN,MED,MAX) #将之前计算的描述统计量合并在一起
result



#--------对每一个X变量，结合Y做进一步的描述统计分析--------
# 首先以因变量Y的中位数为阈值，将数据分成两组（高价值组、低价值组），并生成新变量命名为“cat”
a$cat = as.factor((a$Y > median(a$Y))*1) 
#将低价值组命名为“低”，将高价值组命名为“高”
levels(a$cat) = c("低","高")
# 修改默认的排序为从“高”到“低”
a$cat = factor(a$cat,levels=c("高","低"))
# 对每一个X变量，都对刚刚生成的“cat”变量分组做箱线图（boxplot）
par(mfrow = c(1,3))
boxplot(X1~cat,data=a,xlab="间接价值", ylab="通话时长（百分钟）")
boxplot(X2~cat,data=a,xlab="间接价值", ylab="大网占比（%）")
boxplot(X3~cat,data=a,xlab="间接价值", ylab="小网占比（%）")

#-----通过lm函数建立全模型-------
fit = lm(Y~X1+X2+X3, data = a)
#输出回归模型的拟合结果
summary(fit)

#-----在4:1的比例分配下（80%的训练数据，20%的验证数据），计算外样本判决系数-------
nsimu = 1000					
ss = length(a[,1])
ss0 = round(ss*0.8) #训练数据的数量
R2 = rep(0,nsimu)
for(i in 1:nsimu){
  #将原始数据的顺序打乱
  a = a[order(runif(ss)),] 
  #选取前80%的数据作为训练数据
  a0 = a[c(1:ss0),] 
  #选取后20%的数据作为验证数据
  a1 = a[-c(1:ss0),]
  #利用训练数据建立模型
  fit0 = lm(Y~X1+X2+X3, data=a0)
  #对验证集做预测
  Y.hat = predict(fit0, a1)
  Y.true = a1$Y
  #计算因变量中由于随机干扰项所产生的变异性
  sse = sum((Y.hat-Y.true)^2)
  #计算因变量Y自身的变异性
  sst = sum((Y.true-mean(Y.true))^2)
  #计算外样本判决系数
  R2[i] = (1-sse/sst)*100 
}
#用箱线图展示外样本判决系数
par(mfrow = c(1,1))
boxplot(R2)

#-------------------对每个变量计算VIF---------------
library(car)
vif(fit)

#-------------对每个样本计算Cook距离，并展示出来---------------
plot(fit,which = 4)


#-------------残差图---------------
plot(fit, which = 1)

#------虚构数据的散点图--------------
#生成自变量
x = rnorm(1000) 
#生成因变量
y = x + rnorm(1000) 
#其中第10号样本的残差的绝对值，明显比其他样本的残差要更大
y[10] = y[10] - 30 
myfit = lm(y~x)
plot(myfit, which = 1)

#------模型选择--------------
#AIC模型选择
model.aic = step(fit, trace=F)
#BIC模型选择
model.bic = step(fit, k=log(ss), trace=F)
summary(model.bic)

#------模型应用--------------
#人群细分
library(dplyr)
#以通话时长的中位数为阈值， 将样本分为两组，一组为低通话组，另一组为高通话组
a$X1_new = (a$X1 < median(a$X1))*1
#以大网占比的中位数为阈值， 将样本分为两组，一组为低大网占比组，另一组为高大网占比组
a$X2_new = (a$X2 < median(a$X2))*1
# 将客户群体细分，形成4个细分的客户群体，输出的数字表示每类人群的相对占比
table(a$X2_new,a$X1_new)/nrow(a)
a$X2_new %>% table
# 将客户群体细分，形成4个细分的客户群体，输出的数字表示每类人群的绝对数量
table(a$X2_new,a$X1_new)

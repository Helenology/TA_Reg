#读取数据
a=read.csv("第2章.csv")
#查看前五行
a[1:5,]
#查看数据的维度，第一个数字表示有多少行，第二个数字表示有多少列
dim(a)

#------通过hist命令，对二手房价格做直方图------------
par(mfrow=c(1,2))
#通过hist命令，对二手房原始价格做直方图
hist(a$price,xlab="原始价格",ylab = "频数",main=NULL)
#通过hist命令，对对数变换后的价格做直方图
hist(log(a$price),xlab="对数价格",ylab = "频数",main=NULL)

#------对唯一的连续型X变量，房屋面积做直方图------
par(mfrow=c(1,1))
hist(a$area,xlab="房屋面积（平方米）",ylab = "频数",main=NULL)

# 得到新的Y变量（对数变换后的房屋价格）
a$Y=log(a$price)
summary(a)

#------对其他的离散型X变量，结合Y变量（对数变换后的房屋价格）做描述统计-------
#写一个函数，输入为变量名，输出为该变量和因变量关系的描述性统计量
descrb = function(var){
  Z=a[,var]
  #对输入变量分组计算样本量
  N=tapply(a$Y,Z,length)
  #对输入变量分组计算因变量的均值
  MU=tapply(a$Y,Z,mean)
  #对输入变量分组计算因变量的标准差
  SD=tapply(a$Y,Z,sd)
  #对输入变量分组计算因变量的最小值
  MIN=tapply(a$Y,Z,min)
  #对输入变量分组计算因变量的中位数
  MED=tapply(a$Y,Z,median)
  #对输入变量分组计算因变量的最大值
  MAX=tapply(a$Y,Z,max)
  #按列合并
  out=cbind(N,MU,SD,MIN,MED,MAX)
  out
}
descrb("district") 
descrb("bedrooms") 
descrb("halls") 
descrb("floor") 
descrb("subway") 
descrb("school")

#------对离散型X变量，都分组做Y变量的箱线图（boxplot）------
par(mfrow=c(3,2))
boxplot(Y~district,a,ylab="对数价格",xlab="行政区划")
boxplot(Y~bedrooms,a,ylab="对数价格",xlab="卧室个数")
boxplot(Y~halls,a,ylab="对数价格",xlab="客厅个数")
boxplot(Y~floor,a,ylab="对数价格",xlab="楼层高低",names=c("高","低","中"))
boxplot(Y~subway,a,ylab="对数价格",xlab="有无地铁",names=c("无","有"))
boxplot(Y~school,a,ylab="对数价格",xlab="是否学区房",names=c("否","是"))


#------建立全模型A------
Model.A=lm(Y~district*subway+school+floor+as.factor(bedrooms)+as.factor(halls)+area,data=a)
summary(Model.A)

#------建立对照模型B  （省略了客厅数目这个因素）------
Model.B=lm(Y~district*subway+school+floor+as.factor(bedrooms)+area,data=a)
summary(Model.B)
# 对模型A和模型B做一个对比方差分析，以检验客厅数目这个因素是否重要
anova(Model.B,Model.A)


#------建立没有交互作用的模型C------
Model.C=lm(Y~district+subway+school+floor+as.factor(bedrooms)+
             as.factor(halls)+area,data=a)
#对模型A和模型C做一个对比方差分析, 以检验行政区划与有无地铁的交互是否重要
anova(Model.C,Model.A)
summary(Model.C)


#-----对原始数据做了一点随机扰动--------
#人为地重新生成了卧室数目，完全随机的，同对数房价Y毫无关系; 用这个人为生成的卧室数目替换了原始数据的卧室数目。
aa=a
ss=length(a[,1])
aa$bedrooms=floor(6*runif(ss))
aa[1:5,]

#对随机扰动后的数据重新建立模型
Model.S=lm(Y~district*subway+school+floor+as.factor(bedrooms)+as.factor(halls)+area,data=aa)
summary(Model.S)

#------模型选择--------------
#AIC模型选择
Model.AIC=step(Model.S,trace=F)
#BIC模型选择
Model.BIC=step(Model.S,trace=F,k=log(ss))
summary(Model.BIC)


#------模型应用（二手房价估算器）-------
#假设有一家三口，父母为了能让孩子在西城区上学，想买一套邻近地铁的两室一厅二手房，面积是85平方米，低层楼层，那么房价大约是多少呢？
newdata = data.frame(district = "西城",bedrooms=2,halls=1,area=85,
                     floor="low",subway=1,school=1)
# 可通过计算器，预测得到的单位面积房价为9.50万元/平方米
exp(predict(Model.A,newdata))
# 总价为807.16万元
exp(predict(Model.A,newdata))*85


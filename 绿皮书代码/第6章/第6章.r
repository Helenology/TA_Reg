#读取数据
a=read.csv("第6章.csv",header=T)
nrow(a)
#将列名从左到右命名为"hk","gender","age","Y","C"
names(a)=c("hk","gender","age","Y","C")
# 新增变量age2，按0-10，10-20等10岁为一个年龄区间分组
a$age2=floor(a$age/10)
a[c(1:5),]

# 了解整个数据的大概情况。共有1300个样本，6个变量，离职率77.3%。这说明员工流失很严重。
c(dim(a),mean(a$C))

# 生成整合后的生存数据YS
library(survival)
a$YS=Surv(a$Y,a$C)
# YS这一列中带着加号（被截断）的数据
a[1:5,]
head(a$YS,100)

# 对整合后的生存数据YS做KM估计
km.fit=survfit(a$YS~1)
summary(km.fit)

# 基于KM估计做生存曲线图。
plot(km.fit,xlab="生存时间",ylab="生存概率")

# 基于整合后的生存数据YS，对户口（hk）做基于KM曲线的描述统计分析
plot(survfit(YS~hk,data=a),col=c(1,2),lty=c(1,2),xlab="生存时间",ylab="生存概率")
legend(24,0.9,c("本地户口","异地户口"),col=c(1,2),lty=c(1,2),lwd=c(1,2),cex=0.9)

# 基于整合后的生存数据YS，对性别（gender）做基于KM曲线的描述统计分析
plot(survfit(YS~gender,data=a),col=c(1,2),lty=c(1,2),xlab="生存时间",ylab="生存概率")
legend(24,0.9,c("男性","女性"),col=c(1,2),lty=c(1,2),lwd=c(1,2),cex=1)

# 基于整合后的生存数据YS，对年龄组（age2）做基于KM曲线的描述统计分析
plot(survfit(YS~as.factor(age2),data=a),col=c(1,2,3),lty=c(1,2,3),xlab="生存时间",ylab="生存概率")
legend(24,0.9,c("20-30","30-40","40-50"),col=c(1,2,3),lty=c(1,2,3),lwd=c(1,2),cex=1)

# 基于整合后的生存数据YS，做AFT模型分析
model.aft=survreg(YS~hk+gender+as.factor(age2),data=a)	
summary(model.aft)										

# 基于整合后的生存数据YS，做COX模型分析
model.cox=coxph(YS~hk+gender+as.factor(age2),data=a)
summary(model.cox)

#-------------------------模型应用-----------------------
# 接下来，假设企业正在面试一个新的求职者（男性，年龄25岁，异地户口），
# 预测其在职时长曲线和离职风险的代码如下。

pct = 1:99/100
ptime = predict(model.aft,newdata=data.frame(hk="异地",gender="男",age2=2),
                type="quantile",p=pct) 
# 从下图可以看到该求职者的在职时长曲线
matplot(ptime,1-pct,xlab="在职时长",ylab="在职概率",type="l",col="darkorchid1",lwd=3)
# 再结合分位数可知，该求职者在职时长半年以上的概率小于50%，1年以上的概率小于25%，看起来忠诚度较低。
quantile(ptime)

# 以企业的平均水平（异地户口占比58.38%，女性占比34.08%，30-40岁占比43.08%，40-50岁占比5.15%）为基准，
coefCPH = coef(model.cox) 
meanhk  = sum(a$hk == "异地")/length(a$hk)    #异地户口占比58.38%
meangender  = sum(a$gender == "女")/length(a$gender)                    #女性占比34.08%
age3 = sum(a$age2 == 3)/length(a$age2)   #30-40岁年龄段占比43.08%
age4 = sum(a$age2 == 4)/length(a$age2) #40-50岁年龄段占比5.15%
# 该求职者离职的相对风险（hazard ratio）为1.55，该相对风险取值与时间无关
rMean = exp(coefCPH[1]*meanhk + coefCPH[2]*meangender+coefCPH[3]*age3+coefCPH[4]*age4)
r12 = exp(coefCPH[1]*1 + coefCPH[2]*0+coefCPH[3]*0 + coefCPH[4]*0)
names(r12) = names(rMean) = NULL
r12/rMean


# 在R中只需要在predict函数中选择type=“risk”即可计算相对风险，计算结果和上面代码手动计算结果一致
predict(model.cox,newdata=data.frame(hk="异地",gender="男",age2=2),type="risk")
# 该求职者离职的相对风险，大大高于企业已有员工的平均离职相对风险（均值为1.05，中位数为0.95）。
summary(predict(model.cox,newdata=a,type="risk") )

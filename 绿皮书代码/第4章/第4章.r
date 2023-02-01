#读取数据
a=read.csv("第4章.csv",header=T)
#将列名从左到右命名为"gender","usage","credit","loan","history","accounts","status"
names(a)=c("gender","usage","credit","loan","history","accounts","status")
# 得到新的Y1变量：是否逾期，是为1，否为0
a$Y1=1*(a$status>0)
a$Y2=a$status
a[c(1:5),]
summary(a)

#数据大小
dim(a)
#逾期样本比例
mean(a$Y1)

# 对逾期样本的各种逾期情况分别计算频数，并做柱状图
a1=a[a$Y1==1,]
a1[1:5,]
tab=table(a1$Y2)
barplot(tab,xlab="逾期严重程度",main=NULL)

# 把过度严重的逾期情况（Y2 >= 4）整合成为一类
a$Y2=4*(a$Y2>4)+a$Y2*(a$Y2<=4)
# 单独提取出逾期样本，命名为a1
a1=a[a$Y1==1,]
a1[1:5,]
# 对a1中的变量逾期严重程度Y2绘制新的直方图
tab=table(a1$Y2)
barplot(tab,xlab="逾期严重程度",main=NULL)


#--------自变量描述行分析-----------------

# 计算性别分布情况
tab=table(a$gender)
tab
tab/sum(tab)

# 对连续型解释变量信用卡使用率做直方图
hist(a$usage,xlab="信用卡相对使用率", ylab = "频数",main=NULL)
# 肉眼检查使用率大于4的样本
a[a$usage>4,]


# 定义正常使用率（usage=1）和非常正使用率（usage=0）如下。
# 从中可以看到正常组和异常组样本量分别为：4671和3329，它们全样本占比分别为：58.4%和41.6%
a$usage=1*(a$usage<=1)
tab=table(a$usage)
tab
tab/sum(tab) 

# 获得授信额度直方图，从中可以看到授信额度的直方图并不是非常连续。并且该图严重右偏，这说明有少数用户的授信额度很高
hist(a$credit,xlab="授信额度（元）", ylab = "频数",main=NULL)
# 对于这一情况可以尝试对数变换，绘制对数变换后直方图。 发现分布情况似乎好了很多，整个分布形状更加对称了
a$credit=log(a$credit)
hist(a$credit,xlab="对数授信额度", ylab = "频数",main=NULL)


# 定义跟房贷月供相关的两个解释变量。
# 其中Z1是一个0-1型哑变量，表示该用户是否有房贷，Z1=1表示有房贷，Z1=0表示没有房贷。
a$Z1=1*(a$loan>0)
# Z2的非零部分对应的是有房贷用户的房贷金额
a$Z2=a$loan
#对Z2的非零部分取值做对数变换
a[a$loan>0,]$Z2=log(a[a$loan>0,]$Z2)
a[1:5,]
# 计算房贷比例如下，从中可见有房贷的样本占比为17.0%
mean(a$Z1)
# 对Z2的非零部分（对应的是有房贷用户）的做直方图。 从中可以看出一个非常优美的，近似于正态的分布
a2=a[a$Z1==1,]
hist(a2$Z2,xlab="对数房贷月供", ylab = "频数",main=NULL)

# 对历史逾期次数，计算频数并画图。 可见这是一个优美的柱状图，拖着长长的向右倾斜的尾巴。从中可以看到，在样本中，有不少人有历史逾期记录。
tab=table(a$history)
barplot(tab,xlab="历史逾期次数", ylab = "频数",main=NULL)

# 对信用卡开户数，计算频数并画图如下，可见该变量的分布情况比较连续。但是，令人有点担忧的是有少数样本取值非常高（最高能到67！）。
tab=table(a$accounts)
barplot(tab,xlab="信用卡开户数", ylab = "频数",main=NULL)

#--------------结合Y1（是否逾期）对解释变量做描述统计-------------------
# 结合Y1（是否逾期），对三个离散型解释变量做描述统计。 三个离散型解释变量为： 性别（男性、女性），信用卡使用频率（0=不正常，1=正常），是否有房贷（有，没有）。
mu1=tapply(a$Y1,a$gender,mean)
mu2=tapply(a$Y1,a$usage,mean)
mu3=tapply(a$Y1,a$Z1,mean)
# 从中可以看到男性客户逾期比率64.3%，明显高于女性客户的55.3%。信用卡相对使用情况正常的客户逾期比率为54.1%，远低于非正常客户的71.8%。无房贷客户逾期比率为63.9%，要高于有房贷客户的49.4%。
mu1
mu2
mu3

# 对剩下的连续型（或者近似连续型）解释变量和Y1做箱线图
# 从中可以看到，逾期用户的授信额度偏低，历史逾期次数偏高，而信用卡开户数目似乎差别不大。
par(mfrow=c(1,3))
boxplot(credit~Y1,data=a,ylab="对数授信额度",xlab="是否逾期",names=c("非逾期","逾期"))
boxplot(history~Y1,data=a,ylab="历史逾期次数",xlab="是否逾期",names=c("非逾期","逾期"))
boxplot(accounts~Y1,data=a,ylab="信用卡开户数",xlab="是否逾期",names=c("非逾期","逾期"))


# 对有房贷的用户的月供金额（对数变换后），做箱线图如下。
par(mfrow=c(1,1))
# 首先筛选出所有的有房贷样本
a1=a[a$loan>0,]
# 然后再根据Y1的取值分两组，从下图可以看出逾期用户和非逾期用户之间的对数月供差别似乎并不明显
boxplot(Z2~Y1,data=a1,ylab="对数月供",xlab="是否逾期",names=c("非逾期","逾期"))


#--------------结合Y2（逾期严重程度）对解释变量做描述统计--------------------------
# 由于我们关注的是逾期程度，因此剔除非逾期的正常用户。
aa=a[a$Y1==1,]

#结合Y2对三个离散型解释变量做描述统计。 它们分别是：性别（男性、女性），信用卡使用频率（0=不正常，1=正常），还有是否有房贷（有，没有）。
# 从中可以看到，逾期越严重，男性占比越高，变化范围大概从68%到73%。同时发现，逾期严重程度同信用卡正常使用率占比、房贷占比的关系很混乱，缺乏清晰规律
aa$sex=1*(aa$gender=="男性")

#对Y2的不同逾期严重程度分组计算男性占比，正常使用率占比，房贷占比
par(mfrow=c(1,3))
plot(tapply(aa$sex,aa$Y2,mean),type="b",ylab="男性占比",xlab="逾期严重程度",ylim=c(0,1))
plot(tapply(aa$usage,aa$Y2,mean),type="b",ylab="正常使用率占比",xlab="逾期严重程度",ylim=c(0,1))
plot(tapply(aa$Z1,aa$Y2,mean),type="b",ylab="房贷占比",xlab="逾期严重程度",ylim=c(0,1))


# 对Y2的每一组，观察4个数值型变量（对数授信额度、历史逾期次数、信用卡开户数、对数月供）的分布情况，并通过箱线图展示不同组的差异。
# 从中可以看出，对数授信额度越高的组逾期严重程度似乎越低，就历史逾期次数而言，逾期更严重的两组（3、4组）似乎更高。
# 同样的，信用卡开户数也有类似的规律，逾期最严重的一组（4组）的信用卡开户数似乎最高。而逾期严重程度同对数月供之间缺乏清晰规律
par(mfrow=c(2,2))
boxplot(credit~Y2,data=aa,ylab="对数授信额度",xlab="逾期严重程度")
boxplot(history~Y2,data=aa,ylab="历史逾期次数",xlab="逾期严重程度")
boxplot(accounts~Y2,data=aa,ylab="信用卡开户数",xlab="逾期严重程度")
# 筛选出所有的有房贷样本
aa1=aa[aa$loan>0,]
#通过箱线图展示逾期严重程度同对数月供之间的关系
boxplot(Z2~Y2,data=aa1,ylab="对数月供",xlab="逾期严重程度")

#-----------------逾期风险模型： 对预期风险（即：是否逾期）建立0-1回归分析模型。以Y1为因变量，建立回归全模型-----------
model.full=glm(Y1~gender+usage+credit+Z1+Z2+history+accounts,
               family=binomial(link=logit),data=a)
summary(model.full)
# 空模型（只含有截距项的逻辑回归模型）的离差（deviance）是10665.2，（Null deviance）对应的自由度是：样本量（8000）-1（截距项）=7999。残差的deviance是8924.5，相应的自由度是：样本量（8000）-1（截距项）-解释变量个数（7）=7992。因此，可以做一个关于该模型全局显著性的卡方检验。统计量为：10665.2-8924.5=1740.7，
10665.2-8924.5
# 对应的应该是一个自由度为：7999-7992=7的卡方分布。对应的P-值可以通过下面代码计算得到。
1-pchisq(1740.7,df=7)

#自动寻找最优的AIC模型。与全模型相比，AIC模型放弃了一个解释变量：Z1（是否有月供）。
model.aic=step(model.full,trace=F)
summary(model.aic)
# 可以做一个关于该模型全局显著性的卡方检验。统计量为：10665.2-8925.1=1740.1，
10665.2-8925.1
# 对应的应该是一个自由度为：7999-7993=6的卡方分布。
# 对应的P-值可以通过下面代码计算得到。可见非常显著，这说明该模型是显著的。
1-pchisq(1740.1,df=6)

#自动寻找最优的BIC模型。
ss=length(a[,1])
model.bic=step(model.full,trace=F,k=log(ss))
# 可见模型结果跟AIC模型基本类似。唯一区别在于：BIC模型放弃了一个解释变量：信用卡开户数。
summary(model.bic)

# 空模型（只含有截距项的逻辑回归模型）的deviance是10665.2（Null deviance），对应的自由度是：样本量（8000）-截距项（1）=7999。
# 残差的deviance（即：模型deviance）是8927.9，相应的自由度是：样本量（8000）-截距项（1）-解释变量个数（5）=7994。
# 因此，可以做一个关于该模型全局显著性的卡方检验。统计量为：10665.2-8927.9=1737.3，对应的应该是一个自由度为：7999-7994=5的卡方分布。
10665.2-8927.9
# 对应的P-值可以通过下面代码计算得到。可见非常显著，这说明该模型是显著的
1-pchisq(1737.3,df=5)

#--------------------对三个模型做内样本预测代码如下-----------------
library(pROC)
pred.full=predict(model.full,data=a)
pred.aic=predict(model.aic,data=a)
pred.bic=predict(model.bic,data=a)
# 计算AUC取值
roc.full=roc(a$Y1,pred.full)
roc.aic=roc(a$Y1,pred.aic)
roc.bic=roc(a$Y1,pred.bic)
# 可见三个模型的AUC取值分别为76.77%，76.77%，和76.70%，几乎没有任何差别。因此，BIC模型是一个更好的选择（因为最简单）。
print(c(roc.full$auc,roc.aic$auc,roc.bic$auc))

# 画出ROC曲线如下
par(mfrow=c(1,3))
plot(roc.full,main="全模型")
plot(roc.aic,main="AIC模型")
plot(roc.bic,main="BIC模型")


#-----------------------逾期严重程度模型：在有逾期的样本上建立关于逾期严重程度的定序回归模型。----------------------
library(MASS)
#建立全模型
probit.full=polr(as.factor(Y2)~gender+usage+credit+Z1+Z2+history+accounts,method="probit",Hess=T,data=aa)
summary(probit.full)
# polr函数的输出结果中只包含参数的t统计量，而没有相应的P-value，因此需要通过以下代码计算相应的P-value。
tab = as.data.frame(coefficients(summary(probit.full))) 
tab$p.value = round(2*(1-pnorm(abs(tab$`t value`))),3)
tab
# 建立空模型（只含有截距项的定序回归模型）
probit.null = polr(as.factor(Y2)~1,method="probit",Hess=T,data=aa)
summary(probit.null)
# 空模型的deviance是12802.75，对应的自由度是：样本量（8000）-3（截距项）=7997。残差的deviance是12612.04，相应的自由度是：样本量（8000）-3（截距项）-解释变量个数（7）=7990。
# 因此，可以做一个关于全模型全局显著性的卡方检验。统计量为：12802.75-12612.04=190.71，对应的应该是一个自由度为：7997-7990=7的卡方分布。
12802.75 - 12612.04
# 对应的P-值可以通过下面代码计算得到。可见非常显著，这说明全模型是显著的
1-pchisq(190.71,df=7)


#AIC模型
# AIC模型选中5个解释变量。他们分别是：性别（男性、女性）、授信额度（对数变换后），月供金额（对数变换后）、历史逾期次数，以及信用卡开户数目。
probit.aic=step(probit.full,trace=F)
summary(probit.aic)
tab = as.data.frame(coefficients(summary(probit.aic))) 
# 在10%显著性水平下，这5个变量都显著。
tab$p.value = round(2*(1-pnorm(abs(tab$`t value`))),3)
tab
# 残差的deviance是12614.12，相应的自由度是：样本量（8000）-3（截距项）-解释变量个数（5）=7992。
# 因此，可以做一个关于AIC模型全局显著性的卡方检验。统计量为：12802.75-12614.12=188.63，对应的应该是一个自由度为：7997-7992=5的卡方分布。
# 对应的P-值可以通过下面代码计算得到，非常显著，这说明AIC模型是显著的。
12802.75 - 12614.12 
1-pchisq(188.63,df=5)

#BIC模型
ss2=length(aa[,1])
probit.bic=step(probit.full,trace=F,k=log(ss2))
# 相应的BIC模型比AIC模型更严格，去掉了2个解释变量：性别和月供金额（对数变换后）。
summary(probit.bic)
tab = as.data.frame(coefficients(summary(probit.bic))) 
tab$p.value = round(2*(1-pnorm(abs(tab$`t value`))),3) 
# 被BIC选中的解释变量都是高度显著的。
tab
# 残差的deviance是12621.05，相应的自由度是：样本量（8000）-3（截距项）-解释变量个数（3）=7994。
# 因此，可以做一个关于bic模型全局显著性的卡方检验。统计量为：12802.75-12621.05 =181.7，对应的应该是一个自由度为：7997-7994=3的卡方分布。
# P-值如下，可见非常显著，这说明bic模型是显著的。
12802.75 - 12621.05 
1-pchisq(181.7,df=3)


#------------模型应用-----------------------
# 接下来，展示用以上两个BIC模型（分别以Y1和Y2为因变量）进行预测的代码。
# 假设要预测的申请人的基本情况为：男性，最近一个月的信用卡使用情况正常，授信额度为20000元，每个月需要还房贷4000元，该申请人历史逾期次数为4次，信用卡开户数只有3个。
# 那么请问该申请人未来逾期的风险有多高？如果逾期，那么逾期严重程度有多高？以Y1为因变量的BIC模型预测结果可用来回答第一个问题，代码展示如下。

#0-1模型预测
new = data.frame(gender="男性",usage=1,credit=log(20000),loan=4000,
                 history=4,accounts=3,Z1=1,Z2=log(4000))
pred=predict(model.bic,newdata=new)
# 注意，predict函数得到的"Z" 并非逾期风险，对"Z" 进行exp"(Z)/{1+" exp"(Z)}" 变换之后得到的才是Y1=1（逾期）的估计值，该值正是逾期风险预测值，预测值为78%。
exp(pred)/(1+exp(pred))
dat = data.frame(prediction = exp(predict(model.bic,a))/(1+exp(predict(model.bic,a)))) 
summary(dat)


#定序回归模型预测
# 以Y2为因变量的BIC模型预测结果可用来回答第二个问题，代码展示如下。模型预测该申请人一旦逾期，逾期严重程度为第二类（逾期31-60天）。
predict(probit.bic,newdata=new)


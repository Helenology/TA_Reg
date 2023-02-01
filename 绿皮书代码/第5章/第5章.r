# 通过R代码可以读入
a=read.csv("第5章.csv")
# keyword变量的类型设定为character类型
a$keyword=as.character(a$keyword)
a[1:5,]
a[6004:6008,]

# 计算关键词长度（kwlen）
a$kwlen=nchar(a$keyword)


# 对关键词，通过jiebaR分词
library(jiebaR)
my.worker=worker()
kw.seg=segment(a$keyword,my.worker)
# 计算词根频率
tab=table(kw.seg)
tab=sort(tab,decreasing=T)
# 展示前100的词根，以及他们的分别占比和累计占比。
tab2=tab[1:100]
tab2=tab2/sum(tab)
tab2
# 这些词根虽然只有100个，但是却在总的词根频数中占比接近95%
sum(tab2)
ss=length(a[,1])
# 对各个关键词做各种词根处理，首先处理机票的不同表达方式。
# 从初步的词根分析来看，“飞机票”、“机票”、“航班”是最重要的三种，可以将其他各种关键词统称为“其他”。
ticket=rep("其他",ss);kw=a$keyword
pos=grep("飞机票",kw);ticket[pos]="飞机票";kw=gsub("飞机票","-",kw)
pos=grep("机票",kw);ticket[pos]="机票";kw=gsub("机票","-",kw)
pos=grep("航班",kw);ticket[pos]="航班";kw=gsub("航班","-",kw)
# 这样就生成了一个新的解释变量：机票表达方式（ticket），这是一个定性的因素型变量，有4个不同的水平，分别是：“飞机票”，“机票”，“航班”，和“其他”
a$ticket=ticket;table(ticket)

# 然后处理跟价格表达相关的不同词根。例如：“特价”，“便宜”，“打折”，“折扣”，“低价”，“廉价”，“特惠”，“最”等。
# 其他的价格偏好表达方式（含无表达），全部归为“其他”。
kw.pattern=c("特价","便宜","打折","折扣","低价","廉价","特惠","最")
tmp=rep("其他",ss);kw=a$keyword
for(i in 1:length(kw.pattern)){
  pos=grep(kw.pattern[i],kw);
  tmp[pos]=kw.pattern[i];
  kw=gsub(kw.pattern[i],"-",kw)
}
# 这样就又生成了一个新的解释变量：价格表达方式（price）。这是一个定性的因素型变量，有9个不同的水平，分别是：“特价”，“便宜”，“打折”，“折扣”，“低价”，“廉价”，“特惠”，“最”，还有“其他”
a$price=tmp;
# 计算每个水平下有多少样本量
table(tmp)

# 处理跟购买与查询相关的不同词根。
# 关键词中常常包含了两种不同的意图。一种是直接的购买意图（例如：“订”，“定”，“购”，“买”），还有一种表示查询信息（例如：“查询”，“多少钱”，“价格”，“哪个”，“时刻表”）。其他的意图表达方式（含无表达），全部归为“其他”。
kw.pattern=c("定","订","购","买","查询","多少钱","价格","哪个","时刻表")
tmp=rep("其他",ss);kw=a$keyword
for(i in 1:length(kw.pattern)){
  pos=grep(kw.pattern[i],kw);
  tmp[pos]=kw.pattern[i];
  kw=gsub(kw.pattern[i],"-",kw)
}
# 这样就又生成了一个新的解释变量：购买查询表达方式（buy）。这是一个定性的因素型变量，有10个不同的水平，分别是：“订”，“定”，“购”，“买”，“查询”，“多少钱”，“价格”，“哪个”，“时刻表”，还有“其他”。
a$buy=tmp;
# 计算每个水平下有多少样本量
table(tmp)


# 处理北上广深相关的词根。这是一个定性的因素型变量，有5个不同的水平，分别是：“北京”、“上海”、“广州”、“深圳”、还有“其他”。
kw.pattern=c("北京","上海","广州","深圳")
tmp=rep("其他",ss);kw=a$keyword
for(i in 1:length(kw.pattern)){
  pos=grep(kw.pattern[i],kw);
  tmp[pos]=kw.pattern[i];
  kw=gsub(kw.pattern[i],"-",kw)
}
#生成一个新的解释变量：一线城市（city1）
a$city1=tmp;
# 计算每个水平下有多少样本量
table(tmp)

# 处理热点城市相关词根。
# 所谓热点城市，这里定义为在关键词中出现频率最高的，非北上广深的，20个热点城市。
kw.pattern=c("三亚","厦门","南京","昆明","哈尔滨","天津",
             "长沙","武汉","海口","重庆","大连","乌鲁木齐" ,"杭州",
             "成都","西安","郑州","济南","呼和浩特","长春","南昌")
tmp=rep("其他",ss);kw=a$keyword
for(i in 1:length(kw.pattern)){
  pos=grep(kw.pattern[i],kw);
  tmp[pos]=kw.pattern[i];
  kw=gsub(kw.pattern[i],"-",kw)
}
# 因此，这也是一个定性变量：，热点城市（city2），有21个水平，分别是："三亚"，“厦门”，"南京"，"昆明"，"哈尔滨"，"天津"，"长沙"，"武汉"，"海口"，"重庆"，"大连"，"乌鲁木齐" ，"杭州"，"成都"，"西安"，"郑州"，"济南"，"呼和浩特"，"长春" ，"南昌"，还有“其他”
a$city2=tmp;
# 计算每个水平下有多少样本量
table(tmp)

# 处理跟品牌相关的词根，在很多关键词中还出现了不同销售平台的品牌，例如：“携程网”，“途牛网”等。这是一个重要的信息，表达了消费者的品牌偏好。对于广告主而言，这既可能是自己的品牌名称，也可能是精品品牌。因此，从关键词文本中提取的最后一个X指标是：品牌。
kw.pattern=c("携程","途牛","去哪儿","艺龙","同程","酷讯")
tmp=rep("其他",ss);kw=a$keyword
for(i in 1:length(kw.pattern)){
  pos=grep(kw.pattern[i],kw);
  tmp[pos]=kw.pattern[i];
  kw=gsub(kw.pattern[i],"-",kw)
}
# 这也是一个定性变量：品牌（brand），有7个水平，分别是：“携程”，“途牛”，“去哪儿”，“艺龙”，“同程”，“酷讯”，还有“其他”。
a$brand=tmp;
# 计算每个水平下有多少样本量
table(tmp)


c(dim(a),mean(a$conversion>0))

# 定义两个因变量Y1（有无转化）和Y2（转化量-1）。
a$Y1=1*(a$conversion>0)
a$Y2=a$Y1*(a$conversion-1)
# 定义两个解释变量：点击率CTR， 单位点击成本CPC。
a$CTR=a$click/a$impression*100
a$CPC=a$cost/a$click
summary(a)

# 在有转化前提下，画Y2直方图
aa=a[a$conversion>0,]
# 从下图可以看到，直方图显示数据右偏，大部分关键词的Y2取值不超过20，但是也有少量的关键词产生了大量的转化。
hist(aa$conversion,xlab="转化程度", ylab = "频数",main=NULL)

# 展示转化量过高的关键词如下。为检验数是否有异常值,提取出大量转化（Y2 >60）的关键词数据。
# 结果显示：大量转化的关键词确实是主力热点关键词，因此数据没有问题。
aa[aa$Y2>60,]
aa[1:5,1:13]


# 对展现量（impression），点击率（CTR），以及单位点击成本（CPC），做直方图。可以预期，展现量（impression），点击率（CTR），以及单位点击成本（CPC）三个变量的分布都呈现右偏，
# 因此对这三个变量都做了对数变换。对数变换后的直方图如下所示
par(mfrow=c(1,3))
hist(log(a$impression),xlab="展现量", ylab = "频数",main=NULL)
hist(log(a$CTR),xlab="点击率",ylab = "频数",main=NULL)
hist(log(a$CPC),xlab="单位点击成本",ylab = "频数",main=NULL)


# 对关键词排名（ranking）以及关键词长度（kwlen）做直方图，
# 有些关键词排名能够到二十名开外，这样的关键词排名太靠后，已经难以在搜索引擎页面第一页看到了。另外，有的关键词长度能到十个字符以上，这是非常长尾的关键词。
par(mfrow=c(1,2))
hist(a$ranking,xlab="排名",ylab = "频数",main=NULL)
hist(a$kwlen,xlab="关键词长度",ylab = "频数",main=NULL)


# 对6个离散型变量做柱状图。分别是：机票表达方式（ticket），价格表达方式（price），购买查询表达方式（buy），一线城市（city1），热点城市（city2），品牌（brand）。
# 这些指标都是离散型的，因此可以计算每个解释变量每个水平取值的频数，并作柱状图。但是，这些解释变量都有一个特点就是取值“其他”的那一类频数特别高。一个主要的原因是，“其他”这一类可能还能进一步细分，而目前每个解释变量的水平取值比较粗糙
# 。因此，在汇报频数的柱状图中，特意去掉了“其他”这根柱子，否则其他的柱子高度会被压得很低，无法观察发现数据中存在的其他问题。如下图所示，6个变量的柱状图似乎一切正常。
par(mfrow=c(3,2))
tab=table(a$ticket);tab=tab[names(tab)!="其他"];barplot(tab)
tab=table(a$price);tab=tab[names(tab)!="其他"];barplot(tab)
tab=table(a$buy);tab=tab[names(tab)!="其他"];barplot(tab)
tab=table(a$city1);tab=tab[names(tab)!="其他"];barplot(tab)
tab=table(a$city2);tab=tab[names(tab)!="其他"];barplot(tab)
tab=table(a$brand);tab=tab[names(tab)!="其他"];barplot(tab)


# 合Y1（是否有转化），对三个重要的连续型解释变量再做描述统计。首先利用箱线图展示展现量（impression），点击率（CTR），以及单位点击成本（CPC）三个连续型变量跟Y_1的关系。
# 由于这三个变量的分布都为严重右偏的分布，因此在绘制箱线图前都做了对数变化，
# 结果如下图所示。从中可以看出，展现量大的关键词转化可能性更高，点击率（CTR）高的关键词转化可能性更大，单位点击成本（CPC）越高的关键词，越被市场追逐，转化可能性越高。
par(mfrow=c(1,3))
boxplot(log(a$impression)~a$Y1,ylab="对数展现量",xlab="是否转化",names=c("未转化","转化"))
boxplot(log(a$CTR)~a$Y1,ylab="对数点击率",xlab="是否转化",names=c("未转化","转化"))
boxplot(log(a$CPC)~a$Y1,ylab="对数单位点击成本",xlab="是否转化",names=c("未转化","转化"))

# 再对两个重要的解释变量做描述统计， 排名（ranking）、关键词长度（kwlen）。
# 用箱线图来描述排名（ranking）、关键词长度（kwlen）与Y1之间的关系，
# 如下图所示。从中可以看出，排名取值越大，排名越靠后，转化可能性更好，这和预期似乎不一样。而关键词长度分析说明，长尾关键词的转化可能性似乎不如热点短词。
par(mfrow=c(1,2))
boxplot(a$ranking~a$Y1,ylab="排名",xlab="是否转化",names=c("未转化","转化"))
boxplot(a$kwlen~a$Y1,ylab="关键词长度",xlab="是否转化",names=c("未转化","转化"))

# 再对6个离散型解释变量做描述统计，并作柱状图，这次保留“其他”这一类。
tapply(a$Y1,a$ticket,mean)
tapply(a$Y1,a$price,mean)
tapply(a$Y1,a$buy,mean)
tapply(a$Y1,a$city1,mean)
tapply(a$Y1,a$city2,mean)
tapply(a$Y1,a$brand,mean)

# 从下图中可以看出，（1）对于机票表达方式而言，“飞机票”（11.6%）和“机票”（11.6%）似乎是更好的选择。
# （2）对于价格表达方式而言，“廉价”（22.1%）和“特惠”（30.8%）更有吸引力；
# （3）对于购买方式而言，“订”（20.2%），“定”（17.2%），还有“购”（18.6%）都是非常好的选择。
# （4）对一线城市的分析表明，“其他”的转化可能性更高（11.3%）。
# （5）对热点城市的分析也表明，“其他”转化可能性更高（14.3%）。
# （6）品牌分析表明，“途牛”（20.0%）和“携程”（15.6%）的转化可能性最高。
par(mfrow=c(3,2))
mu=tapply(a$Y1,a$ticket,mean);barplot(mu)
mu=tapply(a$Y1,a$price,mean);barplot(mu)
mu=tapply(a$Y1,a$buy,mean);barplot(mu)
mu=tapply(a$Y1,a$city1,mean);barplot(mu)
mu=tapply(a$Y1,a$city2,mean);barplot(mu)
mu=tapply(a$Y1,a$brand,mean);barplot(mu)

# 对三个重要的解释变量（展现量，点击率，单位点击成本），结合Y2（转化量-1），做描述性统计。
# 从中可以看到，展现量高的关键词，转化量会更大，这是显然的。因为，转化就是从展现开始，经过点击，然后沉淀下来的最后结果。没有大的展现量就不可能有大的转化。此外，还可以看到高点击率（CTR）的关键词似乎转化量并不会更高。
# 点击率更多反映的是广告创意的效果，能够吸引潜在消费者从展现发展到点击。潜在消费者通过点击进入广告主的产品页面，是否会最终下单购买形成转化，最重要的还是看产品本身。
# 最后，跟预期一样，单位点击成本（CPC）高的关键词，都是被市场认可的关键词，因此转化量高。
Y2=aa$Y2;par(mfrow=c(1,3))
Z=aa$impression;Z=1*(Z>median(Z));
Z=factor(Z,labels=c("低","高"));mu=tapply(Y2,Z,mean);
barplot(mu,xlab="展现量",ylab="转化程度",ylim=c(0,5))


Z=aa$CTR;Z=1*(Z>median(Z));
Z=factor(Z,labels=c("低","高"));mu=tapply(Y2,Z,mean);
barplot(mu,xlab="点击率",ylab="转化程度",ylim=c(0,5))

Z=aa$CPC;Z=1*(Z>median(Z));
Z=factor(Z,labels=c("低","高"));mu=tapply(Y2,Z,mean);
barplot(mu,xlab="单位点击成本",ylab="转化程度",ylim=c(0,5))

# 在对两个连续型解释变量（排名，关键词长度）做描述统计。
# 从中可以看出，结果发现关键词排名对转化量影响甚微。但是，长度比较短的关键词，转化量较高。这是符合预期的，因为短词常常是热点词，因此展现量更大，自然转化就更多。
par(mfrow=c(1,2))
Z=aa$ranking;Z=1*(Z>median(Z));
Z=factor(Z,labels=c("前","后"));mu=tapply(Y2,Z,mean);
barplot(mu,xlab="排名",ylab="转化程度",ylim=c(0,5))

Z=aa$kwlen;Z=1*(Z>median(Z));
Z=factor(Z,labels=c("短","长"));mu=tapply(Y2,Z,mean);
barplot(mu,xlab="关键词长度",ylab="转化程度",ylim=c(0,5))

# 对6个离散型X做描述统计。从中可以看出，对于很多解释变量而言，“其他”这一类占比太大，因此在柱状图中不展示“其他”类。
# （1）对于机票表达方式而言，“飞机票”（3.46）和“机票”（3.35）带来的转化量最大。
# （2）对于价格表达方式而言，“特惠”（3.41）和“特价”（3.37）更好；
# （3）对于购买方式而言，“订”（4.03）是最好的选择。
# （4）对一线城市的分析表明，“深圳”是相对更好的选择（1.11）。
# （5）对热点城市的分析表明，“南京”似乎是最好的选择（1.07）。
# （6），品牌分析表明，“途牛”（4.11）和“携程”（2.24）的转化量最高。
tapply(aa$Y2,aa$ticket,mean)
tapply(aa$Y2,aa$price,mean)
tapply(aa$Y2,aa$buy,mean)
tapply(aa$Y2,aa$city1,mean)
tapply(aa$Y2,aa$city2,mean)
tapply(aa$Y2,aa$brand,mean)

par(mfrow=c(3,2))
mu=tapply(aa$Y2,aa$ticket,mean);barplot(mu,ylim=c(0,4))
mu=tapply(aa$Y2,aa$price,mean);barplot(mu,ylim=c(0,4))
mu=tapply(aa$Y2,aa$buy,mean);barplot(mu,ylim=c(0,4))
mu=tapply(aa$Y2,aa$city1,mean);barplot(mu,ylim=c(0,4))
mu=tapply(aa$Y2,aa$city2,mean);barplot(mu,ylim=c(0,4))
mu=tapply(aa$Y2,aa$brand,mean);barplot(mu,ylim=c(0,4))


#------------------------------逻辑回归------------------------
# 以Y1（是否有转化）为因变量的逻辑回归模型。
# 考虑的解释变量有：展现（impression），点击率（CTR），单位点击成本（CPC），平均排名（ranking），关键词长度（kwlen），机票的表达方式（ticket），价格偏好的表达方式（price），购买意愿的表达方式（buy），是否一线城市（city1），是否热点城市（city2），最后是广告主品牌（brand）。其中展现量做了对数变换。
logit.full=glm(Y1~log(impression)+CTR+CPC+ranking+kwlen+ticket+price+buy+city1+city2+brand,family=binomial(link=logit),data = a)
summary(logit.full)
# 空模型（只含有截距项的逻辑回归模型）的离差（deviance）是53405，（Null deviance）对应的自由度是：样本量（80000）-1（截距项）=79999。残差的deviance是33920，相应的自由度是：样本量（80000）-1（截距项）-解释变量个数（55）=79944。因此，可以做一个关于该模型全局显著性的卡方检验。统计量为：53405-33920=19485，对应的应该是一个自由度为：79999-79944=55的卡方分布。对应的P-值可以通过下面代码计算得到。可见非常显著，这说模型是显著的
53405-31620
79999-79944
1-pchisq(21785,df=55)



# 对全模型的内样本计算一下AUC，计算结果表明AUC是87.3%，该值非常高，说明内样本的预测能力很强。
library(pROC)
pred=predict(logit.full,a)
my.roc=roc(a$Y1,pred)
my.roc
# 做ROC曲线如下
plot(my.roc,main = "关键词产生转化的可能性")

# 了解一下有转化数据的大小，转化的数据量为8320
dim(aa)

#------------------------------泊松回归------------------------------
#全模型
poisson.full=glm(Y2~log(impression)+CTR+CPC+ranking+kwlen+ticket+price+buy+city1+city2+brand,family = poisson(),data=aa)
summary(poisson.full)

# 对全模型做AIC变量筛选
poisson.aic=step(poisson.full,trace=F)
# AIC结果和全模型一样
summary(poisson.aic)

# 对全模型做BIC变量筛选
ss2=length(aa[,1])
poisson.bic=step(poisson.full,trace=F,k=log(ss2))
# BIC剔除了热点城市。
summary(poisson.bic)
# 空模型（只含有截距项的泊松回归模型）的离差（deviance）是63932，（Null deviance）对应的自由度是：样本量（8320）-1（截距项）=8319。残差的deviance是30924，相应的自由度是：样本量（8320）-1（截距项）-解释变量个数（35）=8284。因此，可以做一个关于该模型全局显著性的卡方检验。统计量为：63932-30924=33008，对应的应该是一个自由度为：8319-8284=35的卡方分布。对应的P-值可以通过下面代码计算得到。可见非常显著，这说模型是显著的
63932-25911
8319-8285
1-pchisq(38021,df=34)



#-------------------------模型应用-----------------------
# 在模型应用方面，控制展现量为1万次展现，点击率为1%，点击成本为1元/点击，点击率为1%，竞价排名为第1名后，预测得“理论转化量”。

#预测
aa2 = aa
aa2$orirank = aa$ranking
aa2$oriCPC = aa$CPC
#控制展现量为1万次展现
aa2$impression = 10000
#点击率为1%
aa2$CTR = 1
#点击成本为1元/点击
aa2$CPC = 1
#竞价排名为第1名
aa2$ranking = 1
#预测得“理论转化量”。
aa2$p2 = predict(poisson.bic,newdata = aa2)
# 根据"理论转化量"的中位数，可以将所有关键词区分为两类：优质、劣质。
aa2$pc2 = cut(aa2$p2,
              breaks = c(0,median(aa2$p2),max(aa2$p2)+1),
              labels=c("劣质","优质"))
# 同时，根据排名的前后，将排名前七的分为排名靠前，其他的分为排名靠后。
aa2$rankingc = cut(aa2$orirank,
                   breaks = c(0,7,max(aa2$orirank)+1),
                   labels=c("排名靠前","排名靠后"))

#四类关键词的转化成本
# 对比这四类关键词的平均一次转化产生的成本（单位转化成本，CPA）。
# 排名靠前的优质关键词CPA为7.384元，排名靠前的劣质关键词CPA为15.624元，排名靠后的优质关键词CPA为11.072元，排名靠后的优质关键词CPA为14.304元。
new1 = aa2[(aa2$pc2 %in% "优质")&(aa2$rankingc %in% "排名靠前"),]
new2 = aa2[(aa2$pc2 %in% "劣质")&(aa2$rankingc %in% "排名靠前"),]
new3 = aa2[(aa2$pc2 %in% "优质")&(aa2$rankingc %in% "排名靠后"),]
new4 =  aa2[(aa2$pc2 %in% "劣质")&(aa2$rankingc %in% "排名靠后"),]
s1 = round(sum(new1$cost)/sum(new1$conversion),3)
s2 = round(sum(new2$cost)/sum(new2$conversion),3)
s3 = round(sum(new3$cost)/sum(new3$conversion),3)
s4 = round(sum(new4$cost)/sum(new4$conversion),3)
cat(s1,s2,s3,s4)

# 首先关心排名靠前的劣质关键词，因为它们浪费了太多的营销资源，而没有产生相匹配的效果。在全部排名靠前的关键词中，这部分关键词占比53.94%，平均排名大概第5名，消耗了7.33%的营销费用。
aa3 = aa2[(aa2$pc2 %in% "劣质")&(aa2$rankingc %in% "排名靠前"),]
aa30 = aa2[(aa2$rankingc %in% "排名靠前"),]
m1 = nrow(aa3)/nrow(aa30) 
m2 = mean(aa3$orirank) 
m3 = sum(aa3$cost)/sum(aa30$cost) 
m4 = sum(aa3$conversion)/sum(aa30$conversion) 
cat(m1,m2,m3,m4)

# 然后，我们关心排名靠后的优质关键词，它们的营销效果没有得到充分发挥。对于这部分关键词，可以考虑试探性增加出价，提高排名，以期获得更好的转化效果。在全部排名靠后的关键词中，这部分关键词占比50.82%，平均排名大概为第9名，排名相对靠后，消耗了44.79%的营销费用。
aa4 = aa2[(aa2$pc2 %in% "优质")&(aa2$rankingc %in% "排名靠后"),]
aa40 = aa2[(aa2$rankingc %in% "排名靠后"),]
m1 = nrow(aa4)/nrow(aa40) 
m2 = mean(aa4$orirank) 
m3 = sum(aa4$cost)/sum(aa40$cost)
m4 = sum(aa4$conversion)/sum(aa40$conversion) 
cat(m1,m2,m3,m4)
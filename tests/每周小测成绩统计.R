library(readxl)
path = "./tests/小测1/"
setwd(path)
filenames = list.files(path)
filenames

num = 1
for(filename in filenames){
  test = read_excel(filename)
  test = test[, c("学号", "姓名", "成绩")]
  print(table(test$成绩))
  test[test$成绩 == "未交", "成绩"] = '0'
  test$成绩 = as.numeric(test$成绩)
  colnames(test)[3] = paste0("成绩_", num)
  
  if(num == 1){
    test.all = test
  }else{
    test.all = merge(test.all, test, by = c("学号", "姓名"))
  }
  num = num + 1
}

# NA的也是没交的
test.all[is.na(test.all)] = 0
# 计算平均数
test.all$平均成绩 = apply(test.all[, 3:ncol(test.all)], 1, mean)

# 平均成绩直方图
par(family = "STKaiti")
# 直方图标题
title = "小测平均成绩" # strsplit(filename, '_')[[1]][1]
# 直方图
hist(test.all$平均成绩, main = title, ylab = "频数", 
     xlab = "平均成绩", breaks=10)

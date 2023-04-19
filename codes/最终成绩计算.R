library(readxl)
library(dplyr)
library(plyr)


setwd("/Users/helenology/Desktop/光华/助教/实用商务数据分析与预测/2023年春季学期/最终成绩/成绩/")

####################################### 作业成绩
# 获取文件夹中所有xls文件的文件名
hw_files = list.files("作业成绩0416/", full.names = TRUE)
# 从所有文件中读取数据
hw_list = lapply(hw_files, function(hw_file){
  tmp_df = read_excel(hw_file)[, 2:9]
  tmp_df = mutate(tmp_df, hw_file = basename(hw_file))
  tmp_df$成绩 = as.numeric(tmp_df$成绩)
  tmp_df
}) # 如果是NA就是0分
# 将所有数据框按【学号】列合并到一个数据框中
hw_df = bind_rows(hw_list) %>% 
  group_by(学号)
hw_df[is.na(hw_df$成绩), "成绩"] = 0

# 合并同一个同学的分数
a = ddply(hw_df, .(学号), nrow) # 检查一下后发现没问题
hw_mean = ddply(hw_df, .(学号), function(x){
  c(x$姓名[1], mean(x$成绩))
}) 
names(hw_mean) = c("学号", "姓名", "作业平均分")

####################################### 测试成绩
# 获取文件夹中所有xls文件的文件名
test_files = list.files("测试成绩/", recursive = TRUE, full.names = TRUE)
test_files = test_files[grep("\\.xls$", test_files)]
# 从所有文件中读取数据
test_list = lapply(test_files, function(test_file){
  tmp_df = read_excel(test_file)[, 2:6]
  tmp_df = mutate(tmp_df, test_file = basename(test_file))
  tmp_df$成绩 = as.numeric(tmp_df$成绩)
  tmp_df[is.na(tmp_df$成绩), "成绩"] = 0
  tmp_df
}) # 如果是NA就是0分
# 将所有数据框按【学号】列合并到一个数据框中
test_df = bind_rows(test_list) %>% 
  group_by(学号)
# 合并同一个同学的分数
a = ddply(test_df, .(学号), nrow) # 检查一下后发现没问题
test_mean = ddply(test_df, .(学号), function(x){
  c(x$姓名[1], mean(x$成绩))
}) 
names(test_mean) = c("学号", "姓名", "测试平均分")

####################################### 考勤成绩
attendence_df = read_excel("商务数据分析与预测_考勤统计.xls")[, c(2, 3, 8, 19)]
# 缺勤一次不扣分
attendence_df[(attendence_df$旷课总计 > 0), "旷课总计"] = attendence_df[(attendence_df$旷课总计 > 0), "旷课总计"] - 1
attendence_df$缺勤扣分 = - attendence_df$旷课总计 * 5
attendence_df = attendence_df[, -4]

####################################### project成绩
project_mean = read_excel("pre评分表.xlsx")[, c(1, 2)]
names(project_mean) = c("所属固定分组", "project平均分")

####################################### 平均成绩汇总
grades_piece_df = list(hw_mean, test_mean, attendence_df)
# merge all data frames in list
grades_df = Reduce(function(x, y) merge(x, y, by=c("学号", "姓名")), grades_piece_df)
grades_df = merge(grades_df, project_mean, by="所属固定分组")

# 组长说final没干活的
tangping = c("张三", "李四", "王二", "齐五")
grades_df[grades_df$姓名 %in% tangping, "project平均分"] = 0

####################################### 总成绩
# 平时个人作业(50%)+平时个人测验(30%)+小组期末项目(20%)
grades_df[, 4:7] = as.numeric(unlist(grades_df[, 4:7]))
# 注意这里的最后把缺勤扣分去掉了，这个要填在教务给的excel里
grades_df$总成绩 = grades_df$作业平均分 * 0.5 + grades_df$测试平均分 * 0.3 + grades_df$project平均分 * 0.2 #+ grades_df$缺勤扣分
# 按照成绩倒序排列，看看有几个A
grades_df = grades_df[order(grades_df$总成绩, decreasing = T), ]
grades_df$总成绩 = ceiling(grades_df$总成绩) # 向上取整

# 等级制转化
a = cut(grades_df$总成绩, 
        breaks = c(-Inf, 59.5, 62.5, 66.5, 69.5, 72.5, 76.5, 79.5, 84.5, 89.5, 94.5, 96.5, 100),
        right = F)
levels(a)
grades_df$成绩等级 = cut(grades_df$总成绩, 
                         breaks = c(-Inf, 59.5, 62.5, 66.5, 69.5, 72.5, 76.5, 79.5, 84.5, 89.5, 94.5, 96.5, 100),
                         labels = c("F", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"))
par(family = "STKaiti")
table(grades_df$成绩等级)
barplot(table(grades_df$成绩等级))

# 检查不及格的同学
failed_students = grades_df[grades_df$成绩等级 == "F", ]

# 获得A+的人数一般不超过选课人数的5%
table_grade = table(grades_df$成绩等级)
table_grade
table_grade["A+"] / sum(table_grade) # 0.0

# 获得A-以上的人数一般不超过总人数的20%
table_grade
(table_grade["A+"] + table_grade["A"]) / sum(table_grade)


# 保存成绩
grades_df = grades_df[order(grades_df$学号), ] # 按照学号排序
write.csv(grades_df, "../最终成绩加权细分.csv")

# 填写excel表的时候需要这几个比例
grades_df$label = 0
grades_df[grades_df$总成绩 >= 85, "label"] = 1
grades_df[(grades_df$总成绩 >= 70) & (grades_df$总成绩 <= 84), "label"] = 2
grades_df[(grades_df$总成绩 >= 60) & (grades_df$总成绩 <= 69), "label"] = 3
grades_df[(grades_df$总成绩 < 60) , "label"] = 4
table(grades_df$label)

table(grades_df$label) / sum(table(grades_df$label))


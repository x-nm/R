# Summer Schoool of Biosttistics
# 2016.7.5 by xnm

# setwd("D:/Files/Study/2016_summer/Summer2016/Package_advanced2016")
# setwd("D:/Files/Study/2016_summer/R Package_basic Statistics_2016/data set")
my_data <- read.csv("librarian.csv", header = T)

# BMI excercise
data_bmi <- read.table("BMI.txt", header = T)
attach(data_bmi)
bmi <- weight / ( height/100 ) ^ 2 # kg - m ^ 2
data_bmi <- cbind(data_bmi, bmi)
# sol2: data_bmi$bmi <- weight / ( height/100 ) ^ 2
write.csv(data_bmi, "BMI.csv", quote = F)

class(my_data)
fix(my_data)


a <- paste ( "X", c(1:5), sep = "" ) # 得到一个向量
paste("y = ", paste(a, collapse = "+"), sep = "")




#################
#plot
#################

# 对于离散的数据

stripchart(my_data$education, method = "stack", col = 4, ylim = c(0, 40), pch = "*")
# stack：堆叠法处理离散的数据
# pch: symbol的类型，16：实心圆形

barplot(table(my_data$education))
# barplot之前要用table进行一下转换，把各个数据值对应的数目统计一下
table(my_data$education, my_data$randomization)
barplot(table(my_data$education, my_data$randomization), beside = T, legend.text = F )
# 可以将两个图画在一起；还有legend


# 对于连续的数据

hist(my_data$score1, main = "histogram", xlab = "num of questions")
hist(my_data$score1, main = "histogram", xlab = "num of questions", breaks = 13)
# breaks：the number of cells for the histogram

# kernel density estimation
plot(density(my_data$score1), main = "kernel density curve")
# 和上面的直方图2更加相似
# para: bandwidth, N, 

boxplot(my_data$base_score, horizontal = T, xlab = "number of questions correct on quiz")
# 看数值上的分布
# 两边的线长都是1.5倍的IQR（上四分位-下四分位）

stripchart(my_data$base_score, vertical = F, method = "jitter", pch = 21, col = "blue", bg = "yellow", add = T)
# jitter: 增加扰动，不让一样的点堆在一起，而是稍微分散开来一些
# add = T: 加在上一张图上

# with group: 有分组：使用~
boxplot(my_data$base_score ~ my_data$randomization, horizontal = T, xlab = "number of questions correct on quiz")
stripchart(my_data$base_score ~ my_data$randomization, vertical = F, method = "jitter", pch = 21, col = "blue", bg = "yellow", add = T)


libraty("vioplot")
vioplot(my_data$base_score, my_data$score1, my_data$score2, col = "blue", names = c("A","B","C"))
# Violin plot, 与前一联合图一样，可以（？）同时描绘分布和密度

plot(my_data$base_score, my_data$score1, main = "scatter plot", xlab = "baseline", ylab = "2 weeks FU", col = "blue")



###################
#summary statistics
###################

# frequency - table
table(my_data$sex) # 计数
prop.table(my_data$sex) # 计算频率
# 2*2时，prop.table中margin = 1：按行取propotion， 2：按列

quantile()
fivenum()

median() # 相比而言，不容易受到outlier的影响，鲁棒性较好（所以怪不得...秩和检验= =
var()
IQR() # 两个四分位数间的差值

summary() # 总的统计特征
# 对离散的值：mean值代表比例

library(moments)
skewness(my_data$sex) # 看数据分布是否对称:左偏斜，值小于0（但图...丰度集中于右边啊...

kurtosis()
# 峭度：高瘦于标准正态分布（值为3）：值较大
# 在不同的包中值不一样，结果标准化情况不同



set.seed(123)
x <- rnorm(n = 100, mean = 1, sd = 1)
hist(x, freq = F, col = "gray", border = "gray")
lines(density(x), col = "blue")


curve(dnorm(x, mean = 5, sd = 1), from = 2, to = 8)
# density图（概率密度？

curve(pnorm(x, mean = 5, sd = 1), from = 2, to = 8)
# 将y转换为曲线下面积

#########################
#hypothesis testing
#########################

# 95% interval estimating
# ”模拟100次，有95次落在...“

# for single population 单组样本
t.test(my_data$base_score, mu = 8)
prop.test(table(my_data$smoker), p = 0.6) # 比例检测

# for two population means 两组数据的 均值 的检验
t.test(my_data$base_score ~ my_data$randomization)
# df: degree of freedom 变成了小数（进行过校正）

# for two population proportions
# chi-square test
t.test(table(my_data$sex, my_data$smoker))
# 尽量不要用chi-square, 都用fisher.test
fisher.test(table(my_data$sex, my_data$smoker))

# for paired data
t.test(my_data$base_score, my_data$score1, paired = T)

# 连续值转换为离散值
mytable <- table(my_data$base_score >= 6, my_data$score1 >= 6, dnn = c("baseline","2weeks fu"))

mcnemar.test(mytable)
# 可以用此取代propotion test...（？）

# 多组数据，方差分析
fit <- aov(my_data$base_score,~ pt_literacy)
#？
summary(fit)


# 正态强假设不存在的情况下，用非参检验（各有一种对应的非参检验法，见PPT
# 但参数型的power要相对高一些...所以实际上很多还是用参数型
# 可以把参数的都忘记，只记住非参的：
wilcox.test()
mcnemar.test()
kruskal.test()


##################
#linear regression
##################

# 相关系数
# pearon 连续的，
# spearman 排序的数据（”？,属于非参数型

cor(data1, data2, method = "pearson")
cor(data1, data2, method = "spearman")

cor.test()

fit <- lm(y~x, data = my_data)
summary(fit)

# R square = 0.752: 可以用该模型解释75.2%的数据
# 校正R square: 避免参数过多的过拟合

plot(fit, which = 1)
plot(fit, which = 2)
# ?

predict()
# 置信区间
band() #??

# logistic
# 和线性回归的主要区别：binary outcome

# 离散化处理
ifelse(a >1, 1, 0)

lg.fit <- glm(gc ~ wt, family = binomial("logit"))
# glm：广义线性模型
summary(lg.fit)

pred <- data.frame(Y = a, wt= wt, am = am, p.hat = round(predict(lg.fit, type = "resoinse"), 4))
# 对新的数据得到预测值

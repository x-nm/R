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


a <- paste ( "X", c(1:5), sep = "" ) # �õ�һ������
paste("y = ", paste(a, collapse = "+"), sep = "")




#################
#plot
#################

# ������ɢ������

stripchart(my_data$education, method = "stack", col = 4, ylim = c(0, 40), pch = "*")
# stack���ѵ���������ɢ������
# pch: symbol�����ͣ�16��ʵ��Բ��

barplot(table(my_data$education))
# barplot֮ǰҪ��table����һ��ת�����Ѹ�������ֵ��Ӧ����Ŀͳ��һ��
table(my_data$education, my_data$randomization)
barplot(table(my_data$education, my_data$randomization), beside = T, legend.text = F )
# ���Խ�����ͼ����һ�𣻻���legend


# ��������������

hist(my_data$score1, main = "histogram", xlab = "num of questions")
hist(my_data$score1, main = "histogram", xlab = "num of questions", breaks = 13)
# breaks��the number of cells for the histogram

# kernel density estimation
plot(density(my_data$score1), main = "kernel density curve")
# �������ֱ��ͼ2��������
# para: bandwidth, N, 

boxplot(my_data$base_score, horizontal = T, xlab = "number of questions correct on quiz")
# ����ֵ�ϵķֲ�
# ���ߵ��߳�����1.5����IQR�����ķ�λ-���ķ�λ��

stripchart(my_data$base_score, vertical = F, method = "jitter", pch = 21, col = "blue", bg = "yellow", add = T)
# jitter: �����Ŷ�������һ���ĵ����һ�𣬶�����΢��ɢ����һЩ
# add = T: ������һ��ͼ��

# with group: �з��飺ʹ��~
boxplot(my_data$base_score ~ my_data$randomization, horizontal = T, xlab = "number of questions correct on quiz")
stripchart(my_data$base_score ~ my_data$randomization, vertical = F, method = "jitter", pch = 21, col = "blue", bg = "yellow", add = T)


libraty("vioplot")
vioplot(my_data$base_score, my_data$score1, my_data$score2, col = "blue", names = c("A","B","C"))
# Violin plot, ��ǰһ����ͼһ�������ԣ�����ͬʱ���ֲ����ܶ�

plot(my_data$base_score, my_data$score1, main = "scatter plot", xlab = "baseline", ylab = "2 weeks FU", col = "blue")



###################
#summary statistics
###################

# frequency - table
table(my_data$sex) # ����
prop.table(my_data$sex) # ����Ƶ��
# 2*2ʱ��prop.table��margin = 1������ȡpropotion�� 2������

quantile()
fivenum()

median() # ��ȶ��ԣ��������ܵ�outlier��Ӱ�죬³���ԽϺã����Թֲ���...�Ⱥͼ���= =
var()
IQR() # �����ķ�λ����Ĳ�ֵ

summary() # �ܵ�ͳ������
# ����ɢ��ֵ��meanֵ��������

library(moments)
skewness(my_data$sex) # �����ݷֲ��Ƿ�Գ�:��ƫб��ֵС��0����ͼ...��ȼ������ұ߰�...

kurtosis()
# �Ͷȣ������ڱ�׼��̬�ֲ���ֵΪ3����ֵ�ϴ�
# �ڲ�ͬ�İ���ֵ��һ���������׼�������ͬ



set.seed(123)
x <- rnorm(n = 100, mean = 1, sd = 1)
hist(x, freq = F, col = "gray", border = "gray")
lines(density(x), col = "blue")


curve(dnorm(x, mean = 5, sd = 1), from = 2, to = 8)
# densityͼ�������ܶȣ�

curve(pnorm(x, mean = 5, sd = 1), from = 2, to = 8)
# ��yת��Ϊ���������

#########################
#hypothesis testing
#########################

# 95% interval estimating
# ��ģ��100�Σ���95������...��

# for single population ��������
t.test(my_data$base_score, mu = 8)
prop.test(table(my_data$smoker), p = 0.6) # �������

# for two population means �������ݵ� ��ֵ �ļ���
t.test(my_data$base_score ~ my_data$randomization)
# df: degree of freedom �����С�������й�У����

# for two population proportions
# chi-square test
t.test(table(my_data$sex, my_data$smoker))
# ������Ҫ��chi-square, ����fisher.test
fisher.test(table(my_data$sex, my_data$smoker))

# for paired data
t.test(my_data$base_score, my_data$score1, paired = T)

# ����ֵת��Ϊ��ɢֵ
mytable <- table(my_data$base_score >= 6, my_data$score1 >= 6, dnn = c("baseline","2weeks fu"))

mcnemar.test(mytable)
# �����ô�ȡ��propotion test...������

# �������ݣ��������
fit <- aov(my_data$base_score,~ pt_literacy)
#��
summary(fit)


# ��̬ǿ���費���ڵ�����£��÷ǲμ��飨����һ�ֶ�Ӧ�ķǲμ��鷨����PPT
# �������͵�powerҪ��Ը�һЩ...����ʵ���Ϻܶ໹���ò�����
# ���԰Ѳ����Ķ����ǣ�ֻ��ס�ǲεģ�
wilcox.test()
mcnemar.test()
kruskal.test()


##################
#linear regression
##################

# ���ϵ��
# pearon �����ģ�
# spearman ��������ݣ�����,���ڷǲ�����

cor(data1, data2, method = "pearson")
cor(data1, data2, method = "spearman")

cor.test()

fit <- lm(y~x, data = my_data)
summary(fit)

# R square = 0.752: �����ø�ģ�ͽ���75.2%������
# У��R square: �����������Ĺ����

plot(fit, which = 1)
plot(fit, which = 2)
# ?

predict()
# ��������
band() #??

# logistic
# �����Իع����Ҫ����binary outcome

# ��ɢ������
ifelse(a >1, 1, 0)

lg.fit <- glm(gc ~ wt, family = binomial("logit"))
# glm����������ģ��
summary(lg.fit)

pred <- data.frame(Y = a, wt= wt, am = am, p.hat = round(predict(lg.fit, type = "resoinse"), 4))
# ���µ����ݵõ�Ԥ��ֵ
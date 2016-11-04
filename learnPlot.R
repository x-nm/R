#2016.11.3

###############################################################################
# 颜色
###############################################################################

#COLORS
heat.colors(n, alpha = 1) # 白黄橙红，类似default，可以调节N更加细腻一点
terrain.colors(n, alpha = 1) # 白黄绿
topo.colors(n, alpha = 1) # 黄绿蓝
# e.g n =100, 256

library("RColorBrewer")
library("gplots")
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
library("WGCNA")
# blueWhiteRed(100)


# colorRampPalette




# 可选函数
# 图片效果的参数调节：清晰度，大小，字体，边缘


###############################################################################
# 散点图+拟合曲线
###############################################################################
data("trees")
scatter.smooth(trees$Girth, trees$Volume) #既有散点又有平滑拟合曲线

# 散点图+拟合直线，法一
x <- trees$Girth
y <- trees$Volume
plot(x, y)
zlm <- lm(y~x)
lines(x, fitted(zlm)) # 线条起止都在x范围内

# 散点图，法二
plot(x, y)
fit <- lm(y ~ x)
abline(fit, col="red") #比前者更佳
# 加回归方程
# a <- fit$fitted.values[1]
# b <- fit$fitted.values[2] # 真的是这个？应该是coefficients里面的截距吧..
a <- fit$coefficients[2] # k
b <- fit$coefficients[1] # b
a <- round(a, 3)
b <- round(b, 3)
# text(x = 6, y = 10, labels = paste("y = ", a, " * x + ", b, sep = ""), cex = 1)
legend("left",
       bty = "n",
       legend =  paste("y = ", a, " * x  ", b, sep = ""))

# 加R方：是pearson相关系数的平方
r = cor(trees$Girth,trees$Volume,method = "pearson")
Rsqrt <- sqrt(r)
legend("bottomright",
       legend = paste("R^2 = ",round(Rsqrt,4)), 
       bty = "n") # bty = "n" ,legend没有外框


# 但是，lm中的R square...
summary(fit) #可以看到，而且还有校正后的...跟pearson^2不太一样...说的应该是模型的？？
R.sqr = summary(fit)$adj.r.squared 

###############################################################################
# ggplot2
###############################################################################



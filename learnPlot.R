#2016.11.3

###############################################################################
# ��ɫ
###############################################################################

#COLORS
heat.colors(n, alpha = 1) # �׻ƳȺ죬����default�����Ե���N����ϸ��һ��
terrain.colors(n, alpha = 1) # �׻���
topo.colors(n, alpha = 1) # ������
# e.g n =100, 256

library("RColorBrewer")
library("gplots")
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
library("WGCNA")
# blueWhiteRed(100)


# colorRampPalette




# ��ѡ����
# ͼƬЧ���Ĳ������ڣ������ȣ���С�����壬��Ե


###############################################################################
# ɢ��ͼ+�������
###############################################################################
data("trees")
scatter.smooth(trees$Girth, trees$Volume) #����ɢ������ƽ���������

# ɢ��ͼ+���ֱ�ߣ���һ
x <- trees$Girth
y <- trees$Volume
plot(x, y)
zlm <- lm(y~x)
lines(x, fitted(zlm)) # ������ֹ����x��Χ��

# ɢ��ͼ������
plot(x, y)
fit <- lm(y ~ x)
abline(fit, col="red") #��ǰ�߸���
# �ӻع鷽��
# a <- fit$fitted.values[1]
# b <- fit$fitted.values[2] # ����������Ӧ����coefficients����Ľؾ��..
a <- fit$coefficients[2] # k
b <- fit$coefficients[1] # b
a <- round(a, 3)
b <- round(b, 3)
# text(x = 6, y = 10, labels = paste("y = ", a, " * x + ", b, sep = ""), cex = 1)
legend("left",
       bty = "n",
       legend =  paste("y = ", a, " * x  ", b, sep = ""))

# ��R������pearson���ϵ����ƽ��
r = cor(trees$Girth,trees$Volume,method = "pearson")
Rsqrt <- sqrt(r)
legend("bottomright",
       legend = paste("R^2 = ",round(Rsqrt,4)), 
       bty = "n") # bty = "n" ,legendû�����


# ���ǣ�lm�е�R square...
summary(fit) #���Կ��������һ���У�����...��pearson^2��̫һ��...˵��Ӧ����ģ�͵ģ���
R.sqr = summary(fit)$adj.r.squared 

###############################################################################
# ggplot2
###############################################################################


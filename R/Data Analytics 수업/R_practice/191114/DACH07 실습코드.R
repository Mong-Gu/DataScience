# DACH07 12p~19p : 상관계수(Correlation Coefficient)
DA0701 <-read.csv("C:/DA2019/191114_CH07/DA예제-0701.csv")
DA0701
cov(DA0701, use = "complete.obs")

cor.test(DA0701$x, DA0701$y, method = "pearson") # cor = 0.92, p-value = 0. 아주 강한 상관관계가 있다고 할 수 있다.
cor.test(DA0701$x, DA0701$z, method = "pearson") # cor = 0.59, p-value = 0.06. 통계적으로 유의하지 않다는 결론이므로 상관관계가 없다고 볼 수 있다.
cor.test(DA0701$y, DA0701$z, method = "pearson") # cor = 0.76, p-value = 0.01. 강한 상관관계가 있다고 할 수 있다.

install.packages("corrplot")
library(corrplot)
M <-cor(DA0701)
corrplot(M, method = "circle")
corrplot(M, method = "number")

# DACH07 23p : 편상관분석(Partial Correlation Analysis)
DA0702 <-read.csv("C:/DA2019/191114_CH07/DA예제-0701.csv")
DA0702
install.packages("ggm")
library(ggm)
pcor(c("x", "y", "z"), var(DA0702))

# DACH07 27p~30p : 정준상관분석(Canonical Correlation Analysis)
DA0703 <-read.csv("C:/DA2019/191114_CH07/DA예제-0703.csv")
DA0703
a <-(DA0703)
X <-a[,1:2]
X
Y <-a[,3:4]
Y
cor(X)
cor(Y)
cor(a)
cancor(X, Y)

if (require(corrplot)) {M <-cor(cbind(X, Y))
corrplot(M, method = "ellipse", order = "hclust", addrect = 2, addCoef.col = "red")}

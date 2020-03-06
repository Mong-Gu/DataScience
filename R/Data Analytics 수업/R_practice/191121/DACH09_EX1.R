# DACH09 30p : 로지스틱 회귀분석 연습문제

DACH09_EX1 <-read.csv("C:/DA2019/191121_CH09,10/DACH09_EX01.csv")
DACH09_EX1
fit.full <-glm(y ~ x1 + x2 + x3, family = binomial(), data = DACH09_EX1)
summary(fit.full)
confint(fit.full)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(fit.full)
18

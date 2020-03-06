#01
df01 <- read.csv("C:/DA2019/exercise2/DA_AS02-01.csv")
df01

t.test(df01$x1, df01$x2, paired=TRUE, alternative = "greater")

#02
df02 <- read.csv("C:/DA2019/exercise2/DA_AS02-02.csv")
df02

fit02 <- aov(df02$sales ~ df02$program, data=df02)
anova(fit02)
TukeyHSD(fit02)
plot(TukeyHSD(fit02))

#03
df03 <- read.csv("C:/DA2019/exercise2/DA_AS02-03.csv")
df03

# 평균
mean(df03$weight)
mean(df03$height)

# 분산
var(df03$weight)
var(df03$height)

# 공분산
cov(df03, use="complete.obs")

# 상관관계
cor.test(df03$weight, df03$height, data = df03)

#04
df04 <- read.csv("C:/DA2019/exercise2/DA_AS02-04.csv")
df04

fit04 <- lm(y ~ x1 + x2 + x3, data = df04)
summary(fit04)

coefficients(fit04)
confint(fit04, level = 0.95)
fitted(fit04)
residuals(fit04)
anova(fit04)
vcov(fit04)
influence(fit04)

layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(fit04)

# 잔차의 정규성 검정 추가분석 : 샤피로-윌크스검정
shapiro.test(rstandard(fit04))

# 잔차의 등분산성
install.packages("cars")
library(car)
ncvTest(fit04)

# 다중공선성 검정 
vif(fit04)

# gvlma패키지
install.packages("gvlma")
library(gvlma)
gvlma.result = gvlma::gvlma(fit04)
summary(gvlma.result)

#05
##### 로지스틱회귀 #####
df05_1 <- read.csv("C:/DA2019/exercise2/DA_AS02-05.csv")
df05_1

# 1단계 : 모든 독립변수를 적용한 로지스틱 회귀모형
fit05_full <-glm(y ~ x1 + x2 + x3 + x4, family = binomial(), data = df05_1)
summary(fit05_full)
confint(fit05_full)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(fit05_full)

# 2단계 : 최적의 로지스틱 회귀모형 설정
## (1). 수동으로 조정해보기 : full model에서 p-value < 0.05 인 독립변수만 적용하기
fit05_reduced1 <- glm(y ~ x4, family = binomial(), data = df05_1)
summary(fit05_reduced1)

## (2). 자동으로 조정해보기 : step 함수를 이용하여 최적의 독립변수 적용하기
fit05_reduced2 = step(fit05_full)
summary(fit05_reduced2)

## step 함수를 이용하여 얻은 fit05_reduced2 모델의 AIC가 수동으로 조정한 fit05_reduced1 모델의 AIC보다 작기 때문에 fit05_reduced2를 이용하고자 함.

# 3단계 : fit05_full 과 fit05_reduced2 간의 차이검정 실시 후 log(odds)구하기 
anova(fit05_reduced2, fit05_full, test ="Chisq")
# p-value = 0.2668 > a = 0.05 이므로, 전체 독립변수가 투입된 모델과 유의한 독립변수만 투입된 모델 간에 차이가 없음. 따라서 fit05_reduced2를 계속 이용.

coef(fit05_reduced2)
exp(coef(fit05_reduced2))

# 다른 변수의 값을 일정하게 놓고, 사교성이 1단위 증가 시 승진할 확률은 승진하지 않을 확률보다 약 1.0496배 증가
# 다른 변수의 값을 일정하게 놓고, 직무성적이 1단위 증가 시 승진할 확률은 승진하지 않을 확률보다 약 1.1187배 증가

# 4단계 : test 데이터로 예측하기
testdata_1 <-read.csv("C:/DA2019/exercise2/DA_AS02-05test.csv")
testdata_1
testdata_1$prob <-predict(fit05_reduced2, newdata = testdata_1, type = "response")
testdata_1

# 5단계 : 과대산포 검정
## (1) 교수님께서 알려주신 과대산포 검정 방법 : 1.039616 (> 1) 의 결과값을 가지므로 과대산포가 있다고 판단됨.
deviance(fit05_reduced2)/df.residual(fit05_reduced2)

## (2) 카이제곱을 이용한 과대산포 검정 방법 : 0.4573546 (> 0.05) 의 결과값을 가지므로 과대산포가 없다고 판단됨.
fit05_reduced2.od <- glm(y ~ x1 + x4, family = quasibinomial(), data = df05_1)
pchisq(summary(fit05_reduced.od)$dispersion * fit05_reduced$df.residual, fit05_reduced$df.residual, lower = F)

# 6단계 : 로지스틱회귀모형 시각화
library(ggplot2)
ggplot(df05_1, aes(x = x1 + x4, y = y)) + geom_point() + stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "black", se = FALSE)

# 7단계 : 판별분석이 가능한지 알아보기 위해  독립변수에 대한 기본 가정 파악
## (1) 독립변수의 정규성 검정
library(car)
scatterplotMatrix(df05_1[1:5])
# x1 ~ x4 모두 peak가 있기 때문에 정규분포를 따른다고 할 수 있다.
# 또한, 각 독립변수의 관측치가 70개이므로 중심극한정리에 의해 정규분포에 근접

## (2) 다중공선성 검정
vif_test <- lm(y ~ x1 + x2 + x3 + x4, data = df05_1)
vif(vif_test)

## (3) 등분산 검정
install.packages("lawstat")
library(lawstat)

fit <- aov(x1~x2+x3+x4, data=df05_1)
anova(fit)
var(df05_1$x2)
var(df05_1$x3)
var(df05_1$x4)

tmp <- c(df05_1$x1, df05_1$x2,df05_1$x3, df05_1$x4)
group <- c(rep(1, 70), rep(2, 70), rep(3, 70), rep(4, 70))
group
dat<-data.frame(group,tmp)
dat
levene.test(tmp,group)
bartlett.test(tmp~group,dat) 
# 등분산이 아니다. 즉, 판별분석의 기본 가정에 위배되어 판별분석은 불가능할 것으로 보인다.

##### 그럼에도 불구하고 판별분석 시도 #####
df05_2 <- read.csv("C:/DA2019/exercise2/DA_AS02-05.csv")
head(df05_2)
df05_2$y[df05_2$y == 1] <- 2
df05_2$y[df05_2$y == 0] <- 1
head(df05_2)

install.packages("MASS")
library(MASS)
Mda.lda <-lda(y ~ ., data = df05_2)
Mda.lda

Mda.lda.values <- predict(Mda.lda)
ldahist(data = Mda.lda.values$x[,1], g = df05_2$y)

layout(matrix(c(1), 1, 1))
plot(Mda.lda.values$x[,1])
text(Mda.lda.values$x[,1], cex = 0.7, pos = 4, col = "red")

testdata_2 <-read.csv("C:/DA2019/exercise2/DA_AS02-05test.csv")
testdata_2
predict(Mda.lda, newdata = testdata_2)

#06
##### 주성분분석 #####
df06 <- read.csv("C:/DA2019/exercise2/DA_AS02-06.csv")
head(df06)

mydata <-na.omit(df06)
head(mydata)
cor(mydata)

fit <-princomp(mydata, cor = TRUE)
summary(fit) # 4개(누적분산 64.69%) - 6개(누적분산 77.30%) 가 적당.
biplot(fit)

e_value = eigen(cor(df06))
e_value # 4개일 때 1.0496848, 5개일 때는 0.8877708. 고유값에 의하면 4개가 적당.
plot(fit, type = "lines") # 위 고유값을 scree 도표로 표시

loadings(fit)

fit$scores

install.packages("FactoMineR")
library(FactoMineR)
result <-PCA(mydata)

library(psych)

# 주성분분석 이후 적용 (4요인)
refa1 <-data.frame(mydata$x3, mydata$x7, mydata$x8, mydata$x9, mydata$x12, mydata$x13)
alpha(refa1, na.rm = TRUE)
# 크론바하 알파계수 = 0.79
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.

refa2 <-data.frame(mydata$x5, mydata$x6)
alpha(refa2, na.rm = TRUE)
# 크론바하 알파계수 = 0.8
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.

refa3 <-data.frame(mydata$x10, mydata$x11)
alpha(refa3, na.rm = TRUE)
# 크론바하 알파계수 = 0.78
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.

refa4 <-data.frame(mydata$x2, mydata$x4)
alpha(refa4, na.rm = TRUE)
# 크론바하 알파계수 = 0.47
# 크론바하 알파계수가 다소 낮다. 개선할 필요가 보인다.

##### 공통요인분석 #####
# 공통요인분석 이후 적용 (4요인)
fit2 <-factanal(df06, 4, rotation = "varimax") 
print(fit2, digits = 2, cutoff = .3, sort = TRUE)
# p-value = 0.00412 < 0.05가 되어 'H0 : 4요인으로 구성된 모형은 적합하다'를 기각하게 됨.
# 4요인은 불가능하여 5요인으로 넘어감

# 공통요인분석 이후 적용 (5요인)
fit2 <-factanal(df06, 5, rotation = "varimax") 
print(fit2, digits = 2, cutoff = .3, sort = TRUE)
# p-value = 0.517 > 0.05가 되어 'H0 : 5요인으로 구성된 모형은 적합하다'을 기각할 수 없게 됨

refa1 <-data.frame(mydata$x9, mydata$x10, mydata$x11)
alpha(refa1, na.rm = TRUE)
# 크론바하 알파계수 = 0.79
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.

refa2 <-data.frame(mydata$x5, mydata$x6)
alpha(refa2, na.rm = TRUE)
# 크론바하 알파계수 = 0.8
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.

refa3 <-data.frame(mydata$x12, mydata$x13)
alpha(refa3, na.rm = TRUE)
# 크론바하 알파계수 = 0.73
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.

refa4 <-data.frame(mydata$x7, mydata$x8)
alpha(refa4, na.rm = TRUE)
# 크론바하 알파계수 = 0.78
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.

refa5 <-data.frame(mydata$x2, mydata$x3, mydata$x4)
alpha(refa5, na.rm = TRUE)
# 크론바하 알파계수 = 0.56
# 어떤 변수를 제거하더라도 크론바하 알파계수가 증가하지 않으므로 멈춘다.
# 0.6에 근접하여 괜찮은 수준으로 판단한다.

# 공통요인분석의 파라미터를 6요인으로 설정하고 분석한 결과
# 요인 6에는 실질적으로 어떠한 변수도 포함되지 않는 것을 알 수 있다.
# 소스코드에서는 생략하도록 한다.

#07

##### 계층적 군집분석 #####
df07 <- read.csv("C:/DA2019/exercise2/DA_AS02-06.csv")
head(df07)
mydata07 <-na.omit(df07)

d <-dist(mydata07, method = "euclidean")
fit0701_1 <-hclust(d, method = "ward.D")
plot(fit0701_1)

groups <-cutree(fit0701_1, k = 3)
rect.hclust(fit0701_1, k = 3, border = "red")

install.packages("psych")
library(psych)
describeBy(mydata07, groups)

##### 비계층적 군집분석 ##### 
fit0702_2 <-kmeans(mydata07, 4)
library(cluster)
clusplot(mydata07, fit0702_2$cluster, color = TRUE, xhade = TRUE, labels = 2, lines = 0)

install.packages("fpc")
library(fpc)
plotcluster(mydata07, fit0702_2$cluster)

fit0702_2 <-factanal(df07, 5, rotation = "varimax")
print(fit0702_2, digits = 2, cutoff = 0.3, sort = TRUE)
load <-fit0702_2$loadings[,1:2]
plot(load, type = "n")
text(load, labels = names(df07), cex = .7)

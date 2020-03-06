# 2019.11.07. 실습코딩

#1 (DACH06 18p)
DA0601 <-read.csv("C:/DA2019/191107_CH06/DA예제-0601.csv")
DA0601
fit <-aov(satis~com, data = DA0601) # satis : 종속변수
anova(fit)
with(DA0601, tapply(satis, com, mean))
boxplot(satis~com, col="sky blue", data = DA0601)
# 분석 결과
# com1에서 com4로 갈수록 만족도가 떨어지는 것을 확인할 수 있다.
# -------------------------------------------------------------------------------------------------------
#2 (DACH06 23p)
# 1번에서 fitting된 것을 그대로 쓴다.
TukeyHSD(fit)
plot(TukeyHSD(fit))
# 분석 결과
# SPSS와 다르게 통계적으로 유의미한 것을 명시해놓지 않았다
# p 값을 확인하여 0.05보다 작은 것들을 눈여겨 봐라.
# 터키테스트를 하는 이유는, 1번에서 적어도 하나는 다르다 라는 결론이 나왔기 때문! 사후분석을 해줘야 한다.
# -------------------------------------------------------------------------------------------------------
#3 (DACH06 34p-35p)
DA0602 <-read.csv("C:/DA2019/191107_CH06/DA예제-0602.csv")
DA0602
fit <-aov(yield ~ worker + machine, data = DA0602)
anova(fit)
boxplot(yield ~ worker + machine, col = "red", data = DA0602)
plot(fit)
# Hit <Return> to see next plot 이거 뜨면 Ctrl+Enter 눌러서 다음 그래프 확인
# -------------------------------------------------------------------------------------------------------
#4 (DACH06 46p-47p)
DA0603 <-read.csv("C:/DA2019/191107_CH06/DA예제-0603.csv")
DA0603
fit <-aov(yield ~ tem + pre + tem*pre, data = DA0603)
anova(fit)
# 분석결과
# 기온과는 상관이 없다
# 압력과는 상관이 있다
# 기온과 압력과 상호작용 효과가 없다
library(plyr)
anova0603 <-ddply(DA0603, c("pre", "tem"), summarise, nyield = mean(yield))
library(ggplot2)
ggplot(anova0603, aes(x = pre, y = nyield, color = tem, group = tem)) + geom_line()
# 분석결과
# p250-p300 사이에서 그래프가 교차하므로 기온과 온도의 상호작용이 있다고 볼 수도 있다
# DACH06 48p에 좀 자세히 적긴 했는데, 이건 그냥 무시하고 넘어가도 좋다
# -------------------------------------------------------------------------------------------------------
#5 (DACH06 51p)
DA0604 <-read.csv("C:/DA2019/191107_CH06/DA예제-0604.csv")
DA0604
fit <-aov(strength ~ fertility + block + treatment, data = DA0604)
fit1 <-aov(strength ~ block + treatment, data = DA0604)
anova(fit)
anova(fit1)
anova(fit1, fit) # fit1과 fit 의 결과치를 비교해본다.


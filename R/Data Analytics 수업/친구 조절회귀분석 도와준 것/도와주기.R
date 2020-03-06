library(readxl)
new <- read_excel("C:/DA2019/data.xlsx")
View(new)

#가정1. POS -> OCB
fit1 <- lm(OCB_AVG ~ sex+age+year+POS_AVG, data = new)
summary(fit1)

#가정2. SAT -> OCB (조절 : POS)
fit2_1 <- lm(OCB_AVG ~ sex + age + year + SAT_AVG, data = new)
fit2_2 <- lm(OCB_AVG ~ sex + age + year + SAT_AVG + POS_AVG, data = new)
fit2_3 <- lm(OCB_AVG ~ sex + age + year + SAT_AVG + POS_AVG + POS_SAT, data = new)
summary(fit2_1)
summary(fit2_2)
summary(fit2_3)

#가정3. STR -> OCB (조절 : POS)
fit3_1 <- lm(OCB_AVG ~ sex + age + year + STR_AVG, data = new)
fit3_2 <- lm(OCB_AVG ~ sex + age + year + STR_AVG + POS_AVG, data = new)
fit3_3 <- lm(OCB_AVG ~ sex + age + year + STR_AVG + POS_AVG + POS_STR, data = new)
summary(fit3_1)
summary(fit3_2)
summary(fit3_3)

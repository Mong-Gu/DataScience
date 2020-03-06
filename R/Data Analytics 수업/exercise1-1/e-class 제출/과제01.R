# 1번
ID <-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Math <-c(61, 87, 92, 65, 64, 88, 75, 55, 79, 90)
Science <-c(72, 59, 69, 71, 53, 91, 62, 76, 59, 88)
English <-c(82, 91, 65, 80, 86, 95, 71, 63, 93, 52)
Korean <-c(59, 77, 72, 68, 92, 73, 59, 94, 75, 67)
data_Assignment <-data.frame(ID, Math, Science, English, Korean)
View(data_Assignment)

# 2번
write.csv(data_Assignment, file = "C:/DA2019/R_practice/assignment/과제01.csv")

# 3번
data_Assignment$Average <- (data_Assignment$Math + data_Assignment$Science + data_Assignment$English + data_Assignment$Korean)/4
View(data_Assignment)

# 4번
install.packages("psych")
library(psych)
describe(Math)
describe(Science)
describe(English)
describe(Korean)

# 5번
mean(Science)
t.test(Science, mu = 76)

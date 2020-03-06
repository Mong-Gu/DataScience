ID <-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
GENDER <-c("F","M","F","M","M","F","F","F","M","M")
AGE<-c(50, 40, 28, 50, 27, 23, 56, 47, 21, 22)
AREA <-c("서울", "경기", "제주", "서울", "서울", "서울", "경기", "서울", "인천", "경기")

dataframe_ex <-data.frame(ID, GENDER, AGE, AREA)
dataframe_ex

str(dataframe_ex)

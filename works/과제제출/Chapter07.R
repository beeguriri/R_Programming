#Chapter7, Chapter8 연습문제

getwd() 

install.packages("ggplot2")
install.packages("lattice")

library(ggplot2)
library(lattice)


#Chapter7 연습문제
dataset <- read.csv("./data/dataset.csv", header = T)
str(dataset)

#7-2 resident 칼럼 대상으로 NA값 제거후 resident2 변수에 저장
summary(dataset$resident) #NA's 21
resident2 <- na.omit(dataset$resident)
resident2

#7-3 gender칼럼 대상으로 1 -> 남자, 2-> 여자로 변경하여 gender2 칼럼추가, 파이차트
head(dataset["gender"], 10)
summary(dataset$gender) #NA 없음
dataset$gender2[dataset$gender == 1] <- '남자'
dataset$gender2[dataset$gender == 2] <- '여자'
head(dataset[c("gender", "gender2")],10)
pie(table(dataset$gender2))

#7-4 age칼럼 대상으로 30세이하 -> 1, 30~55세 -> 2, 55이상 -> 3으로 age3칼럼에 추가
head(dataset["age"], 10)
summary(dataset$age) #NA's 31
dataset2 <- na.omit(dataset)
head(dataset2)

dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55 ] <- "장년층"

dataset2$age3[dataset2$age <= 30] <- '1'
dataset2$age3[dataset2$age > 30 & dataset2$age <=55] <- '2'
dataset2$age3[dataset2$age > 55] <- '3'
head(dataset2[c("age", "age2", "age3")],10)


#Chapter8 연습문제


#8-1 산점도 시각화
str(quakes)

#조건1 : 수심(depth)을 3개 영역으로 범주화
summary(quakes$depth)
depthgroup <- equal.count(quakes$depth, number=3, overlap=0)
depthgroup
#조건2 : 리히터 규모(mag)를 2개 영역으로 범주화
summary(quakes$mag)
maggroup <- equal.count(quakes$mag, number=2, overlap=0)
maggroup

#조건3 : 수심과 리히터 규모를 3행 2열 구조의 패널로 산점도 그래프 그리기
xyplot(lat~long | maggroup * depthgroup, data = quakes,
      main = "Fiji Earthquakes", ylab="latitude", xlab="longitude")

#8-2 SeatacWeather 월별 최저기온과 최고기온 선그래프
#lattice패키지의 xyplot()이용, type="l"
install.packages("latticeExtra")
library(latticeExtra)
str(SeatacWeather)
head(SeatacWeather, 10)
summary(SeatacWeather$day)
summary(SeatacWeather$month)
xyplot(min.temp + max.temp ~ day | month, data = SeatacWeather, type="l") #x축 : day, #y축 기온 => 월별로


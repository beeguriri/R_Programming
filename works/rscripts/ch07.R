#루트폴드 확인
getwd()

#데이터 불러오기
dataset <-read.csv("./data/dataset.csv", header=T)
View(dataset)

# 데이터의 경향성 확인
# 끝으로 갈수록 na가 없어지면 좋은 데이터 경향
head(dataset) 
tail(dataset)

#데이터셋 구조 확인
#데이터의 자료구조 (칼럼명)
#자료형태 int, num(double) 확인
#모든 데이터는 숫자로 구성되는 것을 원칙으로 함
str(dataset)

x <- dataset$age

# 데이터 경향성 확인 (outlier)
plot(dataset$price)

# dictionary : key와 value로 구성된 데이터 조회
dataset["gender"] #컬럼명(key)로 조회
dataset[1] #R은 배열이 1부터 시작
dataset[c("job","price")]

#결측치 처리
#결측치 삭제 / 결측치 보관 (데이터의 비율에 따라 결정)
#결측치 보관할때는 보통 결측치에 평균값으로 대체
summary(dataset) #NA의 갯수 확인
# 결측치 삭제
sum(dataset$price, na.rm=T)
price2 <- na.omit(dataset$price)
price2

#극단치 처리

#데이터 시각화
#산점도로 데이터 경향성 확인
#히스토그램으로 빈도 확인
#선그래프로 추세선 그림

#결과물 : 보고서



#Chapter7 연습문제 풀이

getwd() #파일 시작할때 무조건 내 workspace 확인하기

dataset <- read.csv("./data/dataset.csv", header=T) #헤더 내용 함께 가져오기
dataset2 <- subset(dataset, price>=2 & price<=8) #데이터셋 추출
str(dataset2)

#문제 7-1
pos <- dataset2$position #dataset2의 position값 변수에 넣기
cpos <- 6-pos #등급체계 분류할때 읽기편하기 위해서(파생변수)
dataset2$position <- cpos
dataset2$position[dataset2$position == 1] <- '1급'
dataset2$position[dataset2$position == 2] <- '2급'
dataset2$position[dataset2$position == 3] <- '3급'
dataset2$position[dataset2$position == 4] <- '4급'
dataset2$position[dataset2$position == 5] <- '5급'

#문제 7-2
range(dataset2$resident, na.rm = T) #resident의 데이터 범위를 확인(상한,하한)
dataset2 <- subset(dataset2, !is.na(dataset2$resident)) #resident의 na를 제거하고 dataset2에 저장
head(dataset2)
View(dataset2)

#문제 7-3
dataset2$gender2[dataset2$gender == 1] <- '남자'
dataset2$gender2[dataset2$gender == 2] <- '여자'
pie(table(dataset2$gender2))
?table





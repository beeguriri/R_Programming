# Chapter 04
# 실습: 기본 함수를 사용하여 요약 통계량과 빈도수 구하기 
# 단계 1: 파일 불러오기 
setwd("C:/Rwork/Part-I")
test <- read.csv("test.csv", header = TRUE)
head(test)

# 단계 2: 요약 통계량 구하기 
summary(test)

# 단계 3: 특정 변수의 빈도수 구하기 
table(test$A)

# 단계 4: 각 칼럼 단위의 비도수와 최대값, 최소값 계산을 위한 사용자 함수 정의하기 
data_pro <- function(x) {
  for(idx in 1:length(x)) {
    cat(idx, "번째 칼럼의 빈도 분석 결과")
    print(table(x[idx]))
    cat("\n")
  }
  
  for(idx in 1:length(x)) {
    f <- table(x[idx])
    cat(idx, "번째 칼럼의 최대값/최소값\n")
    cat("max = ", max(f), "min = ", min(f), "\n")
  }
}

data_pro(test)

# 실습: 분산과 표준편차를 구하는 사용자 함수 정의 

x <- c(7, 5, 12, 9, 15, 6)
x
var_sd <- function(x) {
  var <- sum((x - mean(x))^2) / (length(x) - 1)
  sd <- sqrt(var)
  cat("표본분산: ", var, "\n")
  cat("표본표준편차: ",sd)
}

var_sd(x)


# 실습: 결측치를 포함하는 자료를 대상으로 평균 구하기 
# 단계 1: 결측치(NA)를 포함하는 데이터 생성
data <- c(10, 20, 5, 4, 40, 7, NA, 6, 3, NA, 2, NA)
data
is.na(data) #결측치는 TRUE
data = ifelse(!is.na(data), data, 0)

# 단계 2: 결측치 데이터를 처리하는 함수 정의 
na <- function(x) {
  # 1차: NA 제거 
  print(x)
  print(mean(x, na.rm = T))
  
  # 2차: NA를 0으로 대체 
  data = ifelse(!is.na(x), x, 0)
  print(data)
  print(mean(data))
  
  # 3차: NA를 평균으로 대체 
  data2 = ifelse(!is.na(x), x, round(mean(x, na.rm = TRUE), 2))
  print(data2)
  print(mean(data2))
}

# 단계 3: 결측치 처리를 위한 사용자 함수 호출
na(data)

# 1. 다음과 같은 백터 객체를 생성하시오.\
# 조건1
Vec1 <- rep("R", 5) 
Vec1
# 조건2
Vec2 <- seq(1,10, 3)
Vec2
# 조건3
Vec3 <- rep(Vec2, 3)
Vec3
# 조건4
Vec4 <- c(Vec2, Vec3)
Vec4
# 조건5
Vec <- seq(25,15, -5)
Vec
# 조건6
Vec5 <-Vec4[seq(1,length(Vec4), 2)]
Vec5

#2. 다음과 같은 벡터를 칼럼으로 갖는 데이터프레임을 생성하시오.
name <- c("최민수", "유관순", "이순신", "김유신", "홍길동")
age <- c(55, 45, 45, 53, 15)
gender <- c(1,2,1,1,1)
job <- c("연예인", "주부", "군인", "직장인", "학생")
sat <- c(3,4,2,5,5)
grade <- c("C", "C", "A", "D", "A")
total <- c(44.4, 28.5, 43.5, NA, 27.1)
# 조건1
user <- data.frame(name,age,gender,job,sat,grade,total)
user
# 조건2
hist(user$gender)
# 조건3
user2 <- user[seq(2,nrow(user),2), ]
user2

#3. Data를 대상으로 apply()를 적용하여 행/열 방향으로 조건에 맞게 통계량을 구하시오.
kor <- c(90, 85, 90)
eng <- c(70,85,75)
mat <- c(86,92,88)
# 조건1
Data <- data.frame(kor,eng,mat)
Data
# 조건2
apply(Data, 1, max) #행단위 최대값
apply(Data, 2, max) #열단위 최대값
# 조건3 : round(Data, 자릿수)
round(apply(Data, 1, mean),2) #행단위 평균
round(apply(Data, 2, mean),2) #열단위 평균
# 조건4
apply(Data, 1, sd) #분산
apply(Data, 1, var) #표준편차

#4. 다음의 Data2 객체를 대상으로 정규표현식을 적용하여 문자열을 처리하시오.
install.packages("stringr")
library(stringr)
Data2 <- c("2017-02-05 수입3000원", "2017-02-06 수입4500원", "2017-02-07 수입2500원")
# 조건1 : "3000원" "4500원" "2500원"
str1 <- str_extract_all(Data2, "[0-9]{4}[가-힣]{1}")
str1 <- paste(str1, collapse=",")
str1 <- str_split(str1, ",")
str1
# 조건2 : --수입원, --수입원, --수입원
str2 <- str_replace_all(Data2, "[0-9]{2}", "")
str2 <- str_replace_all(str2, " ", "")
str2
# 조건3 : "2017/02/05 수입3000원" ...
str3 <- str_replace_all(Data2, "-", "/")
str3 
# 조건4 : paste(데이터셋, cpllapse = "구분자")
str4 <- paste(Data2, collapse =",")
str4

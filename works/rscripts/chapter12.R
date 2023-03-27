# Chapter 12. 교차검증과 카이제곱

getwd()
data <- read.csv("./data/cleanDescriptive.csv", 
                 head = T, 
                 fileEncoding = "euc-kr")
head(data)
View(data)

# 변수 리코딩
x <- data$level2
y <- data$pass2

# 데이터프레임 생성(반드시 범주형 데이터)
result <- data.frame(Level = x, Pass = y)
View(result)
table(result)

# 교차분할표 작성을 위한 패키지
install.packages("gmodels")
library(gmodels)
library(ggplot2)

# 교차분할표 작성
# 부모님의 학력이 자녀의 학력을 결정하는가?
CrossTable(x, y)

# 가설이 유의미한가 => 카이제곱
CrossTable(x, y, chisq = T)
#유의값 p 는 0.05 이하여야 유의미한 데이터다
# p =  0.2507057  => 가설폐기


# 교육센터 만족도
data <- read.csv("./data/homogenity.csv", 
                 head = T)
head(data)

# 결측치 제거
data <- subset(data, !is.na(survey), c(method, survey))

# 범주형데이터
data$method2[data$method == 1] <- "방법1"
data$method2[data$method == 2] <- "방법2"
data$method2[data$method == 3] <- "방법3"

data$survey2[data$survey == 1] <- "1. 매우만족"
data$survey2[data$survey == 2] <- "2. 만족"
data$survey2[data$survey == 3] <- "3. 보통"
data$survey2[data$survey == 4] <- "4. 불만족"
data$survey2[data$survey == 5] <- "5. 매우불만족"

# 교차 분할표 작성
table(data$method2, data$survey2)

# 동질성 검정
chisq.test(data$method2, data$survey2)
#X-squared = 6.5447, df = 8, p-value = 0.5865
#자유도(df)가 높으면 결과에 영향을 주는 요인이 많다는 뜻

# p값이 유의도 아래이고 자유도가 낮은 값부터 데이터 분석을 한다 (경험적)

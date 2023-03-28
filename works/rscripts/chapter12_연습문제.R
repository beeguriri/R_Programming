#Chapter12 연습문제

getwd()
library(gmodels)

#12-1 교육 수준과 흡연율 간의 관련성 분석 (독립성 검정)
#[단계1] 파일 가져오기
smoke <- read.csv("./data/smoke.csv", header = T)
head(smoke)

#[단계2] 코딩변경
smoke$education2[smoke$education==1]  <- "1.대졸"
smoke$education2[smoke$education==2]  <- "2.고졸"
smoke$education2[smoke$education==3]  <- "3.중졸"
smoke$smoking2[smoke$smoking==1]  <- "1.과다흡연"
smoke$smoking2[smoke$smoking==2]  <- "2.보통흡연"
smoke$smoking2[smoke$smoking==3]  <- "3.비흡연"
table(smoke$education2, smoke$smoking2)

#[단계3] 교차분할표 작성, 독립성 검정, 검정결과 해석
CrossTable(smoke$education2, smoke$smoking2, chisq=T)
# d.f. =  4 => 자유도가 높다? 데이터 분석이 안됨
# p =  0.0008182573  => p값이 0.05 미만이므로 가설이 유의미하다



#12-2 나이와 직위 간의 관련성 단계 분석 (독립성 검정)
#[단계1] 파일 가져오기
data <- read.csv("./data/cleanData.csv", header = T, fileEncoding = "euc-kr")
str(data)
head(data)

x <- data$position
y <- data$age3

#[단계3] 산점도를 이용한 변수간의 관련성 보기
plot(x,y, abline(lm(y~x)))

#[단계4] 교차분할표 작성, 독립성 검정, 검정결과 해석
CrossTable(x,y, chisq =T)
# p는 매우 낮고 자유도가 높음 => 
#               전반적인 흐름의 경향성은 맞지만, 데이터를 뽑으면 일치하지않음 



#12-3 직업유형에 따른 응답 정도에 차이가 있는가를 단계별로 검정 (동질성 검정)
#[단계1] 파일 가져오기
result <- read.csv("./data/response.csv", header = T)
head(result)
str(result)

#[단계2] 코딩 변경 - 리코딩
result$job2 [result$job == 1] <- "1.학생"
result$job2 [result$job == 2] <- "2.직장인"
result$job2 [result$job == 3] <- "3.주부"
result$response2 [result$response == 1] <- "1.무응답"
result$response2 [result$response == 2] <- "2.낮음"
result$response2 [result$response == 3] <- "3.높음"
table(result$job2, result$response2)

#[단계3] 교차분할표 작성
chisq.test(result$job2, result$response2)
# X-squared = 58.208, df = 4, p-value = 6.901e-12
# 학생 전체의 응답데이터보다 직장인, 주부의 데이터가 훨씬 많음
# 여러개의 데이터가 들쭉날쭉할때 믿을수 있는 데이터인지 확인
# 보통은 crosstable의 p-value만 볼줄 알면 됨!
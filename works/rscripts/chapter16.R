# Chapter 16


# 실습: 유클리디안 거리 계산법
# 단계 1: matrix 객체 생성
x <- matrix(1:9, nrow = 3, by = T)
x

# 단계 2: 유클리디안 거리 생성
dist <- dist(x, method = "euclidean")
dist


# 실습: 1행과 2행 변량의 유클리디안 거리 구하기 
s <- sum((x[1, ] - x[2, ]) ^ 2)
sqrt(s)


# 실습: 1행과 3행 변량의 유클리디안 거리 구하기 
s <- sum((x[1, ] - x[3, ]) ^ 2)
sqrt(s)


# 1.2 계층적 군집 분석
# 실습: 유클리디안 거리를 이용한 군집화
# 단계 1: 군집분석(Clustering)을 위한 패키지 설치
install.packages("cluster")
library(cluster)

# 단계 2: 데이터 셋 생성
x <- matrix(1:9, nrow = 3, by = T)

# 단계 3: matrix 객체 대상 유클리디안 거리 생성
dist <- dist(x, method = "euclidean")

# 단계 4: 유클리디안 거리 matrix를 이용한 군집화 
hc <- hclust(dist)
hc
# 단계 5: 클러스터 시각화 
plot(hc)



# P550 ~
# 실습: iris 데이터 셋을 대상으로 군집 수 자르기 
# 단계 1: 유클리디안 거리 계산
idist <- dist(iris[1:4])
hc <- hclust(idist)
plot(hc, hang = -1)
rect.hclust(hc, k=3, border="red")

# 단계 2: 군집 수 자르기 
ghc <- cutree(hc, k = 3)
ghc
#결과는 군집 번호

# 단계 3: iris 데이터 셋에 ghc 칼럼추가 
iris$ghc <- ghc
table(iris$ghc)
head(iris)
View(iris)

# 단계 4: 요약 통계량 구하기 
g1 <- subset(iris, ghc == 1)
summary(g1[1:4])
g2 <- subset(iris, ghc == 2)
summary(g2[1:4])
g3 <- subset(iris, ghc == 3)
summary(g3[1:4])



# 1.4 비계층적 군집 분석
# 실습: K-means 알고리즘에 군집 수를 적용하여 군집별로 시각화 
# 단계 1: 군집분석에 사용할 변수 추출
library(ggplot2)
data(diamonds)
# nrow : 다이아몬드 전체 레코드갯수 중
# sample : 1000개를 랜덤하게 뽑음
t <- sample(1:nrow(diamonds), 1000) 
nrow(diamonds)
t

test <- diamonds[t, ]
dim(test)
head(test, 10)

mydia <- test[c("price", "carat", "depth", "table")]
head(mydia)


# 단계 2: 계층적 군집분석(탐색적 분석)
result <- hclust(dist(mydia), method = "average")
result
plot(result, hang = -1)
rect.hclust(result, k=3, border="red")

# 단계 3: 비계층적 군집분석
result2 <- kmeans(mydia, 3)
names(result2)
str(result2)
result2$cluster

mydia$cluster <- result2$cluster
head(mydia)

# 단계 4: 변수 간의 상관계수 보기 
cor(mydia[ , -5], method = "pearson")
# carat  0.912078093
# 다이아몬드에 영향을 가장 많이 주는 항목 "carat"
plot(mydia[ , -5])

# 단계 5: 상관계수를 색상으로시각화 
# 상관계수가 음수면 정반대의 관계가 있다(??)
install.packages("mclust")
library(mclust)
install.packages("corrgram")
library(corrgram)
corrgram(mydia[ , -5], upper.panel = panel.conf)
corrgram(mydia[ , -5], lower.panel = panel.conf)


# 단계 6: 비계층적 군집 시각화 
plot(mydia$carat, mydia$price, col = mydia$cluster)
points(result2$centers[ , c("carat", "price")],
       col = c(3, 1, 2), pch = 8, cex = 5)


# p.557 
## 2. 연관분석 
# 실습: 트랜잭션 객체를 대상으로 연관규칙 생성
# 단계 1: 연관분석을 위한 패키지 설치
library(arules)

# 단계 2: 트랜잭션 객체 생성
getwd()
tran <- read.transactions("./data/tran.txt", 
                          format = "basket", sep = ",")
tran


# 단계 3: 트랜잭션 데이터 보기 
inspect(tran)

# 단계 4: 규칙 발견
rule <- apriori(tran, parameter = list(supp = 0.5, conf = 0.1))
inspect(rule) # 규칙 9개

rule <- apriori(tran, parameter = list(supp = 0.3, conf = 0.1))
inspect(rule) # 규칙 16개

rule <- apriori(tran, parameter = list(supp = 0.1, conf = 0.1))
inspect(rule) # 규칙 35개


# 실습: single 트랜잭션 객체 생성
stran <- read.transactions("./data/demo_single", format = "single", cols = c(1, 2))
inspect(stran)

# 실습: 중복 트랜잭션 제거 
# 단계 1: 트랜잭션 데이터 가져오기 
stran2 <- read.transactions("./data/single_format.csv", format = "single",
                            sep = ",", cols = c(1, 2), rm.duplicates = T)

# 단계 2: 트랜잭션과 상품수 확인
stran2

# 단계 3: 요약 통계량 제공
summary(stran2)

# 실습: 규칙 발견(생성)
# 단계 1: 규칙 생성하기 
astran2 <- apriori(stran2)
astran2 <- apriori(stran2, parameter=list(supp=0.3, conf=0.1))
#rule <- apriori(tran, parameter = list(supp = 0.5, conf = 0.1))

# 단계 2: 발견된 규칙 보기 
inspect(astran2)

# 단계 3: 상위 5개의 향상도를 내림차순으로 정렬하여 출력
inspect(head(sort(astran2, by = "lift")))

# 실습: basket 형식으로 트랜잭션 객체 생성
btran <- read.transactions("./data/demo_basket", format = "basket", sep = ",")
inspect(btran)


# p.565
## 2.3 연관규칙 시각화
# 실습: Adult 데이터 셋 가져오기 
data(Adult)
Adult
head(Adult)
inspect(head(Adult))

# 실습: AdultUCI 데이터 셋 보기 
data("AdultUCI")
str(AdultUCI)


# 실습: Adult 데이터 셋의 요약 통계량 보기 
# 단계 1: data.frame 형식으로 보기 
adult <- as(Adult, "data.frame")
str(adult)
head(adult)

# 단계 2: 요약약 통계량
summary(Adult)


# 실습: 다양한 신뢰도와 지지도를 적용한 예 
# 지지도 10%와 신뢰도 80%가 적용된 경우, 6137개 연관규칙 발견
ar <- apriori(Adult, parameter = list(supp = 0.1, conf = 0.8))

# 지지도를 20%로 높인 경우 1,306개 규칙 발견
ar1 <- apriori(Adult, parameter = list(supp = 0.2)) 

# 지지도를 20%, 신뢰도를 95%로 높인 경우 348개 규칙 발견
ar2 <- apriori(Adult, parameter = list(supp = 0.2, conf = 0.95))

# 지지도를 30%, 신뢰도를 95%로 높인 경우 124개 규칙 발견
ar3 <- apriori(Adult, parameter = list(supp = 0.3, conf = 0.95))

# 지지도를 35%, 신뢰도를 95%로 높인 경우 67개 규칙 발견
ar4 <- apriori(Adult, parameter = list(supp = 0.35, conf = 0.95))

# 지지도를 40%, 신뢰도를 95%로 높인 경우 36개 규칙 발견
ar5 <- apriori(Adult, parameter = list(supp = 0.4, conf = 0.95))


# 실습: 규칙 결과 보기 
# 단계 1: 상위 6개 규칙 보기 
inspect(head(ar5))

# 단계 2: confidence(신뢰도) 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(ar5, decreasing = T, by = "confidence")))

# 단계 3: lift(향상도) 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(ar5, by = "lift")))



# 실습: 연관규칙 시각화 
# 단계 1: 패키지 설치 
install.packages("arulesViz")
library(arulesViz)

# 단계 2: 연관규칙 시각화
plot(ar3, method = "graph", control = list(type = "items"), engine = "htmlwidget")
plot(ar5, method = "graph", control = list(type = "items"), engine = "htmlwidget")


# 실습: Groceries 데이터 셋으로 연관분석 하기 
# 단계 1: Groceries 데이터 셋 가져오기 
data("Groceries")
str(Groceries)
Groceries

# 단계 2: data.frame으로 변환
Groceries.df <- as(Groceries, "data.frame")
head(Groceries.df)

# 단계 3: 지지도 0.001, 신뢰도 0.8 적용규칙 발견 (410개)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# 단계 4: 규칙을 구성하는 왼쪽(LHS) -> 오른쪽(RHS)의 item 빈도수 보기 
plot(rules, method = "grouped")



# 실습: 최대 길이가 3이하인 규칙 생성
# maxlen : 아이템의 개수 제한
rules <- apriori(Groceries, 
                 parameter = list(supp = 0.001, conf = 0.80, maxlen = 3))


# 실습: Confidence(신뢰도) 기준 내림차순으로 규칙 정렬
rules <- sort(rules, decreasing = T, by = "confidence")
inspect(rules)


# 실습: 발견된 규칙 시각화 
# wholemilk와 othervegetables가 제일 많은 영향을 가지는걸 볼 수 있음
plot(rules, method = "graph", engine = "htmlwidget")
plot(rules, method = "grouped")


# 실습: 특정 상품(item)으로 서브 셋 작성과 시각화 
# 단계 1: 오른쪽 item이 전지분유(whole milk)인 규칙만 서브 셋으로 작성
wmilk <- subset(rules, rhs %in% 'whole milk')
wmilk # set of 18 rules

inspect(wmilk)
plot(wmilk, method = "graph", engine = "htmlwidget")

# 단계 2: 오른쪽 item이 other vegetables인 규칙만 서브 셋으로 작성
oveg <- subset(rules, rhs %in% 'other vegetables')
oveg
inspect(oveg)
plot(oveg, method = "graph", engine = "htmlwidget")

# 단계 3: 왼쪽 item이 yogurt 단어가 포함된 규칙만 서브 셋으로 작성
yog <- subset(rules, lhs %in% 'yogurt')
yog
inspect(yog)
plot(yog, method = "graph", engine = "htmlwidget")

# 단계 4: 왼쪽 item이 butter 또는 yogurt인 규칙만 서브 셋으로 작성
butter_yogurt <- subset(rules, lhs %in% c('butter', 'yogurt'))
butter_yogurt
inspect(butter_yogurt)
plot(butter_yogurt, method = "graph", engine = "htmlwidget")


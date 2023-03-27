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

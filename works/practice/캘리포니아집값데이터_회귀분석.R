#회귀분석 예제
#캘리포니아 집 값 데이터

library(tidyverse)
library(reshape2)

house <- read.csv("./practice/housing.csv")
head(house)
View(house)

### 단계 1 : 데이터 전처리
summary(house)
# NA 확인
# ocean_proximity : tyep이 int가 아님(character)

## 데이터 시각화 (데이터 사전 확인)
colnames(house)
par(mfrow=c(2,5))
# ggplot

## 결측값 처리 (outlier 처리 -> 정규화)
house$mean_bedrooms = house$total_bedrooms / house$households
house$mean_rooms = house$total_rooms / house$households
drop = c('total_bedrooms', 'total_rooms')
house = house[, !(names(house) %in% drop)]
head(house)

## 전처리(상식을 사용해서 가정에 대한 데이터를 별도로 분리)
str(house)
categories = unique(house$ocean_proximity)
categories
cat_house = data.frame(ocean_proximity = house$ocean_proximity)
cat_house

for(cat in categories) {
  cat_house[,cat] = rep(0, times=nrow(cat_house))
}
# 원핫인코딩
for(i in 1:length(cat_house$ocean_proximity)) {
  cat = as.character(cat_house$ocean_proximity[i])
  cat_house[,cat][i] = 1
}

head(cat_house)
tail(cat_house)








### 단계 2 : 종속-독립변인 설정
# y : median_house_value
# x : 가설을 세워야함 (바다가 가까운가? 수입? 자가? )

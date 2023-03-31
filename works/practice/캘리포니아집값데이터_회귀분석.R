library(tidyverse)
library(reshape2)

#회귀분석 예제
#캘리포니아 집 값 예측 (y : 집값예측결과)

## 예측 순서 
# 1. 데이터 불러오기 확인 -> 시각적인 확인
# 2-1. 전처리 과정 -> NA 제거
# 2-2. 후처리 과정 -> 표준화와 정규화
# 3. 데이터 분리 -> 학습과 검증
# 4. 학습 -> 기울기와 절편
# 5. 검증 -> 모델을 검증 (3~5반복)


# 1. 데이터 불러오기
housing <- read.csv("./practice/housing.csv")
View(housing) # 전체 데이터 확인
head(housing) # 보고서에 담을때 사용
str(housing) # 변수 요약정보 확인 (data type 확인 : not number 처리)
summary(housing) # 기술통계정보 확인 (NA 확인, Median~Mean 확인)
colnames(housing)

## 데이터 시각화(경향 확인)
par(mfrow=c(3,3))
ggplot(data = melt(housing), mapping = aes(x=value)) + 
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = 'free_x')

# => 결과물 : 데이터에 대한 인상
### => 전반적으로 데이터가 편향되어져있음 확인 
### => median_house_value : median값보다 최대값이 크다 ...
### => NA처리, not number 처리, total_bedrooms와 total_rooms => mean값으로 처리


# 2-1. 데이터 전처리
## 연속형 숫자(소수) => 산점도 
## 불연속적(정수) => histogram

## NA 처리
### mean과 median 선택을 위한 시각화
### 어떤 데이터의 편향이 강하면(Overwhelm) 평균값을 사용X, 중앙값 사용 
bedroom_mean <- mean(housing$total_bedrooms, na.rm=TRUE)
bedroom_median <- median(housing$total_bedrooms, na.rm=TRUE)
ggplot(housing, aes(x = total_bedrooms)) +
  geom_histogram(bins = 40, color = "black", fill = "blue") +
  geom_vline(aes(xintercept = bedroom_mean, color = "Mean"), lwd = 1.5) +
  geom_vline(aes(xintercept = bedroom_median, color = "Median"), lwd = 1.5) +
  xlab("Total Bedrooms") +
  ylab("Frequency") +
  ggtitle("Histogram of Total Bedrooms (noncontinuous variable)") +
  scale_color_manual(name = "Summary Stats", labels = c("Mean", "Median"), values = c("red", "green"))
# 결과가 mean이 좀 더 우측으로 치우쳐져있으므로 median 값 사용 결정
# total_bedrooms의 값을 median으로 대체 및 na 처리
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm = T)
sum(is.na(housing))


# 2-2. 후처리 과정 -> 표준화와 정규화 (중복제거, 단위통일), 데이터 경향성 확인 후 둘중 하나 결정
## 정규화 : min = 0, max = 1을 갖도록 변환 (아웃라이어 많을때 사용)
## 표준화 : 평균 = 0, 표준편차 =1 을 갖도록 변환

## 불연속적 데이터 처리
housing$mean_bedrooms <- housing$total_bedrooms/housing$households
housing$mean_rooms <- housing$total_rooms/housing$households

## 불필요한 특징 삭제
drops <- c('total_bedrooms', 'total_rooms')
housing <- housing[, !(names(housing) %in% drops)]
head(housing)

## 범주형 데이터 처리
### 데이터프레임 생성, ocean_proximity : character -> int로 변환
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)

#### 0으로 우선 채워주고
for(cat in categories) {
  cat_housing[,cat] = rep(0, times=nrow(cat_housing))
}

#### 필요한 데이터만 1로 업데이트
for(i in 1:length(cat_housing$ocean_proximity)){
  cat <- as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] <- 1
}

head(cat_housing)
tail(cat_housing)

#### 기존특징 삭제 (사용X)
cat_columns <- names(cat_housing)
keep_columns <- cat_columns[cat_columns != 'ocean_proximity']
cat_housing <- select(cat_housing,one_of(keep_columns))

## 수치형 변수 처리 (단위맞춤)
colnames(housing)
## 명목형 변수(`ocean_proximity`)와 예측 변수(`median_house_value`)는 대상에서 제외하도록 하겠습니다.
drops <- c('ocean_proximity','median_house_value')
housing_num <-  housing[ , !(names(housing) %in% drops)]
head(housing_num)

scaled_housing_num <- scale(housing_num)

## 데이터 표준화
scaled_housing_num <- scale(housing_num)
head(scaled_housing_num)

## 정리된 데이터 결합
cleaned_housing <- cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)

# => 2번 과정 결과물 : clean_data




# 3. 데이터를 분리 -> 학습과 검증
set.seed(42) #임의랜덤 고정해놓고 경향성 확인 후 -> 주석달고 고정 해제
sample <- sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train <- cleaned_housing[sample, ] #just the samples
test  <- cleaned_housing[-sample, ] #everything but the samples

# 분리된 데이터가 전체 데이터를 반영하고 있는지 확인
nrow(train) + nrow(test) == nrow(cleaned_housing)

## 예측모델 생성 및 평가
# 선형 모형 테스트를 위한 3개 변수 (->p값 낮은거 들고오든가,.. )
glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)

# 과적합(overfit) : 테스트케이스는 100% 적합이나 실제 케이스 넣으면 맞지않음
# 과적합 막기 위해 교차검증함 (학습데이터와 검증데이터), 랜덤 횟수 조정
# glm : 일반적인 선형 모델을 만들어 주세요
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)

# delta값 : 오차의 평균 => 판단이 힘들다
k_fold_cv_error$delta

# 루트를 씌워서 값을 떨어트림 +-8만불 정도의 차이는 보일 것 같다
glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

# 오차를 줄여야 함 => "단순선형회귀를 베이스"로 해서 개선해야함
glm_house$coefficients
## (Intercept) median_income    mean_rooms    population
##  206855.817     82608.959     -9755.442     -3948.293

# => 결과물 : 모델
# 분석을 통해 소득 중앙값(`median_income`)이 주택 가격(`median_house_value`)에 가장 큰 영향을 미친다고 판단할 수 있습니다.





# 머신러닝 제1목표 : 베이스라인 설정 (단순선형회귀의 rmse값이 준수)
## => 데이터에 대한 인상
## => 전처리 끝난 clean data 필요
### => rmse 베이스라인 획득
### => rmse 넘을 수 있는 다른 알고리즘, 다른 분석기법 찾기, 기존 rmse대비 얼마 낮아졌다~

# 제2목표 : 설명가능한 rmse 개선
# 설명가능한 rmse값 나오게하는 법 : (분류)
# 랜덤포레스트(설명가능, 전처리 안해도 꽤 괜찮음, 회귀로도 사용가능), 의사결정트리
# 최종적으로 "xgboost"까지 가는게 목표 (업계에서 실무적으로 표준..ㅋㅋ)

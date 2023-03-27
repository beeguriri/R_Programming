# 인구 통계
install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse)
library(readxl)

df <- read_excel("./practice/시군구_성_월별_출생19972021.xlsx")
View(df)

#남녀총계와 전국데이터의 월별 평균출생아수를 보고자 함
#원래 데이터는 건들지않도록 추가로 변수 선언하기!
df2 <- df %>%
  filter(!is.na(시점)) %>% #남녀총계행
  select(시점, 전국) %>%
  separate(시점, into = c("년도", "월"))

df2 <- df2 %>%
  group_by(월) %>%    #column명을 사용하므로 쌍따옴표 사용 안함
  summarise(평균출생수 = mean(전국))

df2 %>%
  qplot(x=월, y=평균출생수, data=.)   # "." : 참조연산자(df2 자기자신을 뜻함)

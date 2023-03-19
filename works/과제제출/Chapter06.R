# 5. reshape2 패키지를 이용하여 단계별로 iris 데이터셋을 처리하시오.

install.packages("reshape2")
library(reshape2)
data(iris)
str(iris)
head(iris)
iris_melt <- melt(iris, id=c("Species"), na.rm =T)
iris_melt
iris_wide <- dcast(iris_melt, Species ~ variable, sum)
iris_wide


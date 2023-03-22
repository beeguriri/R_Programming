#버전 change없이 4.2.2에서 koNLP 사용하는 방법
#참고사이트 : https://rdmkyg.blogspot.com/2022/06/r-42-windows-rjava-kolnp.html

install.packages("multilinguer")

# 의존성을 설치 한다. 
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# Git hub로 설치 한다. 
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

# KoNLP 예제 
library(KoNLP)
library(Sejong)
useSejongDic()
sentence <- '아버지가 방에 스로륵 들어가신다.'
extractNoun('테스트 입니다.')
extractNoun(sentence)

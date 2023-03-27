#Chaper 8 연습문제 풀이

getwd()

data(quakes)
?quakes
??equal.count #함수의 패키지 확인

library(lattice)

# 문제 8-1
str(quakes)
equal.count(quakes$depth, number=3, overlap=0) 
equal.count(quakes$mag, number=2, overlap=0)
#number : 몇개의 균등구간으로 나누어줌 (보통 3~5 사용)

depthgroup <- equal.count(quakes$depth, 
                          number=3, overlap=0) 
magnitudegroup <- equal.count(quakes$mag, 
                              number=2, overlap=0)
xyplot(lat ~ long | magnitudegroup * depthgroup, data=quakes)

# 문제 8-2
library(latticeExtra)
xyplot(min.temp + max.temp ~ day | month, 
       data = SeatacWeather, type="l")

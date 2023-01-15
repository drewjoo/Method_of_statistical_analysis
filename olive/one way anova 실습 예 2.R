# 데이터 읽어오기

olive<-read.csv("olive.csv")

str(olive)

# 분석 목표 : region에 따라 palmitic 산의 양이 다른지를 알아보자

region<-olive[,"region"]
palmitic<-olive[,"palmitic"]

# region 1:Centre-North, 2: South, 3:Sardinia


table(region)
summary(palmitic)


# 히스토그램을 이용한 비교

par(mfrow=c(3,1))
hist(palmitic[region==1],xlim=c(500,2000),main="Centre-North")
hist(palmitic[region==2],xlim=c(500,2000),main="South")
hist(palmitic[region==3],xlim=c(500,2000),main="Sardinia")

par(mfrow=c(1,1))

# 분포의 모양은 Central-North와 south에서는 좌우대칭인 종모양에 
# 가까우나, Sardinia에서는 왼쪽으로 쏠려있는 단봉형으로 보인다.
# 중심 위치는 Central-North에서 가장 커보이고 South와 Sardinia에서는
# 비슷해보인다. 즉, region에 따라 중심의 위치가 달라 보인다.
# 퍼진 정도는 Central-North에서 가장 크고 South에서 가장 작아인다. 
# 즉, region에 따라서 퍼진 정도가 달라 보인다.

# density plot

d1<-density(palmitic[region==1],bw=80)
d2<-density(palmitic[region==2],bw=20)
d3<-density(palmitic[region==3],bw=30)

plot(d1,xlim=c(500,2000),ylim=c(0,0.01),col="red")
lines(d2,col="green")
lines(d3,col="blue")

# 히스토그램의 결과와 비슷하나 South에서의 중심의 위치가 Sardinia보다
# 커보이고 South에서의 퍼진 정도가 Sardinia보다 작아보인다.
# 또한, Sardinia에서의 쏠린 정도가 히스토그램보다 약하고 거의 
# 좌우 대칭으로 보인다.

boxplot(palmitic~region)

# density의 결과와 비슷하고 Central-North에서 꼬리가 두꺼워보인다. (경계선을 
# 넘는 점들이 많다.)

qqplot(palmitic[region==2],palmitic[region==3])
abline(a=0,b=1)

# South와 Sardinia를 비교하면 역 S자 형태로 Sardinia에서의 분포가
# 꼬리가 두껍다는 것을 알 수 있다. 전반적으로 기울기는 1에 가깝고 
# 절편은 0보다 약간 작다는 것을 알 수 있다. 즉 region==2(South)의 
# 중앙값이 region==3(Sardinia)의 중앙값보다 크다는 것을 의미한다.



qqplot(palmitic[region==1],palmitic[region==2])
abline(a=0,b=1)

qqplot(palmitic[region==1],palmitic[region==3])
abline(a=0,b=1)

# 위의 그림은 비슷한 형태를 나타내는데 x 축의 중앙값 근처의 값들이
# y=x 선 밑에 위치하므로 x축의 중앙값이 y축의 중앙값보다 크다. 즉
# Central-North에서의 중심의 위치가 South나 Sardinia보다 크다는 것을
# 의미한다.
# 전반적으로 기울기가 1보다 작기 때문에 x축의 퍼진 정도보다 y축의 퍼진
# 정도가 작다. 따락서, Central-North에서의 퍼진 정도 South나 Sardinia의
# 퍼진 정도보다 작다는 것을 의미한다.

# 통계량을 이용한 비교

tapply(palmitic,region,mean)
# 표본평균은 Central-North, South, Sardinia 순으로 크다. Central-
# North는 다른 지역에 비하여 월등히 커 보이고 South와 Sardinia의 차이는
# 커보이지 않는다.

tapply(palmitic,region,var)
tapply(palmitic,region,sd)
# 퍼진 정도는 Central-North에서 가장 크고 Sardinia, South 순으로 
# 크다.

# 분산 비교

install.packages("car")
library(car)

install.packages("lawstat")
library(lawstat)

bartlett.test(palmitic~region)
leveneTest(palmitic~as.factor(region))
fligner.test(palmitic~region)

bartlett.test(palmitic[region!=1]~region[region!=1])
leveneTest(palmitic[region!=1]~as.factor(region[region!=1]))
fligner.test(palmitic[region!=1]~region[region!=1])

var.test(palmitic[region==2],palmitic[region==3])

# test 결과, Central-Norh, South, Sardinia 모두 분산이 다르다고 
# 할 수 있다.

# 정규성 검정

qqnorm(palmitic[region==1])
qqline(palmitic[region==1])
# Central-North에 대하여 역S자 형태로 첨도가 3보다 클 것으로 예상된다.

qqnorm(palmitic[region==2])
qqline(palmitic[region==2])
# South에 대하여 직선에 가까우므로 정규분포에 가까운 분포라 할 수 있다.

qqnorm(palmitic[region==3])
qqline(palmitic[region==3])
# Sardinia에 대하여 약하게 역S자 형태를 보이고 정규분포에 가까울 것 같지 않다.

install.packages("moments")
library(moments)

install.packages("nortest")
library(nortest)

tapply(palmitic,region,skewness)
tapply(palmitic,region,agostino.test)
tapply(palmitic,region,kurtosis)
tapply(palmitic,region,anscombe.test)
tapply(palmitic,region,lillie.test)

# South(region=2)의 데이터는 왜도가 0이라고 할 수 있고 첨도도 3이라 할 수 있다.
# 또한, 정규성검정에 south의 데이터는 정규분포로부터 도출되었다고 할 수 있다.
# (유의수준 0.05)
# 그러나, Central-North(region=1)와 Sardinia(region=3)의 데이터는 왜도가 음수로
# 오른쪽으로 쏠려 있는 분포를 나타내고 있고 결과적으로 정규분포로부터 도출되었다고 
# 하기 힘들다. (유의수준 0.05)


# 만약, 각 범주에서 또는 각 모집단에서의 데이터의 분포가 정규분포를 따르나, 
# 등분산성을 만족하지 않는다고 하면, Welch의 approximation으로  p-value를 얻을
# 수 있다.

oneway.test(palmitic~region,var.equal=F)

# oneway.test의 결과 p-value가 0.05보다 작으므로 각 지역의 plamitic의 평균이 
# 같다고 할 수 없다.
# 따라서, region에 따른 palmiticd의 분포가 정규분포 따르나 분산이 다르다고 
# 가정했을 때, 각 지역의 plamitic의 평균이 같다고 할 수 없다.

# 정규분포를 만족하지 않는 경우 비모수적인 방법을 통하여 가설 검정할 수 있다.
# Kruskal-Wallis 방법은 비모수적인 방법으로 순위를 기반으로 분산분석을 한다.

install.packages("dplyr")
library(dplyr)

kruskal.test(palmitic~region) 

# 정규분포와 등분산성을 가정하지 않더라도 Kruskal-Wallis 방법을 통하여 3 지역의
# palmitic산의 중심의 위치가 동일한지를 검정할 수 있다. 위의 결과에서 p-value가
# 0.05보다 작으므로 유의수준 0.05에서 세 지역의 palmitic산의 중심의 위치가 같다고
# 할 수 없다.



 






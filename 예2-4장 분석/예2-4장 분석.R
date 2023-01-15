## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## 2장                                                                     ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## 예제 12 ------------------------------------------------------------------
# 자료 입력
death <- c(2,1,2,4,2,5,3,3,5,6,3,8,3,
           3,6,3,6,5,3,5,2,6,2,3,4,3,
           2,9,2,2,3,2,7,3,2,10,6,2,3,
           1,2,3,3,4,3,2,6,2,2,3,2,3,
           4,3,2,3,5,2,5,5,3,4,3,6,2,
           1,2,3,2,6,3,3,6,3,2,3,6,4,
           6,5,3,5,6,2,6,3,2,3,2,6,2,
           6,3,3,2,6,9,6,3,6,6,2,3,2,
           3,5,3,5,2,3,2,3,3,1,3,3,2,
           3,3,4,3,6,6,3,3,3,2,3,3,6)

# 도수분포표
table(death)

# 3가 제일 많이 발생하고 2가 두번째로 발생하는 것을 
# 알 수 있다. 1, 7, 8, 9, 10 have small 
# frequency.

freq.table<-table(death)

# 막대그래프 그리기
barplot(table(death))
barplot(freq.table)
barplot(freq.table,col="red",
        main="사망원인에 대한 막대그래프",
        ylab="빈도수", xlab="사망원인")

cause<-c("감염성 질환", "각종 암", 
      "순환기 질환", " 호흡기 질환",
      "소화기 질환","각종 사고사", 
      "비뇨기 질환", "정신병","노환", 
      "신경계 질환")

barplot(freq.table, col=rainbow(10),
        main="사망원인에 대한 막대그래프",
        ylab="빈도수", xlab="사망원인",
        legend.text=cause,
        args.legend=list(x="topleft"))

# 원형그래프
pie(freq.table,label=cause,
        main="사망원인에 대한 원형그래프",
        cex=0.7)

# 파레토 그래프
install.packages("qcc")
library(qcc)
pareto.chart(freq.table,
             main="사망원인에 대한 파레토그림",
             xlab="질병종류",
             ylab="도수",ylab2="누적상대도수",
             legend.text=cause)

# 3(순환기 질환), 2(각종 암), 6(사고사)가
# 전체 사망의 80% 가까이 차지하고 있는 것을
# 알 수 있다.


## 예제 13 ------------------------------------------------------------------
# 자료 입력
drink <- c(101.8,101.5,101.8,102.6,101,96.8,102.4,100,98.8,98.1,
           98.8,98,99.4,95.5,100.1,100.5,97.4,100.2,101.4,98.7,
           101.4,99.4,101.7,99,99.7,98.9,99.5,100,99.7,100.9,
           99.7,99,98.8,99.7,100.9,99.9,97.5,101.5,98.2,99.2,
           98.6,101.4,102.1,102.9,100.8,99.4,103.7,100.3,100.2,101.1,
           101.8,100,101.2,100.5,101.2,101.6,99.9,100.5,100.4,98.1,
           100.1,101.6,99.3,96.1,100,99.7,99.7,99.4,101.5,100.9,
           101.3,99.9,99.1,100.7,100.8,100.8,101.4,100.3,98.4,97.2)

hist(drink, freq=F)
hist.result<-hist(drink, freq=F)
lines(x=hist.result$mids,
      y=hist.result$density,
      type="b",
      pch=20)

# 단봉형으로 오른쪽으로 약간 쏠린 것으로 
# 보이나 쏠린 정도가 심한 것 같지는 않다.
# 데이터가 100 중심으로 나타난 것 같다.

summary(drink)
mean(drink)
median(drink)
q<-quantile(drink)

quantile(drink,
         probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95))
var(drink)
sd(drink)  
min(drink)
max(drink)
IQR(drink)
quantile(drink,0.75)-quantile(drink,0.25)
q[4]-q[2]

install.packages("psych")
library(psych)
describe(drink)

# 평균과 중앙값을 볼 때 중심의 위치는 100에 
# 가까우며 대90% 이상이 97.3과 102.2 사이에
# 있다. 왜도를 볼 때 약간 오른쪽으로 쏠려 
# 있는 것 같다.




## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## 4장                                                                     ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## 예제 05 ------------------------------------------------------------------
# 자료 입력
height <- c(181,161,170,160,158,168,162,179,183,178,171,177,163,158,160,160,158,
            173,160,163,167,165,163,173,178,170,167,177,175,169,152,158,160,160,
            159,180,169,162,178,173,173,171,171,170,160,167,168,166,164,173,180)
weight <- c(78,49,52,53,50,57,53,54,71,73,55,73,51,53,65,48,59,
            64,48,53,78,45,56,70,68,59,55,64,59,55,38,45,50,46,
            50,63,71,52,74,52,61,65,68,57,47,48,58,59,55,74,74)

plot(x=height, y=weight,xlab="키(cm)",
	ylab="몸무게(kg)",
	main="키와 몸무게 산점도")

# 키가 증가할수록 몸무게가 증가하는 경향이 있다.
# 직선의 경향을 보이지만 좀 퍼져 있는 패턴이 
# 있는 것으로 보인다.


cor(height,weight)
cor.test(height,weight)
# 상관계수가 0.74로 양의 선형관계를 나타내는 
# 것을 알 수 있다. p-value가 매우 작으므로
# 직선의 관계가 있다는 것을 알 수 있다.


cor.test(height,weight,method="spearman")


# 머리색깔과 눈색깔

haireye<-matrix(data=c(
	 68, 15,  5, 20,
	119, 54, 29, 84,
	 26, 14, 14, 17,
	  7, 10, 16, 94
	),
	nrow=4,ncol=4,byrow=TRUE,
	dimname=list(
		c("black","brunette","red","blond"),
		c("brown","hazel","green","blue")
	)
) 
haireye
margin.table(haireye) # 전체합
margin.table(haireye,margin=1) # 행별 합
margin.table(haireye,margin=2) # 열별 합

# 머리색깔

pie(margin.table(haireye,margin=1))
barplot(margin.table(haireye,margin=1))
# 갈색(brenette)이 전체 반정도 차지하고 있고
# 금색(blond)과 검은색(black)이 비슷한 도수를 보이며
# 빨간색(red)가 가장 작은 빈도를 보이나,
# 금색, 검은색, 빨간색 간의 빈도 차이가 아주
# 커 보이지 않는다.

# 눈색깔

pie(margin.table(haireye,margin=2))
barplot(margin.table(haireye,margin=2))
# brown과 blue가 비슷한 빈도로 많은 부분을 
# 차지하고 있고, hazel과 green은 상대적으로
# 적은 빈도를 차지하고 있다.

# 머리색깔 vs. 눈색깔
barplot(haireye)
barplot(t(haireye), beside = T, legend=colnames(haireye),
        args.legend = list(x="topleft"), main = "머리색깔 vs. 눈색깔")

mosaicplot(haireye,main="머리색깔 vs. 눈색깔",
	color=T)
# 머리색깔이 black인 경우 눈색깔이 brown인 경우가
# 제일 많고 blue와 hazel이 그 다음 빈도를 차지
# 하고 있고 green이 소수인 반면,
# blond에서는 blue가 가장 많은 빈도를 차지하고 있고
# blue, green, hazel 순서로 빈도를 차지하고
# 있다.
# 각 머릿색깔에서 눈색깔이 차지하고 있는 비율이 
# 서로 다른 경향을 보이고 있으므로 머리색깔과 
# 눈색깔 간의 연관이 있어 보인다.



# 분할표

install.packages("gmodels")
library(gmodels)

CrossTable(haireye)
CrossTable(haireye,prop.t=F,prop.c=F,prop.chisq=F)
CrossTable(haireye,prop.t=F,prop.c=F,prop.chisq=F,chisq=T)
# 분할표에서 눈색깔에 대한 marginal 분포는 brown:hazel:green:blue=
# 0.372:0.157:0.108:0.363이다. 반면, 각 머리색깔에서 눈색깔의 비율은 
# marginal 분포와 다른 경향을 보이고 있다.
# 머리색깔이 black인 경우 눈색깔이 brown인 사람의 비율이 매우 높고 
# 눈색깔이 green이나 blue인 사람의 비율이 전체에 비하여 낮은 것을 알 수 있다.
# 머리색깔이 brunette인 경우 눈색깔이 brown이나 hazel의 비율이 약간 높고
# green의 비율은 비슷하며 blue이 비율이 낮다.
# 머리색깔이 red인 경우 brown의 비율은 비슷하고 hazel의 비율은 약간 높으며
# green의 비율은 높고 blue은 비율은 낮다.
# 머리색깔이 blond인 경우 전체와 매우 다른 경향을 보이고 있다.
# brown의 비율이 매우 낮으며 hazel의 비율도 낮은 반면, blue의
# 비율이 매우 높으며 green의 비율도 높다.
# 독립성 검정에 대한 가설검정에서 p-value가 매우 작으므로 
# 머리색깔과 눈색깔이 독립이라고 하기 힘들다. (유의수준=0.05)
# 전체적으로 머리색깔에 따른 눈색깔의 비율은 같다고 하기 힘들며
# 눈색깔과 머리색깔은 서로 연관이 있는 것으로 보인다.

CrossTable(haireye[c(2,3),],prop.t=F,prop.c=F,prop.chisq=F,chisq=T)
# 머리색깔이 brunette과 red인 경우 눈색깔이 비율이 차이가 커
# 보이지 않고 독립성 검정에서도 p-value가 0.05보다 크므로 
# 독립이라고 보기 힘들다. 따라서, 머리색깔이 brunette과 red인 
# 경우에 눈색깔이 비율이 다른다고 볼 수 있는 증거가 약하다고 할 
# 수 있다.

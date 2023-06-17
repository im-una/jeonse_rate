setwd("D:/8th/경영데이터분석/과제")

library(readr)
library(tidyverse)
library(dplyr)
library(psych)

ratio <- read_csv("유형별_매매가격_대비_전세가격_비율.csv", locale = locale('ko', encoding = 'euc-kr'))
ratio <- rename(ratio, time202207=`2022.07`, time202304=`2023.04`) # 변수명 변경
colnames(ratio) <- gsub("[[:punct:]]", "", colnames(ratio)) # 변수명 기호 제거

#################### 대응표본 t-검정 ########################
### 과거 전세가율(매매가격 대비 전세금액)과 현재 전세가율이 차이가 있는지 ###
############ STEP1: 가설수립 ############
# 귀무가설 : u현 - u과 = 0 즉, ud(두 모평균의 차이)=0
# 대립가설 : u현 - u과 != 0

############ STEP2: 차이 변수 만들기 ############ 
# 2022.07 전세가율에서 2023.04 전세가율 뺀 변수(d) 생성
ratio <- ratio %>% mutate(d = time202207 - time202304) 

############ STEP3: d의 정규성 검토 ############
## 이미 sample size가 205개로 30개가 넘어서 중심극한정리에 따라서 두 표본평균의 차이는 정규분포를 그림

## 그냥 한번 봐봄
shapiro.test(ratio$d) ## 한번 확인해봤는데.. p-value가 유의해서 정규성 조건을 만족하기는 어려움ㅋ
summary(ratio$d)
hist(ratio$d, breaks=seq(-4,9,0.5))

## 이상치
library(psych)
descr <- describe(ratio$d)
descr <- descr %>% as.data.frame() %>% mutate(LL = mean-2*sd) #LL: Lower Limit
descr <- descr %>% as.data.frame() %>% mutate(UL = mean+2*sd) #UL: Upper Limit
# ratio %>% filter(ratio$d < descr$LL)
# table(ratio$d > descr$UL)
#### 그래도 제거하지 않고 다 포함시킬 것

############ STPE4: 대응표본 t검정 통한 가설검정 ############
## 이상치 존치 ##
t.test(ratio$time202207, ratio$time202304, alternative = "two.sided", paired = T)
#### 결과: Xbar(과거) - Xbar(현재)가 양수가 나온 것.
#### p-value < alpha -> 대립가설 채택
#### 두 모평균의 차이가 0이 아니다.

## 최종결론 : 대립가설 채택, 과거의 전세가율 > 현재 전세가율
## 이 차이는 통계적으로 유의한 차이이다.
## 전체적으로 전세가율이 하락했다고 할 수 있다.

#################### One-way ANOVA???? ########################

ratio%>%group_by(주택유형별1)%>%summarise(mean(d))
#### 결과보면..연립다세대 전세가율은 오히려 증가했음
ratio%>%group_by(지역별1)%>%summarise(mean = mean(d)) %>% arrange(mean)
#### 지역별로 보았을 때, 특히 경남, 전북 지역의 전세가율에 대한 주의가 필요해보임.
ratio%>%group_by(주택유형별1, 지역별1)%>%summarise(mean = mean(d)) %>% arrange(mean)

ratio%>%group_by(지역별1,지역별2)%>%summarise(mean = mean(d)) %>% arrange(-mean)

ratio%>%filter(주택유형별1=="아파트" & 지역별1=="경기") %>% group_by(지역별3)%>%summarise(mean = mean(d)) %>% arrange(mean)

ratio%>%group_by(지역별1, 지역별2)%>%summarise(mean = mean(time202207)) %>% arrange(-mean)

####### 전세가율 상승률만 보기..?
# + - 나눠서?

####### 현재 전세가율 높은 지역만 보기..?
View(ratio)
str(ratio)
#barplot(mean ~ 지역별1, data=now_1, ylim=c(40,80))
library(ggplot2)

## 현재 지역별 전세가율에 대해서만
now_ratio <- ratio%>%group_by(지역별1)%>%summarise(mean = mean(time202304))

ggplot(now_ratio, aes(x=지역별1, y=mean, fill=지역별1))+
  geom_bar(stat="identity", width=0.5)+
  geom_hline(yintercept=70,  color='red', size=1)

#### -> 강원,경남,경북,전남,전북, 충남, 충북 지역은 현재 전세가율이 70%를 넘음.

###### 소계 없애고...안할래...........귀찮..ㅎㅎ
now_2 <- ratio%>%group_by(지역별2)%>%summarise(mean = mean(time202304))

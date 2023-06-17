setwd("D:/8th/경영데이터분석/과제/2023")

library(tidyverse)
library(dplyr)
library(psych)

lent <- read_csv("서울시 부동산 전월세가 정보.csv", locale = locale('ko', encoding = 'euc-kr')) # 전월세 거래
sell <- read_csv("서울시 부동산 실거래가 정보.csv", locale = locale('ko', encoding = 'euc-kr')) # 매매 거래


jeonse <- read_csv("서울시 부동산 전월세가 정보.csv", locale = locale('ko', encoding = 'euc-kr')) # 전월세 거래
maemae <- read_csv("서울시 부동산 실거래가 정보.csv", locale = locale('ko', encoding = 'euc-kr')) # 매매 거래


str(jeonse)
str(maemae)


colnames(maemae)

# 변수명 공백 제거
colnames(jeonse) <- gsub("\\s", "", colnames(jeonse))
colnames(maemae) <- gsub("\\s", "", colnames(maemae)) 
# 변수명에 포함되어있는 기호 제거
colnames(jeonse) <- gsub("[[:punct:]]", "", colnames(jeonse)) 
colnames(maemae) <- gsub("[[:punct:]]", "", colnames(maemae))

# 자치구명+법정동명 합친 변수(지역) 생성
maemae$지역 <- paste(maemae$자치구명, maemae$법정동명)
jeonse$지역 <- paste(jeonse$자치구명, jeonse$법정동명)

# factor형으로 변환
jeonse <- jeonse %>% mutate_at(c('자치구명', '법정동명','건물용도','지역'), as.factor)
maemae <- maemae %>% mutate_at(c('자치구명', '법정동명','건물용도','지역'), as.factor)

# 월세 거래는 관심사가 아니므로 전세만 추출
jeonse <- jeonse %>% filter(전월세구분=="전세") 

# 결측치 확인
colSums(is.na(jeonse)) 
colSums(is.na(maemae))

# 건물명 겹치는 것
# name <- intersect(charter$건물명, sell$건물명)
# name <- na.omit(name)
# name
# 
# df_jeonse <- charter %>% filter(건물명 %in% name)
# df_jeonse
# 
# df_maemae <- sell %>% filter(건물명 %in% name)
# df_maemae

################################################################################################
###### 이상치 제거 ######
# 1. 건물면적(㎡) 
descr <- describe(maemae$건물면적)
descr <- descr %>% mutate(UL = mean + 2*sd) # UL = upper limit
descr <- descr %>% mutate(LL = mean - 2*sd) # LL = lower limit
descr
#### descr 보면, max가 UL보다 큼 -> UL 넘어가는 이상치 의심 값들이 있음
table(maemae$건물면적 > descr$UL)
View(maemae[maemae$건물면적 > descr$UL,]) 
boxplot(maemae$건물면적)
#### https://data.seoul.go.kr/dataList/232/C/2/datasetView.do 사이트 보면
#### 230㎡ 초과되는 곳들도 많은 것으로 보아, 이상치로 판단하기는 어려움 


# 2. 물건금액(만원)
descr2 <- describe(maemae$물건금액만원)
descr2 <- descr2 %>% mutate(UL = mean + 2*sd)
descr2 <- descr2 %>% mutate(LL = mean - 2*sd)
descr2
table(maemae$물건금액만원 > descr2$UL)
boxplot(maemae$물건금액만원)
View(maemae[maemae$물건금액만원 > descr2$UL,]) 
View(maemae[maemae$물건금액만원 > 1000000,]) 
# 이상치보다는 실제 비싼 곳으로 보임 -> 제거하지 않을 것

###### maemae 데이터에서 자치구명,법정동명,건물용도,건물면적(㎡) 기준으로 물건금액(만원) 도출하기 : groupby -> 평균으로..?
# 1. 건물면적(m^2) binning
### 살펴보기
h = hist(maemae$건물면적, breaks=500)
h$breaks
describe(maemae$건물면적) # min = 10.79 max = 1840.51

# gpa=cut(maemae$건물면적, breaks=c(0,70,80,90,100),right=FALSE,
#         labels=c("D","C","B","A"),include.lowest=TRUE)

# maemae <- maemae %>% mutate(건물면적_구간 = cut(maemae$건물면적,
#                                 breaks=seq(10,1850,20),
#                                 labels = seq(20,1840,20))) # 10부터 1845까지 20간격으로 binning

maemae <- maemae %>% mutate(건물면적_구간 = cut(maemae$건물면적,
                            breaks=seq(0,1860,20)))

# 2. groupby
### 법정동코드 unique한지 확인
# n_distinct(maemae$법정동코드)
# maemae %>% group_by(자치구명,법정동명) %>% summarise(count = n())
# View(distinct (maemae, 자치구명, 법정동명, 법정동코드))
# 
mean_price <- maemae %>%
  group_by(지역,건물용도,건물면적_구간)%>%
  summarise(매매가평균 = mean(물건금액만원)) %>%
  arrange(-매매가평균)

mean_price

############ 시도
# 기타 : 금액 예측하는 회귀분석 적용
# str(maemae)
# str(jeonse)
# str(new)


# model <- lm(물건금액만원 ~ 자치구명 + 법정동명 + 건물용도 + 건물면적, data = maemae)
# model <- lm(물건금액만원 ~ 지역 + 건물용도 + 건물면적 + 지역*건물용도, data = maemae)
# summary(model)
# View(summary(model)$coef)

### 전세 거래에는 있는데, 매매 거래에는 없는 지역 : 얘네는 제거
# sigu <- setdiff(jeonse$지역, maemae$지역)
# jeonse_new <- jeonse %>% filter(!지역 %in% sigu)
# jeonse_new

### 전세거래 데이터에서 새로운 파생변수: 예측물건금액 생성
# pred <- jeonse_new[,c("자치구명","법정동명", "건물용도","임대면적")]
# pred <- jeonse_new[,c("지역", "건물용도","임대면적")]
# pred <- pred %>% rename ("건물면적"="임대면적")
# 
# jeonse_new$매매금액 <- predict(model, pred)
# table(jeonse_new$매매금액<0) # 음수가 나옴..ㅠ

################################################################################################
# 건물면적(m^2) 구간 나누기
hist(jeonse$임대면적, breaks=500) # 확실히 범위가 좁음.
describe(jeonse$임대면적)

jeonse <- jeonse %>%
  mutate(면적_구간 = cut(jeonse$임대면적, breaks=seq(0,280,20)))

#jeonse %>% group_by(자치구명,법정동명,건물용도,면적_구간)%>%summarise(금액평균 = mean(보증금만원)) %>% arrange(-금액평균)

# 자치구명,법정동명,건물용도,면적_구간 얘네 기준으로 join!
df <- left_join(jeonse, mean_price,
                by=c("지역", "건물용도", "면적_구간"="건물면적_구간"))
View(df)

na <- df[is.na(df$금액평균),]
View(na)

View(df %>% arrange(자치구명,법정동명,건물용도,면적_구간))

df_new <- df %>% filter(!is.na(매매가평균))

table(df_new$건물용도)

#### 한계 : 건물용도가 단독다가구일때, 겹치는 게 많지 않음..
boxplot(jeonse[jeonse$건물용도=="단독다가구",]$임대면적, maemae[maemae$건물용도=="단독다가구",]$토지면적)
######### 그냥 겹치는 거로만 대충 해보고
######### 전국 30개로 해보기..

# charter_rate(전세가율): 주택매매가격에 대비한 전세가격의 비율

df_new <- df_new %>% mutate(전세가율=보증금만원/매매가평균)

View(df_new)

########################### One-way ANOVA ########################################
table(df_new$건물용도)
table(df_new$자치구명)
######### STEP1 : 가설수립 #########
### 건물용도에 따른 전세가율 비교
df_new %>% group_by(건물용도) %>% summarise(mean(전세가율, na.rm = T))

######### 이상치 검토 및 제거 ######### 
### 이상치 검토 후 제거하여 anova_new 데이터프레임 만들기
boxplot(df_new$전세가율)
# descr <- describe(df_new$전세가율)
# descr <- descr %>% mutate(UL = mean + 2*sd)
# descr <- descr %>% mutate(LL = mean - 2*sd)
#### descr 보면, max가 UL(upper limit)을 넘음 -> 이상치 case들이 존재
#### -> UL 초과하는 이상치만 제거해주기
# table(anova1$price > descr$UL)

table(df_new$전세가율>1.5)
anova1_new <- df_new %>% filter(전세가율 <= 1.5)

### 건물용도에 따른 전세가율 비교

anova1_new %>% group_by(건물용도) %>%
  summarise(주택종류별_전세가율=mean(전세가율, na.rm = T))

### 자치구에 따른 전세가율 비교

table(anova1_new$지역)

anova1_new %>% group_by(자치구명) %>% summarise(구별_전세가율=mean(전세가율, na.rm = T)) %>%
  arrange(-구별_전세가율)
anova1_new %>% group_by(지역) %>% summarise(건수=n() ,구별_전세가율=mean(전세가율, na.rm = T)) %>% arrange(-구별_전세가율)
anova1_new %>% group_by(지역) %>% summarise(n=n() ,동별_전세가율=mean(전세가율, na.rm = T)) %>%
  filter(n>=30) %>% arrange(-동별_전세가율)

######### STEP2: 서브 데이터프레임 만들기 #########
### 집단간 데이터프레임 생성하기
anov1_단독다가구 <- anova1_new %>% filter(건물용도 == "단독다가구")
anov1_아파트 <- anova1_new %>% filter(건물용도 == "아파트")
anov1_연립다세대 <- anova1_new %>% filter(건물용도 == "연립다세대")
anov1_오피스텔 <- anova1_new %>% filter(건물용도 == "오피스텔")

######### STEP3: 정규성 검토 ######### 
# 네 개의 서브 데이터프레임에 대해 종속변수 정규성 검토
summary(anov1_단독다가구$전세가율)
summary(anov1_아파트$전세가율)
summary(anov1_연립다세대$전세가율)
summary(anov1_오피스텔$전세가율)

hist(anov1_단독다가구$전세가율, breaks = seq(0, 1.5, 0.1))
hist(anov1_아파트$전세가율, breaks = seq(0, 1.5, 0.1))
hist(anov1_연립다세대$전세가율, breaks = seq(0, 1.5, 0.1))
hist(anov1_오피스텔$전세가율, breaks = seq(0, 1.5, 0.1))

#### hist 그려보면, 서브데이터프레임 모두 전세가율 종속변수는 정규분포를 따른다고 볼 수 있나..?

shapiro.test(anov1_단독다가구$전세가율) # p-value가 유의하게 나와서 정규성 만족하지 못함.
shapiro.test(anov1_아파트$전세가율) # 샘플의 크기가 5000보다 커서 shapiro 검정을 수행하지 못함. 
shapiro.test(anov1_연립다세대$전세가율)
shapiro.test(anov1_오피스텔$전세가율)

# 설사 정규성조건 만족하지 못하더라도, 서브데이터프레임 모두 sample size가 30개 넘어가서 anova를 진행해도 된다고 판단.

######### STEP4: 등분산성 조건 검토 ######### 
# car 패키지에 있는 leveneTest 함수 사용
# ANOVA에서의 등분산성 조건은 : 집단이 여러개라서 leveneTest 사용
library(car)
# 등분산 조건 확인하는건 : 전체 데이터프레임 대상으로함 

leveneTest(전세가율~건물용도, data = anova1_new)

#### 결과: p-value가 0.05보다 작아서 등분산조건 만족하지 못함. -> Welch test 해야함.

######### STEP5: 이분산 가정 one-way ANOVA 시행(Welch test 시행) ######### 
# 등분산조건 만족하지 못해서는 내장함수인 oneway.test 함수 사용
oneway.test(전세가율~건물용도, data = anova1_new) 
#### 결과: p-value가 유의함.-> 독립변수인 건물용도를 기준으로 집단을 구분했을때, 집단 간에 종속변수 모평균 차이가 존재할 것이다.

######### STEP6: 사후분석 #########

## 사후분석 결과해석: 신용카드 expense 모평균 > 계좌이체 expense 모평균 = 간편결제 expense 모평균 ##

library(dunn.test) 
dunn.test(anova1_new$전세가율, anova1_new$건물용도, method = "bonferroni") # 앞에 종속변수, 뒤에 독립변수 써줌

########## 본페로니 행렬 해석 ########## 
# 모든 쌍의 p-value가 0으로 유의함.

### 단독다가구에서 아파트 뺐을 때 t통계량이 -46.67905 음수임. -> 아파트 전세가율의 표본평균이 단독다가구 전세가율의 표본평균보다 크다. 
### 단독다가구와 아파트 p-value=0 : alpha보다 작음 -> 유의함 -> u아파트 >  u단독다가구 

### -> 단독다가구 모평균 < 아파트 모평균 < 연립다세대 모평균 < 오피스텔 모평균

########################### Two-way ANOVA ########################################
## H0: 두 독립변수 간에 상호작용효과가 없다 ##
## Ha: 두 독립변수 간에 상호작용효과가 있다 ##
#### 기존 독립변수 건물용도와 종속변수 전세가율 간의 관계에 대해서, 새로운 독립변수인 자치구가 영향을 미치는지 

#### STEP7: two-way ANOVA 시행 및 그래프 그리기 ####
anova2_new <- anova1_new

two_anova_result <- aov(전세가율~건물용도*자치구명, data = anova2_new) ## 기존의 독립변수 건물용도, 새로운 독립변수 자치구
summary(two_anova_result)
## 건물용도:자치구명 (IV1*IV2)
#### p-value<0.05 -> IV1*IV2도 유의
#### 단, 이때 조건은: IV1에 따른 p-value가 유의해야함.(STEP5에서 확인했음) / IV1*IV2에 따른 상호작용도 유의
#### -> 두가지 조건 다 만족해서 Two-way anova 결과 대립가설 채택됨 ##
#### -> 두 독립변수 간에 상호작용효과 존재

#### 그냥 참고로, os에 따른 expense 차이 어떻게 나는지 봐보자
anova2_new %>% group_by(자치구명) %>% summarise(구별_전세가율 = mean(전세가율)) %>% arrange(-구별_전세가율)

#### 그래프 그려볼 것
library(HH)
table(anova2_new$건물용도)
## female이 먼저 출력됨. 그래프에서도 female이 먼저 나와서 해석이 쉽지 않음.
#### 그냥 놔두면 출력선수가 female -> male인데, male->female 순서로 출력순서 바꿔주겠다. 
#### 그래프 잘 그리려면, one-way anova에서 평균 작게 나온 걸 먼저 출력되게 하는 게 좋음.
#### -> male이 먼저 나오도록 출력선수 바꿔주기
# two_anova_new$gender <- factor(two_anova_new$gender, levels = c("Male", "Female")) ## One-way ANOVA에서 표본평균이 작은 순서로 집단을 먼저 출력하도록 함 ##

interaction2wt(전세가율~factor(건물용도)*factor(자치구명), data = anova2_new, type="plot")

# interaction2wt plot의 첫번째 그래프가 잘 안보여서 새롭게
library("RColorBrewer")
palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))

interaction.plot(x.factor = anova2_new$건물용도, 
                 trace.factor = anova2_new$자치구명,
                 response = anova2_new$전세가율,
                 main="주택종류와 자치구의 상호작용효과",
                 xlab="주택종류",
                 ylab="전세가율",
                 lty = 1,
                 lwd = 3,
                 col = sample(palette3_all, length(unique(anova2_new$자치구명))),
                 type="b")
#                 col = rainbow(length(unique(anova2_new$자치구명))))

## 서북권(northwest), 동북권(northeast), 서남권(southwest), 동남권(southeast) 나눠서 그래프 관찰
northwest <- c("은평구","서대문구","종로구","중구","마포구","중구","용산구")
northeast <- c("도봉구","노원구","강북구","성북구","중랑구","동대문구","성동구","광진구")
southwest <- c("강서구","양천구","구로구","영등포구","동작구","금천구","관악구")
southeast <- c("서초구","강남구","송파구","강동구")

  
#anova2_new <- anova2_new %>% mutate(권역구분 = case_when(자치구명 %in% northwest~"서북권", 자치구명 %in% northeast~"동북권", 자치구명 %in% southwest~"서남권", 자치구명 %in% southeast~"동남권"))
anova2_new$자치구명 <- as.character(anova2_new$자치구명)

northwest <- anova2_new %>% filter(자치구명 %in% northwest)
northeast <- anova2_new %>% filter(자치구명 %in% northeast)
southwest <- anova2_new %>% filter(자치구명 %in% southwest)
southeast <- anova2_new %>% filter(자치구명 %in% southeast)

interaction.plot(x.factor = southwest$건물용도, 
                 trace.factor = southwest$자치구명,
                 response = southwest$전세가율,
                 main="주택종류와 자치구(서남권)의 상호작용효과",
                 xlab="주택종류", ylab="전세가율",
                 lty = 1, lwd = 3, 
                 col = sample(palette3_all,
                              length(unique(southwest$자치구명))),
                 type="b",legend=T)
##### 결과해석 : 기울기 변화가 다름.......


interaction.plot(x.factor = northwest$건물용도, 
                 trace.factor = northwest$자치구명,
                 response = northwest$전세가율,
                 main="주택종류와 자치구의 상호작용효과",
                 xlab="주택종류",
                 ylab="전세가율",
                 lty = 1, #line type
                 lwd = 3, #line width
                 col = sample(palette3_all, length(unique(northwest$자치구명))),
                 type="b")
interaction.plot(x.factor = northeast$건물용도, 
                 trace.factor = northeast$자치구명,
                 response = northeast$전세가율,
                 main="주택종류와 자치구의 상호작용효과",
                 xlab="주택종류",
                 ylab="전세가율",
                 lty = 1, #line type
                 lwd = 3, #line width
                 col = sample(palette3_all, length(unique(northeast$자치구명))),
                 type="b")
interaction.plot(x.factor = southeast$건물용도, 
                 trace.factor = southeast$자치구명,
                 response = southeast$전세가율,
                 main="주택종류와 자치구의 상호작용효과",
                 xlab="주택종류",
                 ylab="전세가율",
                 lty = 1, #line type
                 lwd = 3, #line width
                 col = sample(palette3_all, length(unique(southeast$자치구명))),
                 type="b",
                 legend=T)

###### 
top7count <- anova2_new %>% group_by(자치구명) %>%
  summarise(mean=mean(전세가율)) %>% mutate(rank = rank(desc(mean))) %>%
  filter(rank<=7) %>% dplyr::select(자치구명) %>% deframe()
top7_df <- anova2_new %>% filter(자치구명 %in% top7count)
interaction.plot(x.factor = top7_df$건물용도, 
                 trace.factor = top7_df$자치구명,
                 response = top7_df$전세가율,
                 main="주택종류와 전세가율Top7자치구의 상호작용효과",
                 xlab="주택종류", ylab="전세가율",
                 lty = 1, lwd = 3,
                 col = sample(palette3_all, length(unique(top7_df$자치구명))),
                 type="b", legend=T)

##### 결과해석 : 전세가율 높은 지역에서는 추이가 어느정도 비슷하다고 볼 수 있을듯
###################################여기까지!!!!!!!!#############################################  

########################### Two-way ANOVA (2) ########################################
## H0: 두 독립변수 간에 상호작용효과가 없다 ##
## Ha: 두 독립변수 간에 상호작용효과가 있다 ##
#### 기존 독립변수 건물용도와 종속변수 전세가율 간의 관계에 대해서, 새로운 독립변수인 자치구가 영향을 미치는지 

#### STEP7: two-way ANOVA 시행 및 그래프 그리기 ####

str(anova2_new)
anova2_new <- anova2_new %>% mutate_at(c('자치구명', '법정동명','건물용도','지역','신규갱신여부'), as.factor)

two_anova_result <- aov(전세가율~건물용도*자치구명, data = anova2_new) ## 기존의 독립변수 건물용도, 새로운 독립변수 자치구
summary(two_anova_result)
## 건물용도:자치구명 (IV1*IV2)
#### p-value<0.05 -> IV1*IV2도 유의
#### 단, 이때 조건은: IV1에 따른 p-value가 유의해야함.(STEP5에서 확인했음) / IV1*IV2에 따른 상호작용도 유의
#### -> 두가지 조건 다 만족해서 Two-way anova 결과 대립가설 채택됨 ##
#### -> 두 독립변수 간에 상호작용효과 존재

#### 그냥 참고로, os에 따른 expense 차이 어떻게 나는지 봐보자
anova2_new %>% group_by(신규갱신여부) %>% summarise(전세가율 = mean(전세가율)) %>% arrange(-전세가율)

#### 그래프 그려볼 것
library(HH)
table(anova1_new$건물용도)
## female이 먼저 출력됨. 그래프에서도 female이 먼저 나와서 해석이 쉽지 않음.
#### 그냥 놔두면 출력선수가 female -> male인데, male->female 순서로 출력순서 바꿔주겠다. 
#### 그래프 잘 그리려면, one-way anova에서 평균 작게 나온 걸 먼저 출력되게 하는 게 좋음.
#### -> male이 먼저 나오도록 출력선수 바꿔주기
# two_anova_new$gender <- factor(two_anova_new$gender, levels = c("Male", "Female")) ## One-way ANOVA에서 표본평균이 작은 순서로 집단을 먼저 출력하도록 함 ##

interaction2wt(전세가율~factor(건물용도)*factor(자치구명), data = anova1_new, type="plot")

# interaction2wt plot의 첫번째 그래프가 잘 안보여서 새롭게
library("RColorBrewer")
palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))
interaction.plot(x.factor = anova1_new$건물용도, 
                 trace.factor = anova1_new$자치구명,
                 response = anova1_new$전세가율,
                 main="주택종류와 자치구의 상호작용효과",
                 xlab="주택종류",
                 ylab="전세가율",
                 lty = 1, #line type
                 lwd = 3, #line width
                 col = sample(palette3_all, length(unique(anova1_new$자치구명))))
#                 col = rainbow(length(unique(anova1_new$자치구명))))

############## 결과해석 ############## 
#### cf. 오른쪽 그래프 2개는 : 첫번째 독립변수를 os로, 두번째 독립변수를 gender로 해서 보여준 것.
#### 우리의 관심사는 첫번째 그래프. 첫번째 독립변수를 gender로, 두번째 독립변수를 os로. 
#### 파란색 선이든 분홍색 선이든, male에서 female로 갈때 y값이 증가함.
#### os의 종류와 무관하게 male보다는 female일때 expense의 모평균이 커진다고 할 수 있음. 
######## ★상호작용효과★는 기울기의 경사 변화로 얘기함 : iOS의 경사가 더 높음. 
#### ★ os가 무엇이든 남성보다는 여성의 expense 모평균이 더 큰데, os가 iOS면 그차이가 더 큼. 
#### ★ 이게 상호작용효과지. 원래 성별에 따라 expense 차이가 있는데, os 뭘 쓰냐에 따라 성별에 따른 차이가 더 벌어질 수 있는것. 
#### ★ 즉, 차이가 벌어졌다는 건 기존의 관계가 강화된 것.
###### STEP7로 Two-way ANOVA는 끝남 ######

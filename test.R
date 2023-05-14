library(tidymodels)
library(tidyverse)
library(pracma)

mid <- c('철', '민', '지', '현', '혜', '준', '명')
last <- c('수', '연', '혜', '진', '운', '동', '희')

names <- c()
set.seed(100) 

for(i in 1:100) {
  mid_idx <- sample(1:7, 1)
  last_idx <- sample(1:7, 1)
  
  name <- paste(mid[mid_idx], last[last_idx], sep="")
  names <- c(names, name)
}
set.seed(100)
korean = sample(60:100, 100, replace=TRUE)
english = sample(60:100, 100, replace=TRUE)
math = sample(60:100, 100, replace=TRUE)
nums = c(1001:1100)

sts <- data.frame(nums, names, korean, english, math)

colnames(sts) <- c("번호", "이름", "국어", "영어", "수학")

options(dplyr.print_max = 1000000)

## 그룹핑을 하기 위해 지역 데이터 생성
locations = c('서울', '대전', '대구', '부산', '울산', '광주', '인천')
set.seed(10)
idx <- sample(1:7, 100, replace=TRUE)
sts$지역 <- locations[idx]

sts %>%
  mutate(평균=(수학+영어+국어) / 3 ) %>%
  arrange(desc(평균)) -> sts2

sts2$공부시간 <- sort(round(abs(rnorm(100, 5, 2))), decreasing = TRUE)
sts2$수학 <- unlist(sts2 %>% arrange(desc(수학)) %>% select(수학))

sts2 %>% arrange(번호) -> sts

## 그룹핑을 하기 위해 성별 데이터 생성
genders = c('F', 'M')
set.seed(15)
idx <- sample(1:2, 100, replace=TRUE)
sts$성별 <- genders[idx]

str(sts)

seo <- read.csv('data/소상공인시장진흥공단_상가(상권)정보_서울_202212.csv')
dae <- read.csv('data/소상공인시장진흥공단_상가(상권)정보_대전_202212.csv')

apt1 <- read.csv('data/apartment_price1.csv')
apt2 <- read.csv('data/apartment_price2.csv')
apt3 <- read.csv('data/apartment_price.csv')



param <- sample(1:100000, size=100000, replace = T)
cate_uni <- c('A', 'B', 'C', 'D', 'E', 'F', 'G')

cate <- cate_uni[sample(1:7, size=100000, replace = T)]

param_df <- data.frame(name = cate, value = param)



## 모집단 -> param_df
## 표본크기 -> 30
## 표본집단 -> 15

### 모평균
param_df %>%
  summarise(평균=mean(value))

### 모표준편차
param_df %>%
  summarise(편차=sd(value) / 30)


### 분포
param_df %>%
  ggplot(aes(x=value)) +
  geom_histogram(binwidth = 1000)


### 샘플링
s1 <- param_df %>% 
  rep_sample_n(reps=10000, size=30, replace = T)

s1 %>%
  group_by(replicate) %>%
  summarise(v = mean(value)) %>%
  ggplot(aes(x=v)) +
  geom_histogram(binwidth = 2000)

### 단 하나의 표본
s2 <- param_df %>% 
  rep_sample_n(reps=10, size=20, replace = T) %>%
  group_by(replicate) %>%
  summarise(평균 = mean(value))

s2 %>%
  summarise(평균 = mean(평균))

### 부트스트래핑
s3<- s2 %>%
  rep_sample_n(reps=1000, size=20, replace = T)


### 부트스트래핑의 결과
s3 %>%
  summarise(평균 = mean(평균)) %>%
  summarise(결과 = mean(평균))

### 샘플링된 단 하나의 표본 히스토그램
s2 %>%
  ggplot(aes(x=평균)) +
  geom_histogram(binwidth = 2500)

### 부트스트랩된 표본들의 히스토그램
s3 %>%
  summarise(평균 = mean(평균)) %>%
  ggplot(aes(x=평균)) +
  geom_histogram(binwidth = 1500)


apt2 %>%
  mutate(분양가격 = as.numeric(분양가격.제곱미터.)) %>%
  filter(!is.na(분양가격)) -> apt22


## 지역이 집값에 영향을 미치지 않는다. -> 귀무가설(H0)
## 지역이 집값에 영향을 미친다. -> 대립가설(H1)


## 지역에 따른 분양가 영향
apt22 %>%
  specify(response = 분양가격, explanatory = as.factor(지역명)) %>%
  hypothesize(null = 'independence') %>%
  generate(reps=1000, type = 'permute') %>%
  calculate(stat='chisq')



# 모수의 추정은 신뢰도로(부트스트랩핑)
# 두 그룹간 통계적 차이 검정은 가설 검정(순열 검정)



# 부트스트랩과 순열검정 차이
# 부트스트랩 -> 재표짐(resampling)을 할 때, 복원 추출. 추정값의 신뢰구간 측정에 사용
# 순열검정 -> 재표짐(resampling)을 할 때, 비복원 추출. 가설 검정에서 사용
##   - 순열은 비복원추출. 재표집시 순서만 바뀐 샘플이 생성된다. 부트스트랩은 복원 추출이라서
##     동일한 데이터가 중복 생성되어 최초의 샘플 모양을 바꾸어버린다.

str(apt22)
unique(apt22$규모구분)
# 가설 검정 -> 순열 검정 ( 두 그룹간의 차이가 있다. )
## 귀무가설 : 전용면적 60제곱 미터이하와 102제곱미터 초과 사이에는 분양가격 차이가 없다
## 대립가설 : 전용면적 60제곱 미터이하와 102제곱미터 초과 사이에는 분양가격 차이가 있다
apt22 %>%
  specify(response = 분양가격, explanatory = 규모구분) %>%
  hypothesize(null = 'independence') %>%
  generate(reps=1, type="permute") # 1번 섞어서 순열 -> 4321


apt22 %>%
  filter(규모구분 %in% c('전용면적 102제곱미터초과', '전용면적 60제곱미터이하')) %>%
  specify(분양가격 ~ 규모구분) %>%
  hypothesize(null = 'independence') %>%
  generate(reps=1000, type="permute")%>%# 1000번 섞어서 순열 -> 4321 * 1000
  calculate(stat='diff in means', order = c('전용면적 102제곱미터초과', '전용면적 60제곱미터이하')) -> apt_null

apt_null$stat %>% quantile(probs = 0.95)
apt_null$stat %>% quantile(probs = 0.99)

apt_null %>%
  visualize() +
  geom_vline(xintercept = 389, color='red', lwd=1) +
  geom_vline(xintercept = 116.74, color='blue', lwd=1) +
  geom_vline(xintercept = 163.83, color='green', lwd=1)
  
## 결과 -> 원래 결과가 389는 귀무가설 분포에서 엄청 예외적으로 벗어나 있으므로, 귀무가설은 
##         기각된다.


apt22 %>%
  filter(규모구분)
  group_by(규모구분) %>%
  summarise(평균 = mean(분양가격)) %>%
  filter(규모구분 %in% c('전용면적 102제곱미터초과', '전용면적 60제곱미터이하')) -> tmp


# right, left -> 한쪽검정
# two-sided -> 양쪽검정
# p-value 얻는 방법
apt_null %>%
  get_p_value(obs_stat = 163, direction = 'right')
  

apt_null %>%
  get_p_value(obs_stat = 389, direction = 'right')

# 평균 389차이는 유의한가? 
tmp$평균[1] - tmp$평균[2]







## ==================================================================================
## 귀무가설 : 수학 고점과 수학 저점의 평균 차이는 없다
## 대립가설 : 수학 고점과 수학 저점의 평균 차이는 있다

## 두 그룹 수학 고점/ 수학 저점 나눔. 85 이상 고점, 85 미만 고점X

sts %>%
  mutate(
    수학특기여부 = case_when(
      수학 >= 85 ~ 1,
      TRUE ~ 0
    )
  ) -> sts2


sts2 %>%
  group_by(수학특기여부) %>%
  summarise(결과 = mean(평균)) -> tmp

tmp$결과[2] - tmp$결과[1] 

sts2 %>%
  mutate(수학특기여부 = as.factor(수학특기여부)) %>%
  specify(평균 ~ 수학특기여부) %>%
  hypothesize(null = 'independence') %>%
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 'diff in means', order=c(1, 0)) -> sts_null


sts2 %>%
  mutate(수학특기여부 = as.factor(수학특기여부)) %>%
  specify(평균 ~ 수학특기여부) %>%
  hypothesize(null = 'independence') %>%
  calculate(stat = 'diff in means', order=c(1, 0))


sts_null %>%
  get_p_value(obs_stat = 2.7, direction = 'two-sided')

sts_null %>%
  visualize() +
  geom_vline(xintercept = 2.7, color='red', size=1)


## 수학 고점이 국어 고점이다.
## 귀무가설 : 수학 고점의 국어 평균과 수학 저점의 국어 평균은 차이가 없다.
## 대립가설 : 수학 고점의 국어 평균과 수학 저점의 국어 평균은 차이가 있다.

## 13.2포인트인데.. 수학 고점이 국어도 잘맞는다? 장담 못함 더 해보자
sts2 %>%
  mutate(수학특기여부 = as.factor(수학특기여부)) %>%
  specify(국어 ~ 수학특기여부) %>%
  calculate(stat = 'diff in means', order=c(1,0))


sts2 %>%
  mutate(수학특기여부 = as.factor(수학특기여부)) %>%
  specify(국어 ~ 수학특기여부) %>%
  hypothesize(null = 'independence') %>% 
  generate(reps = 1000, type = 'permute') %>%
  calculate(stat = 'diff in means', order=c(1,0)) -> sts_null2


sts_null2 %>%
  visualize()

## 남학생이 여학생보다 공부를 잘한다.
## 귀무가설 : 남학생과 여학생의 평균점수 차이는 없다
## 대립가설 : 남학생과 여학생의 평균점수보다 높다

sts %>%
  specify(평균 ~ 성별) %>%
  calculate(stat = 'diff in means', order=c('M', 'F'))


sts %>%
  specify(평균 ~ 성별) %>%
  hypothesize(null = 'independence') %>%
  generate(reps=1000, type = 'permute') %>%
  calculate(stat = 'diff in means', order=c('M', 'F')) -> sts_mf_null

sts_mf_null %>%
  get_p_value(obs_stat = -3.155, direction = 'left')

sts_mf_null %>%
  visualize() +
  geom_vline(xintercept = 3.155, color='green', size=1) +
  geom_vline(xintercept = -2.834, color='red', size=1) +
  geom_vline(xintercept = 2.932, color='blue', size=1)

## 채택역에 들어와 있으므로 귀무가설 채택!




# 가설 검정 -> t 검정 (t분포를 이용해 귀무가설 검정)
## t 분포
## t-value의 의미
## 두 대상이 평균적으로 얼마나 차이가 나는가를 표현한 정도
## t-value 값이 클수록 두 대상의 평균의 차이는 크다고 할 수 있다.
## t 공식


## 독립표본 t 검정

# 가설 검정 -> 순열 검정 ( 두 그룹간의 차이가 있다. )
## 귀무가설 : 전용면적 60제곱 미터이하와 102제곱미터 초과 사이에는 분양가격 차이가 없다
## 대립가설 : 전용면적 60제곱 미터이하와 102제곱미터 초과 사이에는 분양가격 차이가 있다
apt22 %>%
  t_test(
    formula = 분양가격 ~ 규모구분,
    order = c('전용면적 102제곱미터초과', '전용면적 60제곱미터이하'),
    alternative = 'less',
    var.equal=T
  )



apt22 %>%
  filter(규모구분 %in% c('전용면적 102제곱미터초과', '전용면적 60제곱미터이하')) %>%
  specify(분양가격 ~ 규모구분) %>%
  hypothesize(null = 'independence') %>%
  calculate(stat='t', order = c('전용면적 102제곱미터초과', '전용면적 60제곱미터이하')) -> apt22_t

apt22_t

apt22_t %>%
  visualize(method = 'theoretical') +
  geom_vline(xintercept = 1.96, color='red') +
  geom_vline(xintercept = -1.96, color='blue') +
  geom_vline(xintercept = 5.71, color='green')


sts %>%
  specify(평균 ~ 성별) %>%
  hypothesize(null = 'independence') %>%
  calculate(stat= 't', order = c('M', 'F')) -> sts_t

sts_t %>%
  visualize(method = 'theoretical') +
  geom_vline(xintercept = 1.96, color='red') +
  geom_vline(xintercept = -1.96, color='blue') +
  geom_vline(xintercept = -0.106, color='green')


## 대응 표본 t 검정
str(apt2)

# 가설 검정 -> 카이검정
##
## 가설 검정 -> ANOVA 검정

### 귀무가설 전용이 크면 따라 분양가격의 차이가 안난다
### 대립가설 전용면적에 따라 분양가격의 차이가 난다 




str(sts)

sts %>%
  t_test(
    formula = 평균~성별, order = c('M', 'F'), alternative = 'less'
  )


# statistic : 검정 통계량. t값이라고도 한다.
# t_df : 자유도. 보통 표본의 개수 - 1
# p_value : 해당 분포에서의 유의 확률
# alternative : 한쪽, 양쪽 검정
# estimate : 추정치
# lower_ci : 최소 신뢰 구간
# upper_ci : 최대 신뢰 구간


## t 검정
sts %>%
  specify(평균~성별) %>%
  hypothesize(null = 'independence') %>%  
  calculate(stat='t', order=c('M', 'F')) %>%
  visualize(method = 'theoretical') +
  stat_function(fun = dnorm, color='red')


sts %>%
  specify(response = 평균) %>%
  t_test(null_value = 80, alternative = "two_sided") %>%
  calculate(stat = "z")
## 카이 제곱 검정






## 수학 점수는 성적에 어떤 영향을 미칠까?

sts %>%
  ggplot(aes(x=수학, y=평균)) +
  geom_point()  












kbo <- read.csv('data/2020_kbo_team_batting.csv')

kbo


set.seed(1234)

## 편차 * 편차 == 편차 제곱
## 편차를 표준편차로 나누는 것 => 표준화
## 편차 * 편차
kbo %>%
  mutate(
    runs_z_score = (runs - mean(runs)) / sd(runs),
    avg_z_score = (avg - mean(runs)) / sd(avg),
    rectangle_size = runs_z_score * avg_z_score
  ) %>%
  summarise(size_sum = sum(rectangle_size) / 9)


cor.test(sts$수학, sts$평균)

# 평균 = 0.982 * 수학

# sts$평균  ==  sd(sts$평균) / sd(sts$수학) * (sts$수학 - mean(sts$수학)) + mean(sts$평균)


sd(sts$평균) / sd(sts$수학) * (sts$수학 - mean(sts$수학)) + mean(sts$평균)


a <- sd(sts$평균) / sd(sts$수학)
b <- a * mean(sts$수학)
c <- mean(sts$평균)


# 회귀식
a * sts$수학 - b + c

a

c - b

my_fun <- function(x) {
  return ((0.5742 * x) + 33.3952)
}

sts %>%
  ggplot(aes(x=수학, y=평균)) +
  geom_point() +
  stat_function(fun=my_fun) +
  geom_smooth(method = 'lm')





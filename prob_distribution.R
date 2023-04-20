# 확률과 확률 분포

## 확률 = 원하는 경우의 수 / 모든 경우의 수

## 45개 중 6개 뽑는 경우의 수(조합)
choose(45, 6)

## 확률
## 주사위 하나를 던져서 짝수가 나올 확률
## 주사위 나올 수 있는 경우의 수 6, 짝수인 경우의 수 3 => 3/6 => 1/2


## 생일 역설
### 사람 n명이 모였을 때 생일이 같을 확률 -> 간의 직감과 수학적 사실이 일치하지 않아 모순으로 인식되기도 하기에 역설이라 불림


### 사람이 한명 있을 때, 생일이 같지 않을 확률
#### 전체 생일 365개중 한개를 선택
365/365 # 1


### 사람이 두명 있을 때, 생일이 같지 않을 확률
#### 전체 생일 365개중 한개를 선택하고 남은 사람은 그 숫자를 피해서 골라야함
365/365 * 364/365 


### 사람이 세명 있을 때, 생일이 같지 않을 확률
#### 전체 생일 365개중 한개를 선택. 위처럼 반복
365/365 * 364/365 * 363/365

#### 사람 세명이 있을 때, 생일이 같을 확률
1 - (365/365 * 364/365 * 363/365)

library(tidymodels)
library(ggplot2)

set.seed(1234)


### rep_sample_n(행을 무작위로 골라내는 함수. 데이터프레임을 넘겨야 함)
df <- data.frame(날짜=1:365)
rep_sample_n(df, 28)

### duplicated(벡터의 값중 중복된 것이 있는지 찾는 함수)
duplicated(c(1,2,3,1,2,3))
sum(duplicated(c(1,2,3,1,2,3)))



### 365개중 28개 뽑는 행위 100번 시행, 

df %>%
  rep_sample_n(reps=100, size=28, replace = T) %>%
  rename(회차=replicate) %>%
  group_by(회차) %>%
  summarise(중복개수 = 날짜 %>% duplicated() %>% sum()) %>%
  filter(중복개수 > 0) %>%
  count()


df %>%
  rep_sample_n(reps=100, size=4, replace=T)

### 50%가 넘어가는 시점??
df2 <- data.frame(사람수 = 2 : 365)

df2 %>%
  rowwise() %>%
  mutate(
    결과 = tibble(날짜 = 1:365) %>%
      rep_sample_n(reps=100, size=사람수, replace = T)
  ) %>%
  group_by(replicate) %>%
  summarise(중복개수 = 날짜 %>% duplicated() %>% any())


df %>%
  rep_sample_n(reps=100, size=2, replace = T)

df2 %>%
  rowwise() %>%
  mutate(
    결과=unlist(data.frame(날짜 = 1:365) %>%
      rep_sample_n(reps=10000, size=사람수, replace = T) %>% group_by(replicate) %>%
      summarise(중복여부 = 날짜 %>% duplicated() %>% any()) %>%
      summarise(결과 = mean(중복여부)))
  ) -> rst


rst %>%
  filter(결과 > 0.5)


## 확률 분포 line
rst %>%
  ggplot(aes(x=사람수, y=결과)) +
  geom_line()





## 이항분포
### 확률변수 X

### 동전 예제
#### 동전을 2번 던져서 앞면이 나오는 횟수를 X라고 하면

##### 앞앞, 앞뒤, 뒤앞, 뒤뒤 
##### 1/4 , 1/4 , 1/4 , 1/4

##### X | 0  | 1  | 2  | 3  |
##### P | 1/4| 1/2| 1/2| 1/4 |


##### sample -> 벡터에서 원소를 무작위로 뽑는 함수
c(1,2,3) %>%
  sample(size = 1)

##### crossing -> 카르테시언곱. 모든 좌표의 경우의 수를 구해준다.
crossing(
    x = c(1,2),
    y = c('a', 'b')
)

crossing(
  실험회차 = c(1:10),
  시행회차 = c(1,2)
) %>%
  rowwise() %>%
  mutate(
    앞뒤 = sample(c(1,0),1)
  )


#### 동전을 100번 던져서 앞면이 나오는 횟수를 X라고 하면

##### X | 0  | 1  | 2  | 3  | .... | 100 |
##### P | p1 | p2 | p3 | p4 | .... | p101|

##### 100 C 0 * (1/2)^0 * (1/2)^100 / 100 C 1 * (1/2)^1 * (1/2)^99

crossing(
  실험회차 = c(1:30000),
  시행회차 = c(1:100)
) %>%
  rowwise() %>%
  mutate(
    앞뒤 = sample(c(1,0),1)
  ) %>% 
  ungroup() -> rst


crossing(
  실험회차 = c(1:100),
  시행회차 = c(1:100)
) %>%
  rowwise() %>%
  mutate(
    앞뒤 = sample(c(1,0),1)
  ) %>% 
  ungroup() -> rst2


rst %>%
group_by(실험회차) %>%
summarise(확률 = mean(앞뒤))

rst %>% 
  group_by(실험회차) %>%
  summarise(앞 = sum(앞뒤)) %>%
  ggplot(aes(x=앞)) +
  geom_histogram(binwidth = 1, color='black', fill='gray')

### 주사위 예제
#### 주사위를 100번 던져서 6의 약수가 나오는 횟수를 X라고 하면
##### X | 0  | 1  | 2  | 3  | .... | 100 |
##### P | p1 | p2 | p3 | p4 | .... | p101|
crossing(
  실험회차 = c(1:30000),
  시행회차 = c(1:100)
) %>%
  rowwise() %>%
  mutate(
    주사위눈 = sample(c(1,2,3,4,5,6),1)
  ) %>% 
  ungroup() -> rst3

rst %>%
  filter( 주사위눈 %in% c(1,2,3,6)) %>%
  group_by(실험회차) %>%
  summarise(나온횟수 = n()) %>%
  ggplot(aes(x=나온횟수)) +
  geom_histogram(binwidth = 1)
# 6의 약수 = 1,2,3,6


# dbinom 함수 -> 이항분포를 만들어주는 함수
## size = 100번 시행
## x = 50번 나올 확률
## prob = 독립시행 확률 
dbinom(x=50, size=100, prob=0.5)

rst2 %>%
  group_by(실험회차) %>%
  filter(앞뒤==1) %>%
  summarise(앞 = n()) %>%
  filter(앞 == 50) %>%
  count()

8 / 100



# pbinom 함수 -> 누적확률분포
## 0 ~ 50 사이의 확률
pbinom(q = 50, size=100, prob=0.5)

## 51 ~ 100 사이의 확률
1 - pbinom(q = 50, size=100, prob=0.5)

## 40 ~ 60 사이의 확률
pbinom(q = 60, size=100, prob=0.5) - pbinom(q = 40, size=100, prob=0.5)


# 이항분포에서 시행횟수가 충분히 많으면 정규분포를 따른다.

## 동전 이항분포 히스토그램 + 정규분포 비교
rst %>% 
  group_by(실험회차) %>%
  summarise(앞 = sum(앞뒤)) %>%
  ggplot(aes(x=앞)) +
  geom_histogram(binwidth = 1, color='black', fill='gray')
  #stat_function(fun=dnorm, args = list(mean=50, sd=4.97), color='black', size=1.5)

rst %>%
  group_by(실험회차) %>%
  summarise(앞 = sum(앞뒤)) %>%
  ggplot(aes(x=앞)) +
  geom_density() +
  stat_function(fun=dnorm, args = list(mean=50, sd=4.97), color='black', size=1.5, alpha=0.3)

rst %>% 
  group_by(실험회차) %>%
  summarise(앞 = sum(앞뒤)) %>%
  summarise(평균=mean(앞), 표준편차=sd(앞))


dfdf <- data.frame(x=c(-5, 5))
  

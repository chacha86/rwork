library(ggplot2)
library(tidymodels)


## 표본집단과 모집단
## 모집단 -> 전체
## 표본을 뽑아 모집단의 특징을 추정
## 표본집단 -> 모집단에서 뽑힌 부분집합
## 표본의 크기 -> 한 표본집단에 뽑히는 데이터의 개수
## 전체 대상에서 30명을 뽑는 행위를 10번 -> 표본 10개, 표본의 크기 30
## 표본을 뽑을 때 주의 -> 임의 추출(랜덤, 무작위), 복원추출 (sampling)



## 샘플링 -> 모집단을 전수 조사해서 통계를 내는 것은 사실상 거의 불가능. 모집단의 특성을
## 보여주는 일부의 샘플 데이터만 뽑아서 모집단을 추정하고자 함.

apt <- read.csv('data/apartment_price2.csv')
store_popul <- read.csv('data/소상공인시장진흥공단_상가(상권)정보_서울_202212.csv')

# 모집단의 평균 및 비율
str(apt)
apt %>%
  mutate(분양가격 = as.numeric(분양가격.제곱미터. )) %>%
  filter(!is.na(분양가격)) -> apt_popul

# 아파트 모집단 가격 평균 -> 3604.662
apt_popul %>%
  summarise(mean(분양가격))


# 크기 30인 샘플링 1000개
apt_popul %>%
  rep_sample_n(reps=1000, size=30, replace=T) %>%
  group_by(replicate) %>%
  summarise(평균가격=mean(분양가격)) %>%
  summarise(표본평균 = mean(평균가격))


# 서울 상가에서 모집단의 강남구의 비율 -> 0.098
len = dim(store_popul)[1]

store_popul %>%
  filter(시군구명=='강남구') %>%
  summarise(강남구비율 = n() / store_popul %>% nrow())

## 샘플링으로 모비율 추정
store_popul %>%
  rep_sample_n(reps=1000, size=30, replace=T) %>%
  filter(시군구명=='강남구') %>%
  group_by(replicate) %>%
  summarise(강남구비율 = n() / 30) %>%
  summarise(표본비율 = mean(강남구비율))



## 이게 가능한 이유?
## 중심극한정리
apt_popul %>%
  rep_sample_n(reps=1000, size=30, replace=T) %>%
  group_by(replicate) %>%
  summarise(평균가격 = mean(분양가격)) %>%
  ggplot(aes(x=평균가격)) +
  geom_histogram(binwidth = 100)


store_popul %>%
  rep_sample_n(reps=1000, size=100, replace=T) %>%
  group_by(replicate, 시군구명) %>%
  summarise(비율 = n() / 100) %>%
  filter(시군구명=='강남구') %>%
  group_by(replicate) %>%
  summarise(표본비율=mean(비율)) %>%
  ggplot(aes(x=표본비율)) +
  geom_histogram()
  



## 부트스트래핑
## 변하지 않는 모집단으로 부터 샘플링하는 것은 현실적으로 어려움. 이미 가지고 있는 샘플링에서
## 리샘플링을 이용해 표본을 늘리는 방법


## 모집단에서 일부 샘플링 -> 300개 밖에 없음.
apt_popul %>%
  rep_sample_n(size = 300, replace = TRUE) -> apt_sample

apt_sample %>%
  rep_sample_n(reps=1000, size = 300, replace = TRUE) -> apt_boot

apt_boot %>%
  group_by(replicate) %>%
  summarise(분양가격평균 = mean(분양가격)) %>%
  ggplot(aes(x=분양가격평균)) +
  geom_histogram(bins=15)


##
store_popul %>%
  rep_sample_n(size = 300, replace = TRUE) -> store_sample

store_sample %>%
  rep_sample_n(reps=3000, size = 300, replace = TRUE) -> store_boot

store_boot %>%
  filter(시군구명=='강남구') %>%
  group_by(replicate) %>%
  summarise(강남구비율 = n() / store_boot %>% nrow() )%>%
  ggplot(aes(x=강남구비율)) +
  geom_histogram(bins=15)


## 신뢰구간
## 일반적으로 정규분포를 따른다고 가정할 때 사용할 수 있음.

## 정규분포 만들고 그리기
data1 = rnorm(n = 10000, mean = 0, sd = 1)
df1 <- data.frame(x=data1)

data2 = rnorm(n = 10000, mean = 0, sd = 3)
df2 <- data.frame(x=data2)

data3 = rnorm(n = 10000, mean = 3, sd = 3)
df3 <- data.frame(x=data3)


df1 %>%
  ggplot(aes(x=x)) +
  geom_density(color='darkgreen') +
  xlim(c(-10, 10)) +
  ylim(c(0, 0.4)) +
  geom_density(data=df2, aes(x=x), color='red') +
  geom_density(data=df3, aes(x=x), color='blue')

df4 <- data.frame(x=c(-10, 10))

## theoretical
df4 %>%
  ggplot(aes(x=x)) +
  stat_function(fun=dnorm, args = list(mean=0, sd=1), color='darkgreen') +
  stat_function(fun=dnorm, args = list(mean=0, sd=3), color='red') +
  stat_function(fun=dnorm, args = list(mean=3, sd=3), color='blue')

df5 <- data.frame(x=c(-4, 4))
df5 %>%
  ggplot(aes(x=x)) +
  stat_function(fun=dnorm, args = list(mean=0, sd=1), color='darkgreen') 

#
pnorm(-1.96) # 0.025
pnorm(1.96) # 0.975

# 95 %
df5 %>%
  ggplot(aes(x=x)) +
  stat_function(fun=dnorm, args = list(mean=0, sd=1), color='darkgreen') +
  stat_function(fun=dnorm, args = list(mean=0, sd=1), color='darkgreen', geom = "area", fill = "skyblue", xlim = c(-1.96, 1.96)) +
  geom_vline(xintercept = -1.96, color='red') +
  geom_vline(xintercept = 1.96, color='red')


# 곧 데이터의 분포가 -1.96 ~ 1.96 사이에 95%가 몰려 있기 때문에 신뢰구간을
# -1.96 ~ 1.96이라고 하고 신뢰도를 95%라고 한다. 

## 이걸 어따 써먹냐? 바로 표본 평균의 신뢰도를 구할 때 사용한다.

## 중심극한정리에 의해 크기 30이상인 표본은 정규분포를 따르게 된다.(30보다 적으면 t분포일 수도 있다.)
## 따라서 95%의 신뢰구간을 정해 모평균을 특정 신뢰도에 맞게 추정할 수 있다.

## 부트스트랩한 아파트 평균가격 모평균을 95%의 신뢰도로 추정해보자

apt_boot %>%
  group_by(replicate) %>%
  summarise(평균=mean(분양가격)) -> apt_boot_means


apt_boot_means %>%
  ggplot(aes(x=평균)) +
  geom_density() +
  geom_vline(
    xintercept = unlist(
      apt_boot_means %>% 
        summarise(
          q025 = quantile(평균, 0.025), 
          q975 = quantile(평균, 0.975)
        )
      ), color='red', size=2
    )

## 100번 샘플링하면 95번은 빨간선 안쪽에 들어온다는 것. 그말은 평균이 3400 ~ 3700 사이다
## 라고 얘기하면 95% 확률로 맞춘다는 뜻.


## 검정
## z 검정 or t 검정
## 카이제곱 검정
## ANOVA 검정



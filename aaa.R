setwd(getwd())

library(dplyr)
library(gt)
library(reshape2)
library(ggplot2)

# 파일 로드
df1 <- read.csv("data/store_daejoen_202209.csv")
df2 <- read.csv("data/store_seoul_202209.csv")

df <- rbind(df1, df2)



# 미리보기
## head
head(df) %>% gt()

## tail
tail(df) %>% gt()


# 데이터 프레임 모양
dim(df)

# 인덱스 정보 보기
rownames(df)

# 컬럼 정보 보기
colnames(df)

# str
str(df)

# summary
summary(df)


#  결측치 확인
## 전체 결측치
is.na(df)
sum(is.na(df))

## 각 컬럼의 결측치 확인하기
aa <- function(x) {
  return (sum(is.na(x)))
}
apply(as.matrix(df), 2, aa)

n <- colSums(is.na(df))

ndf <- data.frame(column=names(n), na=n)

rownames(ndf) <- NULL

ndf
## 결측치 시각화 (막대그래프 버티컬)
ggplot(ndf, aes(x=column, y=na)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 70))

## 결측치 시각화 (막대그래프 호라이즌)
ggplot(ndf, aes(x=column, y=na)) +
  geom_bar(stat='identity') +
  coord_flip()

## 결측치 시각화 (정렬해서)
ggplot(ndf, aes(x=reorder(column, na), y=na)) +
  geom_bar(stat='identity') +
  coord_flip()

# 결측치가 많은 것은 제거하기
## 호정보, 건물부번지, 지번부번지 제거
del_target <- ndf %>%
  arrange(desc(na)) %>%
  slice_head(n=3) %>% 
  select(column)

typeof(unlist(del_target))

df3<-df %>% select(-unlist(del_target))

## 결측치 수 확인
sum(is.na(df3))
dim(df3)
str(df3)

head(df3) %>% gt()


df3 %>% slice_head(n=5) %>% gt()

## 코드 관련 컬럼 제거
del_target2 <- colnames(df3)[(grepl("코드", colnames(df3))) | (grepl("번호", colnames(df3)))]

df4 <- df3 %>%
  select(-del_target2)


# 지워진거 확인
dim(df4)


colnames(df4)

# 상호명만 가져오기
head(df4) %>% gt()
View(df4)
df4 %>% select(상호명) %>% slice_head(n=5)

# 상호명, 도로명주소 가져오기
df4 %>% select(상호명, 도로명주소) %>% slice_head(n=5) %>% gt()

# 1번째 행 가져오기
df4 %>% slice(1) %>% gt()

# 1,2,3번째 행 가져오기
df4 %>% slice(1,2,3) %>% gt()
df4 %>% slice(1:3) %>% gt()

# 0번째 행의 "상호명"만 가져오기
df4 %>% slice(1,2,3) %>% select(상호명) %>% gt()

# 기술 통계

# summary로 요약
?summary(df4)

## 단변량 분석

## 다변량 분석

## 위도 경도만 가져와 요약해보기
summary(df4 %>% select(위도, 경도))

## 개별 기술 통계
### 평균
df4 %>% 
  #select(위도) %>%
  summarise(mean(위도))
### 분산
df4 %>% 
  summarise(var(위도))

### 표준편차
df4 %>% 
  summarise(sd(위도))

### 표준편차
### 사분위
#### 1사분위(25%)
df4 %>%
  summarise('1st' = round(quantile(위도, probs = 0.25), 2))

#### 2사분위(50%)
df4 %>%
  summarise('2st' = round(quantile(위도, probs = 0.5), 2))

### 중앙값
df4 %>%
  summarise(round(median(위도), 2))

### 최대/최속, 최대위치/최소위치
df4 %>% 
  summarise(which.max(위도), which.min(위도), min(위도), max(위도))


# 단변량 수치형 변수 시각화
## 위도를 displot으로 그리기 - 서울과 대전 두개의 정규분포가 그려짐
ggplot(df4, aes(x=위도)) +
  geom_histogram(aes(y=..density..), binwidth = 0.005, color="black", alpha=.5) +
  geom_density(fill="red", alpha=.2) +
  geom_vline(aes(xintercept=mean(위도)), 
             color="blue", linetype=2, size=1) +
  geom_vline(aes(xintercept=median(위도)), 
           color="red", linetype=3, size=1) 
  
df4 %>% slice_head(n=5) %>% gt()



ll <- df4 %>% 
  group_by(시도명) %>%
  summarise(m=mean(위도)) 

ll


##
ggplot(df4, aes(x=위도, fill=시도명)) +
  geom_histogram(aes(y=..density..), binwidth = 0.005, color="black", alpha=.5) +
  geom_density(fill="red", alpha=.2) +
  geom_vline(aes(xintercept=unlist(ll %>% filter(시도명=="서울특별시") %>% select("m")))) + 
  geom_vline(aes(xintercept=unlist(ll %>% filter(시도명=="대전광역시") %>% select("m"))))


# 상관관계
## 이변량 변수 분석
## 피어슨 상관 계수 -> X와 Y과 각각 변하는 정도 / X와 Y가 함께 변하는 정도
### 강한 양적 관계, 음적 관계, 무관계

## 각 변수의 상관관계 구하기
#cor(df4)
df6 <- df4 %>% select(지번본번지, 건물본번지, 경도, 위도)
View(cor(df6))


ggplot(df6, aes(x=c(지번본번지, 건물본번지, 경도, 위도))) + 
  geom_tile(color = "black")

## 각 변수별 상관관계 시각화를 위한
library(ggcorrplot)

ggcorrplot(cor(df6), lab=T) # 값 추가
ggcorrplot(cor(df6), lab=T, colors=c("white", "light blue", "dark blue"),) # 색상 조절

## 산점도로 경도와 위도 표현
ggplot(df6, aes(x=경도, y=위도)) +
  geom_point()


df7 <- df4 %>% slice(sample(1:dim(df4)[1], 1000))

## 회귀선 그리기
ggplot(df7, aes(x=경도, y=위도)) +
  geom_point(color="blue") +
  geom_smooth(method = "lm", color="red")

df7 %>% slice_head(n=5)
## 서울 대전 각각 회귀선 그리기
ggplot(df7, aes(x=경도, y=위도, color=시도명)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(vars(시도명), scales="free")
setwd(getwd())
library(gt)
library(dplyr)
library(ggplot2)
# 서브셋
head(df7) %>% gt()
dim(df7)

## 강남구의 상권업종대분류가 음식인 것만 보기
df7 %>% 
  filter(시군구명 == "강남구", 상권업종대분류명=="음식") %>%
  gt()
  
## 상권업종중분류명 별로 빈도수 구하기
df4 %>%
  filter(시도명=="서울특별시", 시군구명=="강남구") %>%
  group_by(상권업종중분류명) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  gt()

## 상권업종중분류명과 상점수로 시군구별 bar plot 그리기
df4 %>% 
  filter(상권업종대분류명=="음식") %>%
  group_by(상권업종중분류명, 시군구명) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(상권업종중분류명, n), y=n, fill=시군구명)) +
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() +
  theme(
    legend.position = "none"
  )

# 강남구의 상권업종중분류명으로 그래프 그리기 
df4 %>% 
  filter(상권업종대분류명=="음식", 시군구명 == "강남구") %>%
  group_by(상권업종중분류명) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(상권업종중분류명, n), y=n, fill=상권업종중분류명)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(
    legend.position = "none"
  )

## 서브 플롯으로 시군구별 음식점 분류 수
df4 %>% 
  filter(상권업종대분류명=="음식") %>%
  group_by(상권업종중분류명, 시군구명) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(상권업종중분류명, n), y=n, fill=시군구명)) +
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() +
  theme(
    legend.position = "none"
  ) +
  facet_wrap(vars(시군구명), ncol=8, scales="free")

unique(df4$상권업종소분류명)
grepl("학원", unique(df4$상권업종중분류명))
unique(df4$상권업종대분류명)

## 구별 학원수 비교
df4 %>%
  filter(시도명=="서울특별시", 상권업종대분류명 == "학문/교육") %>%
  group_by(시군구명) %>%
  summarise(n=n()) %>%
  gt()

## 학원의 분류명 알아내기 (학문/교육)
df4 %>%
  slice(grep("학원", df4$상권업종소분류명)) %>%
  select(상권업종대분류명, 상권업종중분류명, 상권업종소분류명) %>%
  group_by(상권업종대분류명) %>%
  summarise(n=n())

## 시군구명으로 빈도수 구하기
df4 %>%
  filter(시도명=="서울특별시", 상권업종대분류명=="학문/교육") %>%
  group_by(시군구명) %>%
  summarise(n=n())

## 상권업종소분류명으로 빈도수 구하고 위에서 30개만 보기
df4 %>%
  filter(시도명=="서울특별시", 상권업종대분류명=="학문/교육") %>%
  group_by(상권업종소분류명) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  slice_head(n=30) %>%
  gt()

## 위경도로 서울 시군구 표현하기
df4 %>%
  filter(시도명=="서울특별시") %>%
  select(위도, 경도, 시군구명) %>%
  ggplot(aes(x=경도, y=위도, color=시군구명)) +
  geom_point(size=2)

## 위경도로 시군구명을 다르게 하여 어린이집
  
df4 %>%
  filter(시도명=="서울특별시", 상권업종소분류명 == "어린이집") %>%
  select(위도, 경도, 시군구명) %>%
  ggplot(aes(x=경도, y=위도, color=시군구명)) +
  geom_point(size=2)

unique(df4$상권업종소분류명)

df4$상권업종대분류명 == ""

## 위경도로 시군구명을 다르게 하여 입시학원 
df4 %>%
  filter(시도명=="서울특별시", 상권업종소분류명 == "학원-입시") %>%
  select(위도, 경도, 시군구명,상권업종소분류명) %>%
  ggplot(aes(x=경도, y=위도, color=시군구명)) +
  geom_point(size=2)
  #geom_point(size=2) +
  #geom_point(aes(x=경도, y=위도, color=상권업종소분류명), size=2)


bb <- df4 %>%
  filter(시도명=="서울특별시", 상권업종소분류명 == "어린이집" | 상권업종소분류명 == "학원-입시")%>%
  select(위도, 경도, 시군구명, 상권업종소분류명)
  #ggplot(aes(x=경도, y=위도, color=상권업종소분류명)) +
  #geom_point(size=2, alpha=0.6)
  
cc <- df4 %>%
  filter(시도명=="대전광역시", 상권업종소분류명 == "어린이집" | 상권업종소분류명 == "학원-입시")%>%
  select(위도, 경도, 시군구명, 상권업종소분류명)

install.packages("ggmap")

library(ggmap)
register_google(key = "AIzaSyCJ3Ols1YNstVVdqCj_0eiZ3xsOBBXAeSs")
register_google(key = "AIzaSyDgurXTvy5_TcmKosGUgPlNhLywecHUDVw")

aa<- df4 %>% 
  filter(시도명=="서울특별시")
soeul <- ggmap::get_map(c(mean(aa$경도), mean(aa$위도)), zoom=11, maptype = "roadmap")
gm <- ggmap::ggmap(soeul)

daejeon <- ggmap::get_map(c(mean(cc$경도), mean(cc$위도)), zoom=12, maptype = "roadmap")
gm2 <- ggmap(daejeon)
gm +
  geom_point(data=bb, aes(x=경도, y=위도, color=상권업종소분류명), size=3, alpha=0.6) 

gm2 +
  geom_point(data=cc, aes(x=경도, y=위도, color=상권업종소분류명), size=3, alpha=0.6) 
  
daejeon2 <- ggmap::get_map(c(127.38, 36.35), zoom=13, maptype="roadmap")

ggmap(daejeon2) +
  geom_point(data=cc, aes(x=경도, y=위도, color=상권업종소분류명), size=5, alpha=0.6) 


unique(df4$상권업종소분류명)
unique(df4$상권업종중분류명)
unique(df4$상권업종대분류명)

geom_point(aes(x=)

dd <- df4 %>% filter(시도명=="대전광역시", 상권업종중분류명 == "캠프/별장/펜션")


daejeon3 <- ggmap::get_map(c(127.38, 36.35), zoom=11, maptype="roadmap")
ggmap(daejeon3) +
  geom_point(data=, aes(x=경도, y=위도, color=상권업종소분류명), size=5, alpha=0.6) 

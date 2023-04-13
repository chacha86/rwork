# 데이터 불러오기
df1 <- read.csv("data/store_seoul_202209.csv")
df2 <- read.csv("data/store_daejoen_202209.csv")

head(df1)
tail(df1)

dim(df1)

str(df1)

# 결측치 보기
## sum은 갯수
## mean은 비율
colMeans(is.na(df1))

library(dplyr)
#파일로 저장
df2 <- df1 %>% select(위도, 경도)

write.csv(df2, "data/test.csv")

# 문자열의 소문자로 변경하는 메소드 사용
df1 <- df1 %>%
  mutate(소문자 = tolower(상호명))


df2<-df1 %>% slice(grep("(베|배)스킨", 소문자)) %>%
  select(상호명)

# 상호명 소문자로 가져오기

# 상호명 던킨도너츠
df3 <- df1 %>% slice(grep("던킨|dunkin", 소문자)) %>%
  select(상호명)

# 상호명_소문자 컬럼으로 배스킨|베스킨|던킨|dunkin을 가져와 변수에 담기
df4 <- df1 %>% slice(grep("배스킨|베스킨|baskin|던킨|dunkin", 소문자))

# 브랜드명을 만들어주세요.
dim(df4)
df5<-df4 %>%
  filter(grepl("배스킨|베스킨|baskin", 상호명)) %>%
  mutate(브랜드명 = "배스킨라빈스")

df4$브랜드명 <- NA

head(df4)



df4$브랜드명[grep("배스킨|베스킨|baskin", df4$상호명)] <- "배스킨라빈스"

df4$브랜드명[is.na(df4$브랜드명)] = "던킨"

head(df4)

# 데이터가 제대로 모아졌는지 확인
## 상권업종대분류명으로 확인
df4 %>%
  group_by(상권업종대분류명) %>%
  summarise(n=n())

## 소매는 뭔지 확인
### 참고. r에서 in 사용하기
# target <- c("Tom", "Lynn")
# filter(dat, name %in% target)

df4 %>% filter(상권업종대분류명 == "소매")
  
# 소매는 제외
df6 <- df4 %>% filter(상권업종대분류명 != "소매")

dim(df6)

# 각 브랜드의 빈도수 막대그래프로


# 시군구명으로 빈도수를 막대그래프로 색상을 다르게 표현 (브랜드별로 막대그래프를 따로 그려주세요)


# 산점도로 각 상점의 위치를 그리기(브랜드별로 구별)


# 지도 위에 마커 그리기. 각 브랜드별로 마커 색상 다르게


gc <- geocode(enc2utf8("서울"))
cen = as.numeric(gc)
map2 <- get_googlemap(center = cen, zoom=11, maptype = "roadmap")

colnames(df6)

ggmap(map2) +
  geom_point(data=df6, aes(x=경도, y=위도, shape=브랜드명, color=브랜드명), size=5)

# 지도 위에 마커 그리기. 각 시군구별로 마커 나타내되 개수에 따라 원의 크기가 달라야 함
df7 <- df6 %>% group_by(시군구명) %>% summarise(cnt=n(), 경도2=mean(경도), 위도2=mean(위도))
  geom_point(data=df7, aes(x=경도, y=위도, color=시군구명), size=5)

df7
  
ggmap(map2) +
  geom_point(data=df7, aes(x=경도2, y=위도2, color=시군구명, size=cnt), alpha=0.6) +
  scale_size(range = c(10, 50)) +
  geom_text(data=df7, aes(x=경도2, y=위도2), size=5, label=df7$cnt)


library(ggmap)

gc <- geocode(enc2utf8("대전"))
gc

cen <- as.numeric(gc)
cen
# center - 지도의 중심 좌표
# zoom - 지도의 확대크기 3(대륙) ~ 21(빌딩)
# size - 지도의 가로 세로 픽셀 크기
# maptype - 지도유형 (도로맵(roadmap), 터레인(terrain), 위성(satellite), 위성+터레인(bybrid))
map <- get_googlemap(center=cen)

ggmap(map)


# 마커

map2 <- get_googlemap(center = cen, maptype = "roadmap", marker=gc)
ggmap(map2)

# 여러 지역에 마커
addr <- df2 %>% slice_head(n=3) %>% select(상호명,경도, 위도)
cen <- c(mean(addr$경도), mean(addr$위도))


str(addr)

map2 <- get_googlemap(center = cen, zoom=11, maptype = "roadmap", marker=addr%>%select(경도, 위도))

addr

ggmap(map2) +
  geom_text(data=addr, aes(x=경도, y=위도), size=5, label=addr$상호명)


ggmap(map2) +
  geom_point(data=addr, aes(x=경도, y=위도), size=5, label=addr$상호명, alpha=0.5)



dddf <- data.frame(x=c(1,2,3,4), y=c(3,4,5,6))

ggplot(dddf, aes(x=x, y=y)) +
  geom_point() +
  theme(legend.position = 'top')+
  #범례 타이틀 색,크기,진하게 설정
  theme(legend.title = element_text(color = "black", size = 20, face = "bold"))+
  #범레 텍스트 색,크기,진하게 설정
  theme(legend.text = element_text(color = "black", size = 12, face = "bold"))+
  #범례 색 설정
  scale_color_manual(values=c('red','blue','green')) +
  #범례 이름 강제 설정(선택)
  labs(color="legend")





# 상권 데이터에서 배스킨라빈스와 던킨도너츠 데이터를 뽑아 분석해주세요.

df_seoul <- read.csv("data/소상공인시장진흥공단_상가(상권)정보_서울_202212.csv")

unique(df_seoul$상권업종대분류명)
unique(df_seoul$상권업종중분류명)
unique(df_seoul$상권업종소분류명)
unique(df_seoul$상호명)

library(dplyr)

n <-colSums(is.na(df_seoul))

ndf <- data.frame(col=colnames(df_seoul), na_cnt=n)

ndf


df_seoul <- df_seoul %>%
  mutate(상호명_소문자 =  tolower(상호명))

del_target <- ndf %>% 
  arrange(desc(na_cnt)) %>%
  slice_head(n=3) %>%
  select(col)


unlist(del_target)

df_seoul <- df_seoul %>%
  select(-unlist(del_target))

dim(df_seoul)

colnames <- colnames(df_seoul)

## 컬럼명에 번호가 포함되어있거나, 코드가 포함되어 있으면 가져오기
del_target <- colnames[grepl("번호", colnames(df_seoul)) | grepl("코드", colnames(df_seoul))]

## 해당 컬럼 삭제
df_seoul <- df_seoul %>%
  select(-del_target)



df_baskin <- df_seoul %>% 
  filter(grepl("배스킨|베스킨|baskin", df_seoul$상호명) | 
           grepl("베스킨", df_seoul$상호명) | 
           grepl("baskin", df_seoul$상호명))

df_dunkin <- df_seoul %>% 
  filter(grepl("던킨|dunkin", df_seoul$상호명))

dim(df_baskin)

dim(df_dunkin)

my_data <- rbind(df_baskin, df_dunkin)

head(my_data)

my_data <- my_data %>%
  mutate(브랜드="배스킨라빈스")

my_data %>%
  filter(grepl("던킨|dunkin", 상호명)) %>%
  mutate(브랜드="던킨도너츠")


my_data[grepl("던킨|dunkin", my_data$상호명_소문자), "브랜드"] <- "던킨도너츠"
my_data[grepl("던킨|dunkin", my_data$상호명_소문자),]


library(ggplot2)

# 1. 서울 내에 두 브랜드의 개수를 막대 그래프로 보여주세요
ggplot(my_data, aes(x=브랜드)) +
  geom_bar()

# 2. 서울 내에 두 브랜드의 개수를 구별로 막대 그래프로 보여주세요
ggplot(my_data, aes(x=브랜드, fill=시군구명)) +
  geom_bar(position = "dodge")

ggplot(my_data, aes(x=시군구명, fill=브랜드)) +
  geom_bar(position = "dodge")

# 3. 산점도로 각 매장의 위치를 브랜드별로 보여주세요.
ggplot(my_data, aes(x=경도, y=위도, color =브랜드)) +
  geom_point()


# 4. 서울 지역 내의 두 브랜드의 분포를 구글 맵 위에 그려주고 구별로 개수를 원으로 나타내주세요. 
# 원의 크기는 매점 개수에 따라 크기가 다르게 해주세요.
library(ggmap)

cen <- df_seoul %>% 
  select(경도, 위도) %>%
  summarise(경도=mean(경도), 위도=mean(위도))



m1 <- get_googlemap(center = unlist(cen), zoom=12)


# 구별 시군구명 텍스트 구별 경도,위도 평균값 필요.
gu_text <- my_data %>%
  group_by(시군구명, 브랜드) %>%
  summarise(경도=mean(경도), 위도=mean(위도))

# 개수별로 원의 크기 나타내야 하니까 구별 개수
gu_cnt <- my_data %>%
  group_by(시군구명, 브랜드) %>%
  summarise(개수=n(), 경도=mean(경도), 위도=mean(위도))


# 4번 정답 미완
ggmap(m1) +
  geom_point(data=gu_cnt, aes(x=경도, y=위도, size=개수,group=브랜드), alpha=0.6) +
  scale_size(range = c(5, 15)) +
  geom_text(data=gu_text, aes(x=경도, y=위도+0.05, label=시군구명), size=5) +
  geom_text(data=gu_text, aes(x=경도, y=위도, label=브랜드), size=5) +
  geom_text(data=gu_cnt, aes(x=경도, y=위도 - 0.004, label=개수), size=5)



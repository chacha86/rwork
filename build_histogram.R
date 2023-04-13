v1 <- 3
v2 <- rep(4, 2)
v3 <- rep(5, 3)
v4 <- rep(6, 2)
v5 <- 7
v6 <- 8
v7 <- 9
v8 <- rep(10, 4)
v9 <- rep(11, 2)
v10 <- 12
v11 <- 13
v12 <- 14
v13 <- rep(15, 2)
v14 <- rep(18, 2)
v15 <- rep(20, 3)
v16 <- 21
v17 <- rep(24, 2)

# 강사의 수업 준비 시간
rst <- c(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)

mn <- min(rst)
mx <- max(rst)
range <- mx - mn 

# 일반적으로 5~6정도의 class로 나눔
class_num <- ceiling(range / 6) # 일반적으로 올림을 해야 전체 범위를 표현 가능

# 구간의 시작은 3으로 해도 되지만 1과 3이 가까우니 1로 해도 무방
start <- 1

keys <- seq(start, range, class_num)

# 구간별 원소 세기
hist_el <- c()
for(i in seq(1, length(keys) - 1, 1)) {
  cnt <- length(rst[rst >= keys[i] & rst < keys[i+1]])
  hist_el <- c(hist_el, cnt)
}
cnt <- length(rst[rst >= keys[i+1]])
hist_el <- c(hist_el, cnt)

# 각 구간의 이름 부여
names(hist_el) <- c("1-5", "5-9", "9-13", "13-17", "17-21", "21-25")
names(hist_el)

# 결과
hist_el

# 시각화 추후에..


id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
kname <- c("태조", "정종", "태종", "세종", "문종"
           , "단종", "세조", "예종", "성종", "연산군")
ltime <- c(73, 62, 55, 53, 38, 16, 51, 19, 37, 30)
df_king <- data.frame(id, kname, ltime)
view(df_king)

# 이렇게도 쓸 수 있다.
df_king <- data.frame(
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  kname = c("태조", "정종", "태종", "세종", "문종"
           , "단종", "세조", "예종", "성종", "연산군"),
  ltime = c(73, 62, 55, 53, 38, 16, 51, 19, 37, 30),
  nchildren = c(13, 23, 29, 22, 3, 0, 5, 3, 28, 5)
)

# 이렇게도 쓸 수 있다2. tidyverse이용
tb_king <- tibble(
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  kname = c("태조", "정종", "태종", "세종", "문종"
             , "단종", "세조", "예종", "성종", "연산군"),
  ltime = c(73, 62, 55, 53, 38, 16, 51, 19, 37, 30)
)

view(tb_king)

tb_king <- tibble(
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  kname = c("태조", "정종", "태종", "세종", "문종"
            , "단종", "세조", "예종", "성종", "연산군"),
  ltime = c(73, 62, 55, 53, 38, 16, 51, 19, 37, 30),
  nchildren = c(13, 23, 29, 22, 3, 0, 5, 3, 28, 5)
)

df_king[2,2]
df_king[5:10, 2]
df_king[5:10, "kname"]
df_king$kname[5:10]

df_king[5:10, c("kname", "ltime")]
df_king[c(1, 3, 7), c("kname", "ltime")]
df_king[c(1, 3, 7), ]


str(df_king)

df_buy <- read.csv("./Adata2023/buy.csv")
df_buy
str(df_buy)
tb_buy <- read_csv("./Adata2023/buy.csv")
tb_buy


data <- read_csv("./Adata2023/buy.csv", 
                 # col_names = FALSE,
                 locale=locale(encoding="cp949"))
data

# csv로 파일 쓰기
write_csv(data, "./AData2023/buy_write.csv")

# 데이터 구조 확인 및 요약
## 데이터 확인
head(data)
head(data, 15)
tail(data)
slice(data, 5:10)
?slice

## 데이터 구조 확인
str(data)
attributes(data)
dim(data)
nrow(data)
ncol(data)
names(data)
colnames(data)

## 데이터 요약
summary(data)
summary(df_king)


# 벡터 생성 및 함수
seq(1, 4)
seq(1, 10, 3)
seq(10, 1, -2)

length(b2)
sum(b2)
a <- 2*(1:10)
a
max(a)
min(a)
mean(df_buy)
mean(df_buy$prod, na.rm=T)
summary(df_buy)

# 결측값 변경하여 읽어들이기
buy2 <- read_csv("./AData2023/buy2.csv", na = c(".", " ", "NA"))
summary(buy2)
str(buy2)


b1 <- rep(c(1, 2, 3))
b1
b2 <- rep(c(1, 2, 3), times=2)
b2
cbind(b1, b2)
rbind(b1, b2)

# scale, 평균이 0 표준편차가 1인 데이터로 만들어주는 함수
cbind(df_king$ltime, scale(df_king$ltime))
mean(scale(df_king$ltime))
sd(scale(df_king$ltime))      

# which, # index 출력!?
df_king$kname[which.max(df_king$ltime)]

# 소수 몇째자리까지 표시할건가
round(2.563, 3)

# 벡터 연산
x <- 1:10
2^x

length(x)
x1 <- x + 1
x2 <- 10 - x
x1 + x2
x1*x2

# 간단한 함수 적용
y <- 10*runif(5) # runif(): 0과 1 사이의 난수 생성 함수
y
which(y!=1)
which.max(y)
floor(y)

df_king$ltime > 50
which(df_king$ltime > 50)
df_king[df_king$ltime > 50, "kname"]

body <- read_csv("./Adata2023/body.csv")
view(body)
body$Weight[15:20]
body$mHeight <- body$Height/100
mean(body$Weight)
sd(body$Weight)
summary(body)


# for문
n <- nrow(df_buy)
total <- 0

for (i in 1:n){
  total <- total + df_buy[i, 6]
}

total


total <- sum(df_buy$income)
total

tb_buy
tb_buy$buy <- factor(tb_buy$buy, levels = c(0, 1), labels=c("비구매", "구매"))
head(tb_buy)

summary(tb_buy)

tb_buy$age <- factor(tb_buy$age, levels = c(2, 3, 4, 5), labels = c("20대", "30대", "40대", "50대이상"))
tb_buy
str(tb_buy)
summary(tb_buy)


# 변수값 그룹화 함수
df_king$ltime2 <- ifelse(df_king$ltime>= 50, "50년 이상", "50년 미만")
df_king$ltime3 <- case_when(
  df_king$ltime<=30 ~ "~30",
  df_king$ltime<=40 ~ "31~40",
  df_king$ltime<=50 ~ "41~50",
  TRUE ~ "50~"
)

df_king


# 범주형변수의 도수분포표 생성
table(body$Gender)
prop.table(table(body$Gender))
tbl_gender <- table(body$Gender)
table(df_buy$age, df_buy$buy)
prop.table(table(df_buy$age, df_buy$buy))

with(df_buy, table(age, buy))
with(df_buy, prop.table(table(age, buy)))
with(df_buy, prop.table(table(age, buy), 1)) # 행 기준으로 전체값을 계산 
with(df_buy, prop.table(table(age, buy), 2)) # 열 기준으로 전체값을 계산

airquality
require(graphics)
# 데이터의 구조와 내용 파악
glimpse(airquality)

filter(airquality, Temp > 70)

airquality %>% filter(Temp > 70)
filter(airquality, Temp > 80 & Month > 5)


# 새로운 변수 추가
new_air <- airquality %>% mutate(TempInC = (Temp - 32) * 5 / 9)
new_air

# Month와 TempInC만 포함하는 데이터셋
new_air %>% select(Month, TempInC)
# Temp를 제거한 데이터셋
new_air %>% select (-Temp)


# 변수의 전체 편균과 그룹별 평균
summarise(airquality, mean(Ozone))
summarise(airquality, mean(Ozone, na.rm = TRUE))

airquality %>%
  mutate(TempInC = (Temp - 32) * 5 / 9) %>%
  select(-Temp) %>%
  group_by(Month)%>%
  summarise(mean_Temp = mean(TempInC, na.rm = TRUE))


airquality %>%
  mutate(TempInC = (Temp - 32) * 5 / 9) %>%
  select(-Temp) %>%
  sample_n(20)

airquality %>%
  mutate(TempInC = (Temp - 32) * 5 / 9) %>%
  select(-Temp) %>%
  sample_frac(.7)


# 그룹별 관측치 수
count(airquality, Month)


arrange(airquality, desc(Month), Day)
airquality %>%
  mutate(TempInC = (Temp - 32) * 5 / 9) %>%
  select(-Temp) %>%
  arrange(desc(Month), Day)


Hdays <- airquality %>%
  mutate(Hday=ifelse(Temp>86, ">86도", "<=86도")) %>%
  group_by(Month, Hday) %>%
  summarise(nH=n())

Hdays %>%
  group_by(Month) %>%
  mutate(percent = nH / sum(nH))

new_air <- airquality %>%
  mutate(Hday=ifelse(Temp>86, ">86도", "<=86도"))
with(new_air, table(Month, Hday))
with(new_air, prop.table(table(Month, Hday)))
with(new_air, prop.table(table(Month, Hday), 1))
with(new_air, prop.table(table(Month, Hday), 2))

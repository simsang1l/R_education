#######################
######## R을 이용한 자료분석
#### 비율검정

## 비율 검정을 위한 R 함수
prop.test(x, # 성공횟수
          n, # 시행횟수
          p = NULL, # 모집단의 비율
          alternative = c("two.sided"), # 양측검정, (two.sided, less, greater)
          conf.level = 0.95, # 95% 신뢰구간
          correct = TRUE # 연속성 수정
          )

n <- 20
x <- 11
prop.test(x, n, p = 0.5, alternative = 'greater')

# buy데이터 이용하여 특정 상품의 구매비율이 다른지 알아보기
df <- read.csv('buy.csv')
head(df)
df <- na.omit(df)
head(df)
str(df)

# 년도별 구매횟수 계산
nyears <- tapply(df$year, df$year, length)
nyears
ntimes <- tapply(df$buy, df$year, sum)
ntimes

# 탐색적 자료분석 = 데이터를 한번 확인해보자~
library(ggplot2)
ggplot(df, aes(buy, fill = as.factor(year))) +
  geom_bar(position = position_dodge()) +
  labs(fill = 'Year')

ggplot(df, aes(year, fill = as.factor(buy))) +
  geom_bar() +
  labs(fill = 'Buy')

ggplot(df, aes(year, fill = as.factor(buy))) +
  geom_bar(position = 'fill') +
  labs(fill = 'Buy')


# 비율의 동질성 검정
prop.test(ntimes, nyears)


# 연령대별 구매비율의 차이가 있는지 알아보기
## 가설
## H0: 연령대별 구매 비율의 차이가 있다
## H1: 연령대별 구매 비율의 차이가 없다

# 연령대별 구매횟수
nages <- tapply(df$age, df$age, length)
nages
nbuys <- tapply(df$buy, df$age, sum)
nbuys

ggplot(df, aes(age, fill = as.factor(buy))) +
  geom_bar(position = 'fill') +
  labs(fill = 'Buy')

prop.test(nbuys, nages)
# 결론: 연령대별 구매 비율의 차이가 있다

#### 교차분석
# 카이제곱검정
chisq.test(x, #분할표
           correct = TRUE # 연속성 수정
           )

## 예제 1. 남학생, 여학생의 성별에 따라 과목에 대한 선호도가 다른가?

x <- matrix(c(73, 98, 79, 82, 58, 110),
            nrow = 2, ncol = 3,
            byrow = TRUE)

colnames(x) <- c("Korean", "Math", "Eng")
rownames(x) <- c("Male", "Female")
x <- as.table(x)
as.data.frame(x)

# 탐색적자료분석
ggplot(as.data.frame(x),
       aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = 'identity', position = position_dodge())+
  xlab("Gender") + labs(fill = "subject")

ggplot(as.data.frame(x),
       aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = 'identity', position = 'fill')+
  xlab("Gender") + labs(fill = "Subject")

chisq.test(x)


# 연령과 특정 상품의 구매여부 관계 있을까?
df <- read.csv("buy.csv", header = True)
df <- na.omit(df)

# 연령별 구매횟수에 대한 분할표
x <- table(df$age, df$buy); x

# 탐색적 자료분석
vcd::mosaic(~age + buy, data = df,
            highliting = 'age',
            highliting_fill = c("lightblue", "pink"))

vcd::mosaic(~age + buy, data = df,
            highliting = 'buy',
            highliting_fill = c("lightblue", "pink"))

# 두 특성의 독립성 검정
chisq.test(x)


## 나이와 교육수준 사이에 관계가 있는가?
# H0 : 나이와 교육수준 사이에 관계가 없다
# H1 : 나이와 교육수준 사이에 관계가 있다

data <- table(df$age, df$edu)
str(df)
head(data)

ggplot(as.data.frame(df),
       aes(x = df$age, y = df$edu, fill = df$age)) +
  geom_bar(stat = 'identity', position = position_dodge())

vcd::mosaic(~age + edu, data = df,
            highliting = 'edu',
            highliting_fill = c("lightblue", "pink"))

chisq.test(data)

# 결론: 나이와 교육수준 사이에 관계가 있음


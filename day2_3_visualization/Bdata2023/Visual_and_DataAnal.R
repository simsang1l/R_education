
install.packages(c('RColorBrewer', 'rgl', 'corrplot', 'igraph', 'vcd', 'maps'))
install.packages(c('ggplot2', 'GGally', 'lawstat', 'agricolae'))


## ------------------------------------------------------------------------------------------------------------------------------
data("VADeaths"); VADeaths

barplot(VADeaths, beside = TRUE, legend = TRUE, ylim = c(0, 90),
        ylab = "Deaths per 1000", main = "Death rates in Virginia")


## ------------------------------------------------------------------------------------------------------------------------------
data("airquality"); head(airquality)

# 월별 평균기온
heights <- tapply(airquality$Temp, airquality$Month, mean)

barplot(heights, main = 'Mean Temp. by Month',
        names.arg = c('May', 'Jun', 'Jul', 'Aug', 'Sep'),
        ylab = 'Temp (deg. F)')


## ------------------------------------------------------------------------------------------------------------------------------
groupsizes <- c(18, 30, 32, 10, 10)
labels <- c("A", "B", "C", "D", "F")
pie(groupsizes, labels, col = c("grey40", "white", "grey", "black", "grey90"))


## ------------------------------------------------------------------------------------------------------------------------------
# 사용가능한 색깔
RColorBrewer::display.brewer.all()

# 색 변경
pie(groupsizes, labels, col = RColorBrewer::brewer.pal(5, 'Dark2'))


## ------------------------------------------------------------------------------------------------------------------------------
data(UScereal, package = 'MASS')
hist(UScereal$sugars, probability = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------
hist(UScereal$sugars, probability = TRUE, breaks = 'Scott')


## ------------------------------------------------------------------------------------------------------------------------------
boxplot(iris$Sepal.Length, ylab = "Sepal length (cm)", main = "Iris measurements")


## ------------------------------------------------------------------------------------------------------------------------------
boxplot(Sepal.Length ~ Species, data = iris, 
        ylab = "Sepal length (cm)", main = "Iris measurements")


## ------------------------------------------------------------------------------------------------------------------------------
data(Cars93, package = 'MASS'); head(Cars93)
qqplot(Cars93$Min.Price, Cars93$Max.Price)


## ------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2)) # 하나의 페이지에 여러 그래프 그리기

# 원자료
qqnorm(Cars93$Price); qqline(Cars93$Price)

# log변환 자료
qqnorm(log(Cars93$Price)); qqline(log(Cars93$Price))


## ------------------------------------------------------------------------------------------------------------------------------
data(stackloss); head(stackloss)
plot(stackloss$Air.Flow ~ stackloss$stack.loss, pch = 20)


## ------------------------------------------------------------------------------------------------------------------------------
pairs(stackloss)


## ------------------------------------------------------------------------------------------------------------------------------
indexfinger <- read.csv("indexfinger.csv")

# 산점도 그리기
plot(width ~ length, data = indexfinger)

# 특정한 점을 변경하기
with(indexfinger[c(3,7),], points(length, width, pch = 16))

# 점의 형태를 변경하기 
plot(width ~ length, pch = as.character(sex), data = indexfinger)

# 선형회귀직선을 추가하기
abline(lm(width ~ length, data = indexfinger, subset = (sex == "M")), lty = 1)
abline(lm(width ~ length, data = indexfinger, subset = (sex == "F")), lty = 2)

# 범례 추가하기
legend("topleft", legend = c("Male", "Female"), lty = 1:2)


## ------------------------------------------------------------------------------------------------------------------------------
par(mar = c(5, 5, 5, 5) + 0.1)
plot(c(1, 9), c(0, 50), type = 'n', xlab = "", ylab = "")
text(6, 40, "Plot region")
points(6, 20)
text(6, 20, "(6, 20)", adj = c(0.5, 2))
mtext(paste("Margin", 1:4), side = 1:4, line = 3)
mtext(paste("Line", 0:4), side = 1, line = 0:4, at = 3, cex = 0.6)
mtext(paste("Line", 0:4), side = 2, line = 0:4, at = 15, cex = 0.6)
mtext(paste("Line", 0:4), side = 3, line = 0:4, at = 3, cex = 0.6)
mtext(paste("Line", 0:4), side = 4, line = 0:4, at = 15, cex = 0.6)


## ------------------------------------------------------------------------------------------------------------------------------
par(oma = c(1, 3, 1, 1))
plot(stackloss$Water.Temp ~ stackloss$Acid.Conc.)


## ------------------------------------------------------------------------------------------------------------------------------
hist(stackloss$Water.Temp)

# 새로운 창
dev.new()#windows()
hist(stackloss$Water.Temp)
dev.off()


## ------------------------------------------------------------------------------------------------------------------------------
pdf("plot.pdf", width = 10, height = 10)
hist(rnorm(1000))
dev.off()


## ------------------------------------------------------------------------------------------------------------------------------
# 3차원의 산점도
rgl::plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type = "s", size = 0.75, 
            lit = FALSE, xlab = "Weight", ylab = "Displacement", zlab = "MPG")

# 저장
rgl::rgl.postscript("3dplot.pdf", fmt = "pdf")


## ------------------------------------------------------------------------------------------------------------------------------
# 상관행렬 구하기
mcor <- cor(mtcars)

# 상관행렬 그리기
corrplot::corrplot(mcor)
corrplot::corrplot(mcor, method = "shade", shade.col = NA, 
                   tl.col = "black", tl.srt = 45)


## ------------------------------------------------------------------------------------------------------------------------------
# 방향이 있는 네트워크
gd <- igraph::graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(gd)

# 방향이 없는 네트워크
gd <- igraph::graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed = FALSE)
plot(gd, layout = igraph::layout.circle, vertex.label = NA)


## ------------------------------------------------------------------------------------------------------------------------------
madmen <- read.csv("madmen.csv")
madmen

# 자료 추출
m <- madmen[1:nrow(madmen) %% 2 == 1, ]
g <- igraph::graph.data.frame(m, directed = FALSE)

# 그래프 그리기
plot(g, layout = igraph::layout.fruchterman.reingold, vertex.size = 4, 
vertex.label = igraph::V(g)$name, vertex.label.cex = 0.8, 
vertex.label.dist = 0.4, vertex.label.color = "black")


## ------------------------------------------------------------------------------------------------------------------------------
countries <- read.csv("countries.csv", row.names = 1); head(countries)

# 자료의 척도 변환(표준화)
d <- scale(countries)

# 군집분석
hc <- hclust(dist(d))

# 덴드로그램
plot(hc)

# 텍스트정렬
plot(hc, hang = -1)


## ------------------------------------------------------------------------------------------------------------------------------
# 모자이크 그래프 그리기 (Dept -> Gender -> Admit)
vcd::mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions, 
highlighting = "Admit", highlighting_fill = c("lightblue", "pink"), 
direction = c("v", "h", "v"))


## ------------------------------------------------------------------------------------------------------------------------------
# 자료 받기
filenames <- "http://book.flowingdata.com/ch08/geocode/costcos-geocoded.csv"
costcos <- read.csv(filenames, sep=","); head(costcos)

# 지도 그리기
maps::map(database='state')

# 코스트코 위치 표시하기
symbols(costcos$Longitude, costcos$Latitude, circles = rep(1, nrow(costcos)), 
        inches=0.05, add=TRUE)


## ------------------------------------------------------------------------------------------------------------------------------
head(mtcars)

# 표준화
x <- scale(mtcars)

# 히트맵
heatmap(x, scale = 'none')


## ------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
head(diamonds)

# cut
ggplot(diamonds, aes(x = cut)) + geom_bar()

# cut & color
ggplot(diamonds, aes(x = cut, fill = color)) + geom_bar()
ggplot(diamonds, aes(x = cut, fill = color)) + geom_bar(position = position_dodge(0.9))


## ------------------------------------------------------------------------------------------------------------------------------
ggplot(diamonds, aes(factor(1), fill = cut)) + 
  geom_bar() + coord_polar(theta = "y") +
  ylab("") + xlab("")


## ------------------------------------------------------------------------------------------------------------------------------
head(MASS::birthwt)

ggplot(MASS::birthwt, aes(x = bwt)) + geom_histogram()
ggplot(MASS::birthwt, aes(x = bwt)) + geom_histogram(fill = 'white', colour = 'black')


## ------------------------------------------------------------------------------------------------------------------------------
# 그룹별(흡연) 히스토그램
ggplot(MASS::birthwt, aes(x = bwt)) + 
  geom_histogram(fill = 'white', colour = 'black') + facet_grid(smoke ~ .)

# 요인수준의 이름변경
birthwt1 <- MASS::birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
birthwt1$smoke <- plyr::revalue(birthwt1$smoke, c("0" = "No Smoke", "1" = "Smoke"))

ggplot(birthwt1, aes(x = bwt)) +
  geom_histogram(fill = "white", colour = "black") + facet_grid(smoke ~ .)


## ------------------------------------------------------------------------------------------------------------------------------
ggplot(MASS::birthwt, aes(x = 1, y = bwt)) + geom_boxplot()

# x축 변경
ggplot(MASS::birthwt, aes(x = 1, y = bwt)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) + 
  theme(axis.title.x = element_blank())


## ------------------------------------------------------------------------------------------------------------------------------
ggplot(MASS::birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot(width = .5)


## ------------------------------------------------------------------------------------------------------------------------------
hw <- read.csv("heightweight.csv"); head(hw)

ggplot(hw, aes(sample = ageYear)) + stat_qq() + stat_qq_line()


## ------------------------------------------------------------------------------------------------------------------------------
ggplot(hw, aes(sample = heightIn, colour = factor(sex))) + 
  stat_qq() + stat_qq_line()


## ------------------------------------------------------------------------------------------------------------------------------
ggplot(hw, aes(x = ageYear, y = heightIn)) + geom_point(shape = 21, size = 1.5)

# 성별에 따른 키 vs 나이
ggplot(hw, aes(x = ageYear, y = heightIn, shape = sex, colour = sex)) + 
  geom_point() 

# 몸무게와 성별에 따른 키 vs 나이
ggplot(hw, aes(x = ageYear, y = heightIn, colour = sex, size = weightLb)) + 
  geom_point()


## ------------------------------------------------------------------------------------------------------------------------------
GGally::ggpairs(hw)


## ------------------------------------------------------------------------------------------------------------------------------
east_asia <- map_data("world", region = c("Japan", "China",
                                          "North Korea", "South Korea"))

ggplot(east_asia, aes(x = long, y = lat, group = group, fill = region)) +
  geom_polygon(colour = "black")


## ------------------------------------------------------------------------------------------------------------------------------
head(USArrests)

# 자료변환
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(crimes)

states_map <- map_data("state")
crime_map <- merge(states_map, crimes, by.x = 'region', by.y = 'state')
crime_map <- plyr::arrange(crime_map, group, order)

# 범죄 지도
ggplot(crime_map, aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(colour = "black") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.length = unit(0, "cm"))


## ------------------------------------------------------------------------------------------------------------------------------
prop.test(x,                            # 성공횟수
          n,                            # 시행횟수
          p = NULL,                     # 모집단의 비율
          alternative = c("two.sided"), # 양측검정
          conf.level = 0.95,            # 95% 신뢰구간
          correct = TRUE)               # 연속성 수정


## ------------------------------------------------------------------------------------------------------------------------------
# 자료입력
n <- 20; x <- 11
  
# 모비율 검정
prop.test(x, n, p = 0.5, alternative = 'greater')


## ------------------------------------------------------------------------------------------------------------------------------
# 자료 읽기
df <- read.csv("buy.csv", header = TRUE)
df <- na.omit(df) # 결측치 생략
str(df)
  
# 년도별 구매횟수 계산
nyears <- tapply(df$year, df$year, length)
nyears
  
ntimes <- tapply(df$buy, df$year, sum)
ntimes


## ------------------------------------------------------------------------------------------------------------------------------
ggplot(df, aes(buy, fill = as.factor(year))) +
  geom_bar(position = position_dodge()) +
  labs(fill = 'Year')


## ------------------------------------------------------------------------------------------------------------------------------
prop.test(ntimes, nyears)


## ------------------------------------------------------------------------------------------------------------------------------
chisq.test(x,               # 분할표
           correct = TRUE)  # 연속성 수정


## ------------------------------------------------------------------------------------------------------------------------------
# 자료 입력
x <- matrix(c(73, 98, 79, 82, 58, 110), nrow = 2, ncol = 3, byrow = TRUE)
colnames(x) <- c("Korean", "Math", "Eng")
rownames(x) <- c('Male', 'Female')
x <- as.table(x)
x


## ------------------------------------------------------------------------------------------------------------------------------
ggplot(as.data.frame(x), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  xlab("Gender") + labs(fill = "Subject")


## ------------------------------------------------------------------------------------------------------------------------------
chisq.test(x)
  
# 또는
summary(x)


## ------------------------------------------------------------------------------------------------------------------------------
# 자료 읽기
df <- read.csv("buy.csv", header = TRUE)
df <- na.omit(df) # 결측치 생략
  
# 연령별 구매횟수에 대한 분할표
x <- table(df$age, df$buy); x


## ------------------------------------------------------------------------------------------------------------------------------
vcd::mosaic( ~ age + buy, data = df, 
             highlighting = 'age', highlighting_fill = c("lightblue", "pink"))


## ------------------------------------------------------------------------------------------------------------------------------
chisq.test(x)


## ------------------------------------------------------------------------------------------------------------------------------
t.test(formula, data = ...,               
       paired = FALSE,             # 대응표본
       alternative = "two.sided",  # 양측검정
       var.equal = FALSE,          # 분산 동질성
       conf.level = 0.95)          # 95% 신뢰구간


## ------------------------------------------------------------------------------------------------------------------------------
# 자료 읽기
df <- read.csv("buy.csv", header = TRUE)
df <- na.omit(df) # 결측치 생략

# 변수 age를 범주화(Young, Old)
df$yage <- ifelse(df$age < 4, 1, 0)
df$yage <- factor(df$yage, levels = c(1, 0), labels = c("Young", "Old"))
head(df)


## ------------------------------------------------------------------------------------------------------------------------------
# 도수 분포표
table(df$yage)

# yage 수준별로 prod변수 summary
with(df, tapply(prod, yage, summary))

# 상자도표
ggplot(df, aes(yage, prod)) + 
  geom_boxplot() + xlab("age")

# 히스토그램
ggplot(df, aes(x = prod)) + 
  geom_histogram(binwidth = 1.5) +
  facet_grid(yage ~ .) +
  ggtitle("Histogram of Prod by Age")


## ------------------------------------------------------------------------------------------------------------------------------
# yage 수준별로 prod 변수 정규성 검정
shapiro.test(df$prod[df$yage == "Young"])
shapiro.test(df$prod[df$yage == 'Old'])

# 또는
with(df, tapply(prod, yage, shapiro.test))


## ------------------------------------------------------------------------------------------------------------------------------
var.test(prod ~ yage, data = df)


## ------------------------------------------------------------------------------------------------------------------------------
t.test(prod ~ yage, data = df,
       alternative = "two.sided", 
       var.equal = FALSE, 
       conf.level = 0.95)


## ------------------------------------------------------------------------------------------------------------------------------
data(sleep)  # 내장자료 불러오기
?sleep       # 자료에 대한 설명


## ------------------------------------------------------------------------------------------------------------------------------
# 수면제 종류대로 정렬한 후 ID순서대로 정렬
sleep <- sleep[order(sleep$group, sleep$ID), ]
t.test(extra ~ group, sleep, paired = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------
# sleep자료를 변환
sleep_wide <- data.frame(ID = 1:10, 
                         group1 = sleep$extra[1:10], 
                           group2 = sleep$extra[11:20])
sleep_wide

# 두 가지 방법으로 검정 가능
# 1.
t.test(sleep_wide$group1, sleep_wide$group2, paired = TRUE)

# 2.
t.test(sleep_wide$group1 - sleep_wide$group2, mu = 0)



## ------------------------------------------------------------------------------------------------------------------------------
# 자료 읽기
df <- read.csv("buy.csv", header = TRUE)
df <- na.omit(df) # 결측치 생략

# 변수 age를 범주형 변수 age4로 변환
df$age4 <- factor(df$age,
                  levels = c(2, 3, 4, 5),
                  labels = c("20대", "30대", "40대", "50대"))


## ------------------------------------------------------------------------------------------------------------------------------
# 도수 분포표
table(df$age4)

# age4 수준별로 prod변수 summary
with(df, tapply(prod, age4, summary))

# 상자도표
ggplot(df, aes(age4, prod)) + 
  geom_boxplot() + xlab("age")

# 히스토그램
ggplot(df, aes(x = prod)) + 
  geom_histogram(binwidth = 1.5) +
  facet_grid(age4 ~ .) +
  ggtitle("Histogram of Prod by Age")


## ------------------------------------------------------------------------------------------------------------------------------
lawstat::levene.test(df$prod, df$age4) 
# bartlett.test(prod ~ age4, data = df) 


## ------------------------------------------------------------------------------------------------------------------------------
oneway.test(prod ~ age4, data = df, var.equal = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------
aov.out <- aov(prod ~ age4, data = df)
summary(aov.out)


## ------------------------------------------------------------------------------------------------------------------------------
# Tukey HSD
TukeyHSD(aov.out, conf.level = 0.95)

# Scheffe (more conservative)
agricolae::scheffe.test(aov.out, 
                        "age4",         # 집단변수
                        alpha = 0.05,   # 유의수준
                        console = TRUE) # 화면출력

## ------------------------------------------------------------------------------------------------------------------------------
# 자료 읽기
df <- read.csv("buy.csv", header = TRUE)
df <- na.omit(df) # 결측치 생략
str(df)


## ------------------------------------------------------------------------------------------------------------------------------
# 기술 통계량
summary(df[,c('co', 'prod')])

# 산점도
ggplot(df, aes(y = prod, x = co)) + 
  geom_point()


## ------------------------------------------------------------------------------------------------------------------------------
fit <- lm(prod ~ co, data = df)
summary(fit)


## ------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(fit)


## ------------------------------------------------------------------------------------------------------------------------------
# 로그 변환
df$lprod <- log(df$prod)
df$lco <- log(df$co)

# 산점도
ggplot(df, aes(y = lprod, x = lco)) + 
  geom_point()


## ------------------------------------------------------------------------------------------------------------------------------
fit1 <- lm(lprod ~ lco, data = df)
summary(fit1)


## ------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(fit1)


## ------------------------------------------------------------------------------------------------------------------------------
# 회귀계수와 신뢰구간
coef(fit1)
confint(fit1, level = 0.95)


## ------------------------------------------------------------------------------------------------------------------------------
# 새로운 co값 10, 20, 30에 대한 예측값
newda <- data.frame(co = c(10, 20, 30))
newda$lco <- log(newda$co)

predict(fit1, newda, interval = 'confidence')
predict(fit1, newda, interval = 'predict')


## ------------------------------------------------------------------------------------------------------------------------------
#자료 읽기
df <- read.csv("buy.csv", header = TRUE)
df <- na.omit(df) # 결측치 생략

# 변수 age를 범주화(Young, Old)
df$yage <- ifelse(df$age < 4, 1, 0)
df$yage <- factor(df$yage, levels = c(1, 0), labels = c("Young", "Old"))
head(df)


## ------------------------------------------------------------------------------------------------------------------------------
# 산점도
ggplot(df, aes(y = lprod, x = lco, color = yage)) + geom_point()


## ------------------------------------------------------------------------------------------------------------------------------
fit2 <- lm(lprod ~ lco + yage, data = df); summary(fit2)
fit3 <- lm(lprod ~ lco*yage, data = df); summary(fit3)


## ------------------------------------------------------------------------------------------------------------------------------
anova(fit2, fit3)


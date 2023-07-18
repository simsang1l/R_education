######################
## 패키지 설치
install.packages(c('rgl', 'corrplot', 'igraph', 'vcd', 'maps'))
install.packages(c('ggplot2', 'GGally', 'lawstat', 'agricolae'))

getwd()
# 데이터가 있는 working directory로 설정
setwd('../day2_3_visualization/Bdata2023')

######################

############# R을 이용한 데이터 시각화
###### ggplot2
#### 기본 그래프
## 막대그래프


## 예제 1
data(VADeaths)
# 딱히 위의 코드를 안써도 됨. R에 내장되어 있기 때문에..
VADeaths

# barplot(도수분포표)
barplot(VADeaths, beside = TRUE
        , legend = T
        , ylim = c(0, 90)
        , ylab = "Deaths per 1000"
        , main = "Death rates in Virginia"
        )

barplot(t(VADeaths), beside = TRUE
        , legend = T
        , ylim = c(0, 90)
        , ylab = "Deaths per 1000"
        , main = "Death rates in Virginia"
)


## 예제 2
airquality

mean(airquality[airquality$Month == 5,]$Temp)
heights <- tapply(airquality$Temp, airquality$Month, mean)
heights

barplot(heights
        , main = "Mean Temp by Month"
        , names.arg = c("May", "Jun", "Jul", "Aug", "Sep")
        , ylab = 'Temp (deg. F)'
        , col = 'steelblue'
        )


# 파이차트 (pie chart)
groupsizes <- c(18, 30, 32, 10, 10)
labels <- c("A", "B", "C", "D", "F")
pie(groupsizes, labels
    , col = c("grey40", "white", "grey", "black", "grey90")
    )

library(RColorBrewer)
RColorBrewer::display.brewer.all()

pie(groupsizes, labels
    , col = RColorBrewer::brewer.pal(5, 'Set3')
)


# 히스토그램
data(UScereal, package = 'MASS')
str(UScereal)
hist(UScereal$sugars, probability = TRUE, breaks = "Scott")


## 상자 그림
summary(iris)
head(iris)
hist(iris$Sepal.Length)

boxplot(iris$Sepal.Length
        , ylab = "Sepal length (cm)"
        , main = "Iris measurements"
        # , horizontal = TRUE
        , boxwex = 0.5
        )

# "~" 두 변수 간의 관계를 보여달라는 의미!
boxplot(Sepal.Length ~ Species
        , data = iris
        , ylab = "Sepal length (cm)"
        , main = "Iris measurements"
        # , horizontal = TRUE
        , boxwex = 0.5
)


## QQ plot
data(Cars93, package = 'MASS')
head(Cars93)

qqplot(Cars93$Min.Price, Cars93$MPG.highway)

hist(Cars93$Price)
# 하나의 페이지에 여러 그래프 그리기
par(mfrow = c(1, 2))

# 원자료
qqnorm(Cars93$Price); qqline(Cars93$Price)

# log변환 자료
qqnorm(log(Cars93$Price)); qqline(log(Cars93$Price))

## 산점도
data(stackloss)
head(stackloss)

# y축 ~ x 축
# plot(x축, y축)
plot(stackloss$Air.Flow ~ stackloss$stack.loss, pch = 11, cex = 0.5)
plot(stackloss$stack.loss, stackloss$Air.Flow, pch = 20)

pairs(stackloss)


## 성인의 왼손 검지손가락 자료
indexfinger <- read.csv("indexfinger.csv")
plot(width ~ length
     , data = indexfinger, pch = 16)
points(indexfinger[c(3, 7),]$length,
       indexfinger[c(3, 7),]$width, pch = 8, cex = 2, col = 'red')

# 특정한 점을 변경하기
plot(width ~ length, pch = as.character(sex),
     data = indexfinger)

# 선형회귀직선을 추가하기
abline(lm(width ~ length,
       data = indexfinger,
       subset = (sex == 'M')), lty = 1) # lty = 1  실선, lty = 2 점선
abline(lm(width ~ length,
          data = indexfinger,
          subset = (sex == 'F')), lty = 2, lwd = 5) # lty = 1  실선, lty = 2 점선
abline (h = 2.2, col = "grey")

abline(v = 6.5, col = 'darkorange')


# 범례 추가하기
legend("topleft", legend = c("Male", "Female"), lty = 1:2, col=c("red", "blue"))
legend(x = 6.5, y = 2.5, legend = c("Male", "Female"), lty = 1:2)


## 그림영억 바깥 부분에 주석달기
par(mar = c(4, 5, 5, 5) + 0.1) # 아래 왼 위 오 순서
plot(c(1, 9), c(0, 50),
     type = 'n', xlab = "", ylab = "")
text(6, 40, "Plot region")
points(6, 20)
text(6, 20, "(6, 20)", adj = c(0.5, 2))
mtext(paste("Margin", 1:4), side = 1:4, line = 3)
mtext(paste("Line", 0:4), side = 1, line = 0:4, at = 3, cex = 0.6)
mtext(paste("Line", 0:4), side = 2, line = 0:4, at = 15, cex = 0.6)
mtext(paste("Line", 0:4), side = 3, line = 0:4, at = 3, cex = 0.6)
mtext(paste("Line", 0:4), side = 4, line = 0:4, at = 15, cex = 0.6)

par(oma = c(1, 3, 1, 1))
plot(stackloss$Water.Temp ~ stackloss$Acid.Conc.)


## 그래프 옵션 설정하기

# 새로운 창에 그래프 그리기
x <- c(1, 2, 3)
a <- c(1, 3, 8)
hist(x)

dev.new() # windows()
hist(stackloss$Water.Temp)

pdf("plot.pdf", width = 10, height = 10)
png("png.png")
hist(rnorm(1000))
dev.off()


# 상관 행렬 구하기
mtcars
mcor <- cor(mtcars)
round(mcor, 3)

# 상관 행렬 그리기
corrplot::corrplot(mcor)
corrplot::corrplot(mcor, method = "shade", shade.col = NA,
                   tl.col = "black", tl.srt = 45)

# 네트워크 그래프

# 방향이 있는 네트워크
gd = igraph::graph(c(1,2 , 2,3 , 2,4 , 1,4, 5,5, 3,6)
                   , directed = FALSE)
gd
plot(gd)
plot(gd, layout = igraph::layout.circle, vertex.label = NA)

madmen <- read.csv("madmen.csv")
madmen

m <- madmen[1:nrow(madmen) %% 2 == 1, ]
g <- igraph::graph.data.frame(m, directed = FALSE)

# 그래프 그리기
plot(g, layout = igraph::layout.fruchterman.reingold, vertex.size = 4,
vertex.label = igraph::V(g)$name, vertex.label.cex = 0.8, vertex.label.dist = 0.4
, vertex.label.color = "black")


## 덴드로그램
countries <- read.csv("countries.csv", row.names = 1); head(countries)

# 자료의 척도 변환(표준화)
d <- scale(countries)

# 군집분석
hc <- hclust(dist(d))
plot(hc)

plot(hc, hang = -1)


## 모자이크 그래프

# 버클리 대학의 입학 자료
UCBAdmissions

vcd::mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions,
             highliting = "Admit", highlighting_fill = c("lightblue", "pink"),
             direction = c("v", "h", "v"))

# 공간자료 그래프
# 미국 코스트코 자료
filenames <- "http://book.flowingdata.com/ch08/geocode/costcos-geocoded.csv"
costcos <- read.csv(filenames, sep = ","); head(costcos)

# 지도 그리기
maps::map(database = "state")
symbols(costcos$Longitude, costcos$Latitude, circles = rep(1, nrow(costcos)),
        inches = 0.05, add = TRUE, fg = "red")


## 히트맵
head(mtcars)

# 표준화
x <- scale(mtcars)

# 히트맵
heatmap(x, scale = "none")
graphics.off()


########################################
######### R을 이용한 시각화
####  ggplot2

### 기본그래프
## 막대그래프 
library(ggplot2)
head(diamonds)
str(diamonds)

# cut
ggplot(diamonds, aes(x = cut)) +
  geom_bar(stat = "count"
           , fill = "lightblue"
           , colour = "black"
           ) + theme_classic()

# cut & color
ggplot(diamonds, aes(x = cut, fill = color)) + geom_bar()
ggplot(diamonds, aes(x = cut, fill = color)) + geom_bar(position = position_dodge())

# 파이차트
ggplot(diamonds, aes(factor(1), fill = cut)) + geom_bar() + coord_polar(theta = "y") + ylab("") + xlab("")


## histogram

# 저체중아 자료
head(MASS::birthwt)

ggplot(MASS::birthwt, aes(x = bwt)) + geom_histogram()
ggplot(MASS::birthwt, aes(x = bwt)) + geom_histogram(fill = "white", colour = "black")
ggplot(MASS::birthwt, aes(x = bwt)) +
  geom_histogram(fill = "white"
                 , colour = "black"
                 # , bins = 10
                 ) +
  facet_grid(smoke ~ .)

# 그룹별(흡연) 히스토그램
# 사용 방법: plt + facet_grid(smoke~ .)

birthwt1 <- MASS::birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
birthwt1$smoke <- plyr::revalue(birthwt1$smoke, c("0" = "No Smoke", "1" = "Smoke"))

ggplot(birthwt1, aes(x = bwt)) +
  geom_histogram(fill = "white", colour = "black") + facet_grid(smoke ~ .)

# 겹쳐 그리기
ggplot(birthwt1, aes(x = bwt, fill = smoke)) + 
  geom_histogram(position = "identity", alpha = 0.4)


### 상자 그림
# 하나의 집단 
ggplot(MASS::birthwt, aes(x = 1, y = bwt)) +
  geom_boxplot()

ggplot(MASS::birthwt, aes(x = 1, y = bwt)) +
  geom_boxplot(outlier.size = 1,
               outlier.shape = 8) +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank())


# 집단별
ggplot(MASS::birthwt,
       aes(x = factor(race), y = bwt)) +
  geom_boxplot(width = .5) + 
  # coord_flip() +
  stat_summary(fun.y = "mean", geom = "point"
                , shape = 23
                , fill = "white") +
  scale_x_discrete(limits = c("1", "2", "3"))


# 분위수-분위수(Q-Q)그래프
hw <- read.csv("heightweight.csv");head(hw)
ggplot(hw, aes(sample = ageYear)) + 
  stat_qq() + 
  stat_qq_line()

ggplot(hw, aes(x = ageYear))+
  geom_histogram()

ggplot(hw, aes(sample = heightIn,
               colour = factor(sex))) +
  stat_qq() + stat_qq_line()


### 산점도
# 학생들의 키와 몸무게
ggplot(hw, aes(x = ageYear, y = heightIn)) + 
  geom_point(shape = 21, size = 1.5) +
  geom_hline(yintercept = mean(hw$heightIn),
             col = "blue", lty = 2) +
  geom_vline(xintercept = mean(hw$ageYear), col = "darkgreen", lty = 2)

# 성별에 따른 키 vs 나이
ggplot(hw, aes(x = ageYear, y = heightIn, shape = sex, colour = sex)) + geom_point()

# 몸무게와 성별에 따른 키 vs 나이
ggplot(hw, aes(x = ageYear, y = heightIn, shape = sex, colour = sex, size = weightLb)) + geom_point()

# 산점도 행렬
GGally::ggpairs(hw)
pairs(hw[,-1])


### 공간자료 시각화
east_asia <- map_data("world", region = c("Japan", "China", "North Korea", "South Korea"))
head(east_asia)
head(east_asia[east_asia$region == "South Korea", ])

ggplot(east_asia, aes(x = long, y = lat, group = group, fill = region)) +
  geom_polygon(colour = "black") +
  scale_fill_brewer(palette = "Set2")

# 미국의 폭력 범죄율
USArrests
head(USArrests)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(crimes)

states_map <- map_data("state")
states_map
crime_map <- merge(states_map, crimes, by.x = 'region', by.y = 'state')
crime_map <- plyr::arrange(crime_map, group, order)

# 범죄 지도
ggplot(crime_map, aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.length = unit(0, "cm"))

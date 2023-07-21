## ----include=FALSE-----------------------------------------------
opts_chunk$set(eval=FALSE, fig.width=6, fig.height=5)


## ----------------------------------------------------------------
install.packages(c('ade4','psych'))


## ----------------------------------------------------------------
n <- 100
x1 <- rnorm(n,1,0.2)
x2 <- 0.5 + 0.5*x1
par(mfrow=c(1,1))
plot(x1,x2,xlim=c(0,2),ylim=c(0,2),main="A")
library(MASS)
# 이변량 정규분포에서 자료생성
mu <- c(1,1)
sigma <- matrix(c(0.04,0.018,0.018,0.01),2,2)
x <- mvrnorm(n,mu,sigma)
plot(x,xlim=c(0,2),ylim=c(0,2),main="B")
    

## ----------------------------------------------------------------
data("USArrests"); USArrests
#산점도
plot(USArrests)
#요약 
summary(USArrests)
# 변수별 변동(분산)
apply(USArrests, 2, var)
#주성분분석 시행
p1 <- princomp(USArrests, cor=TRUE) # 상관행렬에 의한 주성분분석 시행
ls(p1) # 어떤 object들이 포함되어 있는지 확인
p1$loadings  # 각 변수들의 주성분에의 기여도 (고유벡터 집합)
p1$sdev      # 각 주성분의 표준편차 
summary(p1)  # 주성분의 표준편차, 각 주성분의 변동의 상대비율, 누적변동비율
screeplot(p1,type="lines")  # scree plot
pve <- cumsum(sort(p1$sdev^2,decreasing = T))/sum(p1$sdev^2)
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", 
     ylim=c(0,1),type='b')
biplot(p1)                  # biplot : 제 1,2 주성분점수에 의한 산점도 + 적재계수
plot(p1$scores[,1],p1$scores[,2]) # 제 1,2 주성분점수에 의한 산점도
text(p1$scores[,1], p1$scores[,2], labels = rownames(USArrests))

## ----------------------------------------------------------------
# 패키지 로딩
library(ade4)
data("olympic"); olympic
#요약 
summary(olympic$tab)
p2 <- princomp(olympic$tab, cor=TRUE) # 상관행렬에 의한 주성분분석 시행
p2$loadings  # 각 변수들의 주성분에의 기여도 (고유벡터 집합)
p2$sdev      # 각 주성분의 표준편차 
summary(p2)  # 주성분의 표준편차, 각 주성분의 변동의 상대비율, 누적변동비율
screeplot(p2,type="lines")  # scree plot
pve <- cumsum(sort(p2$sdev^2,decreasing = T))/sum(p2$sdev^2)
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", 
     ylim=c(0,1),type='b')
biplot(p2)                  # biplot : 제 1,2 주성분점수에 의한 산점도 + 적재계수
plot(p2$scores[,1], olympic$score)  # 제 1 주성분 점수와 전체 10종경기 총점 사이의 산점도


## ----------------------------------------------------------------
#패키지 로딩
library(psych)
data("Harman74.cor"); Harman74.cor
Harman74.FA <- factanal(factors = 1, covmat = Harman74.cor) # 1-factor model
Harman74.FA
Harman74.FA$loadings  # 요인의 설명력
Harman74.FA$uniquenesses # 특정분산 추정치
Harman74.FA$scores


## ----------------------------------------------------------------
for(factors in 2:5) print(update(Harman74.FA, factors = factors)) # 2 to 5 factors
Harman74.FA <- factanal(factors = 5, covmat = Harman74.cor,
                        rotation = "none")
(LL <- Harman74.FA$loadings)  
rowSums(LL^2)       # commuality


## ----------------------------------------------------------------
Harman74.FA.rotation <- factanal(factors = 5, covmat = Harman74.cor,
                        rotation = "promax")  # promax 회전
Harman74.FA.rotation
plot(Harman74.FA.rotation$loadings)
text(Harman74.FA.rotation$loadings, labels = colnames(Harman74.cor$cov),cex=0.5)


## ----------------------------------------------------------------
pc <- principal(Harman74.cor$cov,nfactors=5,rotate="none") # 주축인자법
pc$loadings
pc$communality
pc.rot <- principal(Harman74.cor$cov,nfactors=5,rotate="promax")
pc.rot$loadings
pc.rot$communality
plot(pc.rot$loadings)
text(pc.rot$loadings, labels = colnames(Harman74.cor$cov),cex=0.5)


## ----------------------------------------------------------------
install.packages(c("mclust","ggplot2","caret","ROCR","rpart","randomForest"))


## ----------------------------------------------------------------
set.seed(1)
nc=2;n=50    # 2개의 군집, 50개의 관측치
x=matrix(rnorm(n*nc),ncol=nc)   # 모의실험 자료 생성
x[1:20,1]=x[1:20,1]+1   
x[21:n,1]=x[21:n,1]-2
plot(x,cex=2)
par(mfrow=c(1,2))
km = kmeans(x,2)   # 2개의 군집으로 분할
plot(x,pch=km$cluster,col=km$cluster,main="k-means clustering with 2 clusters",cex=2)
km = kmeans(x,3)   
plot(x,pch=km$cluster,col=km$cluster,main="k-means clustering with 3 clusters",cex=2)


## ----------------------------------------------------------------
set.seed(5)
xdata=iris[,c(2,4)]
km1 = kmeans(xdata,3) # 3개의 군집으로 분류 (1차)
km1$withinss          # 군집내제곱합
km2 = kmeans(xdata,3) # 3개의 군집으로 분류 (2차)  
km2$withinss          # 군집내제곱합
par(mfrow=c(1,2))
plot(xdata,pch=km1$cluster,col=km1$cluster,cex=2)
plot(xdata,pch=km2$cluster,col=km2$cluster,cex=2)


## ----------------------------------------------------------------
data("iris"); iris
#데이터구조
str(iris)
set.seed(2019)
xdata=iris[,c(2,4)]   # 2개의 변수만 선택
km.res = kmeans(xdata,3)  # 3개의 군집으로 분할
km.res
km.res$betweenss/km.res$totss   # information with 1 start
# 초기치를 다르게 하여 25회 반복수행 후 최적결과 저장
km.res25 = kmeans(xdata,3,nstart = 25)
# information with 25 starts
km.res25$betweenss/km.res25$totss 
km.res25$cluster  # 분류된 군집 번호
km.res25$centers  # 각 군집의 중심
km.res25$size   # 군집별 관측치의 개수
# 군집화 결과
plot(xdata,pch=20,col=km.res25$cluster+1
     ,cex=1.5,main="k-means clustering with 3 clusters")


## ----------------------------------------------------------------
N = 10      
infom = c()     
# 군집의 개수를 1~10까지 변화시키며 수행
for (i in 1:N)
{
  km.out = kmeans(xdata,i,nstart=10)
  infom[i] = km.out$betweenss/km.out$totss
}   
plot(1:N,infom, type="b",col=2,pch=16
     ,xlab="number of clusters",ylab="information")


## ----------------------------------------------------------------
data("iris"); iris
xdata=iris[,c(2,4)]   
# 계층적 군집분석 수행. 최장연결법-유클리디안 거리
hc.res = hclust(dist(xdata),method="complete")  
hc.res
plot(hc.res,hang=-1,labels=FALSE) # dendrogram
(ct = cutree(hc.res,k=2:4)) # 2 to 4 clusters
table(k2=ct[,"2"],k4=ct[,"4"]) # 군집화 결과 요약 
par(mfrow=c(1,2))
plot(xdata,pch=20,cex=2,col=ct[,"2"],main="2 clusters")
plot(xdata,pch=20,cex=2,col=ct[,"4"],main="4 clusters")


## ----------------------------------------------------------------
set.seed(1)
n=100
p=0.4  # 혼합비율
u=rbinom(n,1,p)  # cluster index
x1=rnorm(n,10,1)  # cluster 1
x2=rnorm(n,17,1.5) # cluster 2
x=u*x1+(1-u)*x2    # 관측치
plot(density(x))   

library(mclust)    
M1=Mclust(x,G=2)  # K=2 
M1$parameters     # 추정 모수
M1$z[,1]>0.5
M2=Mclust(x,G=2,modelNames ="E")  # K=2 with equal variance
M2$parameters     # 추정 모수
M2$z[,2]>0.5

# 클수록 좋다
M1$bic
M2$bic
 

## ----------------------------------------------------------------
# 자료 불러오기i
challenger <- read.csv('https://bit.ly/2YPCg4V')
str(challenger)
challenger
# 간단한 시각화 
library(ggplot2)   
# 온도와 실패횟수의 관계
ggplot(challenger,aes(temperature,distress_ct)) + geom_point(color="red",size=3)
ggplot(challenger,aes(factor(distress_ct),temperature)) + geom_boxplot()
# 반응변수 정의
xm = cbind(challenger$distress_ct,challenger$o_ring_ct-challenger$distress_ct)
fit = glm(xm~temperature, data=challenger, family="binomial")
summary(fit)
# 예측확률
predict(fit,data.frame(temperature=30))    # linear predictor
predict(fit,data.frame(temperature=30), type="response") # estimated prob.


## ----------------------------------------------------------------
# 자료 불러오기
adult <- read.table('https://bit.ly/2zRvsrQ',sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin', 'educatoin_num', 
                  'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                  'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 
                  'income')
str(adult)
# 간단한 시각화 
(f1 =  ggplot(adult) + aes(x=as.numeric(age),  fill=income) + 
  geom_density(alpha=0.5))
f1 + facet_grid(race~sex)
# 데이터 분할
set.seed(0716)
(n = length(adult$income))
idx = sample(1:n,size=round(n*0.7)) # 훈련자료 index
training = adult[idx,]   # 훈련자료
test = adult[-idx,]      # 평가자료
# 모형 적합
fit1 = glm(factor(income)~race+age+sex+hours_per_week+educatoin_num, data=training, 
           family="binomial")
summary(fit1)
(p1=predict(fit1, newdata=test[1:5,], type="response")) # prediction
p1>0.5
# 관측값과 예측값의 비교
yobs=test$income
yhat=predict(fit1, newdata=test, type="response")
ggplot(data.frame(yobs,yhat),aes(yobs,yhat,group=yobs,fill=yobs)) + geom_boxplot()
# 혼동행렬
install.packages("caret")
library(caret)
yhat1 = as.factor(ifelse(yhat>0.5,">50K","<=50K"))
confusionMatrix(yhat1, factor(yobs))
table(yhat1,yobs)
# ROC 곡선
install.packages("ROCR")
library(ROCR)
pred = prediction(yhat,yobs)
perf = performance(pred,measure = "tpr", x.measure = "fpr")
plot(perf, col=2, main="ROC curve")
abline(a=0,b=1)
performance(pred,"auc")@y.values


## ----------------------------------------------------------------
# 나무모형 적합
library(rpart)
(dt = rpart(income~race+age+sex+hours_per_week+educatoin_num, data=training))
printcp(dt); summary(dt)
plot(dt)
text(dt, use.n=TRUE)
# 모형평가
yhat_tr = predict(dt,test)[,">50K"]
ggplot(data.frame(yobs,yhat_tr),aes(yobs,yhat_tr,group=yobs,fill=yobs)) +
  geom_boxplot()
# 혼동행렬
yhat_tr1 = as.factor(ifelse(yhat_tr>0.5,">50K","<=50K"))
confusionMatrix(yhat_tr1, factor(yobs))
table(yhat_tr1,yobs)
# ROC 곡선
library(ROCR)
pred_tr = prediction(yhat_tr,yobs)
perf_tr = performance(pred_tr,measure = "tpr", x.measure = "fpr")
plot(perf_tr, col=2, main="ROC curve")
plot(perf, col=4, add=TRUE) # 로지스틱과의 비교
abline(a=0,b=1)
performance(pred_tr,"auc")@y.values


## ----------------------------------------------------------------
# 랜덤포레스트
# install.packages("randomForest")
library(randomForest)
(rf = randomForest(factor(income)~race+age+sex+hours_per_week+educatoin_num, data=training))
varImpPlot(rf)  # 변수중요도 
# 모형평가
yhat_rf = predict(rf,test,type="prob")[,">50K"]
pred_rf = prediction(yhat_rf,yobs)
perf_rf = performance(pred_rf,measure = "tpr", x.measure = "fpr")
plot(perf_rf, col=3, main="ROC curve")
plot(perf, col=2, add=TRUE) # 로지스틱과의 비교
plot(perf_tr, col=4, add=TRUE) # 단일나무모형과의 비교
abline(a=0,b=1)
performance(pred_rf,"auc")@y.values

?randomForest
# tree개수 바꿔보기
(rf = randomForest(factor(income)~race+age+sex+hours_per_week+educatoin_num, , ntree = 2000, data=training))
varImpPlot(rf)  # 변수중요도 
# 모형평가
yhat_rf = predict(rf,test,type="prob")[,">50K"]
pred_rf = prediction(yhat_rf,yobs)
perf_rf = performance(pred_rf,measure = "tpr", x.measure = "fpr")
plot(perf_rf, col=3, main="ROC curve")
plot(perf, col=2, add=TRUE) # 로지스틱과의 비교
plot(perf_tr, col=4, add=TRUE) # 단일나무모형과의 비교
abline(a=0,b=1)
performance(pred_rf,"auc")@y.values

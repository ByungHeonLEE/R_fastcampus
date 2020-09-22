#현업에서의 작업 프로세스 
#1. 아이디어 도출 및 구체화
#2. 데이터 분석가 작업
#데이터 추출, 전처리, 모델링, 결과 추출 ,추천용 데이터셋 구성, 추천 로직 정리
#3. 개발자 작업
#추천 로직 대로 화면 에 노출되도록 개발
#가설1: 고객들은 반복구매를 할 때 유사한 종류의 와인을 구매할 것이다 
#가설2: 고객들은 간헐적으로 접해보지 못한 종류의 와인을 구매할 것이다 

#와인들 간의 유사도(거리)계산
#군집분석을 통해 와인 분류
#분류된 와인을 선택하여 배치
#고객들의 반응 확인ㅇ
library(dplyr)
library(ggplot2)
getwd()
setwd('/Users/topkids/dev/R/r_fastcampus/R_fastcampus')
df <- read.csv('wine.data.csv')
colnames(df) <- c('Cultivar', 'Alcohol', 'Malic acid', 'Ash', 'Alcalinity of ash', 'Magnesium', 'Total phenols', 'Flavanoids', 'Nonflavanoid phenols', 'Proanthocyanins','Proanthocyanins', 'Hue', 'OD280/OD315 of diluted wines', 'Proline')
str(df)
colSums(is.na(df))
par(mfrow=(c(1,2)))
boxplot(df[,-1], ylim=c(0, 30),  cex.axis=0.5)
df.train <- df[,-1]#cultivar 제외
df.train.scale <- scale(df.train)
df.train
#유클리디언 거리 구하기
df.dist <- dist(df.train.scale, method = "euclidean") %>% as.matrix()
df.dist
head(df.dist)
#k-means 군집분석
install.packages('factoextra')
library(factoextra)
set.seed(1234)
fviz_nbclust(df.train.scale, kmeans, method = 'wss', k.max = 15) + theme_minimal() + ggtitle("the elbow") #elbow-method
fviz_nbclust(df.train.scale, kmeans, method = 'silhouette', k.max = 15) + theme_minimal() + ggtitle("silhouette")#silhouette

#군집화
df.kmeans = kmeans(df.train.scale, centers = 3, iter.max = 1000)
install.packages('useful')
library(useful)
plot(df.kmeans, df.train.scale)
#2차원 그ㅍ래프로 군집별 데이터 확인

df.kmeans$centers
barplot(t(df.kmeans$centers), beside = T, col = 2:14)
legend("topleft", colnames(df.train.scale), fill = 2:14, cex = 0.3, bty = 'n')
df$kmeans_cluster <- df.kmeans$cluster
head(df)

#k-medoiod
#partitional algorithm method
fviz_nbclust(df.train.scale, pam, method = 'wss', k.max = 15) + theme_minimal() + ggtitle("the elbow") #elbow-method
fviz_nbclust(df.train.scale, pam, method = 'silhouette', k.max = 15) + theme_minimal() + ggtitle("silhouette")#silhouette

library(cluster)
df.kmedoids <- pam(df.train.scale, k = 3)
plot(df.kmedoids)
barplot(t(df.kmedoids$medoids), beside = T, col = 2:14, names.arg = c(1:3))
legend("bottomleft", colnames(df.train.scale), fill = 2:14, cex = 0.3, bty = 'n')

#hierarchical clustering
fviz_nbclust(df.train.scale, hcut, method = 'wss', k.max = 15) + theme_minimal() + ggtitle("the elbow") #elbow-method
fviz_nbclust(df.train.scale, hcut, method = 'silhouette', k.max = 15) + theme_minimal() + ggtitle("silhouette")#silhouette
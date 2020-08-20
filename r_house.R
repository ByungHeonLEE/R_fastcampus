setwd('/R_fastcampus')
getwd()
setwd("/Users/topkids/dev/R/r_fastcampus/R_fastcampus")
region_cd <- read.csv('region_cd.csv', encoding = 'UTF-8')

install.packages('dplyr')
install.packages('Tmisc')
install.packages('XML')

library(dplyr)
library(Tmisc)#%like% 쓰려고
library(XML)#api 쓰려고 


LAWD_CD <- region_cd %>% filter(`region` %like% '서울특별시')#들어가는 것들을 필터 해줘라
LAWD_CD <- LAWD_CD[,2]

temp <- merge(c(2010:2020), c(1:12)) #연 월 떙기고 
temp$y <- if_else(temp$y<10, paste0(0, temp$y), as.character(temp$y))
#연 월 필요하니 10보다 작으면 0을 붙여준다
DEAL_YMD <- paste0(temp$x, temp$y) %>% as.integer()

head(temp)
head(DEAL_YMD)

df <- NULL
API_KEY <- 'wOct3P8euWcmrtpLz3lm%2Bkx6tZxx0LYdnkO4HW1HgBAAOnI3NPAe6lSlWMpMdoWvhugMlr9aBgRWR1cSlDA0Jg%3D%3D'
for(i in LAWD_CD){
  print(i)
  for (l in DEAL_YMD){
    url <- paste0('http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=',API_KEY,'&LAWD_CD=',i,'&DEAL_YMD=',l,'')
    raw.data <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "utf-8")
    xml_rootNode <- try(xmlRoot(raw.data))
    xml_result <- xmlToDataFrame(xml_rootNode[[2]][['items']])
    df <- rbind(df, xml_result)}}
df$거래금액 <- 
  gsub(',', '', df$거래금액) %>% as.integer()
df$법정동 <- gsub(' ', '', df$법정동)
df$qrt <- ifelse(df$월 < 4, 'Q1', ifelse(df$월 < 7, 'Q2', ifelse(df$월 < 10, 'Q3', 'Q4')))
df$전용면적 <- as.numeric(df$전용면적)
df$yyyyqrt <- paste0(df$년, df$qrt)
df$평수 <- round(df$전용면적/3.3)
df$평단가 <- df$거래금액/df$평수
#가설1을 검정
df.1 <- df %>% group_by(yyyyqrt) %>% summarise(`평균평단가` = mean(`평단가`))
library(dplyr)
library(ggplot2)
theme_set(theme_grey(base_family='NanumGothic'))
ggplot(df.1, aes(x=yyyyqrt, y = `평균평단가`, group = 1)) + geom_line() + xlab("년도/분기")+ylab("평균 가격(만원") + ggtitle("서울 아파트 평당 가격 변화 추이 ") +  stat_smooth(method='lm') + ylim(0, max(df.1$평균평단가))
df
dong <- '무악동'
df.2 <- df %>% filter(`법정동` == dong)
View(df.2)
df.2 <- df.2 %>% group_by(yyyyqrt) %>% summarise(`평균평단가` = mean(`평단가`))
View(df.2)
df.2.plot <- ggplot(df.2, aes(x=yyyyqrt, y = `평균평단가`, group = 1)) + geom_line() + xlab("년도/분기")+ylab("평균 가격(만원") + ggtitle("서울 아파트 평당 가격 변화 추이 ") + theme(axis.text.x=element_text(angle=90))+ stat_smooth(method='lm') + ylim(0, max(df.2$평균평단가))

df.2.plot
#그래프여러개 보는법
#library(gridExtra)
#grid.arrange(df.2.plot,..., nrow= =2, ncol=2)
View(df)
#
df.fil <- df %>% filter(!yyyyqrt %in% c('2020Q1', '2020Q2', '2020Q4'))
dong.list <- unique(df$법정동)
df.rs <- NULL

install.packages('TTR')
library(TTR)
for (i in dong.list){
  print(i)
  temp <- merge(i, df.fil %>% filter(`법정동` == i) %>%
                  group_by(yyyyqrt) %>% 
                  summarise(`평균평단가` = mean(`평단가`)) %>% 
                  mutate(ma3 = runMean(`평균평단가`, 3)))
  df.rs <- rbind(df.rs, temp) %>% na.omit()
}
df.rs

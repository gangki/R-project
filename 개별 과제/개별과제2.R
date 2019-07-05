# 1. NAVER 영화('그린북') 일반인 리뷰 크롤링
install.packages('xlsx')
install.packages('KoNLP')
install.packages('wordcloud2')
install.packages('rJava')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('dplyr')
library(KoNLP)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rJava)
library(wordcloud2)
library(dplyr)
library(rvest)
library(dplyr)
library(stringr)
library(xlsx)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

url_base <- 'https://movie.naver.com'
start_url <- '/movie/bi/mi/point.nhn?code=171539#tab'
url <- paste0(url_base, start_url, encoding="euc-kr")
html <- read_html(url)
page <- '&page='

html %>%
  html_node('iframe.ifr') %>%
  html_attr('src') -> url2
ifr_url <- paste0(url_base, url2) 
html2 <- read_html(ifr_url)
html2 %>%
  html_node('div.score_total') %>%
  html_node('strong.total') %>%
  html_text('em') %>%
  trim() -> n

str_sub(n, 9, 13) -> total #숫자 추출
total1 <- as.numeric(gsub(",", "", total))
str(total1)
total2 <- ceiling(total1/10) # 소수점 이하 올림처리

i <- 1
df_reviews <- data.frame(score=c(), review=c(), writer=c(), time=c())

for (i in 1:total2){
  ifr_url_page <- paste0(url_base, url2, page, i) 
  html2 <- read_html(ifr_url_page)
  html2 %>%
    html_node('div.score_result') %>%
    html_nodes('li') -> lis
  
  score <- c()
  review <- c()
  writer <- c()
  time <- c()
  
  for (li in lis) {
    score <- c(score, html_node(li, '.star_score') %>% html_text('em') %>% trim())
    li %>%
      html_node('.score_reple') %>%
      html_text('p') %>%
      trim() -> tmp
    idx <- str_locate(tmp, "\r")
    review <- c(review, str_sub(tmp, 1, idx[1]-1))
    tmp <- trim(str_sub(tmp, idx[1], -1))
    idx <- str_locate(tmp, "\r")
    writer <- c(writer, str_sub(tmp, 1, idx[1]-1))
    tmp <- trim(str_sub(tmp, idx[1], -1))
    idx <- str_locate(tmp, "\r")
    time <- c(time, str_sub(tmp, 1, idx[1]-1))
  }
  review = data.frame(score=score, review=review, writer=writer, time=time)
  df_reviews <- rbind.data.frame(df_reviews, review)
}
head(df_reviews)

write.xlsx(df_reviews, file="D:/Workspace/R-project/01_Crawling/reviews_GreenBook.xlsx", 
           sheetName="네티즌평점", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

# 2. 네티즌 리플 워드클라우드 생성
getwd()
setwd('D:/Workspace/R-project/01_Crawling')
data <- read.csv('reviews_GreenBook.csv', stringsAsFactors = F)
str(data)

data1 <- data %>% select(review)
data2 <- sapply(data1, extractNoun, USE.NAMES = F) # 단어 추출
data3 <- unlist(data2)
head(data3, 100)

# 데이터 전처리 1
data3 <- gsub(" ","", data3)     # 공백 제거
data3 <- gsub("\\d+","", data3)  # 숫자 제거
data3 <-  gsub("[][!#$%()*,.:;<=>@^_`|~.{}]", "", data3) # 특수문자 제거
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) # 한글자 제거

# 공백 제거
write(unlist(data3),"reviews_Greenbook.txt")
data4 <- read.table("reviews_Greenbook.txt")
head(sort(table(data4), decreasing = T), 100)

# 제거 단어 목록
txt <- readLines("greenbook_gsub.txt") 
cnt_txt <- length(txt)

# 데이터 전처리 2
i <- 1
for(i in 1:cnt_txt) {
  data3 <- gsub((txt[i]), "", data3)
}
# 공백 제거 2
write(unlist(data3),"reviews_Greenbook_1.txt")
data5 <- read.table("reviews_Greenbook_1.txt")
head(sort(table(data5), decreasing = T), 100)
nrow(data5) 

wordcount <- head(sort(table(data5), decreasing = T), 500)
wordcloud2(wordcount, color = "random-light", backgroundColor = "black", size = 1)

# 3. 일자별/시간대별 평점 분석

# 문자열 datetime -> date, time 으로 분리 및 요일 생성
data_1 <- data %>%
  mutate(DateTime = ymd_hm(time)) %>%
  mutate(Dates = format(as.POSIXct(strptime(DateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")) %>%
  mutate(Hours = format(as.POSIXct(strptime(DateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")) %>%
  mutate(Days = weekdays(as.Date(DateTime))) %>%
  select(score, Dates, Hours, Days)
str(data_1) ; head(data_1)

# 날짜별 평균
data_Dates <- data_1 %>%
  mutate(Dates = format(as.POSIXct(strptime(Dates,"%Y-%m-%d",tz="")) ,format = "%m/%d")) %>%
  group_by(Dates) %>%
  summarise(score_mean = mean(score))
str(data_Dates); head(data_Dates)

# 시간별 평균
data_Hours <- data_1 %>%
  mutate(Hours = format(as.POSIXct(strptime(Hours,"%H:%M:%S",tz="")) ,format = "%H")) %>% 
  group_by(Hours) %>%
  summarise(score_mean = mean(score))
str(data_Hours); head(data_Hours)

# (기타)월별 평균
data_Months <- data_1 %>%
  mutate(Months = format(as.POSIXct(strptime(Dates,"%Y-%m-%d",tz="")) ,format = "%y-%m")) %>%
  group_by(Months) %>%
  summarise(score_mean = mean(score))
str(data_Months); head(data_Months)

# (기타)요일별 평균
data_Days <- data_1 %>%
  group_by(Days) %>%
  summarise(score_mean = mean(score))
str(data_Days); head(data_Days)

# 날짜별 평점 추이
ggplot(data_Dates, aes(Dates, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 0.5) +
  ggtitle("GreenBook 날짜별 평점 추이") +
  labs(x = "날짜", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 5))
str(data_Dates) ; head(data_Dates)

# 시간대별 평점 추이
ggplot(data_Hours, aes(Hours, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 1) +
  ggtitle("GreenBook 시간별대 평점 추이") +
  labs(x = "시간", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text.x = element_text(angle=0, hjust=1, size = 8))

# (기타)월별 평점 추이
ggplot(data_Months, aes(Months, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 1) +
  ggtitle("GreenBook 월별 평점 추이") +
  labs(x = "월", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text = element_text(size = 10))

# (기타)요일별 평점 추이
ggplot(data_Days, aes(Days, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 1) +
  ggtitle("GreenBook 요일별 평점 추이") +
  labs(x = "요일", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text = element_text(size = 10)) +
  scale_x_discrete(limits=c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))

# ggplot(data_Days, aes(Days, score_mean, fill = Days )) + 
# geom_bar(stat = "identity") +
# scale_x_discrete(limits=c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))



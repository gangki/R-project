# NAVER 영화('그린북') 일반인 리뷰 크롤링
install.packages('xlsx')
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

# 워드클라우드
install.packages('KoNLP')
install.packages('wordcloud2')
install.packages('rJava')
library(rJava)
library(wordcloud2)
library(KoNLP)
library(dplyr)

getwd()
setwd('D:/Workspace/R-project/01_Crawling')
data <- read.csv('reviews_GreenBook.csv', stringsAsFactors = F)
str(data)

data1 <- data %>% select(review)
data2 <- sapply(data1, extractNoun, USE.NAMES = F)
head(unlist(data2), 50)
data3 <- unlist(data2)
head(data3, 30)

data3 <- gsub("\\d+","", data3)  # 숫자 제거
data3 <- gsub(" ","", data3) # 공백 제거
data3 <- gsub("-","", data3) # - 제거
data3 <- Filter(function(x) {nchar(x) >= 2}, data3) # 한글자 제거

write(unlist(data3),"reviews_Greenbook.txt")
data4 <- read.table("reviews_Greenbook.txt")
nrow(data4) # 총 18,125 단어 추출
wordcount <- table(data4)
head(sort(wordcount, decreasing=T),100)

txt <- readLines("greenbook_gsub.txt") # 제거 단어 목록
cnt_txt <- length(txt)

i <- 1
for(i in 1:cnt_txt) {
  data3 <- gsub((txt[i]), "", data3)
}

write(unlist(data3),"reviews_Greenbook.txt")
data4 <- read.table("reviews_Greenbook.txt")
nrow(data4) # 총 14,980 단어 추출
wordcount <- table(data4)
head(sort(wordcount, decreasing=T),100)

wordcount1 <- as.data.frame(wordcount) 
head(arrange(wordcount1, desc(Freq)),100)

wordcount2 <- wordcount1 %>% filter(Freq >= 22)
head(arrange(wordcount2, desc(Freq)),100)

wordcloud2(wordcount2, color = "random-light", backgroundColor = "white", shape = 'circle')

# 일자별/시간대별 평점 분석
install.packages('lubridate')
install.packages('ggplot2')
install.packages('dplyr')
library(dplyr)
library(lubridate)
library(ggplot2)

data_1 <- data %>%
  mutate(DateTime = ymd_hm(time)) %>%
  mutate(Dates = format(as.POSIXct(strptime(DateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")) %>%
  mutate(Hours = format(as.POSIXct(strptime(DateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")) %>%
  mutate(Days = weekdays(as.Date(DateTime)))

data_Dates <- data_1 %>%
  mutate(Dates = format(as.POSIXct(strptime(Dates,"%Y-%m-%d",tz="")) ,format = "%m/%d")) %>%
  group_by(Dates) %>%
  summarise(score_mean = mean(score))

data_Hours <- data_1 %>%
  mutate(Hours = format(as.POSIXct(strptime(Hours,"%H:%M:%S",tz="")) ,format = "%H")) %>% 
  group_by(Hours) %>%
  summarise(score_mean = mean(score))

data_Months <- data_1 %>%
  mutate(Months = format(as.POSIXct(strptime(Dates,"%Y-%m-%d",tz="")) ,format = "%Y-%m")) %>%
  group_by(Months) %>%
  summarise(score_mean = mean(score))

data_Days <- data_1 %>%
  group_by(Days) %>%
  summarise(score_mean = mean(score))

head(data_Hours, 10)
str(data_1)
head(data_1)
head(arrange(data_1, desc(Dates)),100)

ggplot(data_Dates, aes(Dates, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 1) +
  ggtitle("GreenBook 날짜별 평점") +
  labs(x = "날짜", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 8))

ggplot(data_Hours, aes(Hours, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 1) +
  ggtitle("GreenBook 시간별 평점") +
  labs(x = "시간", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text.x = element_text(angle=0, hjust=1, size = 8))

ggplot(data_Months, aes(Months, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 1) +
  ggtitle("GreenBook 월별 평점") +
  labs(x = "월", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text = element_text(size = 10))

ggplot(data_Days, aes(Days, score_mean, group = 1)) + 
  geom_line(color = 'red', size = 1) +
  ggtitle("GreenBook 요일별 평점") +
  labs(x = "요일", y = "평점") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +
  theme(axis.title = element_text(face = "bold", size = 10)) +
  theme(axis.text = element_text(size = 10)) +
  scale_x_discrete(limits=c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))
  
# ggplot(data_Days, aes(Days, score_mean, fill = Days )) + 
# geom_bar(stat = "identity") +
# scale_x_discrete(limits=c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))



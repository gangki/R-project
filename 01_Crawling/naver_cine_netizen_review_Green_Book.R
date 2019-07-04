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



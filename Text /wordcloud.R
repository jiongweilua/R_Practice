library(topicmodels)
library(quanteda)
library(readtext)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)

cv <- readtext('/Users/luajiongwei/Downloads/cv')
cv_corpus <- corpus(cv)
toks <- tokens(cv_corpus, remove_numbers = T, remove_separators = T, remove_punct = T)
head(toks)
cv_dfm <- dfm(cv_corpus)
cv_dfm <- dfm_select(cv_dfm, stopwords("en"), selection = 'remove')
cv_dfm <- dfm_trim(cv_dfm, max_docfreq = 0.1)

cv_lda <- LDA(cv_dfm, k = 3)
cv_topics <- tidy(cv_lda, matrix = "beta")

terms <- cv_topics %>% group_by(topic) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

df_1 <- data.frame(terms %>% filter(topic == 1) %>% select(term, beta) %>% arrange(-beta) )
df_1 %>% head()
wordcloud(words = df_1$term,
          freq = df_1$beta,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))

df_2 <- data.frame(terms %>% filter(topic == 2) %>% select(term, beta) %>% arrange(-beta) )
df_2 %>% head()
wordcloud(words = df_2$term,
          freq = df_2$beta,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))

df_3 <- data.frame(terms %>% filter(topic == 3) %>% select(term, beta) %>% arrange(-beta) )
df_3 %>% head()
wordcloud(words = df_3$term,
          freq = df_3$beta,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors=brewer.pal(8, "Dark2"))

install.packages(c('httr','jsonlite'))
library(httr)
library(jsonlite)
username <- 'j.w.lua@lse.ac.uk'
password <- 'Jiongweisayshi890'
access_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjE2NTUsInVzZXJfaWQiOjE2NTUsImVtYWlsIjoiai53Lmx1YUBsc2UuYWMudWsiLCJmb3JldmVyIjpmYWxzZSwiaXNzIjoiaHR0cDpcL1wvb20yLmRmZS5vbmVtYXAuc2dcL2FwaVwvdjJcL3VzZXJcL3Nlc3Npb24iLCJpYXQiOjE1MzEwMzY4MTcsImV4cCI6MTUzMTQ2ODgxNywibmJmIjoxNTMxMDM2ODE3LCJqdGkiOiI1MzFjOWJkODg4ZDhjZmM5Y2QyMTdiYWVjZTQwMWI0MSJ9.rxIZRDeiaaeGCyh-Tl7OaRQCVGgZYzEqsmsFHnAX1E4"
planning_area_list <- GET(url = 'http://developers.onemap.sg/privateapi/popapi/getPlanningareaNames',
                          token = access_token)

planning_area_list
names(planning_area_list)
httr::content(planning_area_list)
onemap <- GET(url= 'http://developers.onemap.sg/privateapi/popapi/getEconomicStatus',
              token = access_token,
              year = 2010, 
              planning-AREA 
)



#Yamaha Midterm Exam Talish Barmare R codes used.

library(tidyverse)
library(rtweet)
library(lubridate)
library(scales)
library(dplyr)
library(tidytext)

#  Install Requried Packages for sentiment analysis
installed.packages("SnowballC")
installed.packages("tm")
installed.packages("twitteR")
installed.packages("syuzhet")
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")


#extraction code

## install dev version of rtweet from github
devtools::install_github("mkearney/rtweet")
install.packages("maps")
## load rtweet package
library(rtweet)
library(maps)


## access token method: create token and save it as an environment variable
create_token(
  app = "Test_of_the_API_platform",
  consumer_key = 'plWdlTcT9mTCqQrdAcp2GF45f',
  consumer_secret =  'poUnlq3n1tbSjwcKpQ6jIZwwusL1SU0npEKRDeGBcoA0xw5FKv',
  access_token = '14122740-mZmHHiWrmiVcfQjurTTfnEQTcM3jiATPQ1NouT9Rw',
  access_secret = '3SbTYrdWGWOOQON1lzxMZzfhXQa6X7T0DFz8q5v0mTnAS')

## Google API key for accessing geo location data through Google Maps
westland_api <- 'AIzaSyCErk3aBmPoG1qNUz6elhD6ZrR2MQtN7W0'

ymh_tweets <- search_tweets(
  "piano OR yamahapianoNU1X OR  OR digitalpiano",
  n = 18000, include_rts = F, 
  retryonratelimit =T
)

ymh_tweets1 <- search_tweets(
  "piano OR pianocorder OR  OR digitalpiano",
  n = 18000, include_rts = F, 
  retryonratelimit =T
)


ymh_tweets2 <- search_tweets(
  "piano OR 'grand piano' OR 'yamaha piano' OR yamahapiano OR music OR digitalpiano",
  n = 18000, include_rts = F, retryonratelimit =T )

ymh_tweets3 <- search_tweets(
  "Avantgrand OR piano OR grandpiano OR digitalpiano OR yamahapiano OR pianocorder",
  n = 18000, include_rts = F, 
  retryonratelimit =T
)


ymh_tweets5 <- search_tweets(
  "piano OR pianocorder OR N1Ux OR digitalpiano",
  n = 18000, include_rts = F, 
  retryonratelimit =T
)

ymh_tweets3 <- search_tweets(
  "piano OR 'grand piano' OR 'yamaha piano' OR yamahapiano OR music OR digitalpiano",
  n = 18000, include_rts = F, retryonratelimit =T )

ymh_tweets4 <- search_tweets(
  "disklavier OR piano OR grandpiano OR digitalpiano OR yamahapiano OR pianocorder",
  n = 18000, include_rts = F, 
  retryonratelimit =T
)

ymh_tweets7 <- search_tweets(
  " avantgrand OR piano sales OR piano music OR piano lessons OR hybrid pianos OR YamahaMusicEU OR Electronic Pianos OR acoustic sound OR digital sound OR NU1 OR NU1X OR N3X OR pianist OR pianist magazine OR Yamaha AvantGrand Hybrid Piano OR hybrid pianos OR Pianist Magazine OR grand piano OR Pianocorder",
  n = 18000, include_rts = F, 
  retryonratelimit =T
)

ymh_tweets6 <- search_tweets(
  " avantgrand OR piano sales OR piano music OR piano lessons OR hybrid pianos OR YamahaMusicEU OR Electronic Pianos OR acoustic sound OR digital sound OR NU1 OR NU1X OR N3X OR pianist OR pianist magazine OR Yamaha AvantGrand Hybrid Piano OR hybrid pianos OR Pianist Magazine OR grand piano OR Pianocorder",
  n = 18000, include_rts = F, 
  retryonratelimit =T
)


#combine all the extracted data
tweetdatafile <- rbind(ymh_tweets1,ymh_tweets2,ymh_tweets3,ymh_tweets5,ymh_tweets7,ymh_tweets6,ymh_tweets)
dim(tweetdatafile)

#remove japanese lang tweets
tweetdatafile1 <- tweetdatafile

tweetdatafile1 <- tweetdatafile1[tweetdatafile1$account_lang != "ja", ]

(tweetdatafile1)

#choosing columns with relevance information
tweets <- tweetdatafile1 %>%
  select(screen_name, name, created_at, location, source, text)  


## TEXT ANALYSIS WITH tidytext to extract a list of words
install.packages('tidytext')
library(tidytext)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words

#wordcloud for keywords of tweets

tweet_words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 70))

#word cloud for source of tweets
sourcecloud <- tweet_words %>%
  anti_join(stop_words) %>%
  count(source) %>%
  with(wordcloud(source, n, max.words = 40))


#word cloud for screen name
screennamecloud1 <- tweetdatafile1 %>%
  count(screen_name) %>%
  with(wordcloud(screen_name, n, max.words = 10))

#word cloud for location

locationcloud1 <- tweetdatafile1 %>%
  count(location) %>%
  with(wordcloud(location, n, max.words = 40))

# graph to understand timespent by source
tweetdatafile1 %>%
  filter(source %in% c("Twitter for Android", "Twitter Web Client", "Twitter for iPhone", "Instagram")) %>%
  count(source, second = second(with_tz(created_at, "EST"))) %>%  ## hour, minute and second come from lubridate package
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(second, percent, color = source)) +
  geom_line()

#sentiment analysis
word.df <- as.vector(tweetdatafile1$text)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweetdatafile1, emotion.df) 

head(emotion.df2)

#The above output shows us the different emotions present in each of the tweets.
#Now, we will use the get_sentiment function to extract sentiment score for each of the tweets

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]

most.positive

#most.positive tweet
#"no but it must be so beautiful and interesting to work with someone as lively and passionate as jongdae;\nhe's an amazing vocalist,an amazing lyricst,got interested in composing and overall the most cheerful and brilliant artist who only wants to learn more-https://t.co/xMxsECNFzA"

#negative sentiment
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

#"@colleenpatricia @MissvalCa @gatewaypundit @AOC Octave and a 3/4 on my piano, but don't worry about my reach; try cracking a history book now and then, you might actually dispel some of your sad ignorance and avoid becoming the latest \"sucker born every minute\" for this motley crew of illiterate socialist thugs."

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)


#"KAWAI - BE MY BABY (Official Music Video) https://t.co/t9nhLfXRJ9 via @YouTube"                                                                                                                                                                                                                                 
#[3] "@reserve_dmm18 @kawai_asuna 美"                                                                                                                                                                                                                                                                                 
#[4] "Realistic Concertmate 200 (AKA Casio VL-Tone) https://t.co/vu3CNo18yg https://t.co/Mwzrm38CKK"                                                                                                                                                                                                                  
#[5] "Roland SH-101 Synth (Pristine Cond.) w/Controller, Cord, Strap, Orig. Pkg. / Box SN 320769 https://t.co/Hlr2wSjDjU https://t.co/DFgyWM79p5"                                                                                                                                                                     
#[6] "China Plates - Live performance with Yamaha Montage https://t.co/es4gj4vhOP https://t.co/4Q7Ssj3rsK"                                                                                                                                                                                                            


negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)
#[1] "Kau rasa kelas korg je ada kelas ke?? The fuck??"                                                                                                                                                
#[2] "Yamaha Puncaki Tes Pramusim MotoGP di Qatar\ncc: @YamahaIndonesia @IDMotoGP_mania \nhttps://t.co/v7zxUyQ5kS https://t.co/JgRCaUOhhI"                                                             
#[3] "Yamaha MT-15 India Launch on March 15; Bookings Open\n\nhttps://t.co/9oFGSwyh95"                                                                                                                 
#[4] "@dust_move 어으어 화질때문에 내 이미지가 뭔지 잘 모르겠어서 그런데…! 혹시 설명해줄 수 있을까…?ㅠㅜㅜㅠ 해줘서 고마워!"                                                                           
#[5] "@dust_move 앗앗 치즈 종류중에…!! 어,,, 어 나도 많이는 안먹어봐서 잘 모르겠는데 어 있어!(???) 되게 막 부드러운…! 그런…! 치즈…!\n으악 설명 잘 못하겠어 정확한 건 검색 추천해…!!!"                  
#[6] "@dust_move 헉ㄲ 그렇구나…!! 둘 다 본 적이 없어서…ㅠㅜ(카포에이라만 들어봤음)\n내 닉네임 카포에이라 관련인줄 아시는 분들 몇분 계시는데,,, 마스카포네 치즈에서 따온 거야! 내가 먹고 싶었어,,,(???)"


neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)


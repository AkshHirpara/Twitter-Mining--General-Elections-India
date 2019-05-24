#Set directory

#The problem i faced first was related to data frame. I converted tweets$posts into data frame.
#I should do it in characters.


rm(list = ls())
setwd("F:/Data Science")
getwd()

#Tokens

#install.packages('twitteR')
library('twitteR')

consumer_key="YR7GQeiRQ38SfQ8dgVrAlb0NI"
consumer_secret="7WgbZPjuYpcV0d9rdly84Sjlo3SDiFYs7E5fodTwD6w3g5aw2w "
acc_token="578608888-omniZ3P1VtPWIeYPE2uK8UrWQcw5xffDijpl8Z60"
acc_secret='A60Lxm2pEVRbJkfNxZiVTF0cqhUKyhQWSpgh93lkN0OvO'

#install.packages('rtweet')
library('rtweet')

#tweets=search_tweets("#avenge",n=27,include_rts = FALSE)
#
#tweets


tweets_modi=search_tweets("Modi",n=1000,include_rts = FALSE,since="2019-04-10",untill='2019-04-20',lang='en')

#Extract tweets for wordcloud

modi=tweets_modi$text
modi1=as.character(tweets_modi$text)
modi1=as.matrix(tweets_modi$text)
#####################################################
#Create Corpus
#install.packages("corpus")
#install.packages('tm')
library(corpus)
library(tm)

modi_corp=Corpus(VectorSource(modi1))

#Inspect any random document from corpus and verify it with original data frame
writeLines(as.character(modi_corp[6]))

#check it out
modi_corp
inspect(modi_corp)
#Remove Punctuation from Corpus
modi_corp=tm_map(modi_corp,removePunctuation)

#remove upper cases and transform to lower

modi_corp=tm_map(modi_corp,tolower)

#remove numbers
modi_corp=tm_map(modi_corp,removeNumbers)

#remove undesirable words such as the of,till etc
#modi_corp=tm_map(modi_corp,removeWords)

#remove spaces
modi_corp=tm_map(modi_corp,stripWhitespace)

#remove stop words
modi_corp=tm_map(modi_corp,removeWords,stopwords_en)

#perform Stemming
#install.packages('SnowballC')
library('SnowballC')

#Now after preprocessing convert data into Plain Text Document
tweets_modi=tm_map(modi_corp,PlainTextDocument)

#Build a Term Documment Matrix, For building into TDM again convert in corpus
tweets_modi=Corpus(VectorSource(tweets_modi))
tdm=TermDocumentMatrix(tweets_modi)
tdm=as.data.frame(t(as.matrix(tdm)))



#In dplyr library you would find rollup
library(dplyr)


#install.packages('wordcloud')
library('wordcloud')
search_result_pal<-brewer.pal(9,"Set1")
#wordcloud(tweets_modi,min.freq=10,max.words=1000,  random.order=T, colors=search_result_pal)
df=wc
wc=tm_map(tweets_modi,removeWords,c('amp','india','congress','govt','old','many',
                        'bjp','said','also','well','gone','cant','day','done','today','now','want','can','via','like',
                        'hai','app','just','even','take','know','ever','come','back','doesnt','ask','next','says',
                        'get','talk','sir','nda','exclusive','dont','tell','say','listed','put','give','never','since',
                        'need','modi','rahul','narendra','rahulgandhi','narendramodi','big','words','gandhi','see'))

#wordcloud(wc,min.words=10, max.words = 600, random.order = FALSE)
set.seed(1234)
df=wordcloud(wc, min.freq = 10,scale = c(2,0.8),
          max.words=1000, random.order=FALSE, rot.per=0.50, 
          colors=brewer.pal(8, "Dark2"),res=300)



#First build a matrix

#ts_plot(tweets_modi,'3 hours') +
 # ggplot2::theme_minimal() +
  #ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  #ggplot2::labs(
   # x = NULL, y = NULL,
    #title = "Frequency of #rstats Twitter statuses from past 9 days",
    #subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    #caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  #)


#Clustering
#install.packages('NbClust')
library('NbClust')

nbc=NbClust(modi1,min.nc = 3,max.nc = 10,method = 'Kmeans')

#On basis of Nbclust and elbow method we will select k value after that we will go for kmeans clustering

Kmeansmodel=kmeans(modi1,3,nstart = 25)

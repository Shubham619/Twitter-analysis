packs<-c("slam","topicmodels","tm","wordcloud","twitteR","RYandexTranslate","textcat","syuzhet")

instd_packs<-packs %in% installed.packages()

 for(i in 1:length(instd_packs)){
   if(instd_packs[i]==FALSE){
     install.packages(packs[i])
   }
 } 
 
 for(i in packs){
   library(i,character.only = TRUE)
 }
 
 

 consumer_key<-readline("Enter the consumer_key")
 #G3oOAX9WH11dCPKU3ISzw253Q (API key)
 
 
 
 consumer_secret<-readline("Enter the consumer_secret")
 #8jld82Pbc0tLz5arq4fSps5pSSlNOHGTHPiUQccxq8Hqzq6QqM (API secret key)
 
 access_token<-readline("Enter the access_token")
 #441550750-nQBw6A06qkZXPl7MTvpOdBPBX2a1r6BshqFw2c9t (Access token)
 
 
 
 
 access_secret<-readline("Enter the access_secret")
 #ipd3PaewrO12c9IoNlD1ERUMcKKqC3GOYVyibGdgwVlys (Access token secret)
 
 
 
 setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
 n1=as.integer(readline("Enter the number of observations you want to process"))
 start_date<-as.Date.character(readline("Enter the date from when you want the data(YYYY-MM-DD)"))
 until_date<- as.Date.character(readline("Enter the date upto when you want the data(YYYY-MM-DD)"))
 Topic<-gsub(" ","",paste("#", as.character(readline("Enter the topic name which to "))))

 tw = twitteR::searchTwitter(as.character(Topic), n = n1, since = as.character(start_date),until = as.character(until_date),lang = "en")
 tweets.df = twitteR::twListToDF(tw) 

 
 
 cleanL<-function (x){
   
   tweets.df$text=gsub("&amp", "", tweets.df$text)
   tweets.df$text = gsub("&amp", "", tweets.df$text)
   tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
   tweets.df$text = gsub("@\\w+", "", tweets.df$text)
   tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
   tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
   tweets.df$text = gsub("http\\w+", "", tweets.df$text)
   tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
   tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)
   tweets.df$text = gsub("\n"," ",tweets.df$text)
   
   tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")
   return(tweets.df)
 }
 clean_content<-cleanL()
 
 
 emotions<-get_nrc_sentiment(tweets.df$text)
 emo_bar<-colSums(emotions)
 emo_sum<- data.frame(count=emo_bar,emotion=names(emo_bar))
 emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
 
 
 
 # Visualize the emotions from NRC sentiments
 library(plotly)
 p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
   layout(xaxis=list(title=""), showlegend=FALSE,
          title="Emotion Type for hashtag: #narendrea modi")
 api_create(p,filename="Sentimentanalysis")

 
 # Create comparison word cloud data
 
 wordcloud_tweet = c(
   paste(tweets.df$text[emotions$anger > 0], collapse=" "),
   paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
   paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
   paste(tweets.df$text[emotions$fear > 0], collapse=" "),
   paste(tweets.df$text[emotions$joy > 0], collapse=" "),
   paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
   paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
   paste(tweets.df$text[emotions$trust > 0], collapse=" ")
 )
 
 
 # create corpus
 corpus = Corpus(VectorSource(wordcloud_tweet))
 
 # remove punctuation, convert every word in lower case and remove stop words
 
 corpus = tm_map(corpus, tolower)
 corpus = tm_map(corpus, removePunctuation)
 corpus = tm_map(corpus, removeWords, c(stopwords("english")))
 corpus = tm_map(corpus, stemDocument)
 
 # create document term matrix
 
 tdm = TermDocumentMatrix(corpus)
 
 # convert as matrix
 tdm = as.matrix(tdm)
 tdmnew <- tdm[nchar(rownames(tdm)) < 11,]
 
 # column name binding
 colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
 colnames(tdmnew) <- colnames(tdm)
 comparison.cloud(tdmnew, random.order=FALSE,
                  colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                  title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
 
  
 
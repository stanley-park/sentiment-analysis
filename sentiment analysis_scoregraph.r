keyword <- "theranos_15b_ymd"
#tweet_count = 1064
image_location = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"image.png",sep ="") 
positive_sentiment = 'U:/Twitter project/sentimenet analysis_R/Positive.txt'
negative_sentiment = 'U:/Twitter project/sentimenet analysis_R/Negative.txt'
  
all_tweets_file = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"theranos_15b_ymd.csv",sep ="") 
output_file = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"scorefunction_Analysis",sep ="") 
read_from_file = FALSE
input_file = "U:/Twitter project/sentimenet analysis_R/theranos_15b_ymd.csv"

save_histogram = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"histogram.jpeg",sep="")


###################################Step 1: Load the necessary packages################################


library(devtools)
dev_mode(on=FALSE)
install_github('sentiment140', 'okugami79')

install.packages("twitteR")
install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RCurl")
install.packages("RJSONIO")
install.packages("stringr")
install.packages("tm")
install.packages("SnowballC")


library(twitteR)
library(devtools)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(SnowballC)


pos <- scan(positive_sentiment, what='character', comment.char=';') #folder with positive dictionary
neg <- scan(negative_sentiment, what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

 
###tweets evaluation function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
 {
   require(plyr)
   require(stringr)
   scores <- laply(sentences, function(sentence, pos.words, neg.words){
     sentence = gsub("[^0-9A-Za-z///' ]", "", sentence)
     sentence = gsub('http.*\\s*|RT|Retweet','',sentence)
     sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)   # remove retweet entities
     sentence = gsub("@\\w+", "", sentence)     # remove at people
     sentence = gsub("[[:punct:]]", "", sentence)   # remove punctuation
     sentence = gsub("[[:digit:]]", "", sentence)   # remove numbers
     sentence = gsub("http\\w+", "", sentence)      # remove html links
     sentence = gsub("[ \t]{2,}", "", sentence)     # remove unnecessary spaces
     sentence = gsub("^\\s+|\\s+$", "", sentence)   # remove unnecessary spaces
     sentence <- gsub('\\d+', "", sentence)
     sentence <- tolower(sentence)

     word.list <- str_split(sentence, '\\s+')
     words <- unlist(word.list)
     pos.matches <- match(words, pos.words)
     neg.matches <- match(words, neg.words)
     pos.matches <- !is.na(pos.matches)
     neg.matches <- !is.na(neg.matches)
     score <- sum(pos.matches) - sum(neg.matches)
     return(score)
   }, pos.words, neg.words, .progress=.progress)
   scores.df <- data.frame(score=scores, text=sentences)
   return(scores.df)
 } 
 
 


tweets_df <-read.csv("theranos_15b_ymd.csv", header=TRUE, sep=",")
tweets_df$text <- as.factor(tweets_df$text)
scores <- score.sentiment(tweets_df$text, pos.words, neg.words, .progress='text')


#scores
summary(scores)
 
###score table: Final result
write.table(scores,file=output_file,sep=",",row.names=F)
 
 
jpeg(save_histogram, width=12, height=8, units="in", res=300)
hist(scores$score, xlim=c(-5,6), ylim=c(0,45000))


###sentiment score over time###

stat <- scores
stat$ymd <- tweets_df$ymd
#stat$ymd <- as.Date(stat$ymd, origin="2015-10-16")

stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, ymd)
by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file=paste("U:/Twitter project/sentimenet analysis_R/",keyword, '_opin.csv'), row.names=TRUE)
#chart
ggplot(by.tweet, aes(ymd, number)) + geom_line(aes(group=tweet, color=tweet), size=1) +
geom_point(aes(group=tweet, color=tweet), size=2) +
theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) + ggtitle(keyword)

ggsave(file=paste("U:/Twitter project/sentimenet analysis_R/",keyword, '_plot.jpeg'))

search("______") #enter keyword
#stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +

 #########################################################################################################
 
##Step 4: Perform Sentiment Analysis##
# classify emotion
#class_emo = classify_emotion(text, algorithm="bayes", prior=1.0)
# get emotion best fit
#emotion = class_emo[,7]
# substitute NA's by "unknown"
#emotion[is.na(emotion)] = "unknown"

# classify polarity
#class_pol = classify_polarity(text, algorithm="bayes")
# get polarity best fit
#polarity = class_pol[,4]


##Step 5: Create data frame with the results and obtain some general statistics##
# data frame with results
#sent_df = data.frame(text=text, emotion=emotion,
                     #polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
#sent_df = within(sent_df,
                 #emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
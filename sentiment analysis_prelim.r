keyword <- "volkswagen_15"
tweet_count = 529445
image_location = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"image.png",sep ="") 
positive_sentiment = 'U:/Twitter project/sentimenet analysis_R/Positive.txt'
negative_sentiment = 'U:/Twitter project/sentimenet analysis_R/Negative.txt'
  
all_tweets_file = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"volkswagen_15.csv",sep ="") 
output_file = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"scorefunction_Analysis",sep ="") 
read_from_file = FALSE
input_file = "U:/Twitter project/sentimenet analysis_R/volkswagen_15.csv"

save_histogram = paste("U:/Twitter project/sentimenet analysis_R/",keyword,"histogram.jpeg",sep="")



###################################Step 1: Load the necessary packages################################

install.packages("devtools")
library(devtools)
dev_mode(on=FALSE)
install_github('sentiment140', 'okugami79')

install.packages("twitteR")
install.packages("plyr")
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


#load files
tweets <- read.csv("volkswagen_15.csv", header = TRUE)
tweets <- unique(tweets)
mach_text = sapply(tweets, function(x) tweets$text)

### str(mach_text)
mach_text <- iconv(mach_text,to="utf-8")

 
### create a corpus
mach_corpus <- Corpus(VectorSource(mach_text))

#inspect(mach_corpus[10])


### Cleaning Ddata: Lower cases, remove numbers, cut out stopwords, remove punctuation, strip whitespace
mach_clean <- tm_map(mach_corpus, removePunctuation)
mach_clean <- tm_map(mach_clean, content_transformer(tolower))
mach_clean <- tm_map(mach_clean, removeWords, stopwords("english"))
mach_clean <- tm_map(mach_clean, removeNumbers)
mach_clean <- tm_map(mach_clean, stripWhitespace)
#inspect(mach_clean[1])

###you may want to remove search words theses will obviously be very frequent
mach_clean <- tm_map(mach_clean, removeWords, c("volkswagen"))

###  create document term matrix applying some transformations
#tdm = TermDocumentMatrix(mach_corpus,
                         #control = list(removePunctuation = TRUE,
                                        #stopwords = TRUE,
                                        #removeNumbers = TRUE, tolower = TRUE))

tdm = TermDocumentMatrix(mach_clean)


#write.csv(tdm, file="tdm.csv")

install.packages("slam")
library(slam)

tdm <- rollup(tdm, 2, na.rm=TRUE, FUN = sum)
										
###  define tdm as matrix
m = as.matrix(tdm)

###  get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

###  create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

###  plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

### save the image in png format
#png(image_location, width=12, height=8, units="in", res=300)
#wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))



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
 
 
# define "tolower error handling" function 
#try.error = function(x)
#{
  # create missing value
  #y = NA
  # tryCatch error
  #try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  #if (!inherits(try_error, "error"))
    #y = tolower(x)
  # result
  #return(y)
#}
# lower case using try.error with sapply 
#text = sapply(text, try.error)

# remove NAs in some_txt
#text = text[!is.na(text)]
#names(text) = NULL	
 

tweets_df <-read.csv("volkswagen_15.csv", header=TRUE, sep=",")
Dataset <- tweets_df
Dataset$text <- as.factor(tweets_df$text)
scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
#scores
summary(scores)
 
###score table: Final result
write.table(scores,file=output_file,sep=",",row.names=F)
 
 
 
jpeg(save_histogram, width=12, height=8, units="in", res=300)
hist(scores$score, xlim=c(-5,6), ylim=c(0,200000))


###sentiment score over time###
plot()

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


#install.packages("devtools")
#library(devtools)
#dev_mode(on=T)
#install_github("hadley/ggplot2")
# use dev ggplot2 now
# when finished do:
#dev_mode(on=F)  #and you are back to having stable ggplot2	
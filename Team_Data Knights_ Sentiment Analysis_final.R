#1: Install and load required packages for the prepossessing and analyzing tweets

#TwitteR package includes set of text analytics functions. In this project, this library
#is primarily used to provide interface to Twitter web API.
library(twitteR)
#TM package is a framework of text mining applications which provides variety of functions
# to process raw tweets into high quality information from text.
library(tm)
#HTTR package is a collection of tools for working with URLs and HTTP while accessing online data.
library(httr)
# This package is used to create wordclouds.
library(wordcloud)  
#The stringr package provides string functions for string processing. It provides 
#additional functionalities that is missing in R base package
library(stringr)
# PLYR package is used for split-apply-combine (SAC) procedures i.e.applying functions over grouped data. 
library(plyr)     

#--------------------------------------------------------------------------------------
# 2: Set up connection to Twitter search API for downloading tweets.

#Following is the step by step process:
#->sign up for twitter and create an application in https://apps.twitter.com/ 
#-> save the consumer/API keys and consumer/API secret key and access tokens
# enter the The consumer key provided by Twitter application
api_key = "FbJbSiP9UyCsTfqWcSyNFrIb3"
#enter the consumer secret key supplied by Twitter
api_secret = "ZJhTvkvKb86u2ultefGdOiaUfDUgr3J7UqTu2nvJ6sjuFy8TrQ"
#enter the access token
access_token = "4165808799-ajLfNygmkD7SwOT0eR0IvbTi94MnTCj2mf0qL0o"
#The access secret supplied by Twitter
access_token_secret =  "nPbXBgvvltuBEChmdb7oaHKrI5Af56qV4eGXXAIx568So"

#Set up authentication using following function that uses consumer keys and access tokens
#setup_twitter_oauth function is part of {twitteR} library  and uses OAuth authentication
#handshake function from httr package for a twitter session 
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) 
#---------------------------------------------------------------------------------------

#3 Extract Tweets and create a dataframe for processing

#The Twitter search API allows access to a subset of recent tweets for querying.

#For this project we are extracting tweets related to four delegates: two Republicans and
# two Democrats competing for US Presidential elections

#To avoid re-running basic preprocessing steps four times a function('tweet') is designed
#to return a clean corpus from raw tweets. 
# The two arguments that user need to provide for this function are:
#seachstring - this is a search query to twitter. Twitter searches for all tweets related 
# to this string
#nt- takes number of tweets required

#function SearchTwitter searches for all tweets matching with search string provided
tweet=function(searchstring,nt){
    
    tweets = searchTwitter(searchstring, n=nt,lang="en")
    
    # This function takes a list of tweets returns a data.frame version of the tweets
    tweets_dataframe = twListToDF(tweets)
    # the data at this point has 16 columns: text, favorited, favoriteCount, replyToSN
    #created, truncated, replyToSID, ID, replutoUID...etc
    #for the sentiment analysis exercise we will  retain and work on text captured
    #in this dataframe
    #------------------------------------------------------------------------------------#
    
    
    #4 Data preprocessing and Text mining
    
    #Text mining involves the process of deriving meaningful information from raw text data
    #Twitter data is highly unstructured form of text data, primarily because it is an informal
    #communication which includes presence of undesirable content like expressions,slang's 
    #and high frequency of stop words. The typos and messaging short-forms make it more 
    #difficult to analyze. The data cleaning process aims at removing all the inconsistencies and
    # unwanted words in text that are not useful for analysis. 
    
    # GSUB function comes in handy for performing various data cleaning tasks  
    
    #GSUB searches for matches to argument pattern within each element of a character vector and
    #replaces all occurrences of first argument with the second argument. 
    #We will use GSUB for follow operations:
    
    #a. remove special characters and retain alpha numeric, '//','(quote)and '@' from tweets text
    tweets_data = gsub("[^0-9A-Za-z@///' ]", "", tweets_dataframe$text)
    
    #b. while some of the text is from re-tweets, we would certainly like to get rid of 
    #name of the person whose post was re-tweeted. This will remove word 'RT' and the name of person
    #w=mentioned after RT
    tweets_data = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",tweets_data)
    
    #c. The other form of noise in twitter data is names of people between the text.
    #The function below removes name of people by matching anything that starts with @ symbol as name
    tweets_data = gsub("@\\w+", "", tweets_data)
    
    #d.  Since the purpose of sentiment analysis is to analyze words and not numbers 
    # we can remove the numeric digits between the text using function gsub
    tweets_data = gsub("[[:digit:]]", "", tweets_data)
    
    #e. Data obtained from internet usually contains a lot of html entities and links;which gets 
    #embedded in the raw  data. It is important to get rid of these entities as a part of 
    #data sanitization process. The function below matches http links and removes them
    tweets_data = gsub('http\\S+\\s*',"", tweets_data)
    #--------------------------------------------------------------------------------------
    
    
    #4: Create a corpus and prepare data for analysis
    
    # corpus is a collection of large structured texts and various data preprocessing functions 
    #available in TM package can be applied on corpus only.
    #Following function converts dataframe to corpus
    tweets_corpus <- Corpus(VectorSource(tweets_data))
    
    # To create a consistent pattern we will convert all words in text to lowercase. for example,
    # frequency functions otherwise would count same word in different case as two different words
    tweets_corpus<- tm_map(tweets_corpus, tolower)
    
    #create a plain text document using content in corpus
    tweets_corpus <- tm_map(tweets_corpus, PlainTextDocument)
    
    #All the punctuation marks between text should be removed
    tweets_corpus <- tm_map(tweets_corpus, removePunctuation)
    
    #While carrying out text analysis driven at the word level, it is required to remove the 
    #commonly occurring words also called stop-words. One approach is to create a create a long 
    #list of stop-words and other is too use a predefined language specific libraries
    # We will apply both approaches: 
    #1) use common stop words for english language 
    tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("english"))
    
    #2) based on our observation of data, we will create a customized list to be applied on top of
    # the text cleaned by using predefined stop words library
    
    st = c("amp","you'll","you'd","tedcruz","she","yet","hilari","trump","cruz",
           "you", "yourself", "won't", "wont","will","hillari","hilaryclinton",
           "donald","bernie","berniesanders","sen","ted","cruz","hillary",
           "clinton","i", "me", "my", "myself", "we", "our", "ours", "ourselves", 
           "you", "your", "yours", "yourself", "yourselves", "he", "him", "his",
           "himself", "she", "her", "hers", "herself", "it", "its", "itself",
           "they", "them", "their", "theirs", "themselves", "what", "which", 
           "who", "whom", "this", "that", "these", "those", "am", "is", "are",
           "was", "were", "be", "been", "being", "have", "has", "had", "having",
           "do", "does", "did", "doing", "would", "should", "could", "ought",
           "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", 
           "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", 
           "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", 
           "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
           "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
           "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", 
           "what's", "here's", "there's", "when's", "where's", "why's", "how's", 
           "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", 
           "while", "of", "at", "by", "for", "with", "about", "against", "between",
           "into", "through", "during", "before", "after", "above", "below", "to", 
           "from", "up", "down", "in", "out", "on", "off", "over", "under", "again",
           "further", "then", "once", "here", "there", "when", "where", "why",
           "how", "all", "any", "both", "each", "few", "more", "most", "other", 
           "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than",
           "too", "very")
    
    tweets_corpus <- tm_map(tweets_corpus, removeWords, st) 
    
    # word stemming means to reduce word to its root form i.e. to remove any tense information
    # or to convert derived words to their root form. TM library functions allows us to stem the words
    #and thus identify words with same meaning which  might otherwise look different to machine 
    tweets_corpus <- tm_map(tweets_corpus,stemDocument)
    
    return(tweets_corpus)
    
}
# This function tweets can be dynamically used to retrieve and process tweets. This avoids 
#writing same processing step four times for each search string

# Extract data using tweet function and save in a clean corpus for four delegates : 
#Hilary Clinton, Donald Trump, Ted Cruz and Bernie Sanders  
bs=tweet("berniesanders",200)  
dt=tweet("realDonaldTrump",200) 
hc=tweet("HilaryCLinton",200) 
tc=tweet("tedcruz",200) 

#--------------------------------------------------------------------------------------#

# 6: Retrieve and visualize the most frequent words associates/used for these 4 delegates. 

#These results can be very illuminating as they can help delegates understand the public notion 

#In terms of preparing data for this analysis we will convert corpus into a matrix that will
# identify the most frequent word from each text line and number of times the word occurs

#TM library provides 'DocumentTermMatrix' function to achieve this data transformation

#DocumentTermMatrix which is matrix that describes frequency of words that occur in collection
# of documents ( or text in this case). Each row is document (or text ID) in corpus and 
# each column corresponds to the word/term. The value of column is 1 if word exists in document
# otherwise 0
DTM_hc = DocumentTermMatrix(hc)
DTM_dt = DocumentTermMatrix(dt)
DTM_bs = DocumentTermMatrix(bs)
DTM_tc = DocumentTermMatrix(tc)

#the sum of columns will give number of times each word has been repeated in all the 
#tweets collected for each delegate
freq_hc <- colSums(as.matrix(DTM_hc))
freq_dt <- colSums(as.matrix(DTM_dt))
freq_bs <- colSums(as.matrix(DTM_bs))
freq_tc <- colSums(as.matrix(DTM_tc))
# create a data frame with two columns word and frequency
DTM_hc <- data.frame(word=names(freq_hc), freq=freq_hc)
DTM_dt <- data.frame(word=names(freq_dt), freq=freq_dt)
DTM_bs <- data.frame(word=names(freq_bs), freq=freq_bs)
DTM_tc <- data.frame(word=names(freq_tc), freq=freq_tc)


# Barplots and wordclouds convey the message associated with numbers derived above by comparing
# the most frequent words associated with each delegate


# minimum frequency can be entered here:
min_freq =10
# for example you want to plot only words which occur more than 100 times assign 100 to min_freq
barplot(DTM_hc$freq[DTM_hc$freq>min_freq],names.arg=DTM_hc$word[DTM_hc$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_dt$freq[DTM_dt$freq>min_freq],names.arg=DTM_dt$word[DTM_dt$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_bs$freq[DTM_bs$freq>min_freq],names.arg=DTM_bs$word[DTM_bs$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_tc$freq[DTM_tc$freq>min_freq],names.arg=DTM_tc$word[DTM_tc$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)

# we can use par option to compare four word clouds at a time
opar = par()
par(mfrow=c(2,2))

wordcloud(names(freq_hc),freq_hc, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
wordcloud(names(freq_dt),freq_dt, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
wordcloud(names(freq_bs),freq_bs, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
wordcloud(names(freq_tc),freq_tc, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
# These visualizations will help  perceiving, associations people make with each delegate

par(opar)
#------------------------------------------------------------------------------------#

#7 Polarity Score

#During communicating via Tweets people use it as a microblogging platform to convery their 
# negative as well as positive messages. Machine is not able to classify these words into 
# literal negative and positive categories; so we have to use a mathematical ways of assigning
# polarity to words. 


#For this exercise we have used 'Bag of Words' approach to identify polarity associated
#This approach compares text against a large collection of negative and positive words databases
#and assign polarities to them. 

# Following function is designed to calculate polarity score. Polarities are denoted by +1 for 
#positive words and -1 for negative word in the text. Polarity score is calculated for each
#tweet as sum of polarities. 
# for example if a tweet has 2 positive words and  negative words and 5 neutral. The polarities
#assigned will be as (1,1,-1,-1,-1,0,0,0 )and the Polarity score will be sum of polarites = -1


polarity_score = function(tweets, positive, negative)
{
    score =laply(tweets, function(tweet, positive, negative)
    {
        #before matching words in each tweet we will need to separate multiple attached 
        #words into individual entities. For example, "brokelaptop" to "broke laptop" 
        #str_split function provides this functionality
        
        words = str_split(tweet,"\\s+")
        # unlist function simplifies a list structure i.e. is a sentence into vector which contains
        #all atomic components present in tweets
        words = unlist(words)
        
        # function matches the words with positive and negative databases and calculates score 
        positive_overlap = match(words, positive)
        negative_overlap= match(words, negative)
        positive_overlap =!is.na(positive_overlap)
        negative_overlap= !is.na(negative_overlap)
        score =sum(positive_overlap) - sum(negative_overlap)
        return(score)
    }, positive, negative)
    scores =data.frame(score=score, text=tweets)
    return(scores)
}

#Access the negative and positive library which will be used in the polarity score above
# Please change the path as per your location of files
positive <- scan('D:/Classes/R programming/my project/positive-words.txt', what='character', comment.char=';') #file with positive words 
negative <- scan('D:/Classes/R programming/my project/negative-words.txt', what='character', comment.char=';') #file with negative words

#Before applying function convert corpus to dataframe
hc_df=data.frame(text = sapply(hc, as.character), stringsAsFactors = FALSE)
dt_df=data.frame(text = sapply(dt, as.character), stringsAsFactors = FALSE)
bs_df=data.frame(text = sapply(bs, as.character), stringsAsFactors = FALSE)
tc_df=data.frame(text = sapply(tc, as.character), stringsAsFactors = FALSE)

# Using the function defined above calculate Polarity score for each tweet for all delegates
Sentiment_scores_hc = polarity_score(hc_df$text, positive, negative)
Sentiment_scores_dt = polarity_score(dt_df$text, positive, negative)
Sentiment_scores_bs = polarity_score(bs_df$text, positive, negative)
Sentiment_scores_tc = polarity_score(tc_df$text, positive, negative)


# As a Score less than zero means a Negative polarity and score greater than zero means Positive and neutral if zero
# creating polarity column with negative or positive or neutral as values

Sentiment_scores_hc$polarity=ifelse(Sentiment_scores_hc$score>0,"positive", ifelse(Sentiment_scores_hc$score<0 , "negative", "neutral"))
Sentiment_scores_dt$polarity=ifelse(Sentiment_scores_dt$score>0,"positive", ifelse(Sentiment_scores_dt$score<0 , "negative", "neutral"))
Sentiment_scores_bs$polarity=ifelse(Sentiment_scores_bs$score>0,"positive", ifelse(Sentiment_scores_bs$score<0 , "negative", "neutral"))
Sentiment_scores_tc$polarity=ifelse(Sentiment_scores_tc$score>0,"positive", ifelse(Sentiment_scores_tc$score<0 , "negative", "neutral"))


# we will prepare a table with frequency of negative,neutral and positive tweets
# we have used table function to give the summary data and function t to transpose

x= data.frame(table(Sentiment_scores_hc$polarity),table(Sentiment_scores_bs$polarity),table(Sentiment_scores_dt$polarity),table(Sentiment_scores_tc$polarity))
x = x[,c(2,4,6,8)]
colnames(x)<-c( "Hillary","Bernie","Donald","Tedcruz")
x = t(x)  
colnames(x)<-c("Negative","Neutral", "Positive")

# Bar plot to compare polarities of Hillary,Donald,Bernie,TedCruz

colors = c("red",  "green","yellow","blue") 
barplot(x,beside=T, col=colors,ylim=c(0,1500),ylab="Number of tweets",main="Polarity Comparsion")
legend("top",legend=rownames(x),horiz = TRUE,fill=colors)

# To compare extreme sentiments it will be good to look at the % of negative vs. postive 
# out of the total non-neutral tweets associated with each delegate

# Calcuate the % of positive and negative tweets out of all the non-neutral tweets
#Framing len vector with number of non-neutral tweets for all delegates
#Framing dataframe y  by combining length vector  and matrix x
len = rbind(nrow(Sentiment_scores_hc[Sentiment_scores_hc$score!=0,]),nrow(Sentiment_scores_hc[Sentiment_scores_bs$score!=0,]),nrow(Sentiment_scores_hc[Sentiment_scores_dt$score!=0,]),nrow(Sentiment_scores_hc[Sentiment_scores_tc$score!=0,]))
y= cbind.data.frame(x,len)

#Divide the number of positive and negative score by total number of non-neutral tweets
#converting  and subsetting to get percentages of Negative and positive tweets
y$Negative = y$Negative/y$len
y$Positive = y$Positive/y$len
y= y[,c(1,3)]
y = t(y)

# Bar plot to see what % of tweets are positive vs. negative for each delegate 
colors = c("red",  "green") 
bp = barplot(y,beside=T, col=colors,ylim=c(0,1.5),ylab="Percentages",main="Sentiment Comparsion")
text(bp, 0, round(y,2),cex=1,pos=3)
legend("top",legend=rownames(y),horiz = TRUE,fill=colors)


# Twitter has evolved as source of information for analysts and stakeholders in variety of 
#industries. The nature of these microblogs are very specific and opinionated which makes it 
# more valuable as  source of public sentiment data relate to current issues.
#The results from this project can provide insights to both analysts and delegates about the
#general public notion in a very real time manner.


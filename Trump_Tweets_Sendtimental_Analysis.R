###########################################################################################################################
# Load & Install Packages
###########################################################################################################################

if(!require(jsonlite))
{
  install.packages("jsonlite")
  library(jsonlite) # load data
}

if(!require(curl))
{
  install.packages("curl")
  library(curl) # load data
}

if(!require(tidyverse))
{
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(tidytext))
{
  install.packages("tidytext")
  library(tidytext) # text mining package
}

if(!require(syuzhet))
{
  install.packages("syuzhet")
  library(syuzhet) # NLP library
}

if(!require(lubridate))
{
  install.packages("lubridate")
  library(lubridate) # used for date manipulation
}

###########################################################################################################################
# Data Import & Preperation
###########################################################################################################################

# Import Data from trumptwitterarchive.com
trump_tweets_raw <- stream_in(curl(paste0("trumptwitterarchive.com/data/realdonaldtrump/2009.json")), verbose = FALSE)
for(year in 2010:2019)
{
  trump_tweets_raw <- rbind(trump_tweets_raw, stream_in(curl(paste0("trumptwitterarchive.com/data/realdonaldtrump/",year,".json")), verbose = FALSE))
}

# No retweets, data cleaning & POSIXct format
trump_tweets <- trump_tweets_raw %>%
                filter(!is_retweet) %>%
                filter(!str_detect(text, "^RT")) %>%
                filter(!str_detect(text, "http")) %>%
                filter(!str_detect(text, "https")) %>%
                filter(!str_detect(text, '^"')) %>%
                mutate(created_at = as.POSIXct.default(created_at , format = "%a %b %d %H:%M:%S +0000 %Y"))

###########################################################################################################################
#Part A - Sedimental Analysis: Mean Sedimental Score
###########################################################################################################################

# Define futher things to remove
remove_reg <- "&amp;|&lt;|&gt;"

#Tokenizing & Stopword removal
trump_tweets_words <- trump_tweets %>%
                      mutate(text = str_remove_all(text, remove_reg)) %>%             
                      unnest_tokens(word, text, token = "tweets") %>%
                      filter(!str_detect(word, "@realdonaldtrump")) %>%
                      filter(!word %in% stop_words$word, !word %in% str_remove_all(stop_words$word, "'"), str_detect(word, "[a-z]"))

# Aggregate tweets by two months &  Get Mean Sentimental Score (Bing)
trump_tweets_sentiment_per_twoMonths <- trump_tweets_words %>% 
                                        arrange(created_at) %>% #Order by date
                                        inner_join(get_sentiments("bing"), b = c("word"="word")) %>% # Adds sentiment (pos/neg) as a column
                                        mutate(dategroup = floor_date(created_at, "2 months")) %>% # Groups all tweets by two months
                                        # For every dategroup, count the occurence of words, do OHE (spread in pos and neg) per word, and get therby a sentiment_score
                                        group_by(dategroup) %>% 
                                        count(word, sentiment) %>%
                                        spread(sentiment, n, fill = 0)  %>% 
                                        mutate(sentiment_score = positive - negative) %>% 
                                        # For every dategroup, build the mean of the sentiment score
                                        group_by(dategroup) %>%
                                        summarise(mean_sentiment_score = mean(sentiment_score, na.rm = TRUE)) %>%
                                        mutate(valence = ifelse(mean_sentiment_score > 0, "Positive", "Negative")) # Attache label

# Transform trump_tweets_sentiment_per_twoMonths into a Dataframe to enable modifications during the creation of the barplot
df <- as.data.frame(trump_tweets_sentiment_per_twoMonths)

# Get Barplot
png('Twitter-Sentimental-Analysis-@realdonaldtrump.png', width = 14, height = 6, units = 'in', res = 600)
par(mar= c(5,5,4,1) + 0,1)
bar1 <- barplot(df$mean_sentiment_score, 
                names.arg=df$dategroup,
                col=ifelse(df$valence == "Positive", "blue", "red"), 
                ann = F,
                xaxt='n', 
                yaxt='n',
                ylim = c(-1.6, 1.6),
                xlim = c(2,75)) 

grid(nx=NA, ny=NULL)
box()
axis(1, at = c(bar1[1], bar1[8], bar1[15], bar1[22], bar1[29], bar1[36], bar1[43], bar1[50], bar1[57], bar1[64]), labels = c(df$dategroup[1], df$dategroup[8], df$dategroup[15], df$dategroup[22], df$dategroup[29], df$dategroup[36], df$dategroup[43], df$dategroup[50], df$dategroup[57], df$dategroup[64]), las = 1, col = NA, col.ticks = 1) 
axis(2, at = c(-1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5), las = 1, col = NA, col.ticks = 1)

legend(69, 1.6, legend = c("Positive", "Negative"), fill =  c("blue","red"), bty = 'n')
mtext(side = 3, line = 2, at = bar1[64]/2, cex = 1.8, "Positive & Negative Sentiment of @realDonaldTrump' s Tweets")
mtext(side = 3, line = 0.7, at = bar1[64]/2, cex = 1.2, "An analysis of 20090 tweets between 2009-05-08 and 2019-12-15 aggregated by two months")
title (ylab = 'Mean Sentimental Score (Bing)', xlab = 'Date', cex = 2, cex.lab = 1.1)

abline(v = bar1[37], lwd = 2, col = 'red')   #2015-06-16 in dategroup 2015-05-01 -> bar1[37]
abline(v = bar1[44], lwd = 2, col = 'red')   #2016-09-08 in dategroup 2016-07-01 -> bar1[44]
text(x = bar1[37] + 0.9, y = -1.04, label = "Presidental Candidate", srt = 90, cex = 0.8)
text(x = bar1[44] + 0.9, y = -1.15, label = "Elected President", srt = 90, cex = 0.8)
dev.off()

###########################################################################################################################
#Part B - Sedimental Analysis: Emotions  in tweets of Donald Trump (Anger, Disgust, Joy)
###########################################################################################################################

# Get sentimental scores (Trust, Surprise, Sad, Joy, Fear, Disgust, Anticipation, Angry) for every tweet
nrc_data <- get_nrc_sentiment(trump_tweets$text)

angry_items <- which(nrc_data$anger == max(nrc_data$anger)) #Get all indices of tweets, which have maximum emotion score
trump_tweets$text[angry_items] %>% head() # Print the tweets

disgust_items <- which(nrc_data$disgust == max(nrc_data$disgust)) #Get all indices of tweets, which have maximum emotion score
trump_tweets$text[disgust_items] %>% head() # Print the tweets

joy_items <- which(nrc_data$joy == max(nrc_data$joy)) #Get all indices of tweets, which have maximum emotion score
trump_tweets$text[joy_items] %>% head() # Print the tweets
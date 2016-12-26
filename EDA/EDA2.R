#library setup
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")
library(lubridate)
library(stringr)

#Question 1
##1.1
tweet_df <- read.csv('tweets.csv', header = FALSE)
tweet_df_vector <- as.vector(tweet_df[,1])
flight_flag <- grepl('\\<flight\\>', tweet_df_vector)
flight_tweets <- tweet_df_vector[flight_flag]
flight_tweets

##1.2
questionmark_flag <- grepl('\\?$', tweet_df_vector)
num_tweet_qmend <- sum(questionmark_flag)
num_tweet_qmend

##1.3--some of the 3 capital letter are not aircode
airportcode_flag <- grepl('[A-Z]{3}', tweet_df_vector)
airportcode_tweets <- tweet_df_vector[airportcode_flag]
num_airportcode <- sum(airportcode_flag)
num_airportcode

##1.4
url_flag <- grepl('http', tweet_df_vector)
URL_tweets <- tweet_df_vector[url_flag]
URL_tweets

##1.5
tweet_df_vector <- sub('!{2,}','!',tweet_df_vector)
tweet_df_vector

##1.6--split strings? '.' or '. '?
tweet_df_vector <- gsub("(\\.{2,})|(\\!{2,})|(\\?{2,})",'.',tweet_df_vector)
split_vector <- strsplit(tweet_df_vector,"\\.")
split_vector

##1.7
split_vector <- strsplit(tweet_df_vector," ")
hashtag_vector <- lapply(split_vector, function(x) grep('^#', x, value=TRUE))
hashtag_vector

#Question2
date_df <- read.csv('dates.csv', header = TRUE, 
                    stringsAsFactors = FALSE, na.strings="")
date_df_omitna <- na.omit(date_df)
span_fun <- function(x){
  #span <- interval(mdy_hm(x[1]),mdy_hm(x[2]))
  #as.duration(span)
  x[1]<x[2]
}

##2.1: for loop solution

# duration_flag <- rep(T, nrow(date_df_omitna))
# for (i in c(1:nrow(date_df_omitna))){
#  duration_flag[i] <- (span_fun(date_df_omitna[i,]) < as.duration(0))
# }
# new_date_df <- date_df_omitna[duration_flag,]

##2.2: apply() solution
later_flag <- apply(date_df_omitna,1,span_fun)
new_date_df <- date_df_omitna[later_flag,]
str(new_date_df)

#Question3
speech_read <- readLines("stateoftheunion1790-2012.txt")
speech_char <- gsub('\xd1','',speech_read)
chunk_index0 <- grep('\\*{3}', speech_char)
chunk_index <- c(1, chunk_index0)
chunklst <- split(speech_char[1:(length(speech_char)-1)],
                  rep(1:length(diff(chunk_index)),diff(chunk_index)))
chunklst_speech <- chunklst[2:length(chunklst)]

##3.1: the president's name
presidentname <- sapply(chunklst_speech, function(x) x[4][[1]])
presidentname <- unname(presidentname, force = FALSE)

##3.2: year
string_time <- sapply(chunklst_speech, function(x) x[5][[1]])
Year <- sapply(string_time, function(x){
  s <- strsplit(x,', ')
  s[[1]][2]
})
Year <- as.numeric(unname(Year))

##:3.3: month
Month <- sapply(string_time, function(x){
  s <- strsplit(x,' ')
  s[[1]][1]
})
Month <- unname(Month)

##3.4: day of month
DayofMonth <- sapply(string_time, function(x){
  s <- strsplit(x,' |,')
  s[[1]][2]
})
DayofMonth <- as.numeric(unname(DayofMonth))

##3.5: day of week
date_df <- cbind(Year,Month,DayofMonth)
Dateformat <- function(x) {
  date <- paste(x[1],x[2],x[3], sep='-')
  strptime(date,format="%Y-%b-%d")
}
date_day <- apply(date_df,1,Dateformat)
date_day <- sapply( date_day, paste0, collapse="")
DayofWeek <- sapply(date_day, function(x) weekdays(as.Date(x)))
DayofWeek <- unname(DayofWeek)

##3.6: length of the speech(lines)
chunklst_speech_noempty <- sapply(chunklst_speech,function(x) x[which(x!="")])
lengthofspeech <- sapply(chunklst_speech_noempty,function(x) length(x)-4)

##3.7: number of sentences
numberofsetences <- str_count(chunklst_speech_noempty, fixed("."))

##3.8: number of words
numberofwords_lst <- sapply(chunklst_speech_noempty,nchar)
numberofwords <- sapply(numberofwords_lst,sum)

##final data frame
speech_df <- cbind.data.frame(presidentname,Year,Month,DayofMonth,
                   DayofWeek,lengthofspeech,numberofsetences,
                   numberofwords) 
colnames(speech_df) <- c('PresidentName','Year','Month','DayofMonth',
                         'DayofWeek','LengthofSpeech','NumberofSetences',
                         'NumberofWords')
rownames(speech_df) <- 1:nrow(speech_df)
speech_df



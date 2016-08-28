# ***********Text Analysis to test Starbucks' Twitter strategy************

# Install all the necessary packages
install.packages("twitteR")
install.packages("ROAuth")
install.packages("plyr")
install.packages("dplyr")
install.packages('tree', repo='https://cran.cnr.berkeley.edu/')
install.packages('rpart', repo='https://cran.cnr.berkeley.edu/')
install.packages('party', repo='https://cran.cnr.berkeley.edu/')
install.packages('randomForest', repo='https://cran.cnr.berkeley.edu/')
install.packages("rpart.plot")
install.packages("caret")
install.packages("wordcloud")
install.packages("data.table")
install.packages("rattle")

# Load all the necessary packages
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
require("NLP")
library(tm)
library(SnowballC)
library(topicmodels)
library(tree)
library(rpart)
library(party)
library(randomForest)
library(rpart.plot)
library(caret)
library(wordcloud)
library(data.table)
library(rattle)

# Extract '@starbucks' tweets using R's Twitter API

# Twitter Authentication

# Load tweets into R based on the maximum allowable limit of the query and convert outcome to dataframe

result1<-searchTwitter("@Starbucks",n=5000)
result1df <- twListToDF(result1)

result2<-searchTwitter("@Starbucks",n=5000, maxID = "754327250988859392")
result2df <- twListToDF(result2)

result3 <- searchTwitter("@Starbucks",n=5000, maxID = "753983653068283909")
result3df <- twListToDF(result3)

resultdf <- rbind(result1df, result2df, result3df)

result4 <- searchTwitter("@Starbucks",n=5000, maxID = "753983653068283909")
result4df <- twListToDF(result4)

result5 <- searchTwitter("@Starbucks",n=5000, maxID = "753736867434987521")
result5df <- twListToDF(result5)

result6 <- searchTwitter("@Starbucks",n=5000, maxID = "753616147334004736")
result6df <- twListToDF(result6)

result_1df <- rbind(result4df, result5df, result6df)

result7 <- searchTwitter("@Starbucks",n=5000, maxID = "753337044651368449")
result7df <- twListToDF(result7)

result8 <- searchTwitter("@Starbucks",n=5000, maxID = "753079104426090498")
result8df <- twListToDF(result8)

result9 <- searchTwitter("@Starbucks",n=5000, maxID = "752919486727122945")
result9df <- twListToDF(result9)

result_2df <- rbind(result7df, result8df, result9df)

result10 <- searchTwitter("@Starbucks",n=5000, maxID = "752579998758891520")
result10df <- twListToDF(result10)

result11 <- searchTwitter("@Starbucks",n=5000, maxID = "751953735144779777")
result11df <- twListToDF(result11)

result12 <- searchTwitter("@Starbucks",n=5000, maxID = "751418909459230720")
result12df <- twListToDF(result12)

result_3df <- rbind(result10df, result11df, result12df)

result_max <- rbind(resultdf, result_1df, result_2df, result_3df)

library(dplyr)

result_Max <- unique(result_max)

# Save extracted tweets in .csv format

write.csv(result_Max, "result_Max.csv", row.names = F)

# Load tweets into R based on the maximum allowable limit of the query and convert outcome to dataframe

result_new_1<-searchTwitter("@Starbucks",n=5000)
result_new_1df <- twListToDF(result_new_1)

result_new_2<-searchTwitter("@Starbucks",n=5000, maxID = "764837428381224960")
result_new_2df <- twListToDF(result_new_2)

result_new_3<-searchTwitter("@Starbucks",n=5000, maxID = "764201662705766400")
result_new_3df <- twListToDF(result_new_3)

result_new_4<-searchTwitter("@Starbucks",n=5000, maxID = "763785747425964032")
result_new_4df <- twListToDF(result_new_4)

result_new_5<-searchTwitter("@Starbucks",n=5000, maxID = "763360177613660161")
result_new_5df <- twListToDF(result_new_5)

result_new_6<-searchTwitter("@Starbucks",n=5000, maxID = "762994159657558016")
result_new_6df <- twListToDF(result_new_6)

result_new_7<-searchTwitter("@Starbucks",n=5000, maxID = "762428116866260992")
result_new_7df <- twListToDF(result_new_7)

result_max_new <- rbind(result_new_1df, result_new_2df, result_new_3df, result_new_4df, result_new_5df, result_new_6df, result_new_7df)
result_max_new <- unique(result_max_new)

# Save extracted tweets in .csv format
write.csv(result_max_new, "result_max_new.csv", row.names = F)

# Text Cleaning & Prepocessing

# remove unnecessary columns from both the datasets
result_clean$favorited <- NULL
result_clean_new$favorited <- NULL

result_clean$favoriteCount <- NULL
result_clean_new$favoriteCount <- NULL

result_clean$truncated <- NULL
result_clean_new$truncated <- NULL

result_clean$statusSource <- NULL
result_clean_new$statusSource <- NULL

result_clean$retweeted <- NULL
result_clean_new$retweeted <- NULL

result_clean$latitude <- NULL
result_clean_new$latitude <- NULL

result_clean$longitude <- NULL
result_clean_new$longitude <- NULL

result_clean$replyToSN <- NULL
result_clean_new$replyToSN <- NULL

result_clean$replyToSID <- NULL
result_clean_new$replyToSID <- NULL

result_clean$replyToUID <- NULL
result_clean_new$replyToUID <- NULL

# Arrange the data in descending order of the 'retweetCount' attribute
result_clean_sorted <- arrange(result_clean, desc(retweetCount))
result_clean_sorted_new <- arrange(result_clean_new, desc(retweetCount))

# Convert the entire text to lower case
result_text_vector <- tolower(result_clean_sorted_up$text)
result_text_vector_new <- tolower(result_clean_sorted_new$text)

# Remove old style retweets
result_text_vector = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", result_text_vector)
result_text_vector_new = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", result_text_vector_new)

# Remove everything except english letters and space- symbols, numbers, punctuations, emoticons, etc
result_text_vector = gsub("@\\w+", "", result_text_vector)
result_text_vector_new = gsub("@\\w+", "", result_text_vector_new)


result_text_vector = gsub("[[:punct:]]", "", result_text_vector)
result_text_vector_new = gsub("[[:punct:]]", "", result_text_vector_new)


result_text_vector = gsub("[[:digit:]]", "", result_text_vector)
result_text_vector_new = gsub("[[:digit:]]", "", result_text_vector_new)


result_text_vector = gsub("http\\w+ |^ http | http", "", result_text_vector)
result_text_vector_new = gsub("http\\w+ |^ http | http", "", result_text_vector_new)

result_text_vector = gsub("^rt | rt", "", result_text_vector)
result_text_vector_new = gsub("^rt | rt", "", result_text_vector_new)


result_text_vector = gsub("^edu.*u$| uufe.*|eduaubcedubcubropical |andeduaubdedubu| eduaubdedubuaeduaubdedubuaeduaubdedubua | nrntrending | eduaubdedubuaeduaubdedubuaeduaubdedubua | eduaubdedubuceduaubcedubfubc | eduaubdedubu
                          eduaubdedubudeduaubcedububaeduaubcedububuufef | unicorneduaubeedubu | eduaubdedubu |eduaubcedubduaaeduaubdedubuaa |eduaubcedubduaaeduaubdedubuaa | eduaubdedubuabeduaubcedubcufeduaubcedubcudeduaubcedubcueeduaubdedubuab|
                          eduaubcedububaeduaubcedububuceduaubcedubfubcufef|  eduaubdedububdeduaubdedubuasuicide.* | eduaubdedububaeduaubdedubuagodhatesblackpeople.*|
                          eduaubdedubuaeduaubcedubcub | eduaubdedububdeduaubdedubuaitstime.* | eduaubcedububaeduaubcedububuceduaubcedubfubcufef| 
                          eduaubcedubduceduaubcedubduadeduaubdedubu | ^ueueueueue.*| uaufefeduaubdedubua | eduaubdedubuceduaubcedubdua", "", result_text_vector)

result_text_vector_new = gsub("^edu.*u$| uufe.*|eduaubcedubcubropical |andeduaubdedubu| eduaubdedubuaeduaubdedubuaeduaubdedubua | nrntrending | eduaubdedubuaeduaubdedubuaeduaubdedubua | eduaubdedubuceduaubcedubfubc | eduaubdedubu
                              eduaubdedubudeduaubcedububaeduaubcedububuufef | unicorneduaubeedubu | eduaubdedubu |eduaubcedubduaaeduaubdedubuaa |eduaubcedubduaaeduaubdedubuaa | eduaubdedubuabeduaubcedubcufeduaubcedubcudeduaubcedubcueeduaubdedubuab|
                              eduaubcedububaeduaubcedububuceduaubcedubfubcufef|  eduaubdedububdeduaubdedubuasuicide.* | eduaubdedububaeduaubdedubuagodhatesblackpeople.*|
                              eduaubdedubuaeduaubcedubcub | eduaubdedububdeduaubdedubuaitstime.* | eduaubcedububaeduaubcedububuceduaubcedubfubcufef| 
                              eduaubcedubduceduaubcedubduadeduaubdedubu | ^ueueueueue.*| uaufefeduaubdedubua | eduaubdedubuceduaubcedubdua", "", result_text_vector_new)
result_text_vector_updated = gsub("en|wday|datetimestamp|hour|year|mday|isdst|author|list|introduce|origin|meta|language|description|character.*|min|heading|list.*|meta|mon|yday|year","",result_text_vector_updated)

result_text_vector_updated <- tolower(result_clean_sorted_updated$text)
result_text_vector_new <- tolower(result_text_vector_new)

result_clean_sorted$text <- result_text_vector
result_clean_sorted_updated$text <- result_text_vector_updated
result_clean_sorted_new$text <- result_text_vector_new

# Save cleaned dataset and continue with topic modelling
write.csv(result_clean_sorted, "result_clean_sorted.csv", row.names = F)
write.csv(result_clean_sorted_updated, "result_clean_sorted_updated.csv", row.names = F)
write.csv(result_clean_sorted_new, "result_clean_sorted_new.csv", row.names = F)

# Begin topic modelling

# Combine bothe datasets into one
result_final_new <- rbind(result_clean_sorted_updated, result_clean_sorted_new)

# Pull the text into a separate vector
result_final_vector <- result_final_new$text

# Remove any remaining space and update the dataset
result_final_vector <- gsub("^\\s+|\\s+$", "", result_final_vector)
result_final_new$text <- result_final_vector

# One of the main limitations in twitter analytics is that each retweet is identified as a separate text. This will have an adverse effect on our regression analysis that we will be performing shortly. So we have to remove all the duplicate text
result_final_new <- result_final_new[!duplicated(result_final_new$text), ]

# Build a corpus, and specify the source to be character vector
result_corpus <- Corpus(VectorSource(result_final_new$text))

# Again, make sure that corpus contains only english letters and no extra spaces
result_corpus <- tm_map(result_corpus, removePunctuation)
result_corpus <- tm_map(result_corpus, removeNumbers)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
result_corpus <- tm_map(result_corpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
result_corpus <- tm_map(result_corpus, content_transformer(removeNumPunct)) 
result_corpus <- tm_map(result_corpus, stripWhitespace)

# Remove all the stopwords (default/defined)
myStopwords <- c("can", "say", "amp", "i", "isnt", "ok", "today", "dont", "worry", "eduaubdedubu", "me",
                 "the", "hold", "against", "new", "also", "however", "tell", "will", "much","need", "take",
                 "tend", "like", "particular", "rather", "said", "love", "get", "well", "make", "ask", "come",
                 "first", "two", "help", "often", "may", "stand", "line", "internet", "youre", "you", "cool", "insanely",
                 "might", "see", "thing", "some", "point", "look", "right", "now", "day", "etc", "quit", "drink",
                 "hey", "that", "till", "tomorrow", "awsom", "aww", "awww", "awwww", "awwwwuufef", "awwwww")
myStopwords_1 <- c("awyeduaubdedubub", "axe", "aya", "ayala", "ayalam", "aye", "person", "made", "raise", "every", "thanks", "howard")
myStopwords_2 <- c("ayelen", "ayi", "aylmer", "ayo", "ayooooo", "ayudó", "affogatostyle", "just", "poured",
                   "running", "shot", "top", "way", "want", "city", "got", "never", "riyadh", "thats", "got", "know", "decision", "decision", "donate", "kuwait", "unused")
myStopwords_3 <- c("aziend", "azmak", "azúcar", "azwx", "azz", "year", "origin",
                   "language", "list", "hour", "content", "datetimestamp", "yday",
                   "116", "wday", "isdst","eduaubcedubcubrop", "white", "listauthor",
                   "meta", "listcontent", "listsec", "mon", "mday", "introduc", "description","oz",
                   "ho", "via", "amaaaaaaazing", "how", "jodiefoster", "new", "meet", "w", "our", "packets", 
                   "boyfriend", "asked", "for", "a", "they", "gave", "him", "af", "umm", "hell", "line", "order",
                   "get", "water", "out", "of", "this", "actually", "happened", "girls", "worst", "sold", "in", "both",
                   "win", "each", "bars", "follow", "boxadaygiveaway", "giveaway", "make", "moves", "jump", "app", "order",
                   "ahead", "because", "adding", "your", "to", "good", "always", "good", "trying", "has", "been", "pinnacle",
                   "my", "life", "thus", "far", "sometimes", "sometime", "day", "save", "save", "evening", "best", "last", "say",
                   "name", "so", "that", "shout", "loud", "when", "its", "ready", "have", "supports", "domestic", "terrorism", "crappy",
                   "we", "did", "it", "click", "read", "full", "letter", "from", "afternoon", "eats", "piroshky", "original",
                   "learn", "region", "aye", "up", "hooked", "employees", "behind", "counter", "day", "one", "tell", "she", "can",
                   "take", "some", "venti", "dick", "case", "another", "ourrevolution", "medicareall", "now", "menu", "coverage",
                   "grabbing", "food", "sugar", "fix", "ive","problem", "introducing", "eduaubdedubua", "cant")


result_corpus <- tm_map(result_corpus, removeWords, myStopwords)
result_corpus <- tm_map(result_corpus, removeWords, myStopwords_1)
result_corpus <- tm_map(result_corpus, removeWords, myStopwords_2)
result_corpus <- tm_map(result_corpus, removeWords, myStopwords_3)
result_corpus <- tm_map(result_corpus, removeWords, stopwords("english"))

# Build a document term matrix with text in each row being no less than 2 words. 
result_dtm <- DocumentTermMatrix(result_corpus)
rowTotals_dtm <- apply(result_dtm, 1, sum)
result_dtm_nozero  <- result_dtm[rowTotals_dtm > 2, ] 

# Inspect the top 500 words
freq.terms <- findFreqTerms(result_dtm_nozero, lowfreq = 500)

# Wordcloud
tdm <- TermDocumentMatrix(result_corpus)
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
d <- data.frame(word = names(word.freq),freq=word.freq)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 350,
          max.words=68, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9, "Set1"))

# Implement Latent Dirichlet Allocation to extract six dominant themes in the text
result_lda <- LDA(result_dtm_nozero,k = 6)

# Inspect the first five terms in each topic
result_lda_term <- terms(result_lda, 5)

# Add the 30 terms as 30 new columns in the dataset with each observation being a binary indicator of the presence of the term in the text
result_text_vector_new <- as.character(result_final_new$text)

result_final_new <- result_final_new %>% mutate(CaramelFix_1 = ifelse(grepl(pattern = "starbucks", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CaramelFix_2 = ifelse(grepl(pattern = "iced", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CaramelFix_3 = ifelse(grepl(pattern = "coffee", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CaramelFix_4 = ifelse(grepl(pattern = "card", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CaramelFix_5 = ifelse(grepl(pattern = "caramel", x = result_text_vector_new), 1,0))

result_final_new <- result_final_new %>% mutate(MochaMania_1 = ifelse(grepl(pattern = "coffee", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MochaMania_2 = ifelse(grepl(pattern = "starbucks", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MochaMania_3 = ifelse(grepl(pattern = "mocha", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MochaMania_4 = ifelse(grepl(pattern = "time", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MochaMania_5 = ifelse(grepl(pattern = "ever", x = result_text_vector_new), 1,0))

result_final_new <- result_final_new %>% mutate(StoreTime_1 = ifelse(grepl(pattern = "coffee", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(StoreTime_2 = ifelse(grepl(pattern = "starbucks", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(StoreTime_3 = ifelse(grepl(pattern = "time", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(StoreTime_4 = ifelse(grepl(pattern = "store", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(StoreTime_5 = ifelse(grepl(pattern = "work", x = result_text_vector_new), 1,0))

result_final_new <- result_final_new %>% mutate(CoconutEspresso_1 = ifelse(grepl(pattern = "milk", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CoconutEspresso_2 = ifelse(grepl(pattern = "iced", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CoconutEspresso_3 = ifelse(grepl(pattern = "coconut", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CoconutEspresso_4 = ifelse(grepl(pattern = "going", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(CoconutEspresso_5 = ifelse(grepl(pattern = "macchiato", x = result_text_vector_new), 1,0))

result_final_new <- result_final_new %>% mutate(FreeDrinks_1 = ifelse(grepl(pattern = "coffee", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(FreeDrinks_2 = ifelse(grepl(pattern = "free", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(FreeDrinks_3 = ifelse(grepl(pattern = "tea", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(FreeDrinks_4 = ifelse(grepl(pattern = "thank", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(FreeDrinks_5 = ifelse(grepl(pattern = "drinks", x = result_text_vector_new), 1,0))

result_final_new <- result_final_new %>% mutate(MorningMix_1 = ifelse(grepl(pattern = "coffee", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MorningMix_2 = ifelse(grepl(pattern = "starbucks", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MorningMix_3 = ifelse(grepl(pattern = "morning", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MorningMix_4 = ifelse(grepl(pattern = "milk", x = result_text_vector_new), 1,0))
result_final_new <- result_final_new %>% mutate(MorningMix_5 = ifelse(grepl(pattern = "iced", x = result_text_vector_new), 1,0))

# Include 'month' from the 'created' attribute as a new column
result_final_new$month=sapply(result_final_new$created, function(x) {p=as.POSIXlt(x);format(p, "%m")})

# Include 'date' from the 'created' attribute as a new column
result_final_new$date=sapply(result_final_new$created, function(x) {p=as.POSIXlt(x);format(p, "%d %b")})

# Include 'day of the week' from the 'created' attribute as a new column
result_final_new$day=sapply(result_final_new$created, function(x) {p=as.POSIXlt(x);format(p, "%a")})

# Include two separate columns to identify weekdays and weekends
day_vector <- result_final_new$day
result_final_new <- result_final_new %>% mutate(wday = ifelse(grepl(pattern = "Mon|Tue|Wed|Thu|Fri", x = day_vector), 1,0))
result_final_new <- result_final_new %>% mutate(wend = ifelse(grepl(pattern = "Sat|Sun", x = day_vector), 1,0))

# Include 'hour' from the 'created' attribute as a new column
result_final_new$hour=sapply(result_final_new$created, function(x) {p=as.POSIXlt(x);format(p, "%H")})

# Process the hour value to include distinct columns for daytime, evening and night
time <- as.numeric(sub("(\\d{1,2}):.*", "\\1", result_final_new$hour))
result_final_new$LateNight <- ifelse(00 <= time & time <= 04, 1,0)
result_final_new$Morning <- ifelse(04 < time & time <= 12, 1,0)
result_final_new$Afternoon <- ifelse(12 < time & time <= 16, 1,0)
result_final_new$Evening <- ifelse(16 < time & time <= 20, 1,0)
result_final_new$Night <- ifelse(20 < time & time <= 23, 1,0)

# EDA

# Retweet count distribution over 'dates' attribute
ggplot(result_final_new, aes(x = retweetCount, y = date)) + geom_point(aes(col = day)) + xlab("Retweet Count") + ylab("Date") + ggtitle("Retweet Count Distribution- Dates") 

#Retweet count distribution over 'hour' attribute
ggplot(result_final_new, aes(x = hour, y = retweetCount)) + geom_point(aes(col = day)) + xlab("Hours") + ylab("Retweet Count") + ggtitle("Retweet Count Distribution- Hours") 

# Retweet count distribution over 'day' attribute
ggplot(result_final_new, aes(x = day, y = retweetCount)) + geom_point(aes(col = hour)) + facet_grid(wday ~.) + geom_line(colour = "plum") + xlab("Days") + ylab("Retweet Count") + theme(legend.position='none') + ggtitle("Retweet Count Distribution- Days")
DT <- as.data.table(result_final_new)
DT[, list(totalretweetCount = sum(retweetCount), num = .N), by = wday]

# Save the processed dataset
write.csv(result_final_new, "result_final_new.csv", row.names = F)

#Linear Regression & Trees
# Split the dataset into training and testing set to perform linear regression and predict the model
# Split the data into 80:20 ratio
result_smp_size <- floor(0.80 * nrow(result_final_new))

# Set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(result_final_new)), size = result_smp_size)
train <- result_final_new[train_ind, ]
test <- result_final_new[-train_ind, ]

# Create the model 
Model_1 <- lm(retweetCount ~ isRetweet + CaramelFix_2 + CaramelFix_3 + CaramelFix_5 + StoreTime_4 + Morning, data = train) 

# Inspect the model
summary(Model_1)

# Test the model on 'test' data
result_predict <- predict(Model_1, newdata = test)

# Inspect predicted model for difference in predicted and observed value of the test data
head(result_predict)

# And create a dataframe
lmValues1 <- data.frame(obs = test$retweetCount, pred = result_predict)

# Insected the predicted RMSE & R-squared
defaultSummary(lmValues1)

# Create two simple tree models using rpart and party packages

# with rpart package
myFormula <- retweetCount ~ isRetweet + CaramelFix_2 + CaramelFix_3 + CaramelFix_5 + StoreTime_4 + Morning
rpart_model <- rpart(myFormula, data = train)
predict_rpart <- predict(rpart_model, newdata = test)
head(predict_rpart)
fancyRpartPlot(rpart_model)
postResample(predict_rpart, test$retweetCount)

# with party package
ctree_model <- ctree(myFormula, data=train)
predict_ctree <- predict(ctree_model, newdata = test)
head(predict_ctree)
plot(ctree_model, type="simple") 
postResample(predict_ctree, test$retweetCount)

# Random Forest
rf_model <- randomForest(myFormula, data=train, importance = T, ntree= 100)
predict_rf <- predict(rf_model, newdata = test)
head(predict_rf)
varImpPlot(rf_model)
importance(rf_model)
postResample(predict_rf, test$retweetCount)

# Plot r-squared for all the three models
Model_rsquared <- c("0.029", "0.15", "0.13", "0.12")
Model_Names <- c("Regression Model", "Rpart Model", "CART Model", "Random Forest")
Model_compare <- data.frame(Model_rsquared = Model_rsquared, Model_Names = Model_Names)
ggplot(Model_compare, aes(x = Model_rsquared, y = Model_Names)) + geom_point(aes(col = Model_Names)) + xlab("R-squared value") + ylab("Model Type") + ggtitle("Plot to compare R-squared values") 

# Project done!
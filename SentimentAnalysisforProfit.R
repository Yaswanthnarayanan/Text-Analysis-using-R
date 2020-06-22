sample<-read.csv("C:/Users/Sony/Desktop/Sentiment Analysis/data.csv", stringsAsFactors=FALSE)
pro_data<-sample$Pros
con_data<-sample$Cons

library(tidyr)
library(ggplot2)
library(qdap)
library(tm)
library(RWeka)
library(SnowballC)
library(plotrix)
library(wordcloud)
library(tidytext)
library(dplyr)
library(widyr)
library(stringr)


stops <- c("with","can","to","i","the","has","is","to","and","it","was","are","that","apptivo", "crm")

#"customer support","customer service","support team","chat support","great support","live chat","support chat","excellent customer","support great","online suppport","tech support","good support","help desk","easy","use","great","business","support","like"

pro_clean_data<-tolower(pro_data)
pro_clean_data <- removeWords(pro_clean_data, stopwords("en"))
pro_clean_data<- removeWords(pro_clean_data, stops)
pro_clean_data<-removePunctuation(pro_clean_data)
pro_clean_data<-stripWhitespace(pro_clean_data)
pro_clean_data<-removeNumbers(pro_clean_data)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector<-VectorSource(pro_clean_data)
pro_clean_corpus<-VCorpus(pro_clean_vector)


#uni_word_barplot_analysis
uni_pro_TDM_clean_corpus <- TermDocumentMatrix(pro_clean_corpus)
uni_pro_TDM_clean_corpus_matrix<-as.matrix(uni_pro_TDM_clean_corpus)
uni_pro_frequency <- rowSums(uni_pro_TDM_clean_corpus_matrix)
sorted_uni_pro_frequency<-sort(uni_pro_frequency,decreasing=TRUE)
barplot(sorted_uni_pro_frequency[1:10], col = "tan", las = 2)


#bi_word_barplot_analysis

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus <- TermDocumentMatrix(pro_clean_corpus, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix<-as.matrix(bi_pro_TDM_clean_corpus)
bi_pro_frequency <- rowSums(bi_pro_TDM_clean_corpus_matrix)
bi_pro_sorted_frequency <- sort(bi_pro_frequency, decreasing = TRUE)
bi_pro_sorted_frequency_df<-data.frame(word=names(bi_pro_sorted_frequency), frequency=bi_pro_sorted_frequency)
ggplot(head(bi_pro_sorted_frequency_df,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams") 

#bi_word_wordcloud_analysis

stops2 <- c("with","can","to","i","the","has","is","to","and","it","was","are","that","apptivo", "crm", "customer support","customer service","support team","chat support","online chat","best support","chat support","great support","live chat","support chat","excellent customer","support great","online suppport","tech support","good support","help desk")
#,"easy","use","great","business","support","like"

pro_clean_data_2<- removeWords(pro_clean_data, stops2)
pro_clean_vector_2<-VectorSource(pro_clean_data_2)
pro_clean_corpus_2<-VCorpus(pro_clean_vector_2)
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2 <- TermDocumentMatrix(pro_clean_corpus_2, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2<-as.matrix(bi_pro_TDM_clean_corpus_2)
bi_pro_frequency <- rowSums(bi_pro_TDM_clean_corpus_matrix_2)
wordcloud(names(bi_pro_frequency),bi_pro_frequency, max.words = 25, color = "blue")


#calculating wordpair for uni word

stops3 <-  c("easy","use","great","business","support","like","customer")
pro_clean_data_3<- removeWords(pro_clean_data_2, stops3)



pro_clean_data_df<-as.data.frame(pro_clean_data_3,row.names=NULL, stringsAsFactors=FALSE)
unnest_pro_clean_data_df<-pro_clean_data_df %>%
  mutate(section = row_number() %/% 10, document = 1) %>%
  filter(section > 0) %>%
  rename(text=pro_clean_data_3) %>%
  unnest_tokens(word, text)

word_pairs <- unnest_pro_clean_data_df %>%
  pairwise_count(word, section, sort = TRUE)

head(word_pairs, 20)



#calculating word paid for biwords
bi_unnest_pro_clean_data_df<-pro_clean_data_df %>%
  mutate(section = row_number() %/% 10, document = 1) %>%
  filter(section > 0) %>%
  rename(text=pro_clean_data_3) %>%
  unnest_tokens(word, text, token="ngrams", n=2)


bi_pro_word_pairs <- bi_unnest_pro_clean_data_df %>%
  pairwise_count(word, section, sort = TRUE)

head(bi_pro_word_pairs,20)


#term_freq_name<-names(term_frequency)

associations<-findAssocs(uni_pro_TDM_clean_corpus, "invoice" , 0.25)
associations











con_clean_data<-tolower(con_data)
con_clean_data <- removeWords(con_clean_data, stopwords("en"))
con_clean_data<- removeWords(con_clean_data, stops)
con_clean_data<-removePunctuation(con_clean_data)
con_clean_data<-stripWhitespace(con_clean_data)
con_clean_data<-removeNumbers(con_clean_data)
#pro_clean_data<-stemDocument(con_clean_data)
con_clean_vector<-VectorSource(con_clean_data)
con_clean_corpus<-VCorpus(con_clean_vector)


#uni_word_barplot_analysis
uni_con_TDM_clean_corpus <- TermDocumentMatrix(con_clean_corpus)
uni_con_TDM_clean_corpus_matrix<-as.matrix(uni_con_TDM_clean_corpus)
uni_con_frequency <- rowSums(uni_con_TDM_clean_corpus_matrix)
sorted_uni_con_frequency<-sort(uni_con_frequency,decreasing=TRUE)
barplot(sorted_uni_con_frequency[1:10], col = "tan", las = 2)


#bi_word_barplot_analysis

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_con_TDM_clean_corpus <- TermDocumentMatrix(con_clean_corpus, control = list(tokenize = tokenizer))
bi_con_TDM_clean_corpus_matrix<-as.matrix(bi_con_TDM_clean_corpus)
bi_con_frequency <- rowSums(bi_con_TDM_clean_corpus_matrix)
bi_con_sorted_frequency <- sort(bi_con_frequency, decreasing = TRUE)
bi_con_sorted_frequency_df<-data.frame(word=names(bi_con_sorted_frequency), frequency=bi_con_sorted_frequency)
ggplot(head(bi_con_sorted_frequency_df,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams") 


#bi_word_wordcloud_analysis

stops2 <- c("with","can","to","i","the","has","is","to","and","it","was","are","that","apptivo", "crm", "customer support","customer service","support team","chat support","great support","live chat","support chat","excellent customer","support great","online suppport","tech support","good support","help desk")
#,"easy","use","great","business","support","like"

con_clean_data_2<- removeWords(con_clean_data, stops2)
con_clean_vector_2<-VectorSource(con_clean_data_2)
con_clean_corpus_2<-VCorpus(con_clean_vector_2)
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_con_TDM_clean_corpus_2 <- TermDocumentMatrix(con_clean_corpus_2, control = list(tokenize = tokenizer))
bi_con_TDM_clean_corpus_matrix_2<-as.matrix(bi_con_TDM_clean_corpus_2)
bi_con_frequency <- rowSums(bi_con_TDM_clean_corpus_matrix_2)
wordcloud(names(bi_con_frequency),bi_pro_frequency, max.words = 25, color = "red",scale=c(5.75,.15))



#bi_word_wordcloud_analysis

stops2 <- c("with","can","to","i","the","has","is","to","and","it","was","are","that","apptivo", "crm", "customer support","customer service","support team","chat support","great support","live chat","support chat","excellent customer","support great","online suppport","tech support","good support","help desk")
#,"easy","use","great","business","support","like"

con_clean_data_2<- removeWords(con_clean_data, stops2)
con_clean_vector_2<-VectorSource(con_clean_data_2)
con_clean_corpus_2<-VCorpus(con_clean_vector_2)
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_con_TDM_clean_corpus_2 <- TermDocumentMatrix(con_clean_corpus_2, control = list(tokenize = tokenizer))
bi_con_TDM_clean_corpus_matrix_2<-as.matrix(bi_con_TDM_clean_corpus_2)
bi_con_frequency <- rowSums(bi_con_TDM_clean_corpus_matrix_2)
wordcloud(names(bi_con_frequency),bi_con_frequency, max.words = 25, color = "Red")



#calculating wordpair for uni word

stops3 <-  c("easy","use","great","business","support","like","customer")
con_clean_data_3<- removeWords(con_clean_data_2, stops)



con_clean_data_df<-as.data.frame(con_clean_data_3,row.names=NULL, stringsAsFactors=FALSE)
unnest_con_clean_data_df<-con_clean_data_df %>%
  mutate(section = row_number() %/% 10, document = 1) %>%
  filter(section > 0) %>%
  rename(text=con_clean_data_3) %>%
  unnest_tokens(word, text)

word_pairs <- unnest_con_clean_data_df %>%
  pairwise_count(word, section, sort = TRUE)

head(word_pairs, 20)



#calculating word paid for biwords
bi_unnest_con_clean_data_df<-con_clean_data_df %>%
  mutate(section = row_number() %/% 10, document = 1) %>%
  filter(section > 0) %>%
  rename(text=con_clean_data_3) %>%
  unnest_tokens(word, text, token="ngrams", n=2)


bi_con_word_pairs <- bi_unnest_con_clean_data_df %>%
  pairwise_count(word, section, sort = TRUE)

head(bi_con_word_pairs,20)


#term_freq_name<-names(term_frequency)

associations<-findAssocs(bi_con_TDM_clean_corpus, "search reports" , 0.25)
associations








Positivity_2019 <- sample %>%
  select(Year, Pros) %>%
  filter(Year==2019)

pro_clean_data_2019<-tolower(Positivity_2019$Pros)
pro_clean_data_2019 <- removeWords(pro_clean_data_2019, stopwords("en"))
pro_clean_data_2019<- removeWords(pro_clean_data_2019, stops2)
pro_clean_data_2019<-removePunctuation(pro_clean_data_2019)
pro_clean_data_2019<-stripWhitespace(pro_clean_data_2019)
pro_clean_data_2019<-removeNumbers(pro_clean_data_2019)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector_2019<-VectorSource(pro_clean_data_2019)
pro_clean_corpus_2019<-VCorpus(pro_clean_vector_2019)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2019 <- TermDocumentMatrix(pro_clean_corpus_2019, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2019<-as.matrix(bi_pro_TDM_clean_corpus_2019)
bi_pro_frequency_2019 <- rowSums(bi_pro_TDM_clean_corpus_matrix_2019)

bi_pro_sorted_frequency_2019 <- sort(bi_pro_frequency_2019, decreasing = TRUE)
bi_pro_sorted_frequency_df_2019<-data.frame(word=names(bi_pro_sorted_frequency_2019), frequency=bi_pro_sorted_frequency_2019)
bi_pro_sorted_frequency_df_2019$Year<-2019


ggplot(head(bi_pro_sorted_frequency_df_2019,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent positive bigrams for 2019") 













Positivity_2018 <- sample %>%
  select(Year, Pros) %>%
  filter(Year==2018)

pro_clean_data_2018<-tolower(Positivity_2018$Pros)
pro_clean_data_2018 <- removeWords(pro_clean_data_2018, stopwords("en"))
pro_clean_data_2018<- removeWords(pro_clean_data_2018, stops2)
pro_clean_data_2018<-removePunctuation(pro_clean_data_2018)
pro_clean_data_2018<-stripWhitespace(pro_clean_data_2018)
pro_clean_data_2018<-removeNumbers(pro_clean_data_2018)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector_2018<-VectorSource(pro_clean_data_2018)
pro_clean_corpus_2018<-VCorpus(pro_clean_vector_2018)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2018 <- TermDocumentMatrix(pro_clean_corpus_2018, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2018<-as.matrix(bi_pro_TDM_clean_corpus_2018)
bi_pro_frequency_2018 <- rowSums(bi_pro_TDM_clean_corpus_matrix_2018)

bi_pro_sorted_frequency_2018 <- sort(bi_pro_frequency_2018, decreasing = TRUE)
bi_pro_sorted_frequency_df_2018<-data.frame(word=names(bi_pro_sorted_frequency_2018), frequency=bi_pro_sorted_frequency_2018)
bi_pro_sorted_frequency_df_2018$Year<-2018


ggplot(head(bi_pro_sorted_frequency_df_2018,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent positive bigrams for 2018") 










Positivity_2017 <- sample %>%
  select(Year, Pros) %>%
  filter(Year==2017)

pro_clean_data_2017<-tolower(Positivity_2017$Pros)
pro_clean_data_2017 <- removeWords(pro_clean_data_2017, stopwords("en"))
pro_clean_data_2017<- removeWords(pro_clean_data_2017, stops2)
pro_clean_data_2017<-removePunctuation(pro_clean_data_2017)
pro_clean_data_2017<-stripWhitespace(pro_clean_data_2017)
pro_clean_data_2017<-removeNumbers(pro_clean_data_2017)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector_2017<-VectorSource(pro_clean_data_2017)
pro_clean_corpus_2017<-VCorpus(pro_clean_vector_2017)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2017 <- TermDocumentMatrix(pro_clean_corpus_2017, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2017<-as.matrix(bi_pro_TDM_clean_corpus_2017)
bi_pro_frequency_2017 <- rowSums(bi_pro_TDM_clean_corpus_matrix_2017)

bi_pro_sorted_frequency_2017 <- sort(bi_pro_frequency_2017, decreasing = TRUE)
bi_pro_sorted_frequency_df_2017<-data.frame(word=names(bi_pro_sorted_frequency_2017), frequency=bi_pro_sorted_frequency_2017)
bi_pro_sorted_frequency_df_2017$Year<-2017

ggplot(head(bi_pro_sorted_frequency_df_2017,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent positive bigrams for 2017") 












Positivity_2016 <- sample %>%
  select(Year, Pros) %>%
  filter(Year==2016)

pro_clean_data_2016<-tolower(Positivity_2016$Pros)
pro_clean_data_2016 <- removeWords(pro_clean_data_2016, stopwords("en"))
pro_clean_data_2016<- removeWords(pro_clean_data_2016, stops2)
pro_clean_data_2016<-removePunctuation(pro_clean_data_2016)
pro_clean_data_2016<-stripWhitespace(pro_clean_data_2016)
pro_clean_data_2016<-removeNumbers(pro_clean_data_2016)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector_2016<-VectorSource(pro_clean_data_2016)
pro_clean_corpus_2016<-VCorpus(pro_clean_vector_2016)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2016 <- TermDocumentMatrix(pro_clean_corpus_2016, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2016<-as.matrix(bi_pro_TDM_clean_corpus_2016)
bi_pro_frequency_2016 <- rowSums(bi_pro_TDM_clean_corpus_matrix_2016)

bi_pro_sorted_frequency_2016 <- sort(bi_pro_frequency_2016, decreasing = TRUE)
bi_pro_sorted_frequency_df_2016<-data.frame(word=names(bi_pro_sorted_frequency_2016), frequency=bi_pro_sorted_frequency_2016)
bi_pro_sorted_frequency_df_2016$Year<-2016

ggplot(head(bi_pro_sorted_frequency_df_2016,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent positive bigrams for 2016") 








Positivity_2015 <- sample %>%
  select(Year, Pros) %>%
  filter(Year==2015)

pro_clean_data_2015<-tolower(Positivity_2015$Pros)
pro_clean_data_2015 <- removeWords(pro_clean_data_2015, stopwords("en"))
pro_clean_data_2015<- removeWords(pro_clean_data_2015, stops2)
pro_clean_data_2015<-removePunctuation(pro_clean_data_2015)
pro_clean_data_2015<-stripWhitespace(pro_clean_data_2015)
pro_clean_data_2015<-removeNumbers(pro_clean_data_2015)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector_2015<-VectorSource(pro_clean_data_2015)
pro_clean_corpus_2015<-VCorpus(pro_clean_vector_2015)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2015 <- TermDocumentMatrix(pro_clean_corpus_2015, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2015<-as.matrix(bi_pro_TDM_clean_corpus_2015)
bi_pro_frequency_2015 <- rowSums(bi_pro_TDM_clean_corpus_matrix_2015)

bi_pro_sorted_frequency_2015 <- sort(bi_pro_frequency_2015, decreasing = TRUE)
bi_pro_sorted_frequency_df_2015<-data.frame(word=names(bi_pro_sorted_frequency_2015), frequency=bi_pro_sorted_frequency_2015)
bi_pro_sorted_frequency_df_2015$Year<-2015


ggplot(head(bi_pro_sorted_frequency_df_2015,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent positive bigrams for 2015") 











Positivity_2014 <- sample %>%
  select(Year, Pros) %>%
  filter(Year==2014)

pro_clean_data_2014<-tolower(Positivity_2014$Pros)
pro_clean_data_2014 <- removeWords(pro_clean_data_2014, stopwords("en"))
pro_clean_data_2014<- removeWords(pro_clean_data_2014, stops2)
pro_clean_data_2014<-removePunctuation(pro_clean_data_2014)
pro_clean_data_2014<-stripWhitespace(pro_clean_data_2014)
pro_clean_data_2014<-removeNumbers(pro_clean_data_2014)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector_2014<-VectorSource(pro_clean_data_2014)
pro_clean_corpus_2014<-VCorpus(pro_clean_vector_2014)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2014 <- TermDocumentMatrix(pro_clean_corpus_2014, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2014<-as.matrix(bi_pro_TDM_clean_corpus_2014)
bi_pro_frequency_2014 <- rowSums(bi_pro_TDM_clean_corpus_matrix_2014)

bi_pro_sorted_frequency_2014 <- sort(bi_pro_frequency_2014, decreasing = TRUE)
bi_pro_sorted_frequency_df_2014<-data.frame(word=names(bi_pro_sorted_frequency_2014), frequency=bi_pro_sorted_frequency_2014)
bi_pro_sorted_frequency_df_2014$Year<-2014


ggplot(head(bi_pro_sorted_frequency_df_2014,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent positive bigrams for 2014") 










Positivity_2013 <- sample %>%
  select(Year, Pros) %>%
  filter(Year==2013)

pro_clean_data_2013<-tolower(Positivity_2013$Pros)
pro_clean_data_2013 <- removeWords(pro_clean_data_2013, stopwords("en"))
pro_clean_data_2013<- removeWords(pro_clean_data_2013, stops2)
pro_clean_data_2013<-removePunctuation(pro_clean_data_2013)
pro_clean_data_2013<-stripWhitespace(pro_clean_data_2013)
pro_clean_data_2013<-removeNumbers(pro_clean_data_2013)
#pro_c_data<-stemDocument(pro_c_data)
pro_clean_vector_2013<-VectorSource(pro_clean_data_2013)
pro_clean_corpus_2013<-VCorpus(pro_clean_vector_2013)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_pro_TDM_clean_corpus_2013 <- TermDocumentMatrix(pro_clean_corpus_2013, control = list(tokenize = tokenizer))
bi_pro_TDM_clean_corpus_matrix_2013<-as.matrix(bi_pro_TDM_clean_corpus_2013)
bi_pro_frequency_2013 <- rowSums(bi_pro_TDM_clean_corpus_matrix_2013)

bi_pro_sorted_frequency_2013 <- sort(bi_pro_frequency_2013, decreasing = TRUE)
bi_pro_sorted_frequency_df_2013<-data.frame(word=names(bi_pro_sorted_frequency_2013), frequency=bi_pro_sorted_frequency_2013)
bi_pro_sorted_frequency_df_2013$Year<-2013


ggplot(head(bi_pro_sorted_frequency_df_2013,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams for 2013") 

unified<-rbind(head(bi_pro_sorted_frequency_df_2019,25),head(bi_pro_sorted_frequency_df_2018,25),head(bi_pro_sorted_frequency_df_2017,25),head(bi_pro_sorted_frequency_df_2016,25),head(bi_pro_sorted_frequency_df_2015,25),head(bi_pro_sorted_frequency_df_2014,25))
unified<-as.data.frame(unified)





ggplot(unified, aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") + facet_wrap(~Year) + theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams for all years") 


















Negativity_2019 <- sample %>%
  select(Year, Cons) %>%
  filter(Year==2019)

Con_clean_data_2019<-tolower(Negativity_2019$Cons)
Con_clean_data_2019 <- removeWords(Con_clean_data_2019, stopwords("en"))
Con_clean_data_2019<- removeWords(Con_clean_data_2019, stops2)
Con_clean_data_2019<-removePunctuation(Con_clean_data_2019)
Con_clean_data_2019<-stripWhitespace(Con_clean_data_2019)
Con_clean_data_2019<-removeNumbers(Con_clean_data_2019)

Con_clean_data_2019_df<-as.data.frame(Con_clean_data_2019)
#Con_c_data<-stemDocument(Con_c_data)
Con_clean_vector_2019<-VectorSource(Con_clean_data_2019)
Con_clean_corpus_2019<-VCorpus(Con_clean_vector_2019)
uni_Con_TDM_2019 <- TermDocumentMatrix(Con_clean_corpus_2019)
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_Con_TDM_clean_corpus_2019 <- TermDocumentMatrix(Con_clean_corpus_2019, control = list(tokenize = tokenizer))
bi_Con_TDM_clean_corpus_matrix_2019<-as.matrix(bi_Con_TDM_clean_corpus_2019)
bi_Con_frequency_2019 <- rowSums(bi_Con_TDM_clean_corpus_matrix_2019)

bi_Con_sorted_frequency_2019 <- sort(bi_Con_frequency_2019, decreasing = TRUE)
bi_Con_sorted_frequency_df_2019<-data.frame(word=names(bi_Con_sorted_frequency_2019), frequency=bi_Con_sorted_frequency_2019)
ggplot(head(bi_Con_sorted_frequency_df_2019,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent negative bigrams for 2019") 



associations4<-findAssocs(bi_Con_TDM_clean_corpus_2019 , "recurring invoices" , 0.50)
associations4
























Negativity_2018 <- sample %>%
  select(Year, Cons) %>%
  filter(Year==2018)

Con_clean_data_2018<-tolower(Negativity_2018$Cons)
Con_clean_data_2018 <- removeWords(Con_clean_data_2018, stopwords("en"))
Con_clean_data_2018<- removeWords(Con_clean_data_2018, stops2)
Con_clean_data_2018<-removePunctuation(Con_clean_data_2018)
Con_clean_data_2018<-stripWhitespace(Con_clean_data_2018)
Con_clean_data_2018<-removeNumbers(Con_clean_data_2018)
#Con_c_data<-stemDocument(Con_c_data)
Con_clean_vector_2018<-VectorSource(Con_clean_data_2018)
Con_clean_corpus_2018<-VCorpus(Con_clean_vector_2018)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_Con_TDM_clean_corpus_2018 <- TermDocumentMatrix(Con_clean_corpus_2018, control = list(tokenize = tokenizer))
bi_Con_TDM_clean_corpus_matrix_2018<-as.matrix(bi_Con_TDM_clean_corpus_2018)
bi_Con_frequency_2018 <- rowSums(bi_Con_TDM_clean_corpus_matrix_2018)

bi_Con_sorted_frequency_2018 <- sort(bi_Con_frequency_2018, decreasing = TRUE)
bi_Con_sorted_frequency_df_2018<-data.frame(word=names(bi_Con_sorted_frequency_2018), frequency=bi_Con_sorted_frequency_2018)
ggplot(head(bi_Con_sorted_frequency_df_2018,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent Negative bigrams for 2018") 


associations5<-findAssocs(bi_Con_TDM_clean_corpus_2018 , "real estate" , 0.25)
associations5








Negativity_2017 <- sample %>%
  select(Year, Cons) %>%
  filter(Year==2017)

Con_clean_data_2017<-tolower(Negativity_2017$Cons)
Con_clean_data_2017 <- removeWords(Con_clean_data_2017, stopwords("en"))
Con_clean_data_2017<- removeWords(Con_clean_data_2017, stops2)
Con_clean_data_2017<-removePunctuation(Con_clean_data_2017)
Con_clean_data_2017<-stripWhitespace(Con_clean_data_2017)
Con_clean_data_2017<-removeNumbers(Con_clean_data_2017)
#Con_c_data<-stemDocument(Con_c_data)
Con_clean_vector_2017<-VectorSource(Con_clean_data_2017)
Con_clean_corpus_2017<-VCorpus(Con_clean_vector_2017)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_Con_TDM_clean_corpus_2017 <- TermDocumentMatrix(Con_clean_corpus_2017, control = list(tokenize = tokenizer))
bi_Con_TDM_clean_corpus_matrix_2017<-as.matrix(bi_Con_TDM_clean_corpus_2017)
bi_Con_frequency_2017 <- rowSums(bi_Con_TDM_clean_corpus_matrix_2017)

bi_Con_sorted_frequency_2017 <- sort(bi_Con_frequency_2017, decreasing = TRUE)
bi_Con_sorted_frequency_df_2017<-data.frame(word=names(bi_Con_sorted_frequency_2017), frequency=bi_Con_sorted_frequency_2017)
ggplot(head(bi_Con_sorted_frequency_df_2017,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent Negative bigrams for 2017") 












Negativity_2016 <- sample %>%
  select(Year, Cons) %>%
  filter(Year==2016)

Con_clean_data_2016<-tolower(Negativity_2016$Cons)
Con_clean_data_2016 <- removeWords(Con_clean_data_2016, stopwords("en"))
Con_clean_data_2016<- removeWords(Con_clean_data_2016, stops2)
Con_clean_data_2016<-removePunctuation(Con_clean_data_2016)
Con_clean_data_2016<-stripWhitespace(Con_clean_data_2016)
Con_clean_data_2016<-removeNumbers(Con_clean_data_2016)
#Con_c_data<-stemDocument(Con_c_data)
Con_clean_vector_2016<-VectorSource(Con_clean_data_2016)
Con_clean_corpus_2016<-VCorpus(Con_clean_vector_2016)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_Con_TDM_clean_corpus_2016 <- TermDocumentMatrix(Con_clean_corpus_2016, control = list(tokenize = tokenizer))
bi_Con_TDM_clean_corpus_matrix_2016<-as.matrix(bi_Con_TDM_clean_corpus_2016)
bi_Con_frequency_2016 <- rowSums(bi_Con_TDM_clean_corpus_matrix_2016)

bi_Con_sorted_frequency_2016 <- sort(bi_Con_frequency_2016, decreasing = TRUE)
bi_Con_sorted_frequency_df_2016<-data.frame(word=names(bi_Con_sorted_frequency_2016), frequency=bi_Con_sorted_frequency_2016)
ggplot(head(bi_Con_sorted_frequency_df_2016,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent Negative bigrams for 2016") 








Negativity_2015 <- sample %>%
  select(Year, Cons) %>%
  filter(Year==2015)

Con_clean_data_2015<-tolower(Negativity_2015$Cons)
Con_clean_data_2015 <- removeWords(Con_clean_data_2015, stopwords("en"))
Con_clean_data_2015<- removeWords(Con_clean_data_2015, stops2)
Con_clean_data_2015<-removePunctuation(Con_clean_data_2015)
Con_clean_data_2015<-stripWhitespace(Con_clean_data_2015)
Con_clean_data_2015<-removeNumbers(Con_clean_data_2015)
#Con_c_data<-stemDocument(Con_c_data)
Con_clean_vector_2015<-VectorSource(Con_clean_data_2015)
Con_clean_corpus_2015<-VCorpus(Con_clean_vector_2015)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_Con_TDM_clean_corpus_2015 <- TermDocumentMatrix(Con_clean_corpus_2015, control = list(tokenize = tokenizer))
bi_Con_TDM_clean_corpus_matrix_2015<-as.matrix(bi_Con_TDM_clean_corpus_2015)
bi_Con_frequency_2015 <- rowSums(bi_Con_TDM_clean_corpus_matrix_2015)

bi_Con_sorted_frequency_2015 <- sort(bi_Con_frequency_2015, decreasing = TRUE)
bi_Con_sorted_frequency_df_2015<-data.frame(word=names(bi_Con_sorted_frequency_2015), frequency=bi_Con_sorted_frequency_2015)
ggplot(head(bi_Con_sorted_frequency_df_2015,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent Negative bigrams for 2015") 











Negativity_2014 <- sample %>%
  select(Year, Cons) %>%
  filter(Year==2014)

Con_clean_data_2014<-tolower(Negativity_2014$Cons)
Con_clean_data_2014 <- removeWords(Con_clean_data_2014, stopwords("en"))
Con_clean_data_2014<- removeWords(Con_clean_data_2014, stops2)
Con_clean_data_2014<-removePunctuation(Con_clean_data_2014)
Con_clean_data_2014<-stripWhitespace(Con_clean_data_2014)
Con_clean_data_2014<-removeNumbers(Con_clean_data_2014)
#Con_c_data<-stemDocument(Con_c_data)
Con_clean_vector_2014<-VectorSource(Con_clean_data_2014)
Con_clean_corpus_2014<-VCorpus(Con_clean_vector_2014)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_Con_TDM_clean_corpus_2014 <- TermDocumentMatrix(Con_clean_corpus_2014, control = list(tokenize = tokenizer))
bi_Con_TDM_clean_corpus_matrix_2014<-as.matrix(bi_Con_TDM_clean_corpus_2014)
bi_Con_frequency_2014 <- rowSums(bi_Con_TDM_clean_corpus_matrix_2014)

bi_Con_sorted_frequency_2014 <- sort(bi_Con_frequency_2014, decreasing = TRUE)
bi_Con_sorted_frequency_df_2014<-data.frame(word=names(bi_Con_sorted_frequency_2014), frequency=bi_Con_sorted_frequency_2014)
ggplot(head(bi_Con_sorted_frequency_df_2014,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent Negative bigrams for 2014") 










Negativity_2013 <- sample %>%
  select(Year, Cons) %>%
  filter(Year==2013)

Con_clean_data_2013<-tolower(Negativity_2013$Cons)
Con_clean_data_2013 <- removeWords(Con_clean_data_2013, stopwords("en"))
Con_clean_data_2013<- removeWords(Con_clean_data_2013, stops2)
Con_clean_data_2013<-removePunctuation(Con_clean_data_2013)
Con_clean_data_2013<-stripWhitespace(Con_clean_data_2013)
Con_clean_data_2013<-removeNumbers(Con_clean_data_2013)
#Con_c_data<-stemDocument(Con_c_data)
Con_clean_vector_2013<-VectorSource(Con_clean_data_2013)
Con_clean_corpus_2013<-VCorpus(Con_clean_vector_2013)

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

bi_Con_TDM_clean_corpus_2013 <- TermDocumentMatrix(Con_clean_corpus_2013, control = list(tokenize = tokenizer))
bi_Con_TDM_clean_corpus_matrix_2013<-as.matrix(bi_Con_TDM_clean_corpus_2013)
bi_Con_frequency_2013 <- rowSums(bi_Con_TDM_clean_corpus_matrix_2013)

bi_Con_sorted_frequency_2013 <- sort(bi_Con_frequency_2013, decreasing = TRUE)
bi_Con_sorted_frequency_df_2013<-data.frame(word=names(bi_Con_sorted_frequency_2013), frequency=bi_Con_sorted_frequency_2013)
ggplot(head(bi_Con_sorted_frequency_df_2013,25), aes(reorder(word,-frequency), frequency)) +  
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90, hjust=1)) +  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams for 2013") 


























sample$Cons[sample$Cons=="-"]<-NA
sum(is.na(sample$Cons))

sample$Pros[sample$Pros=="-"]<-NA
sum(is.na(sample$Pros))



sample$Date<-as.Date(sample$Date,"%Y-%m-%d")
typeof(sample$Date)



#Line plot
pro_count_yearwise<-sample %>%
              select(Year,Pros) %>%
            na.omit() %>%
             group_by(Year) %>%
              arrange(Year) %>%
                summarise(Positive_count=n())


con_count_yearwise<-sample %>%
  select(Year,Cons) %>%
  na.omit() %>%
  group_by(Year) %>%
  arrange(Year) %>%
  summarise(negative_count=n())


pro_count_yearwise
con_count_yearwise


Year_count<-merge(pro_count_yearwise,con_count_yearwise)
Year_count

p_year<-ggplot(Year_count,aes(y = Positive_count, x = Date)) + geom_line(color = "#FC4E07", size = 1) +
  xlab("Year")  + ylab("Positive Count")
                       
                      

n_year<-ggplot(Year_count,aes(y = negative_count, x = Year)) + geom_line(color = "#FC4E07", size = 1) +
  xlab("Year")  + ylab("negative Count")





  






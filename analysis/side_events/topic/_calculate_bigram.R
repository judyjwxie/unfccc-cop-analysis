library(openxlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(tm) # text mining package
library(ggplot2)
library(readr)
library(stringi)
library(RWeka)
library(stringr)
library(reshape2)


# text mining tutorial https://medium.com/text-mining-in-data-science-a-tutorial-of-text/text-mining-in-data-science-51299e4e594

file_dir <- "C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/COP_Analysis/data/side_events"
topic <- "energy"
# "food","energy","redd"


calc_bigram <- function(topic,this_cop){
  fname <- paste0("extracted_events_",topic,".csv")
  input_file_name <- file.path(file_dir,fname)
  JT_side_events <- read.csv(input_file_name, encoding = "UTF-8")
  df_title <- data.frame(doc_id=JT_side_events$SE_Code,
                         text=JT_side_events$`Title.Description_new`)
  df_title <- df_title[!duplicated(df_title), ]
  df_title <- data.frame(lapply(df_title, function(x) {
    gsub("=", "", x)}))
  
  #this_cop <- "COP27"
  filtered_df <- df_title[grepl(this_cop, df_title$doc_id), ]
  if(nrow(filtered_df)==0){ # if  event topic doesn't exist in this COP
    empty_df <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(empty_df) <- c("word",cop_number)
    return(empty_df)
  } 
  else{
    
    df_corpus <- VCorpus(DataframeSource(filtered_df))
    # clean dataset
    remove_symbols <- function(x){
      x <- gsub("–"," ",x)
      x <- gsub("="," ",x)
      x <- gsub('”',"",x)
      x <- gsub('“',"",x)
      x <- gsub('‘',"",x)
      x <- gsub('’',"",x)
    }
    df_corpus <- tm_map(df_corpus, removePunctuation) # remove punctuation
    df_corpus <- tm_map(df_corpus, content_transformer(tolower)) # convert to lower case
    df_corpus <- tm_map(df_corpus, removeWords, stopwords("french")) 
    df_corpus <- tm_map(df_corpus, removeWords, stopwords("english")) # remove common English words
    df_corpus <- tm_map(df_corpus, content_transformer(remove_symbols))
    df_corpus <- tm_map(df_corpus, removeNumbers)
    
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
    df_dtm <- TermDocumentMatrix(df_corpus, control=list(tokenize=BigramTokenizer,minWordLength = 2))
    tdm_freq <- as.matrix(df_dtm) 
    words <- sort(rowSums(tdm_freq),decreasing=TRUE) 
    tdm_freq <- data.frame(word = names(words),freq=words)
    return(tdm_freq)
    }
}

# calculate bigrams across years
start_cop <- 9  
end_cop <- 28   
result_df <- NULL
for (cop_number in seq(start_cop, end_cop, by = 1)) {
  this_cop <- paste0("COP", str_pad(cop_number, 2, pad = "0"))
  this_tdm <- calc_bigram(topic,this_cop)
  names(this_tdm)[names(this_tdm) == "freq"] <- cop_number
  if (is.null(result_df)) {
    result_df <- this_tdm
  } else {
    # Concatenate the dataframes column-wise
    common_column <- "word"
    result_df <- merge(result_df, this_tdm, by = common_column, all = TRUE)
  }
  
}

# clean data
result_df_clean <- data.table::copy(result_df)
result_df_clean[is.na(result_df_clean)] <- 0
#if(topic=="food"){
#  result_df_clean$`9`<-0
#  result_df_clean$`12`<-0
#}
col_copnames <- names(result_df_clean)
# group words together
result_df_clean[result_df_clean == "food systems" ]<- "food system"
result_df_clean[result_df_clean == "climate resilience" ]<- "climate resilient"
result_df_clean[result_df_clean == "just transitions" ]<- "just transition"
result_df_clean[result_df_clean == "just energy" ]<- "just transition"
result_df_clean[result_df_clean == "fossil fuels" ]<- "fossil fuel"
result_df_clean[result_df_clean == "climatesmart agriculture" ]<- "smart agriculture"
result_df_clean <- result_df_clean %>%
  group_by(word)%>%
  summarise(across(where(is.numeric),sum))
# remove common words


keywords_to_remove <- c("climate change", "climate action", "event will", "will explore",
                        "net zero","climate crisis","will discuss","side event","change mitigation",
                        "energy sector","will present","will share","climate goals",
                        "adaptation mitigation","session will","changing climate","mitigation adaptation",
                        "change food","lessons learned")
for (keyword in keywords_to_remove) {
  result_df_clean <- result_df_clean[!grepl(keyword, result_df_clean$word),]
}

# summarise
result_df_clean$SUM <- rowSums(result_df_clean[ , names(result_df_clean)[-1]])
result_df_clean <- result_df_clean[order(desc(result_df_clean$SUM)), ]
output_file_name <- file.path(file_dir,paste0("topic/frequency/top_bigram_",topic,"_alltopics.csv"))
write.csv(result_df_clean, output_file_name,row.names = FALSE)
result_df_clean <- result_df_clean[order(desc(result_df_clean$`28`)), ] # order in descending
result_df_top <- result_df_clean %>% slice(1:10)
result_melt <- melt(result_df_top[,col_copnames],id="word")
output_file_name <- file.path(file_dir,paste0("topic/frequency/top_bigram_",topic,"_28.csv"))
write.csv(result_melt, output_file_name,row.names = FALSE)


result_df_clean <- result_df_clean[order(desc(result_df_clean$SUM)), ] # order in descending
result_df_top <- result_df_clean %>% slice(1:8)
result_melt <- melt(result_df_top[,col_copnames],id="word")
output_file_name <- file.path(file_dir,paste0("topic/frequency/top_bigram_",topic,"_SUM.csv"))
write.csv(result_melt, output_file_name,row.names = FALSE)

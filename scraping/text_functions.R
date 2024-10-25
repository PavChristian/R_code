library(textclean)
library(tm)
library(topicmodels)
library(tidytext)
library(SnowballC)
library(magrittr)
library(tidyverse)
library(data.table)
library(sentimentr)


#Preprocess text before sentiment analysis
prepare_text <- function(text) {
    text <- text %>%
        tolower() %>% # Convert to lowercase
        gsub("[[:punct:]]", "", .) %>% # Remove punctuation
        gsub("[[:digit:]]", "", .) %>% # Remove numbers
        removeWords(stopwords("en")) %>% # Remove stopwords
        wordStem(language = "en") %>% # Perform stemming
        lemmatize_words() %>% # Perform lemmatization
        stripWhitespace() %>% # Strip extra whitespace
        gsub("https?://\\S+|www\\.\\S+", "", .) %>% # Remove URLs
        gsub("\\S+@\\S+\\.\\S+", "", .) %>% # Remove email addresses
        replace_contraction() # Expand contractions
    return (text)
}

#Apply to the sample (a limited subset of articles)

process_sample <- function(data, text_column) {
    data <- data %>%
        mutate(!!text_column := map(.[[text_column]], prepare_text)) %>%
        select(c(!!text_column, ID))
    return (data)
}

#Conduct sentiment analysis (iteratively in batches)

compute_sentiment <- function(file_path, text_column, output_file, batch_size = 1000) {
    # Initialize the output file with headers
    fwrite(data.table(ID = integer(), sentiment_score = numeric()), output_file, append = FALSE)
    
    # Set up a counter for IDs
    id_counter <- 1
    
    # Use fread to read in chunks
    chunk <- fread(file_path, select = text_column, nrows = batch_size, skip = 0)
    
    # Iterate over the dataset in batches
    while (nrow(chunk) > 0) {
        # Calculate sentiment score for each row
        sentiment_scores <- sentiment(chunk[[text_column]]) %>% 
        group_by(element_id) %>% 
        summarize(sentiment_score = mean(sentiment))
        
        # Add IDs to the output
        results <- data.table(ID = seq(id_counter, id_counter + nrow(sentiment_scores) - 1),
                            sentiment_score = sentiment_scores$sentiment_score)
        
        # Append results to output file
        fwrite(results, output_file, append = TRUE)
        
        # Update the ID counter and batch start row for the next iteration
        id_counter <- id_counter + nrow(sentiment_scores)
        skip_rows <- id_counter - 1
        chunk <- fread(file_path, select = text_column, nrows = batch_size, skip = skip_rows)
    }
    
    cat("Sentiment analysis complete. Results saved to", output_file, "\n")
}



perform_lda <- function(data, num_topics = 5) {

  # Create a document-term matrix
  dtm <- DocumentTermMatrix(Corpus(VectorSource(data$prepared_text)), 
                            control = list(stopwords = TRUE, stemming = TRUE))
  
  # Fit the LDA model
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  # Get the topic for each document (row)
  topic_probabilities <- posterior(lda_model)$topics
  assigned_topics <- apply(topic_probabilities, 1, which.max)
  
  # Add the assigned topics back to the data
  data <- data %>%
    mutate(topic_num = assigned_topics) %>%
    select(-prepared_text) # Remove the prepared_text column
  return(data)
}

remove_file <- function(file_path){
    # Delete the file
    if (file.exists(file_path)) {
    file.remove(file_path)
    } else {
    message("File does not exist.")
    }
}

main <- function(file_list_tibble) {
    # File list tibble refers to a 4x3 with paths in and paths out
    for (i in seq_len(nrow(file_list_tibble))) {
        path_in <- file_list_tibble$path_in[i]
        path_out <- file_list_tibble$path_out[i]
        #For temp files
        temp_path <- file_list_tibble$temp_path[i]
        #Reading in a file with textual data
        articles <- read_csv(path_in)
        articles <- prepare_text(articles, "full_text")
        write_csv(articles, paste0(temp_path, "/article_prep.csv"))
        topics <- articles
            rename(prepared_text = full_text) %>%
            perform_lda()
        compute_sentiment(paste0(temp_path, "/article_prep.csv"), "prepared_text",
            paste0(temp_path, "/article_sentiment.csv"))
        remove_file(paste0(temp_path, "/article_prep.csv"))
        sentiment <- read_csv(paste0(temp_path, "/article_sentiment.csv")) %>%
            left_join(topics, by = "ID")
        remove_file(paste0(temp_path, "/article_prep.csv"))
        write_csv(sentiment, path_out)
    }
}
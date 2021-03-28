#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(data.table)

models <- readRDS('./word70.rds')

clean_input <- function(text) {
    text <- tokens(text, split_hyphens = TRUE, remove_punct  = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)
    text <- tokens_tolower(text)
    text <- tokens_replace(text, c("”",  "‘", "’", "#"), c("", "", "", ""))
    text <- tokens_replace(text, "[^\x20-\x7E]", "")
    text <- tokens_remove(text, pattern = "[0-9]+[a-z]", valuetype = "regex")
    return (text)
}


get_ngram_from_prefix <- function(prefix) {
    ngram_array <- unlist(strsplit(prefix, "_"))
    ngram_length <- length(ngram_array)
    if (ngram_length > 3) {
        prefix <- paste(ngram_array[(ngram_length-2):ngram_length],collapse= "_")
        ngram_length <- length(ngram_array[(ngram_length-2):ngram_length])
    }
    model <- models[[(ngram_length + 1)]]
    return (model[feature==prefix])
}



get_beta_table <- function(data, prefix) {
    if (nrow(data) > 0) return (data[, .(beta=calc_beta(frequency.x, discount)), by=feature])
    return (data.table(feature=c(prefix), beta=c(1)))
}


calc_beta <- function(frequency, discount){
    all_freq = sum(frequency)
    return(1-sum((discount*frequency)/all_freq))
}


construct_observed_grams_probabilites <- function(obs_grams) {
    all_freq <- sum(obs_grams$frequency.x)
    obs_grams$prob <- (obs_grams$frequency.x * obs_grams$discount) / all_freq
    return (obs_grams)
} 


get_new_prefix <- function(prefix) {
    newPrefix <- unlist(strsplit(prefix, "_"))
    newPrefix <- newPrefix[2:length(newPrefix)]
    return (paste(newPrefix, collapse="_"))
}



prediction <- function(prefix) {
    ngram_array <- unlist(strsplit(prefix, "_"))
    ngram_length <- length(ngram_array)
    if (ngram_length < 3) return (NULL)
    prefix <- paste(ngram_array[(length(ngram_array) - 2):length(ngram_array)], collapse="_")
    observed_4_grams <- get_ngram_from_prefix(prefix)
    observed_4_grams_probs <- construct_observed_grams_probabilites(observed_4_grams)
    
    beta_4 <- get_beta_table(observed_4_grams_probs, prefix) 
    
    observed_words <- observed_4_grams_probs$word
    unobs_4 <- c()
    if (length(observed_words) > 0) { unobs_4 <-models[[1]][!feature %in% observed_words]$feature }
    
    newPrefix <- get_new_prefix(prefix)
    
    observed_3_grams <- get_ngram_from_prefix(newPrefix)
    if (length(unobs_4) > 0) { observed_3_grams <- observed_3_grams[word %in% unobs_4] }
    observed_3_grams_probs <- construct_observed_grams_probabilites(observed_3_grams)
    
    observed_words <- append(observed_words, observed_3_grams_probs$word)
    
    all_3_probs <- sum(observed_3_grams_probs$prob)
    
    to_4_gram <- observed_3_grams_probs
    to_4_gram$feature <- prefix
    to_4_gram$prob <- (beta_4[feature == prefix]$beta / all_3_probs) * to_4_gram$prob
    
    beta_3 <- get_beta_table(observed_3_grams_probs, newPrefix)
    
    unobs_3 <- c()
    if (length(observed_words) > 0) { unobs_3 <- models[[1]][!feature %in% observed_words]$feature }
    
    oldPrefix <- newPrefix
    newPrefix <- get_new_prefix(newPrefix)
    observed_2_grams <- get_ngram_from_prefix(newPrefix)
    if (length(unobs_3) > 0) { observed_2_grams <- observed_2_grams[word %in% unobs_3] }
    observed_2_grams_probs <- construct_observed_grams_probabilites(observed_2_grams)
    
    observed_words <- append(observed_words, observed_2_grams_probs$word)
    
    all_2_probs <- sum(observed_2_grams_probs$prob)
    
    to_3_gram <- observed_2_grams_probs
    to_3_gram$feature <- prefix
    to_3_gram$prob <- (beta_3[feature == oldPrefix]$beta / all_2_probs) * to_3_gram$prob
    beta_2 <- get_beta_table(observed_2_grams_probs, newPrefix)
    
    unobs_2 <- c()
    if (length(observed_words) > 0) { unobs_2 <- models[[1]][!feature %in% observed_words]$feature }
    
    final_gram <- models[[1]]
    if (length(unobs_2) > 0) { final_gram <- final_gram[feature %in% unobs_2] }
    final_gram <- construct_observed_grams_probabilites(final_gram)
    
    all_1_probs <- sum(final_gram$prob)
    
    to_2_gram <- final_gram
    to_2_gram$word <- final_gram$feature
    to_2_gram$feature <- prefix
    to_2_gram$prob <- (beta_2[feature == newPrefix]$beta / all_1_probs) * to_2_gram$prob
    to_2_gram <- to_2_gram[order(-to_2_gram$prob), ]
    
    result <- rbind(observed_4_grams_probs[order(-observed_4_grams_probs$prob), ], to_4_gram[order(-to_4_gram$prob), ], 
                    to_3_gram[order(-to_4_gram$prob), ], to_2_gram)
    
    
    return (result[order(-result$prob)][1:3])
}




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    clean_text <- reactive({
        clean_source <- clean_input(input$source)
        if (length(clean_source[[1]]) > 2) {
            clean_source <- paste(clean_source[[1]][(length(clean_source[[1]]) - 2):length(clean_source[[1]])], collapse="_")
        }
        else clean_source <- paste(clean_source, collapse="_")
        clean_source
    })
    
    
    predictions <- reactive({
        source <- clean_text()
        final <- prediction(source)
        final$word
    })
    
    
    output$word1 <- renderPrint({
        if (is.null(predictions()[1])) ''
        else predictions()[1]
    });
    
    output$word2 <- renderPrint({
        if (is.null(predictions()[2])) ''
        else predictions()[2]
    });
    
    output$word3 <- renderPrint({
        if (is.null(predictions()[3])) ''
        else predictions()[3]
    });

})

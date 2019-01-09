library(dplyr)
library(stringr)
library(readr)

SBO_word_prediction <- function(input_words, four_grams, three_grams, two_grams, unigram, contractions, SOS_word_candidate){
    SOS <- FALSE
    input_words <- trimws(input_words)
    last_input_character <- str_sub(input_words, -1)
    if(last_input_character %in% c(".", "?", "!")){
        SOS <- TRUE
    }
    if(!is.na(input_words)){
        input_sentences <- unlist(strsplit(input_words, "[\\.?!]"))
        input_words <- input_sentences[length(input_sentences)]
    }
    input_words <- gsub("[^a-zA-Z ]", "", input_words)
    input_words <- tolower(input_words)
    input_words <- trimws(input_words)
    input_words_count <- str_count(input_words, "\\S+")
    input_words <- gsub("\\s+", "_", input_words)
    if(is.na(input_words)){
        SOS <- TRUE
    }
    else if(input_words_count == 1){
        final_candidate <- SBO_get_two(input_words, two_grams, unigram)
    }
    else if(input_words_count == 2){
        final_candidate <- SBO_get_three(input_words, three_grams, two_grams, unigram)
    }
    else {
        last_three_words <- word(input_words, start = input_words_count - 2,
                                 end = input_words_count, sep = "_")
        final_candidate <- SBO_get_four(last_three_words, four_grams, three_grams, two_grams, unigram)
    }
    final_candidate$output <- correct_contractions(final_candidate$output, contractions)
    if(SOS){
        final_candidate <- data_frame(output = SOS_word_candidate$SOS_word[1:5],
                                      inferred_from = "Start of sentence")
        final_candidate$output <- str_to_title(final_candidate$output)
    }
    final_candidate$inferred_from <- gsub("_", " ", final_candidate$inferred_from)
    colnames(final_candidate) <- c("Prediction", "Inferred from")
    return(final_candidate)
}

SBO_get_two <- function(input_two_pre, two_grams, unigram){
    most_common_words <- head(unigram$feature, 10)
    most_common_words <- data_frame(output = most_common_words, inferred_from = "Most common words")
    candidate <- two_grams %>%
        filter(pre == input_two_pre) %>%
        head(5) %>%
        select(output, inferred_from = pre)
    if(nrow(candidate) == 5){
        return(candidate)
    }
    else{
        mix_candidate <- rbind(candidate, most_common_words) %>%
            distinct(output, .keep_all = TRUE) %>%
            head(5)
        return(mix_candidate)
    }
}

SBO_get_three <- function(input_three_pre, three_grams, two_grams, unigram){
    candidate <- three_grams %>%
        filter(pre == input_three_pre) %>%
        head(5) %>%
        select(output, inferred_from = pre)
    if(nrow(candidate) == 5){
        return(candidate)
    }
    else{
        last_word <- word(input_three_pre, start = 2, sep = "_")
        mix_candidate <- rbind(candidate, SBO_get_two(last_word, two_grams, unigram)) %>%
            distinct(output, .keep_all = TRUE) %>%
            head(5)
        return(mix_candidate)
    }
}


SBO_get_four <- function(input_four_pre, four_grams, three_grams, two_grams, unigram){
    candidate <- four_grams %>%
        filter(pre == input_four_pre) %>%
        head(5) %>%
        select(output, inferred_from = pre)
    if(nrow(candidate) == 5){
        return(candidate)
    }
    else{
        last_two_words <- word(input_four_pre, start = 2, end = 3, sep = "_")
        mix_candidate <- rbind(candidate, SBO_get_three(last_two_words, three_grams, two_grams, unigram)) %>%
            distinct(output, .keep_all = TRUE) %>%
            head(5)
        return(mix_candidate)
    }
}

KN_word_prediction <- function(input_words, four_grams, three_grams, two_grams, unigram, contractions, SOS_word_candidate){
    SOS <- FALSE
    input_words <- trimws(input_words)
    last_input_character <- str_sub(input_words, -1)
    if(last_input_character %in% c(".", "?", "!")){
        SOS <- TRUE
    }
    if(!is.na(input_words)){
        input_sentences <- unlist(strsplit(input_words, "[\\.?!]"))
        input_words <- input_sentences[length(input_sentences)]
    }
    input_words <- gsub("[^a-zA-Z ]", "", input_words)
    input_words <- tolower(input_words)
    input_words <- trimws(input_words)
    input_words_count <- str_count(input_words, "\\S+")
    input_words <- gsub("\\s+", "_", input_words)
    if(is.na(input_words)){
        SOS <- TRUE
    }
    else if(input_words_count == 1){
        final_candidate <- KN_two_gram(input_words, two_grams, unigram)
    }
    else if(input_words_count == 2){
        final_candidate <- KN_three_gram(input_words, three_grams, two_grams, unigram)
    }
    else {
        last_three_words <- word(input_words, start = input_words_count - 2,
                                 end = input_words_count, sep = "_")
        final_candidate <- KN_four_gram(last_three_words, four_grams, three_grams, two_grams, unigram)
    }
    final_candidate$output <- correct_contractions(final_candidate$output, contractions)
    if(SOS){
        final_candidate <- data_frame(output = SOS_word_candidate$SOS_word[1:5],
                                      inferred_from = "Start of sentence",
                                      probability = as.numeric(NA))
        final_candidate$output <- str_to_title(final_candidate$output)
    }
    final_candidate$inferred_from <- gsub("_", " ", final_candidate$inferred_from)
    colnames(final_candidate) <- c("Prediction", "Inferred from", "Probability")
    return(final_candidate)
}



KN_gram_count<- function(input, grams){
    count <- filter(grams, feature == input)$frequency
    if(length(count) > 0) return(count)
    else return(0)
}

KN_two_gram <- function(input_two_pre, two_grams, unigram){
    observed_two_grams <- filter(two_grams, pre == input_two_pre)
    most_common_words <- head(unigram$feature, 10)
    most_common_words <- data_frame(output = most_common_words, inferred_from = "Most common words",
                                    probability = as.numeric(NA))
    if(nrow(observed_two_grams) < 1){
        return(head(most_common_words_prob, 5))
    }
    else{
        observed_two_grams <- observed_two_grams %>% rowwise() %>%
            mutate(pKN_unigram = KN_gram_count(output, unigram) / nrow(two_grams)) %>%
            ungroup() %>%
            mutate(pKN_bigram = (frequency - 0.5) / sum(frequency) +
                       0.5 / sum(frequency) * pKN_unigram) %>%
            select(output, inferred_from = pre, probability = pKN_bigram) %>%
            arrange(desc(probability))
    }
    if(nrow(observed_two_grams) >= 5){
        return(head(observed_two_grams, 5))
    }
    else{
        mix_candidate <- rbind(observed_two_grams, most_common_words_prob) %>%
            distinct(output, .keep_all = TRUE) %>%
            head(5)
        return(mix_candidate)
    }
    
}

KN_three_gram <- function(input_three_pre, three_grams, two_grams, unigram){
    observed_three_grams <- filter(three_grams, pre == input_three_pre)
    if(nrow(observed_three_grams) < 1){
        last_word <- word(input_three_pre, start = 2, sep = "_")
        return(KN_two_gram(last_word, two_grams, unigram))
    }
    else{
        observed_three_grams <- observed_three_grams %>% rowwise() %>%
            mutate(pKN_unigram = KN_gram_count(output, unigram) / nrow(two_grams),
                   bigram = paste(word(pre, start = 2, sep = "_"), output, sep = "_"),
                   bigram_freq = KN_gram_count(bigram, two_grams)) %>%
            ungroup() %>% mutate(pKN_bigram = (bigram_freq - 0.5) / sum(bigram_freq) +
                                     0.5 / sum(bigram_freq) * pKN_unigram,
                                 pKN_trigram = (frequency - 0.5) / sum(frequency) +
                                     0.5 / sum(frequency) * pKN_unigram) %>%
            select(output, inferred_from = pre, probability = pKN_trigram) %>%
            arrange(desc(probability))
    }
    if(nrow(observed_three_grams) >= 5){
        return(head(observed_three_grams, 5))
    }
    else{
        last_word <- word(input_three_pre, start = 2, sep = "_")
        mix_candidate <- rbind(observed_three_grams,
                               KN_two_gram(last_word, two_grams, unigram)) %>%
            distinct(output, .keep_all = TRUE) %>%
            head(5)
        return(mix_candidate)
    }
}

KN_four_gram <- function(input_four_pre, four_grams, three_grams, two_grams, unigram){
    observed_four_grams <- filter(four_grams, pre == input_four_pre)
    if(nrow(observed_four_grams) < 1){
        last_two_words <- word(input_four_pre, start = 2, end = 3, sep = "_")
        return(KN_three_gram(last_two_words, three_grams, two_grams, unigram))
    }
    else{
        observed_four_grams <- observed_four_grams %>% rowwise() %>%
            mutate(pKN_unigram = KN_gram_count(output, unigram) / nrow(two_grams),
                   trigram = paste(word(pre, start = 2, end = 3, sep = "_"), output, sep = "_"),
                   bigram = paste(word(pre, start = 3, sep = "_"), output, sep = "_"),
                   trigram_freq = KN_gram_count(trigram, three_grams),
                   bigram_freq = KN_gram_count(bigram, two_grams)) %>%
            ungroup() %>%  mutate(pKN_bigram = (bigram_freq - 0.5) / sum(bigram_freq) +
                                      0.5 / sum(bigram_freq) * pKN_unigram,
                                  pKN_trigram = (trigram_freq - 0.5) / sum(trigram_freq) +
                                      0.5 / sum(trigram_freq) * pKN_bigram,
                                  pKN_quadragram = (frequency - 0.5) / sum(frequency) +
                                      0.5 / sum(frequency) * pKN_trigram) %>%
            select(output, inferred_from = pre, probability = pKN_quadragram) %>%
            arrange(desc(probability))
    }
    if(nrow(observed_four_grams) >= 5){
        return(head(observed_four_grams, 5))
    }
    else{
        last_two_words <- word(input_four_pre, start = 2, end = 3, sep = "_")
        mix_candidate <- rbind(observed_four_grams,
                               KN_three_gram(last_two_words, three_grams, two_grams, unigram)) %>%
            distinct(output, .keep_all = TRUE) %>%
            head(5)
        return(mix_candidate)
    }
}

correct_contractions <- function(x, contractions){
    # Cannot correct "we'll" or "we're" because "well" and "were" are legit words,
    # "he'll" can be corrected because "hell" is blocked by profanity filter
    # https://en.wikipedia.org/wiki/Wikipedia:List_of_English_contractions
    for(i in 1:length(x)){
        if(length(which(contractions$input == x[i])) != 0)
            x[i] <- contractions$contraction[which(contractions$input == x[i])]
    }
    return(x)
}

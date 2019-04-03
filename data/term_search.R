library(data.table)
library(tm)
library(magrittr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

lyrics_list <- fread("lyrics_list_complete_year.csv")
kokain_suchworte <- read.table("kokain_suchworte2.txt", header = F, stringsAsFactors = F)
cannabis_suchworte <- read.table("cannabis_suchworte.txt", header = F, stringsAsFactors = F)
german_stopwords <- unlist(read.table("stopwords_de.txt", header = F, stringsAsFactors = F))
Encoding(german_stopwords) <- "UTF-8"

preprocess <- function(x){
  x %>%
    removePunctuation(preserve_intra_word_dashes = F) %>%
    removeNumbers() %>%
    tolower() %>%
    removeWords(german_stopwords) %>%
    stripWhitespace()
}

check_for_drugs <- function(x){
  hat_koka <- length(grep(paste(kokain_suchworte, collapse = "|"), x, ignore.case = T))
  hat_cannabis <- length(grep(paste(cannabis_suchworte, collapse = "|"), x, ignore.case = T))
  return(c(hat_koka, hat_cannabis))
}

check_routine <- function(x){
  x %>%
    preprocess() %>%
    check_for_drugs()
}

drogentest <- t(unname(sapply(lyrics_list$song_text, check_routine)))
colnames(drogentest) <- c("Mentions_Cocain", "Mentions_Cannabis")

lyrics_list <- cbind(lyrics_list, drogentest)
write.csv2(lyrics_list, "lyrics_list_drugcheck2.csv")

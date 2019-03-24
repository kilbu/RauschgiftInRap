
#Benötigte Packages:
library(XML)
library(rvest)
library(RCurl)
library(gsubfn)
library(textcat)
library(geniusr)
library(dplyr)
library(rlist)


Sys.setlocale(category="LC_ALL", locale="German")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
token <- "meinToken"

##############################################################################

# Quelle Rauschmittel: http://www.drug-infopool.de/category/rauschmittel
# Quelle Rapper: https://de.wikipedia.org/wiki/Liste_von_Hip-Hop-Musikern_Deutschlands

parsed_doc <- htmlParse(getURL("https://de.wikipedia.org/wiki/Liste_von_Hip-Hop-Musikern_Deutschlands",followlocation=TRUE))
xpathSApply(doc = parsed_doc, "//td/a",xmlAttrs, "title")[title]
rapper <- unlist(lapply(xpathSApply(doc = parsed_doc, "//td/a",xmlAttrs, "title"), `[[`, 2))
rapper <- gsub("image", "", rapper)
rapper <- gsub("mw-redirect", "", rapper)
rapper <- gsub("new", "", rapper, ignore.case = F)
rapper <- gsub(" \\(.*", "", rapper)
rapper <- gsub(" ", "-", rapper)
rapper <- rapper[which(nchar(rapper) > 0)]
rapper <- unique(rapper)
Encoding(rapper) <- "UTF-8"

write.csv2(rapper, "rapper_names.csv")
####################################################################################################
####################################################################################################
####################################################################################################
rapper <- read.csv2("rapper_names.csv", stringsAsFactors = F)[,-1]
namen <- read.csv2("namen.csv", stringsAsFactors = F)
namen_abfrage <- gsub(",", " |",namen$Synonym)

token <- "meinToken"


#Artist ID Suche

for (artist in rapper){
  print(artist)
  if (any(class(try(search_artist(artist, access_token = token))) != "try-error")){
    artist_meta <- search_artist(artist, access_token = token)
    artist_meta$artist_name <- gsub(" \\(DE\\)", "", artist_meta$artist_name)
    
    artist_meta <- artist_meta[grep(gsub("-"," ",artist), artist_meta$artist_name),]
    
    if (nrow(artist_meta) > 0){
      artist_meta <- cbind(artist_meta[,-3], nrow(artist_meta), artist)
      colnames(artist_meta) <- colnames(artists_list)
      artists_list <- rbind(artists_list, artist_meta)
    } else{
      artists_list <- rbind(artists_list, c(NA, gsub("-"," ",artist), 0, artist))
    }
  } else{
    artists_list <- rbind(artists_list, c(NA, gsub("-"," ",artist), 0, artist))
  }
}

#Jetzt für alle eine Stichprobe, ob die Texte deutsch sind
language <- rep("NA", nrow(artists_list))
for (i in 2:nrow(artists_list)){
  print(paste(i, "von", nrow(artists_list)))
  if (is.na(artists_list$Rapper[i]) == F){
    song1_id <- get_artist_songs(artists_list$Rapper[i], include_features = FALSE,
                                access_token = token)[1,1]
    if (class(try(scrape_lyrics_id(song1_id, access_token = token)$line)) != "try-error"){
      lyrics <- paste(scrape_lyrics_id(song1_id, access_token = token)$line, collapse = " ")
      language[i] <- textcat(lyrics)
    } else{
      language[i] <- "keine_erfolgreiche_stichprobe"
    }
  }
}

artists_list <- cbind(artists_list, language)

write.csv2(artists_list, "artists_list_lang.csv")

##########################################################################################
##########################################################################################
##########################################################################################

artists_list <- read.csv2("artists_list_lang.csv", stringsAsFactors = F)
artist_search <- artists_list[which(artists_list$language == "german" | artists_list$language == "keine_erfolgreiche_stichprobe"),]


lyrics_list <- data.frame(matrix(ncol=7))
colnames(lyrics_list) <- c("artist_id", "artist_name","song_id", "song_name", "album_id", "song_text", "release_date")

for(i in 1:length(artist_search$Rapper)){
  print(paste(i, "von", length(artist_search$Rapper)))
  if (class(try(get_artist_songs(artist_search$Rapper[i], include_features = FALSE,access_token = token))) != "try-error"){
    artist_songs <- get_artist_songs(artist_search$Rapper[i], include_features = FALSE,access_token = token)
    songs <- artist_songs$song_id
    

    for(song_id in songs){
      #print(song_id)
      song_meta <- get_song_meta(song_id, access_token = token)
      
      if (class(try(scrape_lyrics_id(song_id, access_token = token)$line)) != "try-error"){
        song_text <- paste(scrape_lyrics_id(song_id, access_token = token)$line, collapse = " ")
      }else{
        song_text <- ""
      }
      
      lyrics_list <- rbind(lyrics_list, unname(c(song_meta$artist_id, song_meta$artist_name, song_meta$song_id, song_meta$song_name, song_meta$album_id, song_text, song_meta$release_date)))
    }
  }
}

#lyrics_list <- lyrics_list[-1,]

#Noch mal checken, dass nur "UNIQUE" Song_IDs drin sind, wegen der Unterbrechungen

#Bei manchen ist kein Release Date: bei denen gucken, ob es einen anderen Titel mit der
#gleichen Album-ID gibt, die ein Release Date haben

write.csv2(lyrics_list, "lyrics_list.csv")

#lyrics_list <- read.csv2("lyrics_list.csv", stringsAsFactors = F)[,-1]

lyrics_list$album_id[which(is.na(lyrics_list$release_date))]


for(i in unique(lyrics_list$album_id)){
  release_dates <- lyrics_list$release_date[which(lyrics_list$album_id == i)]
  release_dates <- release_dates[which(nchar(release_dates) > 1)][1]
  lyrics_list$release_date[which(lyrics_list$album_id == i)] <- release_dates
}

lyrics_list[, "release_year"] <- NA

lyrics_list$release_year <- substr(lyrics_list$release_date, 1, 4)

lyrics_list_complete_years <- lyrics_list[-(which(is.na(lyrics_list$release_date))),]

write.csv2(lyrics_list_complete_years, "lyrics_list_complete_year.csv")

#Für jeden Künstler
# -> suche Artist songs
# -> nimm Spalten Song ID und Song Name
# -> für jede song_id Song Meta suchen, daraus releade-Date suchen (a, besten gleich das Jahr extrahieren)
# -> Spalte mit Jahren an Songtabelle heften
# -> Dann loop:
#     - Für jedes Jahr, das verfügbar ist
#     - Von jedem Song aus dem Jahr alle Lyrics laden und Wortzählung über Drogensynonyme
#     - Resultat: Eintragung in Tabelle mit (ARTIST | JAHR | DRUG_COUNTS)
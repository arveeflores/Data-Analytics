library(shiny)
library(rvest)
library(data.table)
library(NLP)
library(tm)
library(svDialogs)
library(stringr)
library(wordcloud)
library(downloader)
library(sentiment)

shinyServer(function(input, output, session) {
  #START PROGRAM
  #1ST ITERATION OF SEARCHING FOR RELATED SONGS BASED ON EMOTIONS [START]
  tryCatch({
    withProgress(message = 'Fetching information from the website',
                 value = 0, {
                   
                   song_final1 <- vector(length = 0)  
                   test_song <<- vector(length = 0)
                   vector_final_corpus <<- vector(length = 0)
                   results_related <<- vector(length = 0)
                   title_and_artist1 <- vector(length = 0)
                   for(i in 1:5){
                     ran_letter <- sample(1:27, 1)
                     
                     songlyrics_website <- read_html("http://www.songlyrics.com/a/")
                     letter <- html_nodes(songlyrics_website, ".level1 li+ li a")
                     letter_text <- html_text(letter)
                     letter_final <- paste("http://www.songlyrics.com/", letter_text[ran_letter], sep = "")
                     letter_website <- read_html(letter_final)
                     number <- html_nodes(letter_website, ".li_pagination a")
                     number_text <- html_text(number)
                     number_length <- length(number_text) - 1
                     
                     ran_number <- sample(1:number_length, 1)
                     number_final <- paste(letter_final, "/", ran_number, sep = "" )
                     number_website <- read_html(number_final)
                     
                     ran_artist <- sample(1:100, 1)
                     artist <- html_nodes(number_website, ".box.listbox a")
                     artist_text <- html_text(artist)
                     artist_text_edit <- gsub("[[:punct:]]|\\s", "-", artist_text)
                     artist_text_edited <- gsub("--|---", "-", artist_text_edit)
                     
                     artist_final <- paste("http://www.songlyrics.com/", tolower(artist_text_edited[ran_artist]), "-lyrics", sep = "")
                     artist_website <- read_html(artist_final)
                     song_list <- html_nodes(artist_website, "#colone-container .tracklist a")
                     song_list_text <- html_text(song_list)
                     song_list_text_edit <- gsub("[[:punct:]]|\\s", "-", song_list_text)
                     song_list_text_edited <- gsub("--|---", "-", song_list_text_edit)
                     
                     ran_song <- sample(1:length(song_list), 1)
                     song_final <- paste("http://www.songlyrics.com/", tolower(artist_text_edited[ran_artist]), "/", tolower(song_list_text_edited[ran_song]), "-lyrics/", sep = "")
                     song_final1 <- c(song_final1, song_final)
                     title_and_artist <- c(artist_text[ran_artist], song_list_text[ran_song])
                     title_and_artist1 <- c(title_and_artist1, title_and_artist)
                     print(song_final)
                     print(title_and_artist)
                     html_read <- read_html(song_final)
                     
                     lyric_info <- html_nodes(html_read, "#songLyricsDiv")
                     lyric_info_text <- html_text(lyric_info)
                     test_song <<- lyric_info_text
                     lyric_filename <- paste(tolower((song_list_text_edited[ran_song])), "-lyrics.txt", sep = "")
                     lyric_path <- paste("/lyrics/", artist_text_edited[ran_artist], "/", sep = "")
                     if(file.exists(file.path(getwd(), lyric_path))){
                     }else{
                       dir.create(file.path(getwd(), lyric_path))
                     }
                     
                     lyric_folder <- paste(getwd(), lyric_path, lyric_filename, sep = "")
                     write.table(lyric_info_text, file = lyric_folder, row.names = FALSE, quote = FALSE)
                     
                     corpus_read  <- Corpus(VectorSource(test_song), readerControl = list(reader=readPlain) )
                     corpus <- tm_map(corpus_read, content_transformer(tolower))
                     corpus <- tm_map(corpus, removePunctuation)
                     corpus <- tm_map(corpus, stripWhitespace)
                     corpus <- tm_map(corpus, removeNumbers)
                     corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
                     dtm <- TermDocumentMatrix(corpus)
                     vector_corpus <- dtm$dimnames$Terms
                     vector_whole_song <- paste(vector_corpus[1:length(vector_corpus)], collapse = ' ') 
                     vector_final_corpus <<- c(vector_final_corpus, vector_whole_song)
                     incProgress(i/10)
                   }
                   
                     emotion <- classify_emotion(vector_final_corpus, algorithm = "bayes", prior = 1.0)
                     
                     results_related = emotion[,7]
                     print(results_related)
                     incProgress(10/10)
                   
                 })
    
  }, error = function(cond){
    dlgMessage(c("A problem occured with the internet connection.", "\nPlease click refresh"), c("ok"))
    
  })
  #1ST ITERATION OF SEARCHING FOR RELATED SONGS BASED ON EMOTIONS [END]
  
  
  
  #--------------------------------------------------------------------------------------------------------  
  
  #FUNCTIONS [START]
  #TRIMMING OF WORDS
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  #SELECTING ALL THE CHECK BOXES [START]
  select_all <- function(){ withProgress(message = 'Getting top 10 from songlyrics.com',
                                         value = 0, {
                                           html_topSongs <- "http://www.songlyrics.com"
                                           html_topSongs_read <- read_html(html_topSongs)
                                           top_songs <- html_nodes(html_topSongs_read, ".col-hp-inner-left h3")
                                           top_songs_read <- html_text(top_songs)
                                           
                                           html_topArtists <- "http://www.songlyrics.com/top-artists-lyrics.html"
                                           html_topArtists_read <- read_html(html_topArtists)
                                           top_artists <- html_nodes(html_topArtists_read, ".td-last")
                                           top_artists_read <- html_text(top_artists)
                                           for(x in 1:10){
                                             incProgress(1/10)
                                             Sys.sleep(0.15)
                                             
                                           }
                                           # enable("select_genre")
                                         })
    output$topSongs <- renderUI({
      HTML(paste(1:10, top_songs_read[1:10], '<br/>'))
      
      
    })
    output$topArtists <- renderUI({
      HTML(paste(1:10, top_artists_read[1:10], '<br/>'))
    })
    
    output$topWords <- renderUI({
      
    })
    
  }
  #SELECTING ALL THE CHECK BOXES [END]
  
  #GETTING DATA FOR TOP 10 [START]
  select_top_genre <- function(html_genre){withProgress(message = 'Getting top 10 from songlyrics.com',
                                                        value = 0, {
                                                          html_topSongsArtists <- paste("http://www.songlyrics.com/", html_genre, "-lyrics.php", sep = "")
                                                          html_topSongsArtists_read <- read_html(html_topSongsArtists)
                                                          top_songs <- html_nodes(html_topSongsArtists_read, ".pagetitle-2+ .listbox h3")
                                                          top_songs_read <- html_text(top_songs)
                                                          
                                                          top_artists <- html_nodes(html_topSongsArtists_read, ".pagetitle-2+ .listbox span")
                                                          top_artists_read <- html_text(top_artists)
                                                          
                                                          for(x in 1:10){
                                                            incProgress(1/10)
                                                            Sys.sleep(0.15)
                                                          }
                                                          #enable("select_genre")
                                                        })
    output$topSongs <- renderUI({
      HTML(paste(1:10, top_songs_read[1:10], '<br/>'))
      
      
    })
    output$topArtists <- renderUI({
      HTML(paste(1:10, top_artists_read[1:10], '<br/>'))
    })
    
    output$topWords <- renderUI({
      HTML(paste("xxx"))
    })
  }
  #GETTING DATA FOR TOP 10 [END]
  
  #FUNCTIONS [END]
  
  #--------------------------------------------------------------------------------------------------------
  
  #ARTIST TAB [START]
  observeEvent(input$btnRefresh1,{
    empty.df <- data.frame("Collection is empty!")
    write.table(empty.df, file = "empty.txt", col.names = FALSE, row.names = FALSE, quote = FALSE)
    Empty_Text<- read.table(file="empty.txt", header=FALSE, sep=",")
    if(file.exists("artists_collection.txt")){
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",")
      output$table1 = DT::renderDataTable(
        unique(MyArtists)
      )
      
    }else{
      output$table1 = DT::renderDataTable(
        Empty_Text
      )
    }
    
  })
  output$artistImage <- renderImage({
    s = input$table1_rows_selected
    if(length(s)==0){
      file_artist <- paste(getwd(),"/images/empty.png", sep = "")
      return(list(
        src = file_artist,
        contentType = "image/png",
        height = 200,
        width = 200,
        alt = "Select an artist"
      ))
    }else if(length(s) > 1){
      file_artist <- paste(getwd(),"/images/empty.png", sep = "")
      return(list(
        src = file_artist,
        contentType = "image/png",
        height = 200,
        width = 200,
        alt = "Select only one artist"
      ))
      
    }else{
      ss <- strtoi(s)
      if(length(s)){
        MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
        artist1 <- paste(MyArtists$Artist[ss])
        edit_artist <- gsub("[[:punct:]]|\\s", "-", artist1)
        edited_artist <- gsub("--|---", "_", edit_artist)
        html_artist_site <- paste("http://www.mtv.com/artists/", edited_artist, "/photos/", sep="")
        html_read <- read_html(html_artist_site)
        image_node <- html_read%>% html_nodes("#profile_artist_images .thumb")
        link <- html_attr(image_node, "src")
        link_final <- paste(link[6], sep = "")
        filename <- html_attr(image_node, "img alt")
        if(file.exists(paste(getwd(), "/images/", edited_artist, ".jpg", sep =""))){
          
        }else{
          download(link_final, destfile = paste(getwd(), "/images/", edited_artist, ".jpg", sep ="") )
          
        }
        
        return(list(
          src = paste(getwd(),"/images/", edited_artist, ".jpg", sep =""),
          contentType = "image/jpg",
          
          alt = artist1
        ))
        
        
        
      }else{
        
      }
    }
    
  }, deleteFile = FALSE)
  output$info <- renderPrint({
    
    s = input$table1_rows_selected
    ss <- strtoi(s)
    if(length(s) == 1){
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
      artist1 <- paste(MyArtists$Artist[ss])
      edit_artist <- gsub("[[:punct:]]|\\s", "-", artist1)
      edited_artist <- gsub("--|---", "-", edit_artist)
      html_artist_site <- paste("http://www.mtv.com/artists/", edited_artist, "/biography/", sep="")
      html_read <- read_html(html_artist_site)
      info1 <- html_nodes(html_read, ".bio-carousel-text-inner")
      info1_text <- html_text(info1)
      
      if(as.String(info1_text) == ""){
        cat ("Information unavailable.")
        
      }else{
        cat(info1_text)
        
      }
    }else if(length(s) > 1){
      cat ("Select only one artist to view information.")
    }
    
  })
  #ARTIST TAB [END]
  
  #--------------------------------------------------------------------------------
  #BROWSE COLLECTION TAB [START]
  #REFRESH BUTTON [START]
  observeEvent(input$btnRefresh2,{
    empty.df <- data.frame("Collection is empty!")
    write.table(empty.df, file = "empty.txt", col.names = FALSE, row.names = FALSE, quote = FALSE)
    Empty_Text<- read.table(file="empty.txt", header=FALSE, sep=",")
    
    if(file.exists("songs_collection.txt")){
      MySongs<- read.table(file="songs_collection.txt", header=TRUE, sep=",")
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",")
      MyAlbums<- read.table(file="albums_collection.txt", header=TRUE, sep=",")
      MyGenres<- read.table(file="genres_collection.txt", header=TRUE, sep=",")
      MyCollection <- cbind(MySongs,MyArtists, MyAlbums,MyGenres)
      
      output$table = DT::renderDataTable(
        MyCollection
      )
      
    }else{
      output$table = DT::renderDataTable(
        Empty_Text
      )
      
      
    }
    
  })
  #REFRESH BUTTON [END]
  
  #EMOTION CLASSIFICATION [START]
  final_emotion <- vector(length = 0)
  song_emotion <- vector(length = 0)
  output$emotion <- renderPrint({
    s = input$table_rows_selected
    if(length(s)==1){
      ss <- strtoi(s)
      Mysongs<- read.table(file="songs_collection.txt", header=TRUE, sep=",")
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
      artist1 <- paste(MyArtists$Artist[ss])
      song1 <<- paste(Mysongs$Song[ss])
      
      edit_song <- gsub("[[:punct:]]|\\s", "-", song1)
      edited_song <- gsub("--|---", "-", edit_song)
      
      edit_artist <- gsub("[[:punct:]]|\\s", "-", artist1)
      edited_artist <- gsub("--|---", "-", edit_artist)
      filename <- paste(getwd(),"/lyrics/",edited_artist,"/",edited_song,"-lyrics.txt", sep = "")
      MyLyrics <- readChar(filename, file.info(filename)$size)
      
      corpus_read  <- Corpus(VectorSource(MyLyrics), readerControl = list(reader=readPlain) )
      corpus <- tm_map(corpus_read, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
      dtm <- TermDocumentMatrix(corpus)
      vector_corpus <- dtm$dimnames$Terms
      vector_whole_song <- paste(vector_corpus[1:length(vector_corpus)], collapse = ' ')
      emotion <- classify_emotion(vector_whole_song, algorithm = "bayes", prior = 1.0)
      emotion[is.na(emotion)] = "insufficient data"
      song_emotion <<- emotion[,7]
      final_emotion <- paste("Emotion: ", song_emotion, sep = "")
      cat(final_emotion)
      
    }else if(length(s) > 1){
      cat("")
      
    }else{
      cat("")
    }
    
  })
  #EMOTION CLASSIFICATION [END]
  
  #2ND++ ITERATION OF SEARCHING FOR RELATED SONGS BASED ON EMOTIONS [START]
  observeEvent(input$btnSearchRelated,{
    tryCatch({
      withProgress(message = 'Fetching information from the website',
                   value = 0, {
                     
#                      song_final1 <- vector(length = 0)  
#                      vector_final_corpus <- vector(length = 0)
#                      title_and_artist1 <- vector(length = 0)
                     for(i in 1:5){
                       ran_letter <- sample(1:27, 1)
                       
                       songlyrics_website <- read_html("http://www.songlyrics.com/a/")
                       letter <- html_nodes(songlyrics_website, ".level1 li+ li a")
                       letter_text <- html_text(letter)
                       letter_final <- paste("http://www.songlyrics.com/", letter_text[ran_letter], sep = "")
                       letter_website <- read_html(letter_final)
                       number <- html_nodes(letter_website, ".li_pagination a")
                       number_text <- html_text(number)
                       number_length <- length(number_text) - 1
                       
                       ran_number <- sample(1:number_length, 1)
                       number_final <- paste(letter_final, "/", ran_number, sep = "" )
                       number_website <- read_html(number_final)
                       
                       ran_artist <- sample(1:100, 1)
                       artist <- html_nodes(number_website, ".box.listbox a")
                       artist_text <- html_text(artist)
                       artist_text_edit <- gsub("[[:punct:]]|\\s", "-", artist_text)
                       artist_text_edited <- gsub("--|---", "-", artist_text_edit)
                       
                       artist_final <- paste("http://www.songlyrics.com/", tolower(artist_text_edited[ran_artist]), "-lyrics", sep = "")
                       artist_website <- read_html(artist_final)
                       song_list <- html_nodes(artist_website, "#colone-container .tracklist a")
                       song_list_text <- html_text(song_list)
                       song_list_text_edit <- gsub("[[:punct:]]|\\s", "-", song_list_text)
                       song_list_text_edited <- gsub("--|---", "-", song_list_text_edit)
                       
                       ran_song <- sample(1:length(song_list), 1)
                       song_final <- paste("http://www.songlyrics.com/", tolower(artist_text_edited[ran_artist]), "/", tolower(song_list_text_edited[ran_song]), "-lyrics/", sep = "")
                       song_final1 <- c(song_final1, song_final)
                       title_and_artist <- c(artist_text[ran_artist], song_list_text[ran_song])
                       title_and_artist1 <<- c(title_and_artist1, title_and_artist)
                       print(song_final)
                       print(title_and_artist)
                       html_read <- read_html(song_final)
                       
                       lyric_info <- html_nodes(html_read, "#songLyricsDiv")
                       lyric_info_text <- html_text(lyric_info)
                       test_song <<- lyric_info_text
                       lyric_filename <- paste(tolower((song_list_text_edited[ran_song])), "-lyrics.txt", sep = "")
                       lyric_path <- paste("/lyrics/", artist_text_edited[ran_artist], "/", sep = "")
                       if(file.exists(file.path(getwd(), lyric_path))){
                       }else{
                         dir.create(file.path(getwd(), lyric_path))
                       }
                       
                       lyric_folder <- paste(getwd(), lyric_path, lyric_filename, sep = "")
                       write.table(lyric_info_text, file = lyric_folder, row.names = FALSE, quote = FALSE)
                       
                       corpus_read  <- Corpus(VectorSource(test_song), readerControl = list(reader=readPlain) )
                       corpus <- tm_map(corpus_read, content_transformer(tolower))
                       corpus <- tm_map(corpus, removePunctuation)
                       corpus <- tm_map(corpus, stripWhitespace)
                       corpus <- tm_map(corpus, removeNumbers)
                       corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
                       dtm <- TermDocumentMatrix(corpus)
                       vector_corpus <- dtm$dimnames$Terms
                       vector_whole_song <- paste(vector_corpus[1:length(vector_corpus)], collapse = ' ') 
                       vector_final_corpus <- c(vector_final_corpus, vector_whole_song)
                       incProgress(i/10)
                     }
                     
                     emotion <- classify_emotion(vector_final_corpus, algorithm = "bayes", prior = 1.0)
                     emotion[is.na(emotion)] = "insufficient data"
                     results_related = emotion[,7]
                     print(results_related)
                     incProgress(10/10)
                   })
      
    }, error = function(cond){
      dlgMessage(c("A problem occured with the internet connection.", "\nPlease click refresh"), c("ok"))
      
    })
  })
  
  #2ND++ ITERATION OF SEARCHING FOR RELATED SONGS BASED ON EMOTIONS [END]
  
  #DISPLAY RELATED SONG [START]
  output$related <- renderPrint({
    s = input$table_rows_selected
    if(length(s)==1){
      list_emotion <- tolower(results_related)
      position <<- match(song_emotion,tolower(list_emotion))
      
      if(!is.na(position)){
        display_artist_title <- c("Related song: \n", "Title:", title_and_artist1[position*2], "\nArtist:", title_and_artist1[position*2-1], sep=" ")
        
        cat(display_artist_title)
      }
      else if(song_emotion == "insufficient data"){
        display <- "Not enough data. Click Search Related Song"
        cat(display)
      }
      else{
        display <- "Not enough data. Click Search Related Song"
        cat(display)
      }
    }
    
  })
  #DISPLAY RELATED SONG [END]
  
  #DISPLAY LYRICS [START]
  output$lyrics <- renderPrint({
    s = input$table_rows_selected
    if(length(s)==1){
      # enable("btnDelete2")
      ss <- strtoi(s)
      Mysongs<- read.table(file="songs_collection.txt", header=TRUE, sep=",")
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
      artist1 <- paste(MyArtists$Artist[ss])
      song1 <- paste(Mysongs$Song[ss])
      
      
      
      edit_song <- gsub("[[:punct:]]|\\s", "-", song1)
      edited_song <- gsub("--|---", "-", edit_song)
      
      edit_artist <- gsub("[[:punct:]]|\\s", "-", artist1)
      edited_artist <- gsub("--|---", "-", edit_artist)
      filename <- paste(getwd(),"/lyrics/",edited_artist,"/",edited_song,"-lyrics.txt", sep = "")
      MyLyrics <- readChar(filename, file.info(filename)$size)
      
      cat(MyLyrics)
      
    }else if(length(s) > 1){
      # enable("btnDelete2")
      cat("Select only one lyrics to view song lyrics.")
      
    }else{
      # disable("btnDelete2")
      cat("")
    }
    
    
    
  })
  #DISPLAY LYRICS [END]
  
  #DELETE BUTTON [START]
  observeEvent(input$btnDelete2,{
    s = input$table_rows_selected
    ss <- strtoi(s)
    MySongs<- read.table(file="songs_collection.txt", header=TRUE, sep=",")
    MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",")
    MyAlbums<- read.table(file="albums_collection.txt", header=TRUE, sep=",")
    MyGenres<- read.table(file="genres_collection.txt", header=TRUE, sep=",")
    MySongs <- MySongs[-ss, ]
    MyArtists <- MyArtists[-ss, ]
    MyAlbums <- MyAlbums[-ss, ]
    MyGenres <- MyGenres[-ss, ]
    
    dlgMessage("Item(s) Deleted in the collection!")
    
    write.table(MySongs, file = "songs_collection.txt", col.names = "Song", row.names = FALSE, quote = FALSE)
    write.table(MyArtists, file = "artists_collection.txt", col.names = "Artist", row.names = FALSE, quote = FALSE)
    write.table(MyAlbums, file = "albums_collection.txt", col.names = "Album", row.names = FALSE, quote = FALSE)
    write.table(MyGenres, file = "genres_collection.txt", col.names = "Genre", row.names = FALSE, quote = FALSE)
    
  })
  #DELETE BUTTON [END]
  #BROWSE COLLECTION TAB [END]
  
  #---------------------------------------------------------------------------------------
  
  #GET FROM WEB TAB [START]
  #SEARCH BUTTON [START]
  observeEvent(input$btnSearch, 
               {
                 tryCatch({
                   withProgress(message = 'Searching artist from web',
                                value = 0, {
                                  incProgress(1/15)
                                  artist <- input$txtArtist
                                  initial <- substr(artist, 1, 1)
                                  html_list_final <- NULL
                                  html_read_final <- NULL
                                  artists <- NULL
                                  output_final <- NULL
                                  album_site_list_final <- NULL
                                  html_album_final <- NULL
                                  output_year_final <- NULL
                                  html_site <- "http://www.songlyrics.com/"
                                  html_artist_site <- paste(html_site, initial, sep = "")
                                  html_read <- read_html(html_artist_site)
                                  incProgress(1/15)
                                  ctr <- html_nodes(html_read, ".li_pagination a")
                                  pages <- html_text(ctr)
                                  max_page <- length(pages)
                                  incProgress(1/15)
                                  for(j in 0:max_page){
                                    html_list_temp <- paste(html_artist_site,"/",j,sep= "")
                                    html_list_new <- c(html_list_final, html_list_temp)
                                    html_list_final <- html_list_new
                                  }
                                  incProgress(2/15)
                                  Sys.sleep(7)
                                  incProgress(2/15)
                                  for(k in 1:max_page){
                                    html_read_temp <- read_html(html_list_final[k])
                                    html_read_new <- c(html_read_final, html_read_temp)
                                    html_read_final <- html_read_new
                                    artists <- html_nodes(html_read_temp, ".box.listbox a")
                                    output_temp <-html_text(artists)
                                    output_new <- c(output_final, output_temp)
                                    output_final <- output_new
                                    incProgress(2/15)
                                    if(is.element(tolower(artist), tolower(output_final))){
                                      html_read_temp <- read_html(html_list_final[k+1])
                                      html_read_new <- c(html_read_final, html_read_temp)
                                      html_read_final <- html_read_new
                                      artists <- html_nodes(html_read_temp, ".box.listbox a")
                                      output_temp <-html_text(artists)
                                      if(is.element(tolower(artist), tolower(output_temp))){
                                        output_new <- c(output_final, output_temp)
                                        output_final <- output_new
                                        break
                                      } else {
                                        break
                                      }
                                      
                                    }
                                  }
                                  incProgress(2/15)
                                  selected_artist_index <- grep(artist, output_final, ignore.case = TRUE)
                                  selected_artist <<- output_final[selected_artist_index]
                                  edit_selected_artist <- gsub("[[:punct:]]|\\s", "-", selected_artist)
                                  edited_artist <<- gsub("--|---", "-", edit_selected_artist)
                                  html_artist_site <- paste(html_site, edited_artist, "-lyrics/", sep = "")
                                  
                                  incProgress(2/15)
                                })            
                   observe({
                     
                     print("observer1")
                     if(length(selected_artist) == 1){
                       isolate({
                         withProgress(message = 'Getting data from web',
                                      value = 0, {
                                        
                                        incProgress(3/15)
                                        html_read <- read_html(html_artist_site)
                                        album_titles <- html_nodes(html_read, ".listbox-album h3 a")
                                        album_titles_text <- html_text(album_titles)
                                        edit_album <- gsub("[[:punct:]]|\\s", "-", album_titles_text[1])
                                        edited_album <- gsub("--|---", "-", edit_album)
                                        album_site <- paste(html_site, tolower(edited_artist), "/", edited_album, sep = "")
                                        html_album_site <- read_html(album_site)
                                        
                                        incProgress(3/15)
                                        
                                        year <- html_nodes(html_album_site, ".subtitle")
                                        year_text <- html_text(year)
                                        latest_album_and_year <- paste(album_titles_text[1], " (", year_text, ")", sep = "")
                                        incProgress(3/15)
                                        output$result <- renderUI({
                                          HTML("<b>Artist:</b>" ,paste(selected_artist))
                                        })
                                        
                                        output$albums <- renderUI({
                                          HTML("<b>Latest Album:</b>", paste(latest_album_and_year))
                                        })
                                        incProgress(3/15)
                                        
                                        song_titles <- html_nodes(html_album_site, "#colone-container .tracklist a")
                                        song_titles_text <- html_text(song_titles)
                                        
                                        edit_song <- gsub("[[:punct:]]|\\s", "-", song_titles_text)
                                        edited_song <- gsub("--|---", "-", edit_song)
                                        html_song_site <- paste(html_site, tolower(edited_artist), "/",edited_song, "-lyrics/", sep = "")
                                        chk_edited_song <- paste("chk",edited_song, sep="-")
                                        
                                        incProgress(3/15)
                                        
                                        output$songs <- renderUI({
                                          
                                          checkboxGroupInput("chkSongs", h4("Songs:"),
                                                             choices = c(song_titles_text))
                                          
                                        })
                                        
                                        observeEvent(input$btnSelectAll,{
                                          
                                          updateCheckboxGroupInput(session=session, "chkSongs", h4("Songs:"),
                                                                   choices = c(song_titles_text), selected = song_titles_text)
                                        })
                                        
                                        observeEvent(input$btnDeselectAll,{
                                          
                                          updateCheckboxGroupInput(session=session, "chkSongs", h4("Songs:"),
                                                                   choices = c(song_titles_text), selected = NULL)
                                        })
                                        
                                        
                                      })
                         
                       })
                     }else if(length(selected_artist) == 0){
                       isolate({
                         output$result <- renderUI({
                           HTML(paste('Artist not found!'))
                           
                         })
                         output$albums <- renderUI({
                           HTML(paste(''))
                           
                         })
                         output$songs <- renderUI({
                           HTML(paste(''))
                           
                         })
                       })
                     }else{
                       output$result <- renderUI({
                         radioButtons("rbtnArtists", "Artists: ", choices = c(selected_artist))
                         
                       })
                       observe({
                         print("observer2")
                         if(is.null(input$rbtnArtists)){
                           return()
                         }
                         isolate({ 
                           withProgress(message = 'Getting data from web',
                                        value = 0, {
                                          
                                          incProgress(3/15)
                                          
                                          
                                          Sys.sleep(2)
                                          edit_selected_artist <- gsub("[[:punct:]]|\\s", "-", input$rbtnArtists)
                                          edited_artist <- gsub("--|---", "-", edit_selected_artist)
                                          html_artist_site <- paste("http://www.songlyrics.com/", tolower(edited_artist), "-lyrics/", sep = "")
                                          html_read <- read_html(html_artist_site)
                                          album_titles <- html_nodes(html_read, ".listbox-album h3 a")
                                          album_titles_text <<- html_text(album_titles)
                                          edit_album <- gsub("[[:punct:]]|\\s", "-", album_titles_text[1])
                                          edited_album <- gsub("--|---", "-", edit_album)
                                          
                                          incProgress(3/15)
                                          
                                          album_site <- paste(html_site, tolower(edited_artist), "/", edited_album, sep = "")
                                          html_album_site <- read_html(album_site)
                                          year <- html_nodes(html_album_site, ".subtitle")
                                          year_text <- html_text(year)
                                          latest_album_and_year <- paste(album_titles_text[1], " (", year_text, ")", sep = "")
                                          incProgress(3/15)
                                          output$albums <- renderUI({
                                            HTML("<b>Latest Album:</b>", paste(latest_album_and_year))
                                          })
                                          
                                          incProgress(3/15)
                                          song_titles <- html_nodes(html_album_site, "#colone-container .tracklist a")
                                          song_titles_text <- html_text(song_titles)
                                          incProgress(3/15)
                                          
                                        })
                           output$songs <- renderUI({
                             checkboxGroupInput("chkSongs", h4("Songs:"),
                                                choices = c(song_titles_text))
                             
                           })
                           
                           observeEvent(input$btnSelectAll,{
                             
                             updateCheckboxGroupInput(session=session, "chkSongs", h4("Songs:"),
                                                      choices = c(song_titles_text), selected = song_titles_text)
                           })
                           
                           observeEvent(input$btnDeselectAll,{
                             
                             updateCheckboxGroupInput(session=session, "chkSongs", h4("Songs:"),
                                                      choices = c(song_titles_text), selected = NULL)
                           })
                           
                         })
                       })
                     }
                   })
                 }, error = function(cond){
                   dlgMessage("No internet connection or the webpage is currently unavailable.\nPlease click refresh" , "ok")
                 })
               })
  #SEARCH BUTTON [END]
  
  datasetInputSongs <- reactive({
    perm.vectorSongs <- as.vector(input$chkSongs)
    perm.vectorSongs
  }) 
  
  #SHOW ALL ALBUM BUTTON [START]
  observeEvent(input$btnShowAll,{
    tryCatch({
    list_albums_text <- NULL
    withProgress(message = 'Displaying all albums',
                 value = 0, {
                   
                   incProgress(3/15)
                   if (length(selected_artist) > 1){
                     edit_selected_artist <- gsub("[[:punct:]]|\\s", "-", input$rbtnArtists)
                     edited_artist <- gsub("--|---", "-", edit_selected_artist)
                   }
                   html_artist_site <- read_html(paste("http://www.songlyrics.com/", tolower(edited_artist), "-lyrics/", sep = ""))  
                   list_albums<- html_nodes(html_artist_site, "#colone-container h3")
                   list_albums_text <- html_text(list_albums)
                   incProgress(3/15)
                   output$showAllAlbums <- renderUI({
                     radioButtons("rbtnAlbums", "Albums: ", choices = c(list_albums_text))
                     
                   })
                   incProgress(3/15)
                   observe({
                     tryCatch({
                       print("observer3")
                       if(is.null(input$rbtnAlbums)){
                         
                         return()
                       }else{
                         incProgress(3/15)
                         isolate({
                           edit_selected_album <- gsub("[[:punct:]]|\\s", "-", input$rbtnAlbums)
                           edited_album <- gsub("--|---", "-", edit_selected_album)
                           
                           html_album_site <- read_html(paste("http://www.songlyrics.com/", tolower(edited_artist), "/", edited_album , "/", sep = ""))
                           list_songs<- html_nodes(html_album_site, "#colone-container .tracklist a")
                           list_songs_text <- html_text(list_songs)
                           
                           output$ShowSongs <- renderUI({
                             checkboxGroupInput("chkShowSongs", h4("Songs:"),
                                                choices = c(list_songs_text))
                             
                           })
                           observeEvent(input$btnSelectAll3,{
                             
                             updateCheckboxGroupInput(session=session, "chkShowSongs", h4("Songs:"),
                                                      choices = c(song_titles_text), selected = song_titles_text)
                           })
                           
                           observeEvent(input$btnDeselectAll3,{
                             
                             updateCheckboxGroupInput(session=session, "chkShowSongs", h4("Songs:"),
                                                      choices = c(song_titles_text), selected = NULL)
                           })
                         })
                         incProgress(3/15)
                         
                         
                       }
                     }, error = function(cond){
                       dlgMessage("No internet connection or the webpage is currently unavailable.", "ok")
                     })
                   })
                 })
  }, error = function(cond){
    dlgMessage("No internet connection or the webpage is currently unavailable.", "\nTry again.", "ok")
  })
  })
  
  #SHOW ALL ALBUM BUTTON [END]
  
  datasetInputShowSongs <- reactive({
    perm.vectorShowSongs <- as.vector(input$chkShowSongs)
    perm.vectorShowSongs
  }) 
  
  #ADD SONGS BUTTON1 [START]
  observeEvent(input$btnAdd2,{
    tryCatch({
      songs.df <- data.frame(datasetInputSongs())
      
      colnames(songs.df)[1] <- "Song"
      if (length(selected_artist) > 1){
        edit_selected_artist <- gsub("[[:punct:]]|\\s", "-", input$rbtnArtists)
        edited_artist <- gsub("--|---", "-", edit_selected_artist)
      }
      withProgress(message = 'Adding songs to collection...',
                   value = 0, {
                     
                     for (i in 1:nrow(songs.df)){
                       edit_song <- gsub("[[:punct:]]|\\s", "-", songs.df[i,1])
                       edited_song <- gsub("--|---", "-", edit_song)
                       html_song_site <- paste("http://www.songlyrics.com/", tolower(edited_artist) ,"/", edited_song,"-lyrics/", sep="")
                       html_read <- read_html(html_song_site)
                       song_info <- html_nodes(html_read, ".pagetitle p a")
                       lyric_info <- html_nodes(html_read, "#songLyricsDiv")
                       song_info_text <- html_text(song_info)
                       lyric_info_text <- html_text(lyric_info)
                       
                       artist_name.df <- data.frame(Artist = song_info_text[1])
                       album_title.df <- data.frame(Album = song_info_text[2])
                       genre.df <- data.frame(Genre = song_info_text[3])
                       lyric.df <- data.frame(Lyric = lyric_info_text)
                       lyric_filename <- paste(tolower(trim(edited_song)), "-lyrics.txt", sep = "")
                       lyric_path <- paste("/lyrics/", edited_artist, "/", sep = "")
                       
                       if(file.exists(file.path(getwd(), lyric_path))){
                       }else{
                         dir.create(file.path(getwd(), lyric_path))
                       }
                       lyric_folder <- paste(getwd(), lyric_path, lyric_filename, sep = "")
                       
                       if(file.exists("artists_collection.txt")){
                         write.table(artist_name.df, file = "artists_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
                         write.table(album_title.df, file = "albums_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
                         write.table(genre.df, file = "genres_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
                         
                       } else{
                         write.table(artist_name.df, file = "artists_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
                         write.table(album_title.df, file = "albums_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
                         write.table(genre.df, file = "genres_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
                         
                       }
                       write.table(lyric.df, file = lyric_folder, row.names = FALSE, quote = FALSE)
                       incProgress(1/nrow(songs.df))
                       
                       
                     }
                     
                   })
      if(file.exists("songs_collection.txt")){
        write.table(songs.df, file = "songs_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
      }else{
        write.table(songs.df, file = "songs_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
      }
      
      
      
       dlgMessage("Item(s) have been added in the collection.")
      
    }, error = function(cond){
      dlgMessage("No internet connection available.")
    })
  })
  #ADD SONGS BUTTON1 [END]
  
  #ADD SONGS BUTTON2 [START]
  observeEvent(input$btnAdd3,{
    tryCatch({
      songs3.df <- data.frame(datasetInputShowSongs())
      colnames(songs3.df)[1] <- "Song"
      if (length(selected_artist) > 1){
        edit_selected_artist <- gsub("[[:punct:]]|\\s", "-", input$rbtnArtists)
        edited_artist <- gsub("--|---", "-", edit_selected_artist)
      }
      withProgress(message = 'Adding songs to collection...',
                   value = 0, {
                     
                     for (i in 1:nrow(songs3.df)){
                       edit_song <- gsub("[[:punct:]]|\\s", "-", songs3.df[i,1])
                       edited_song <- gsub("--|---", "-", edit_song)
                       html_song_site <- paste("http://www.songlyrics.com/", tolower(edited_artist) ,"/", edited_song,"-lyrics/", sep="")
                       html_read <- read_html(html_song_site)
                       song_info <- html_nodes(html_read, ".pagetitle p a")
                       lyric_info <- html_nodes(html_read, "#songLyricsDiv")
                       song_info_text <- html_text(song_info)
                       lyric_info_text <- html_text(lyric_info)
                       
                       artist_name.df <- data.frame(Artist = song_info_text[1])
                       album_title.df <- data.frame(Album = song_info_text[2])
                       genre.df <- data.frame(Genre = song_info_text[3])
                       lyric.df <- data.frame(Lyric = lyric_info_text)
                       lyric_filename <- paste(tolower(trim(edited_song)), "-lyrics.txt", sep = "")
                       lyric_path <- paste("/lyrics/", edited_artist, "/", sep = "")
                       
                       if(file.exists(file.path(getwd(), lyric_path))){
                       }else{
                         dir.create(file.path(getwd(), lyric_path))
                       }
                       lyric_folder <- paste(getwd(), lyric_path, lyric_filename, sep = "")
                       
                       if(file.exists("artists_collection.txt")){
                         write.table(artist_name.df, file = "artists_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
                         write.table(album_title.df, file = "albums_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
                         write.table(genre.df, file = "genres_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
                         
                       } else{
                         write.table(artist_name.df, file = "artists_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
                         write.table(album_title.df, file = "albums_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
                         write.table(genre.df, file = "genres_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
                         
                       }
                       write.table(lyric.df, file = lyric_folder, row.names = FALSE, quote = FALSE)
                       incProgress(1/nrow(songs3.df))
                       
                       
                     }
                     
                   })
      if(file.exists("songs_collection.txt")){
        write.table(songs3.df, file = "songs_collection.txt", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
      }else{
        write.table(songs3.df, file = "songs_collection.txt", col.names = TRUE, row.names = FALSE, quote = FALSE)
      }
      
      
      
      dlgMessage("Item(s) have been added in the collection.")
      
    }, error = function(cond){
      dlgMessage("No internet connection available.")
    })
    
  })
  #ADD SONGS BUTTON2 [END]
  #GET FROM WEB TAB [END]
  
  #----------------------------------------------------------------------------------------------
  
  #TOP 10 TAB [START]
  observe({
    if(file.exists(file.path(getwd(), "images"))){
    }else{
      dir.create(file.path(getwd(), "images"))
    }
    
    if(file.exists(file.path(getwd(), "lyrics"))){
    }else{
      dir.create(file.path(getwd(), "lyrics"))
    }
    
    
    tryCatch({
      print("observer4")
      genres <- c("acoustic",
                  "adult-contemporary",
                  "african",
                  "alternative",
                  "avant-Garde",
                  "blues",
                  "childrens-music",
                  "christian",
                  "classical",
                  "comedy",
                  "country",
                  "dance",
                  "electronic",
                  "folk",
                  "funk",
                  "hip-hop-Rap",
                  "holiday",
                  "instrumental",
                  "jazz",
                  "latin",
                  "musical",
                  "new-age",
                  "oldies",
                  "pop",
                  "r-and-b",
                  "reggae",
                  "rock",
                  "ska",
                  "soul",
                  "soundtrack",
                  "vocal",
                  "world")
      
      
      if(input$select_from=="web"){
        if(input$select_genre == "all"){
          select_all()
        }else{
          for(i in genres){
            if(input$select_genre == i){
              select_top_genre(i)
            }
            
          }
        }
        
      }else if(input$select_from=="collection"){
        corpus_read  <-Corpus(DirSource(paste(getwd(), "/lyrics/", sep = "")), readerControl = list(reader=readPlain))
        
        corpus <- tm_map(corpus_read, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
        dtm <- as.matrix(DocumentTermMatrix(corpus))
        frequency <- colSums(dtm)
        frequency <- sort(frequency, decreasing = TRUE)
        words <- names(frequency)
        
        MyArtists<- read.table(file="artists_collection.txt",header=TRUE, sep=",", stringsAsFactors = FALSE)
        artists10 <- names(sort(table(MyArtists), decreasing = TRUE))
        
        
        output$topArtists <- renderUI({
          HTML(paste(1:3, artists10[1:3], '<br/>'))
        })
        
      } 
    }, error = function(cond){
      dlgMessage(c("No internet connection or the webpage is currently unavailable.", "\nPlease click refresh"), c("ok"))
      
    })
  })
  
  #TOP 10 TAB [END]
  
  #----------------------------------------------------------------------------------------------
  
  #WORD CLOUD TAB [START]
  #SHOW ARTIST BUTTON [START]
  observeEvent(input$btnShowArtists,{
    empty.df <- data.frame("Collection is empty!")
    write.table(empty.df, file = "empty.txt", col.names = FALSE, row.names = FALSE, quote = FALSE)
    Empty_Text<- read.table(file="empty.txt", header=FALSE, sep=",")
    
    if(file.exists("artists_collection.txt")){
      
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",")
      
      output$artist_word1 = DT::renderDataTable(
        unique(MyArtists)
      )
      
      output$artist_word2 = DT::renderDataTable(
        unique(MyArtists)
      )
      
    }else{
      output$artist_word1 = DT::renderDataTable(
        Empty_Text
      )
      output$artist_word2 = DT::renderDataTable(
        Empty_Text
      )
      
    }
    
  })
  #SHOW ARTIST BUTTON [END]
  
  #DISPLAY WORD CLOUD1 [START]
  output$word1 <- renderPlot({
    s = input$artist_word1_rows_selected
    
    if(length(s)==1){
      
      ss <- strtoi(s)
      Mysongs<- read.table(file="songs_collection.txt", header=TRUE, sep=",")
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
      artist1 <- paste(MyArtists$Artist[ss])
      song1 <- paste(Mysongs$Song[ss])
      
      #       filename <- paste(getwd(),"/lyrics/",artist1,"-",song1,"-lyric.csv", sep = "")
      #       MyLyrics <- read.csv(file=filename, header=FALSE, sep = "")
      edit_song <- gsub("[[:punct:]]|\\s", "-", song1)
      edited_song <- gsub("--|---", "-", edit_song)
      
      edit_artist <- gsub("[[:punct:]]|\\s", "-", artist1)
      edited_artist <- gsub("--|---", "-", edit_artist)
      corpus_read  <-Corpus(DirSource(paste(getwd(), "/lyrics/", edited_artist, "/", sep = "")), readerControl = list(reader=readPlain))
      corpus <- tm_map(corpus_read, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      
      dtm <- as.matrix(DocumentTermMatrix(corpus))
      frequency <- colSums(dtm)
      frequency <- sort(frequency, decreasing = TRUE)
      words <- names(frequency)
      
      wordcloud(corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
      
      
      
    }else if(length(s) > 1){
      
      cat("Select only one song to view song lyrics.")
      
    }else{
      # disable("btnDelete2")
      cat("Select a song")
    }
    
  })
  #DISPLAY WORD CLOUD1 [END]
  
  #DISPLAY WORD CLOUD2 [START]
  output$word2 <- renderPlot({
    s = input$artist_word2_rows_selected
    
    if(length(s)==1){
      
      ss <- strtoi(s)
      Mysongs<- read.table(file="songs_collection.txt", header=TRUE, sep=",")
      MyArtists<- read.table(file="artists_collection.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
      artist1 <- paste(MyArtists$Artist[ss])
      song1 <- paste(Mysongs$Song[ss])
      
      #       filename <- paste(getwd(),"/lyrics/",artist1,"-",song1,"-lyric.csv", sep = "")
      #       MyLyrics <- read.csv(file=filename, header=FALSE, sep = "")
      edit_song <- gsub("[[:punct:]]|\\s", "-", song1)
      edited_song <- gsub("--|---", "-", edit_song)
      
      edit_artist <- gsub("[[:punct:]]|\\s", "-", artist1)
      edited_artist <- gsub("--|---", "-", edit_artist)
      corpus_read  <-Corpus(DirSource(paste(getwd(), "/lyrics/", edited_artist, "/", sep = "")), readerControl = list(reader=readPlain))
      
      corpus <- tm_map(corpus_read, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      
      dtm <- as.matrix(DocumentTermMatrix(corpus))
      frequency <- colSums(dtm)
      frequency <- sort(frequency, decreasing = TRUE)
      words <- names(frequency)
      
      wordcloud(corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
      
      
      
    }else if(length(s) > 1){
      
      cat("Select only one song to view song lyrics.")
      
    }else{
      # disable("btnDelete2")
      cat("Select a song")
    }
    
  })
  #DISPLAY WORD CLOUD2 [END]
  #WORD CLOUD TAB [END]
  
  #END PROGRAM
})

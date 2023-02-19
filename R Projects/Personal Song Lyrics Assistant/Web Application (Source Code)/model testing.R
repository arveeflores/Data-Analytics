library(tm)
library(sentiment)
# TEST_DATA <- read.delim2("C:/Users/Arvee/Desktop/Data/TEST_DATA.txt", header=FALSE)
# names(TEST_DATA) <- c("STANZA", "EMOTION")

#trace("create_matrix",edit=TRUE)
#install.packages("C:/sentiment_0.2.tar.gz", repos = NULL, type="source")
# song_test_data <- vector(length = 0)
# emotion_table <- vector(length = 0)
# test_song_edited <- vector(length = 0)
# data <- vector(length = 0)
# myFiles <- list.files(path="C:/Users/Arvee/Documents/R Projects/3.9/Test Data", pattern="*.txt")
# for(i in 1:20){
#   directory <- paste("C:/Users/Arvee/Documents/R Projects/3.9/Test Data/", myFiles[i], sep = "")
#   song_test_data[i] <- read.delim2(directory, header=FALSE)
#   
# }




#test_song <- "sad alone arent blue brothers cold comes courage fall feat forever future get gets head hold juny keep lies life live mahiaddin malaysia may must najwa one ooh rain rising see sisters sky stronger sun times together way well wisdom youre"
song_test_data <- vector(length = 0)
song_test_data[1] <- c("Tell me why Ain't nothin' but a heartache Tell me why Ain't nothin' but a mistake Tell me why I never wanna hear you say I want it that way")
song_test_data[2] <- c("Let's Marvin Gaye and get it on You got the healing that I want Just like they say it in the song Until the dawn Let's Marvin Gaye and get it on You've got to give it up to me I'm screaming, Mercy, mercy, please! Just like they say it in the song Until the dawn Let's Marvin Gaye and get it on")
song_test_data[3] <- c("Just let me rock, fuck you back to sleep girl Don't say a word no, don't you talk Just hold on tight to me girl Fuck you back to sleep girl rock you back")
song_test_data[4] <- c("We used to be the life of the party We used to be the ones that they wished they were But now it's like they don't know how to act Maybe they're like me and they want us back It's like there's always an empty space Those memories that nobody can erase Of how bright we burned Well now it hurts, but it's true When they think of me, they think of you")
song_test_data[5] <- c("I got broads in Atlanta Twisting dope, lean, and the Fanta Credit cards and the scammers Hitting off licks in the bando Black X6, Phantom White X6 looks like a panda Going out like I'm Montana Hundred killers, hundred hammers Black X6, Phantom White X6, panda Pockets swole, Danny Selling bar, candy Man I'm the macho like Randy The choppa go Oscar for Grammy Bitch nigga pull up ya panty Hope you killas understand me")
song_test_data[6] <- c("So honey, now, take me into your loving arms Kiss me under the light of a thousand stars Place your head on my beating heart, I'm thinking out loud And maybe we found love right where we are")
song_test_data[7] <- c("Give me sticks, give stones Bend my body, break my bones Use staff and rod to turn me black and blue Cause you can't unhear, you can't unsay But if were up to me to change I'd turn lies and hate to love and truth If I could only kill a word")
song_test_data[8] <- c("I hate you I love you I hate that I love you Don't want to, but I can't put Nobody else above you I hate you I love you I hate that I want you You want her, you need her And I'll never be her")
song_test_data[9] <- c("Take me to church I'll worship like a dog at the shrine of your lies I'll tell you my sins so (and) you can sharpen your knife Offer me that (my) deathless death Good God, let me give you my life Take me to church I'll worship like a dog at the shrine of your lies I'll tell you my sins so (and) you can sharpen your knife Offer me that (my) deathless death Good God, let me give you my life")
song_test_data[10] <- c("Get high baby roll one, cloud nine 'bout to go up Lovin' the feelin' the turbulence get when we turn up When we land we can roll out Show you somethin' you ain't know about Tonight we be takin' off flight with a camera to show out")
song_test_data[11] <- c("Doctor, please, give me a dose of the American Dream Put down the pen and look in my eyes We're in the waiting room and something ain't right All this is on you, we're overprescribed")
song_test_data[12] <- c("Billie Jean Is Not My Lover She's Just A Girl Who Claims That I Am The One But The Kid Is Not My Son She Said I Am The One, But The Kid Is Not My Son")
song_test_data[13] <- c("Oh don't you wonder when the light begins to fade? And the clock just makes the colors turn to grey Forever younger Growing older just the same All the memories that we make will never change We'll stay drunk, we'll stay tan, let the love remain And I swear that I'll always paint you")
song_test_data[14] <- c("We'll find we can meet in the middle Bodies and souls collide Dance in the moonlight Where all the stars align For you and I, for you and I, oh")
song_test_data[15] <- c("Feel like a brand new person (But you make the same old mistakes) Well, I don't care I'm in love (Stop before it's too late) Feel like a brand new person (But you make the same old mistakes) I finally know what it's like (You don't have what it takes) (Stop before it's not too late) (I know there's too much at stake) (Making the same mistakes) And I still don't know why it's happening (Stop while it's not too late) And I still don't know")
song_test_data[16] <- c("Romeo, take me somewhere we can be alone I'll be waiting all there's left to do is run You'll be the prince and I'll be the princess It's a love story baby just say yes")
song_test_data[17] <- c("And if I believe you Will that make it stop? If I told you I need you Is that what you want? And I'm broken and bleeding And begging for help And I'm asking you Jesus, show yourself")
song_test_data[18] <- c("And you're the only thing that's going on in my mind Taking over my life a second time I don't have the capacity for fucking You're meant to be helping me When I said I liked it better without my money, I lied It took a little while to recognize That I, I'm not giving it up again")
song_test_data[19] <- c("I can't feel my face when I'm with you But I love it, but I love it, oh I can't feel my face when I'm with you But I love it, but I love it, oh")
song_test_data[20] <- c("So we'll piss off the neighbours In the place that feels the tears The place to lose your fears Yeah, reckless behavior A place that is so pure, so dirty and raw In the bed all day, bed all day, bed all day Fucking in and fighting on It's our paradise and it's our war zone It's our paradise and it's our war zone")


love_yourself <-c("[Verse 1]
For all the times that you rain on my parade
                  And all the clubs you get in using my name
                  You think you broke my heart, oh girl for goodness sake
                  You think I'm crying, oh my oh, well I ain't
                  
                  [Pre-Chorus]
                  And I didn't wanna write a song cause I didn't want anyone thinking I still care
                  I don't but, you still hit my phone up
                  And baby I be movin' on and I think you should be somethin'
                  I don't wanna hold back, maybe you should know that
                  My mama don't like you and she likes everyone
                  And I never like to admit that I was wrong
                  And I've been so caught up in my job, didn't see what's going on
                  And now I know, I'm better sleeping on my own
                  
                  [Chorus]
                  Cause if you like the way you look that much
                  Oh baby you should go and love yourself
                  And if you think that I'm still holdin' on to somethin'
                  You should go and love yourself
                  
                  [Verse 2]
                  But when you told me that you hated my friends
                  The only problem was with you and not them
                  And every time you told me my opinion was wrong
                  And tried to make me forget where I came from
                  
                  [Pre-Chorus]
                  And I didn't wanna write a song cause I didn't want anyone thinking I still care
                  image: http://static.urx.io/units/web/urx-unit-loader.gif
                  
                  
                  I don't but, you still hit my phone up
                  And baby I be movin' on and I think you should be somethin'
                  I don't wanna hold back, maybe you should know that
                  My mama don't like you and she likes everyone
                  And I never like to admit that I was wrong
                  And I've been so caught up in my job, didn't see what's going on
                  And now I know, I'm better sleeping on my own
                  
                  [Chorus]
                  Cause if you like the way you look that much
                  Oh baby you should go and love yourself
                  And if you think that I'm still holdin' on to somethin'
                  You should go and love yourself
                  
                  [Bridge]
                  For all the times you made me feel small
                  I fell in love, now I fear nothin' at all
                  I never felt so low when I was vulnerable
                  Was I a fool to let you break down my walls?
                  
                  [Chorus]
                  Cause if you like the way you look that much
                  Oh baby you should go and love yourself
                  And if you think that I'm still holdin' on to somethin'
                  You should go and love yourself
                  Cause if you like the way you look that much
                  Oh baby you should go and love yourself
                  And if you think that I'm still holdin' on to somethin'
                  You should go and love yourself")

# sampleX <- c("sad! sad sad sad happy")
# sampleZ <- strsplit(sampleX, " ")[[1]]
corpus_read  <- Corpus(VectorSource(song_test_data[10]))
corpus <- tm_map(corpus_read, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
freq <- colSums(dtm2)
freq <- sort(freq, decreasing = TRUE)
U <- c(rep.int(names(freq), freq))
song_emotion <- classify_emotion(U, algorithm = "bayes")
best_fit = song_emotion[,7]
data <- cbind(song_emotion,U)
emotion_table <- sort(table(best_fit), decreasing = TRUE)
final_emotion <- names(emotion_table[1])

emotion_table
final_emotion




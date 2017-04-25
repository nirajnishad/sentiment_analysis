library(twitteR)
library(plyr)
library(stringr)
library(ggvis)
library(ggplot2)
library(memoise)
library(gridExtra)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#Print messages sent between the R server and the web browser client to the R console. This is useful for debugging. 
options(shiny.trace=TRUE)

#Set Number of tweets to be fetch from twitter(fetching as input from frontend)
# n_tweets <- 1500

#Loading list of Positive words and negative words to identify the sentiments
op_pos <- scan('positive-words.txt', what='character', comment.char=';')
op_neg <- scan('negative-words.txt', what='character', comment.char=';')
pos.words <- c(op_pos, 'upgrade', 'fleek')
neg.words <- c(op_neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical', 'bs', 'bullshit', 'b.s.')

#Twitter API Key 
#Enter your Credentials
consumerKey <- "CrbQnMVf71Ct4reqPfZ0Wt9CB"
consumerSecret <- "3SZxuaKtUOSEiMtXlP9sFEAkgqspazKoz99ap3CquXIklyiyEZ"
acessToken <- "81574898-ooM0VWX3IQ94gwrWoVl1sV6IBumBJkKrdk5iMuiTv"
accessTokenSecret <- "GqTNu0idOVLDJpQKlth9QY00AfIcjmwbrCuRe6jpKYHii"

#Setup connection with twitter to fetch the data
setup_twitter_oauth(consumerKey, consumerSecret, acessToken, accessTokenSecret)


shinyServer(function(input, output, session) {
  # Define a reactive expression for the document term matrix

  ##Function to convert the text to lower case
  tryTolower = function(x){
    y = NA
    try_error = tryCatch(tolower(x), error = function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  ##Function to calculate the sentiment score of the tweets
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)

      # split into words. str_split is in the stringr package
       word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
       words = unlist(word.list)

      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      #TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  get_source <- function(x){
    X <- cleanFun(x[["statusSource"]])
    X
  }
  
  tweets_df <- reactive({
    input$plot_feel
    isolate({
      withProgress({
        setProgress(message = "Processing sentiment...")
        
        if(input$lang=="All")
          tweets <- searchTwitter(input$source1, n=input$n_tweets) 
        else 
        tweets <- searchTwitter(input$source1, n=input$n_tweets, lang=input$lang)
        
        tweets <- strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
        
        df <- twListToDF(tweets)
        df$Search <- input$source1
        
        if( (input$show_source2 == TRUE) && (input$source2 != ""))
        {
          if(input$lang=="All") 
            tweets2 <- searchTwitter(input$source2, n=input$n_tweets) 
          else 
            tweets2 <- searchTwitter(input$source2, n=input$n_tweets, lang=input$lang)
            
          
          tweets2 <- strip_retweets(tweets2, strip_manual=TRUE, strip_mt=TRUE)
          df2 <- twListToDF(tweets2)
          df2$Search <- input$source2
          df <- rbind(df, df2)
          tweets <- c(tweets, tweets2)
        }
        
        
        df$Date <- format(df$created,'%m/%d/%Y %H:%I:%S')
        df$Source <-  apply(df, 1, get_source)
        
        sentences <- sapply(df$text, function(x) tryTolower(x))
        
        scores <- score.sentiment(sentences, pos.words, neg.words)
        df <- cbind(df, scores)
        
        df <- df[, c("id", "text", "Source", "Date", "Search", "created", "score","screenName")]
        names(df) <- c("id", "Post", "Source", "Date", "Search", "created", "score","screenName")
        df
        
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)

    output$wordcloud <- renderPlot({
    df <- tweets_df()
    tweets_result_Table <- df
    
    #Treating the tweets for the words to be displayed on the word cloud
    tweets_result_Table$Post = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_result_Table$Post)
    tweets_result_Table$Post = gsub("[[:punct:]]", "", tweets_result_Table$Post)
    tweets_result_Table$Post = gsub("[[:digit:]]", "", tweets_result_Table$Post)
    tweets_result_Table$Post = gsub("http\\w+", "", tweets_result_Table$Post)
    tweets_result_Table$Post = gsub("[ \t]{2,}", "", tweets_result_Table$Post)
    tweets_result_Table$Post = gsub("^\\s+|\\s+$", "", tweets_result_Table$Post)
    tweets_result <- tweets_result_Table$Post
    docs <- Corpus(VectorSource(tweets_result))
    dtm <- TermDocumentMatrix(docs)
    
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    cloud <- data.frame(word = names(v),freq=v)
    words <-  cloud$word
    freq <- cloud$freq
    
    wordcloud_rep(words, freq , min.freq = min(freq),scale=c(4,0.5),
                max.words= max(freq), random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
  },height = 500, width = 900)
  

  output$trends <- renderPlot({
    df <- tweets_df()
    
    source1 <- df[df$Search==input$source1,]
    p1 <- ggplot(source1, aes(x=created, y=score)) + geom_point(shape=1, size=0)+geom_smooth(se=F)+labs(title=input$source1, x = "Date /Time", y = "Popularity") + ylim(-5, 5)
    
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
    {
      source2 <- df[df$Search==input$source2,]
      
      p2 <- ggplot(source2, aes(x=created, y=score)) + geom_point(shape=1, size=0)+geom_smooth(se=F)+labs(title=input$source2, x = "Date /Time", y = "Popularity") + ylim(-5, 5)
      grid.arrange(p1, p2, nrow=1, ncol=2)
    }
    else
      print(p1)
    
  })
  
  output$twitter_view <- renderPrint({
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
      cat(paste(input$source1, " vs. ", input$source2))
    else
      cat(input$source1)
  })
  
  output$view <- renderTable({
    df <- tweets_df()
    df <- df[df$Search==input$source1,]
    head(df, addrownums=F)
  })
  
  output$vs_view <- renderTable({
    
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
    {
      df <- tweets_df()
      df <- df[df$Search==input$source2,]
      head(df, addrownums=F)
    }
  })
  
  output$Table <- renderDataTable({
    
      df <- tweets_df()
      tweets_result_Table <- df
      #Treating the tweets for the words to be displayed on the bottom table
      tweets_result_Table$Post = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_result_Table$Post)
      tweets_result_Table$Post = gsub("[[:punct:]]", "", tweets_result_Table$Post)
      tweets_result_Table$Post = gsub("http\\w+", "", tweets_result_Table$Post)
      tweets_result_Table$Post = gsub("[ \t]{2,}", "", tweets_result_Table$Post)
      tweets_result_Table$Post = gsub("^\\s+|\\s+$", "", tweets_result_Table$Post)
      tweets_result <- tweets_result_Table

      tweets_result <- data.frame(tweets_result$screenName,tweets_result$Post)
      colnames(tweets_result)[1] <- "User"
      colnames(tweets_result)[2] <- "Tweets"
      
      tweets_result
  })
  
  # Function for generating tooltip text
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)
    
    all_tweets <- isolate(tweets_df())
    tweet <- all_tweets[all_tweets$id == x$id, ]
    
    paste0("<b>", tweet$Post, "</b><br><em><small>from ", tweet$Source, " (", tweet$Date, ")</small></em>")
  }
  
  
#Reactive expression with the ggvis plot
  vis2 <- reactive({
    
    df <- tweets_df()
    
    df[df$Search==input$source2,] %>%  ggvis(~created, ~score) %>% layer_points(fill = ~Search, key := ~id)  %>% layer_lines(stroke=~Search) %>% add_legend(c("fill", "stroke"), orient="left") %>% add_axis("x", title = "Date Time") %>% add_axis("y", title = "Popularity") %>% set_options(width = 800, height = 300) %>% add_tooltip(movie_tooltip, "click")
    
    if( (input$show_source2 != TRUE) || (input$source2 == "") )
      invisible()
  })
  
  vis <- reactive({
    legend_val <- c(input$source1)
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
      legend_val <- c(input$source1, input$source2)
    
    df <- tweets_df()
    
    df %>%  ggvis(~created, ~score) %>% layer_points(fill = ~Search, key := ~id)  %>% layer_lines(stroke=~Search) %>% add_legend(c("fill", "stroke"), orient="left") %>% add_axis("x", title = "Date Time") %>% add_axis("y", title = "Popularity") %>% set_options(width = 800, height = 300) %>% add_tooltip(movie_tooltip, "click")
  })
  
  vis %>% bind_shiny("plot1")
  
})


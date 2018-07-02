# Backend/server of R app
# Performs sentiment analysis of tweets based on emotion and polarity classification
# The visualization of the sentiment class distributions is performed using ggplot2 package

# required pakacges
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(RColorBrewer)

# loading twitter credentials
#load("twitteR_credentials")
#registerTwitterOAuth(twitCred)

api_key <- "Epoyc1VfIrHOyD6K5DIV4QlN3"

api_secret <- "STu3FNc87kimdiMV6Nw4ouWBKKo9Qbxgi6ultTvDVPCWSa0Kgs"

access_token <- "469047588-S8cXA0zY01AlcZUAQMm5afRszhkML3xR9QkKrXxe"

access_token_secret <- "zct6ERd2AkJhJwyXRUOf4x0pIjKlF0wIkPrc9ClDSFE4i"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# loading the helper functions
source('helpers.R')
source('helpers1.R')
source('helpers2.R')
shinyServer(function(input, output) {
  
  # Step 1: Getting the tweets based on search terms
  # cainfo="cacert.pem" is required for data access
 # tweets <- reactive ({ searchTwitter(input$searchTerm,n=1000, lang="en") })
  tweets <- reactive ({ searchTwitter(input$searchTerm,n=1000, lang="en") })
  #tweets <- renderText({ input$searchTerm })
  
  
  # Step2: Preprocessing to clean up the tweets
  txtTweets <- reactive ({ preprocess_tweet (tweets()) })
  
  output$plot_emotion <- renderPlot({  
    
    # Step 3: Emotion sentiment analysis
    emotion <- emotionSentimentAnal(txtTweets())
    
    # Step 4: Polarity sentiment analysis
    polarity <- polaritySentimentAnal(txtTweets())
    
    # Step 5: Store results in dataframe
    results_df <- data.frame(text=txtTweets(), emotion=emotion, polarity=polarity)
    
    # Step 6: Plot distribution of tweet sentiments
    if (input$plot_opt == 'emotion') {
      
      ggplot(results_df) +
        geom_bar(aes(x=emotion, y=..count.., fill=emotion)) +
        ggtitle(paste('Using Bayes Method Intention Mining of Search Term "', input$searchTerm, '"', sep='')) +      
        xlab("Emotion Class") + ylab("No of Tweets") +
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) +
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) + 
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) +
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) + 
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) +
        theme(legend.text = element_text(colour="black", size=16, face='plain')) +
        theme(legend.title = element_text(colour="black", size=18, face='plain')) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 2))
      
    } else {
      
      ggplot(results_df, aes()) +
        geom_bar(aes(x=polarity, y=..count.., fill=polarity), width=0.6) +
        ggtitle(paste('Using Bayes Method Intention Mining of Search Term "', input$searchTerm, '"', sep='')) +
        xlab("Polarity Class") + ylab("No of Tweets") +   
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) +
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) + 
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) +
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) + 
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) +
        theme(legend.text = element_text(colour="black", size=16, face='plain')) +
        theme(legend.title = element_text(colour="black", size=18, face='plain')) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 2))
      
    } 
    
  })  
  
  
  output$plot_emotion1 <- renderPlot({  
    
    # Step 3: Emotion sentiment analysis
    emotion <- emotionSentimentAnal1(txtTweets())
    
    # Step 4: Polarity sentiment analysis
    polarity <- polaritySentimentAnal1(txtTweets())
    
    # Step 5: Store results in dataframe
    results_df <- data.frame(text=txtTweets(), emotion=emotion, polarity=polarity)
    
    # Step 6: Plot distribution of tweet sentiments
    if (input$plot_opt == 'emotion') {
      
      ggplot(results_df) +
        geom_bar(aes(x=emotion, y=..count.., fill=emotion)) +
        ggtitle(paste('Using MAXENT Intention Mining of Search Term "', input$searchTerm, '"', sep='')) +      
        xlab("Emotion Class") + ylab("No of Tweets") +
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) +
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) + 
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) +
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) + 
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) +
        theme(legend.text = element_text(colour="black", size=16, face='plain')) +
        theme(legend.title = element_text(colour="black", size=18, face='plain')) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 2))
      
    } else {
      
      ggplot(results_df, aes()) +
        geom_bar(aes(x=polarity, y=..count.., fill=polarity), width=0.6) +
        ggtitle(paste('Using MAXENT Intention Mining of Search Term "', input$searchTerm, '"', sep='')) +
        xlab("Polarity Class") + ylab("No of Tweets") +   
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) +
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) + 
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) +
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) + 
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) +
        theme(legend.text = element_text(colour="black", size=16, face='plain')) +
        theme(legend.title = element_text(colour="black", size=18, face='plain')) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 2))
      
    } 
    
  })   
  
  output$plot_emotion2 <- renderPlot({  
    
    # Step 3: Emotion sentiment analysis
    emotion <- emotionSentimentAnal2(txtTweets())
    
    # Step 4: Polarity sentiment analysis
    polarity <- polaritySentimentAnal2(txtTweets())
    
    # Step 5: Store results in dataframe
    results_df <- data.frame(text=txtTweets(), emotion=emotion, polarity=polarity)
    
    # Step 6: Plot distribution of tweet sentiments
    if (input$plot_opt == 'emotion') {
      
      ggplot(results_df) +
        geom_bar(aes(x=emotion, y=..count.., fill=emotion)) +
        ggtitle(paste('Using HMM Intention Mining of Search Term "', input$searchTerm, '"', sep='')) +      
        xlab("Emotion Class") + ylab("No of Tweets") +
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) +
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) + 
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) +
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) + 
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) +
        theme(legend.text = element_text(colour="black", size=16, face='plain')) +
        theme(legend.title = element_text(colour="black", size=18, face='plain')) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 2))
      
    } else {
      
      ggplot(results_df, aes()) +
        geom_bar(aes(x=polarity, y=..count.., fill=polarity), width=0.6) +
        ggtitle(paste('Using HMM Intention Mining of Search Term "', input$searchTerm, '"', sep='')) +
        xlab("Polarity Class") + ylab("No of Tweets") +   
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) +
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) + 
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) +
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) + 
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) +
        theme(legend.text = element_text(colour="black", size=16, face='plain')) +
        theme(legend.title = element_text(colour="black", size=18, face='plain')) +
        guides(fill = guide_legend(keywidth = 2, keyheight = 2))
      
    } 
    
  })     
  
  
  
  
})
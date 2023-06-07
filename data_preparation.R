library(tidyverse)
library(tidytext)
library(shiny)
library(shinydashboard)
library(transforEmotion)
library(syuzhet) # library to calculate nrc sentiment scores
library(sentimentr)
library(plotly)
library(stopwords)


library(stringi) # library to convert verbs to straight form
library(stringr)
library(SemNetCleaner) # library to singularize plural words

library(textclean) #library to singularize plural words
library(lexicon)

library(tm)

library(irlba) # pca
 
setwd("../AI_SentimentAnalysis")


# loading datasets
google_articles <- drop_na(read_csv('google_articles.csv',show_col_types = F))
bing_articles <- drop_na(read_csv('bing_articles.csv',show_col_types = F))

# cleaning data
results_subset <- subset(google_articles, !grepl("a\\.i\\.|artificial intelligence", 
                  google_articles$output, ignore.case = TRUE) & !grepl("AI", google_articles$output))
google_articles <- anti_join(google_articles, results_subset,by='url')

google_articles <- google_articles%>% 
                   mutate(index=1:nrow(google_articles))


results_subset <- subset(bing_articles, !grepl("a\\.i\\.|artificial intelligence", 
                         bing_articles$text, ignore.case = TRUE) & !grepl("AI", bing_articles$text))

bing_articles <- anti_join(bing_articles, results_subset,by='url')

bing_articles <- bing_articles %>% 
                 mutate(index=1:nrow(bing_articles)) %>%
                 rename(output=text,
                        num_words=words)%>%
                 select(index,everything())
                  


get_sentiment_scores <-function(df,n_components=43){      
      #  nrc sentiment
      
      nrc_sent <- syuzhet::get_nrc_sentiment(df$output) %>% mutate(index=1:nrow(df))%>%
        select(index,everything()) 
      
      colnames(nrc_sent)[2:11] <- paste('nrc',colnames(nrc_sent)[2:11],sep='_')
      
      
      
      #  sentimenter
      
      # transformer_scores("Who is trump",classes = c("President","Businessman"),multiple_classes = TRUE,transformer = 'facebook-bart' )
      
      sentence_wise_sent <- sentimentr::sentiment(df$output)
        
      article_wise_sent <- sentence_wise_sent %>% group_by(element_id) %>%
                             summarise(sum_sentimentR=sum(sentiment),
                                       abs_sum_sentimentR=sum(abs(sentiment)),
                                       mean_sentimentR=mean(sentiment))  %>%
                             ungroup() %>%
                             rename(index=element_id)
      
      
      
      # plot_ly(data = article_wise_sent)  %>%
      #   add_trace(x=~sum_sentiment,type='histogram',color='red',text='Total sum') %>%
      #   add_trace(x=~abs_sum_sentiment,type='histogram',color='green',text='ABS sum') %>%
      #   add_trace(x=~mean_sentiment,type='histogram',color='blue',text='Mean')
      
      
      
      # text output tokenizing
      
      # this stop words dictionary will help us to remove all auxiliary verbs with not
      # straight form
      stop_words_snow <- data.frame(stopwords())%>%rename(word=stopwords..)
      
      tokens <- df %>% select(index,output)%>%
        unnest_tokens(word,output) %>%
        mutate(word=stri_trans_general(str=word,id='latin-ascii'), # convert to straight form 
               word=stri_trans_general(word,'lower'), #convert to lower case
               word=sub("'.*$","",word),            # remove all apostrophes
               word=gsub("[[:punct:][:digit:]]", "", word),
               word=iconv(word,from='latin1',to='ascii')) %>% 
        anti_join(stop_words,by='word') %>% # remove stop words
        anti_join(stop_words_snow,by='word') %>% # remove stop words for not straight form of auxiliary verbs
        filter(word!='')            #remove all blank rows
      
      
      
      # convert all plurals into singulars 
      
      tokens <- rowwise(tokens)%>%
        mutate(word=singularize(word))

      
      # PCA
      
      tokens_n <- tokens %>% count(word,index,sort=TRUE)
      
      
      # spacrce scaled matrix for pca
      
      sparce <- tokens_n %>% 
                cast_sparse(index,word,n) %>% 
                scale()
      
      
      # calculating principal components
      prc_comp <- prcomp_irlba(sparce,n=n_components,retx = T)
      
      # principal components per word
      
      pca_per_word <- bind_cols(word=colnames(sparce),
                                prc_comp$rotation)
      
      
      # principal components per article
      
      
      pca_per_article <- bind_cols(index=as.integer(rownames(sparce)),
                                   prc_comp$x)
      
      
      
      # pca variance
      
      pca_summary <- summary(prc_comp)$importance
      
      pca_variance <- data.frame(PrincipalComponent=colnames(pca_summary),
                                 StDev=pca_summary[1,],
                                 VarianceProportion=pca_summary[2,],
                                 CumulativeVariance=pca_summary[3,]) %>%
        filter(PrincipalComponent!='')
      
      # tf_idf
      tf_idf <- tokens %>% 
        count(word,index,sort=TRUE) %>%
        select(index,word,n_words=n) %>%
        bind_tf_idf(word,index,n_words) %>%
        arrange(index,n_words)
      
      
      # mean tf_idf per artcile
      tf_idf_per_article <- tf_idf %>%
        group_by(index) %>%
        summarise(mean_df=mean(tf),
                  mean_idf=mean(idf),
                  mean_tf_idf=mean(tf_idf),
                  total_words=sum(n_words)) %>%
        ungroup()
      
      
      # tf_idf per word
      tf_idf_per_word <- tf_idf %>%
        group_by(word) %>%
        summarise(mean_word_tf = mean(tf),
                  mean_word_idf=mean(idf),
                  mean_word_tf_idf=mean(tf_idf))%>%
        ungroup()
      
      
      # bing and affin
      afinn_bing_sentiment <- tokens %>%
        inner_join(get_sentiments('afinn'),by='word') %>%
        inner_join(get_sentiments('bing'),by='word') %>%
        mutate(afinn=value,
               bing=ifelse(sentiment=='positive',1,-1)) %>%
        select(-c(value,sentiment))
      
      #  per article afinn and bing
      afinn_bing_per_article <- afinn_bing_sentiment%>%
                                group_by(index)%>%
                                summarise(mean_afinn=mean(afinn),
                                          abs_sum_afinn=sum(abs(afinn)),
                                          total_sum_afinn=sum(afinn),
                                          mean_bing=mean(bing),
                                          abs_sum_bing=sum(abs(bing)),
                                          total_sum_bing=sum(bing)) %>%
                                ungroup()
      
      # per word afinn and bing
      afinn_bing_per_word <- afinn_bing_sentiment %>%
        group_by(word) %>%
        summarise(mean_afinn=mean(afinn),
                  total_sum_afinn=sum(afinn),
                  mean_bing=mean(bing),
                  total_sum_bing=sum(bing)) %>%
        ungroup()
      
      
      # all dataframes containing article's sentiment scores from different dictionaries
      article_wise_sentiment_df_list = list(df,
                                            nrc_sent,
                                            article_wise_sent,
                                            tf_idf_per_article,
                                            afinn_bing_per_article,
                                            pca_per_article)
      
      
      

    # per article and per word sentiment scores
    
    per_article_sentiment_df <- article_wise_sentiment_df_list %>% reduce(inner_join,by='index')
    
    tf_idf_per_word_pca <- inner_join(tf_idf_per_word,pca_per_word,by='word')
 
    return(list(article_wise=per_article_sentiment_df,
                words_sentiment=afinn_bing_per_word,
                words_tf_idf_pca=tf_idf_per_word_pca,
                pca_variance=pca_variance)) 
}


google_sentiment_output <- get_sentiment_scores(google_articles)

google_articles_sentiments <- google_sentiment_output$article_wise
google_words_sentiments <- google_sentiment_output$words_sentiment
google_words_tf_idf <- google_sentiment_output$words_tf_idf
google_pca_variance <- google_sentiment_output$pca_variance





# bing
bing_sentiment_output <- get_sentiment_scores(bing_articles)

bing_articles_sentiments <- bing_sentiment_output$article_wise
bing_words_sentiments <- bing_sentiment_output$words_sentiment
bing_words_tf_idf <- bing_sentiment_output$words_tf_idf
bing_pca_variance <- bing_sentiment_output$pca_variance



write_csv_ <- function(df_list,var_name='google_'){
  for (df in ls(df_list)){
    write_csv(df_list[[df]],paste0("App/",var_name,df,".csv"))  
    
  }
}


write_csv_(google_sentiment_output)
write_csv_(bing_sentiment_output,var_name = 'bing_')



  
  
  
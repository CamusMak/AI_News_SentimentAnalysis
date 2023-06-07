library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinydashboardPlus)
library(plotly)
library(shinyWidgets)
library(stringr)
library(reshape2)
library(shinyjs)
library(MASS)
library(DT)
library(statsr)
library(tidytext)
library(stopwords)
library(stringi)





# setwd("../AI_SentimentAnalysis/App")


google_articles <- read_csv('google_article_wise.csv',show_col_types = FALSE) %>%rename(mean_tf=mean_df)
bing_articles <- read_csv('bing_article_wise.csv',show_col_types = FALSE) %>%rename(mean_tf=mean_df)


google_words_sentiment <- read_csv('google_words_sentiment.csv',show_col_types = FALSE)
bing_words_sentiment <- read_csv('bing_words_sentiment.csv',show_col_types = FALSE)

google_words_tf_idf <- read_csv('google_words_tf_idf_pca.csv',show_col_types = FALSE)
bing_words_tf_idf <- read_csv('bing_words_tf_idf_pca.csv',show_col_types = FALSE)

bert_google_words <- read_csv('bert_google_words.csv',show_col_types = FALSE) %>%
                     rename(word='...1',
                            Fear='Contains fear',
                            Excitement='containes excitement')

bert_bing_words <- read_csv('bert_bing_words.csv',show_col_types = FALSE) %>%
                   rename(word='...1',
                          Fear='Contains fear',
                          Excitement='containes excitement')





# # bert words all in one dataframe

bert_words <- bert_bing_words %>%
              mutate(source='BING') %>%
              rbind(bert_google_words %>% mutate(source='GOOGLE')) %>%
              pivot_longer(cols=c(Fear,Excitement))


# pca variance

google_pca_variance <- read_csv("google_pca_variance.csv",show_col_types = FALSE)

google_pca_variance <- google_pca_variance %>%
                       mutate(source='GOOGLE',
                              n_pc=as.integer(rownames(google_pca_variance)))


bing_pca_variance <- read_csv("bing_pca_variance.csv",show_col_types = FALSE)

bing_pca_variance <- bing_pca_variance %>%
                     mutate(source='BING',
                            n_pc=as.integer(rownames(bing_pca_variance)))


# pca in one df

pca_variance <- google_pca_variance %>%
                rbind(bing_pca_variance)



# text 

bert_google_text <- read_csv("bert_text_google.csv",show_col_types = F) %>% 
                    rename(index="...1")%>%
                    dplyr::select(-text)



bert_bing_text <- read_csv("bert_text_bing.csv",show_col_types = F) %>% 
                  rename(index="...1")%>%
                  dplyr::select(-text)



bert_google_invormative_text <- read_csv("bert_text_google_informative.csv",show_col_types = F) %>%
                                rename(index="...1",
                                       NotInformative=Not.Informative) %>%
                                dplyr::select(-text)

bert_bing_invormative_text <- read_csv("bert_text_bing_inforamtive.csv",show_col_types = F) %>%
                                rename(index="...1",
                                       NotInformative=Not.Informative) %>%
                                dplyr::select(-text)


google_bert <- inner_join(bert_google_text,bert_google_invormative_text,by='index')
bing_bert <- inner_join(bert_bing_text,bert_bing_invormative_text,by='index')


google_df <- left_join(google_bert,google_articles,by='index') %>%
                   dplyr::select(index,url,output,everything()) %>%
                   dplyr::select(-output)


bing_df <- left_join(bing_bert,bing_articles,by='index') %>%
                 dplyr::select(index,url,output,everything())%>%
                 dplyr::select(-output)



# tf idf vs bert
google_tf_idf_bert <- google_df %>% dplyr::select(index,
                                                  url,
                                                  mean_tf,
                                                  mean_idf,
                                                  mean_tf_idf,
                                                  mean_bing,
                                                  mean_afinn,
                                                  mean_sentimentR,
                                                  Negative,
                                                  Neutral,
                                                  Positive) %>%
                          mutate(Emotion=Positive+Negative)



bing_tf_idf_bert <- bing_df %>% dplyr::select(index,
                                                  url,
                                                  mean_tf,
                                                  mean_idf,
                                                  mean_tf_idf,
                                                  mean_bing,
                                                  mean_afinn,
                                                  mean_sentimentR,
                                                  Negative,
                                                  Neutral,
                                                  Positive)  %>%
                                mutate(Emotion=Positive+Negative)


# tokens for bayesyan

# google
stop_words_snow <- data.frame(stopwords())%>%rename(word=stopwords..)

tokens_google <- google_articles %>%dplyr::select(index,output)%>%
  unnest_tokens(word,output) %>%
  mutate(word=stri_trans_general(str=word,id='latin-ascii'), # convert to straight form 
         word=stri_trans_general(word,'lower'), #convert to lower case
         word=sub("'.*$","",word),            # remove all apostrophes
         word=gsub("[[:punct:][:digit:]]", "", word),
         word=iconv(word,from='latin1',to='ascii')) %>% 
  anti_join(stop_words,by='word') %>% # remove stop words
  anti_join(stop_words_snow,by='word') %>% # remove stop words for not straight form of auxiliary verbs
  filter(word!='')            #remove all blank rows




tokens_google_nums <- tokens_google %>% count(word,index,sort=TRUE)

# bing

tokens_bing <- bing_articles %>%dplyr::select(index,output)%>%
  unnest_tokens(word,output) %>%
  mutate(word=stri_trans_general(str=word,id='latin-ascii'), # convert to straight form 
         word=stri_trans_general(word,'lower'), #convert to lower case
         word=sub("'.*$","",word),            # remove all apostrophes
         word=gsub("[[:punct:][:digit:]]", "", word),
         word=iconv(word,from='latin1',to='ascii')) %>% 
  anti_join(stop_words,by='word') %>% # remove stop words
  anti_join(stop_words_snow,by='word') %>% # remove stop words for not straight form of auxiliary verbs
  filter(word!='')            #remove all blank rows




tokens_bing_nums <- tokens_google %>% count(word,index,sort=TRUE)

# 

google_word_plus_article <- inner_join(bert_google_words,tokens_google_nums,by='word') %>%
                            group_by(index) %>%
                            summarise(Fear=mean(Fear),
                                      Excitement=mean(Excitement)) %>%
                            pivot_longer(cols=c(Fear,Excitement))

bing_word_plus_article <-inner_join(bert_bing_words,tokens_bing_nums,by='word')  %>%
                        group_by(index) %>%
                        summarise(Fear=mean(Fear),
                                  Excitement=mean(Excitement)) %>%
                        pivot_longer(cols=c(Fear,Excitement))







# fear_text_wise

google_text_fear <- read_csv("bert_text_google_fear.csv",show_col_types = F) %>%
                    rename(index='...1',
                           Fear=Contains.fear,
                           Excitement=containes.excitement)%>%
                    dplyr::select(-text) %>%
                    pivot_longer(cols=c(Fear,Excitement))


bing_text_fear <- read_csv("bert_text_bing_fear.csv",show_col_types = F) %>%
                  rename(index='...1',
                         Fear=Contains.fear,
                         Excitement=containes.excitement)%>%
                  dplyr::select(-text) %>%
                  pivot_longer(cols=c(Fear,Excitement))




# text and words

bert_fear_ex <- google_word_plus_article%>%mutate(type='word',source='GOOGLE')%>%
                rbind(google_text_fear%>%mutate(type='text',source='GOOGLE')) %>%
                rbind(bing_word_plus_article%>%mutate(type='word',source='BING'))%>%
                rbind(bing_text_fear%>%mutate(type='text',source='BING'))





# -----------------------------------------------------------------------------------------
#                                                                                         #
#                                   UI                                                    #
#                                                                                         #
###########################################################################################


ui <- dashboardPage(
  
  dashboardHeader(title=h2("Sentiment Analysis")),
  
  dashboardSidebar(),
  
  dashboardBody(
    
    infoBox(title=h3('AI news articles sentiment analysis',
                     style='font-size:40px; color:black;'),
            width = 12),
    
    box(width=6,
        color='ligth-blue',
        
          icon = icon('people-group'),
          h1(strong("Group members")),
          br(),
          h3(strong("  Rafayel Antonyan")),
          br(),
          h3(strong("  Hakob Hakobyan")),
          br(),
          h3(strong("  Alber Makaryan")),
          br()),
    
    
    
    useShinyjs(),
    
    
    # box1 starts
    
    # --------------------------------------
    
    
    fluidRow(
      box(
        title=h3("Bing & Afinn dictionaries: sentiment scores",
                 style='font-size:40px; color:black;',
                 align='center'),
        hr(style="border-top: 1px solid #000000;"),
        width = 12,
        
        
        
        box(title=h3('Google news articles',
                     style="font-size:30px; color:black;",
                     align='center'),
            width = 6,
            prettyRadioButtons(
              inputId='google_sent_lib',
              label='Dictionary',
              choices=c('Afinn'='afinn',
                        'Bing'='bing'),
              selected='afinn',
              inline=TRUE),
            
            sliderInput(
              inputId='google_n_max_sent',
              label='Top N',
              value=14,
              min=2,
              max=100),
            
            plotlyOutput('google_sent_bar'),
            hr(style="border-top: 1px solid #000000;"),
            dataTableOutput('google_afinn_table')),
        
        
        
        
        box(title=h3('Bing API news articles',
                     style="font-size:30px; color:black;",
                     align='center'),
            width = 6,
            prettyRadioButtons(
              inputId='bing_sent_lib',
              label='Dictionary',
              choices=c('Afinn'='afinn',
                        'Bing'='bing'),
              selected='afinn',
              inline=TRUE),
            
            sliderInput(
              inputId='bing_n_max_sent',
              label='Top N',
              value=14,
              min=2,
              max=100),
            
            plotlyOutput('bing_sent_bar'),
            hr(style="border-top: 1px solid #000000;"),
            dataTableOutput('bing_afinn_table')))),
    # box1 ends
    # --------------------------------------
    
    
    
    
    # --------------------------------------
    
    # bert result comparison according to word
    
    fluidRow(
      box(title=h3("BERT result comparison",
                   style='font-size:40px; color:black;',
                   align='center'),
          hr(style="border-top: 1px solid #000000;"),
          width = 12,
          
        box(title=h3('Fear',
                     style='font-size:30px; color:black;',
                     align='center'),
            width = 6,
            
            fluidRow(
              box(
                  checkboxGroupButtons(
                    inputId='fear_bing_google',
                    label='Source',
                    selected='BING',
                    choices=c("BING","GOOGLE"),
                    status='primary')),
                  
              box(  
                prettyRadioButtons(
                    inputId='fear_words_barmode',
                    label='Barmode',
                    selected='overlay',
                    choices=c('group','overlay','stack'),
                    inline = TRUE))),
            
            fluidRow(plotlyOutput('fear_words_hist'))),
        
        box(title=h3('Excitement',
                     style='font-size:30px; color:black;',
                     align='center'),
            width = 6,
            
            fluidRow(
              box(
                checkboxGroupButtons(
                  inputId='excitement_bing_google',
                  label='Source',
                  selected='BING',
                  choices=c("BING","GOOGLE"),
                  status='primary')),
              
              box(  
                prettyRadioButtons(
                  inputId='excitement_words_barmode',
                  label='Barmode',
                  selected='overlay',
                  choices=c('group','overlay','stack'),
                  inline = TRUE))),
            
            fluidRow(plotlyOutput('excitement_words_hist'))))),
    
    
    
    
    # articles wise analysis
    # --------------------------------------
    
    # box3 starts 
    
    fluidRow(
      box(title=h3('Sentiment classes and score by NRC dictionary',
                   style='font-size:40px; color:black',
                   align='center'),
          hr(style="border-top: 1px solid #000000;"),
          width = 12,
          
          
          # google
          
          box(title=h3('Google news articles',
                       style="font-size:30px; color:black;",
                       align='center'),
              width = 6,
              
              fluidRow(
                box(width = 6,
                    background = 'blue',
                    selectizeInput(
                      inputId='nrc_emotion_google',
                      label='NRC emotion',
                      selected='fear',
                      choices=google_articles%>%dplyr::select(starts_with("nrc_"))%>%
                                              rename_at(vars(starts_with("nrc_")),
                                                        ~ sub("nrc_*","",.)) %>%
                            colnames(),
                    multiple=TRUE,
                    
                    options = list(
                      plugins = list("remove_button")))),
                
                actionBttn(
                  inputId = 'google_clear_nrc',
                  label='Clear',
                  style='fill',
                  color='primary'),
                
                box(width = 3,
                    prettyRadioButtons(
                      inputId='nrc_google_barmode',
                      label='Barmode',
                      selected='overlay',
                      choices=c("group", "overlay", "stack"),
                      inline = TRUE))),
                
                fluidRow(plotlyOutput('nrc_plot_google')),
              
                hr(style="border-top: 1px solid #000000;"),
                
                dataTableOutput('nrc_google_data_table')),
          
          # bing
          
          box(title=h3('Bing API articles',
                       style="font-size:30px; color:black;",
                       align='center'),
              width = 6,
              
              fluidRow(
                box(width = 6,
                    background = 'blue',
                    selectizeInput(
                      inputId='nrc_emotion_bing',
                      label='NRC emotion',
                      selected='fear',
                      choices=google_articles%>%dplyr::select(starts_with("nrc_"))%>%
                        rename_at(vars(starts_with("nrc_")),
                                  ~ sub("nrc_*","",.)) %>%
                        colnames(),
                      multiple=TRUE,
                      
                      options = list(
                        plugins = list("remove_button")))),
                
                actionBttn(
                  inputId = 'bing_clear_nrc',
                  label='Clear',
                  style='fill',
                  color='primary'),
                
                box(width = 3,
                    prettyRadioButtons(
                      inputId='nrc_bing_barmode',
                      label='Barmode',
                      selected='overlay',
                      choices=c("group", "overlay", "stack"),
                      inline=TRUE))),
              
              fluidRow(plotlyOutput('nrc_plot_bing')),
              
              hr(style="border-top: 1px solid #000000;"),
              
              dataTableOutput('nrc_bing_data_table')))),
    
    # --------------------------------------
    
    
    # --------------------------------------
    # principal component analysis
    
    
    fluidRow(
      
      h3(strong('Principal Component Analysis'),
         style='font-size:50px; color:black;',
         align='center'),
      
      hr(style="border-top: 1px solid #000000;"),
    
      box(title=h3('Principal Component Analysis: Variance',
                   style='font-size:40px; color:black;',
                   align='center'),
          width = 12,
          
          fluidRow(
            
            box(width = 4,
                checkboxGroupButtons(
                  inputId='pca_bing_google',
                  label='Source',
                  selected = c('BING','GOOGLE'),
                  choices=c('BING',"GOOGLE"),
                  status = 'primary')),
            
            box(width = 4,  
                prettyRadioButtons(
                  inputId='pca_variance',
                  label=character(0),
                  choices = c('Standard Deviation'='StDev',
                              'Proportion of variance' = 'VarianceProportion',
                              'Cumulative variance' = 'CumulativeVariance'),
                  selected = 'StDev',
                  status = 'primary')),
            
            box(width = 3,
                sliderInput(
                  inputId='n_pc',
                  label='Principal Components',
                  value=10,
                  min=1,
                  max=40))),
          
          fluidRow(plotlyOutput('pca_variance_plot')),
          
          hr(style="border-top: 1px solid #000000;"),
          
          box(title=h2("GOOGLE PCA variance",
                       align='center'),
              width = 6,
              dataTableOutput('google_pca_variance_data_table')),
          
          box(title=h2("BING PCA variance",
                       align='center'),
              width = 6,
              dataTableOutput('bing_pca_variance_data_table')))),
    
    
    
    # -----------------------------------------------------------------
    
    
    # pca relative importances
    # -----------------------------------------------------------------
    
    
    fluidRow(
      box(title=h3('Principal Component Analysis: Relative importances of words',
                   style='font-size:40px; color:black;',
                   align='center'),
          hr(style="border-top: 1px solid #000000;"),
          
          width = 12,
          
          # goole
          box(title=h3("GOOGLE",
                       style='font-size:30px; color:black;',
                       align='center'),
              width = 6,
              
              fluidRow(
                 box(width = 4,
                   uiOutput('google_pca_importance')),
                  
                  box(width = 8,
                    sliderInput(
                    inputId='google_pca_importance_top_n',
                    label='Number of words',
                    value=10,
                    min=2,
                    max=100))),
              
              
              fluidRow(
                plotlyOutput('google_words_rel_importance_plot'))),
            
          
          # bing
          box(title=h3("BING",
                       style='font-size:30px; color:black;',
                       align='center'),
              width = 6,
              
              fluidRow(
                box(width = 4,
                    uiOutput('bing_pca_importance')),
                
                box(width = 8,
                    sliderInput(
                      inputId='bing_pca_importance_top_n',
                      label='Number of words',
                      value=10,
                      min=2,
                      max=100))),
              
              
              fluidRow(
                plotlyOutput('bing_words_rel_importance_plot'))))),
    
    
    # -----------------------------------------------------------------
    
    
    # pca relative importance comparison
    # -----------------------------------------------------------------
    
    # fluidRow(
    #   box(title=h3('Principal Component Analysis:Comparison of Relative importances of words',
    #                style='font-size:40px; color:black;',
    #                align='center'),
    #       hr(style="border-top: 1px solid #000000;"),
    #       
    #       width = 12,
    #       
    #       plotlyOutput('comarison_of_rel_importance'),
    #       dataTableOutput('d_table')))
    
    
    # -----------------------------------------------------------------
    
    
    # linear regresion
    # -----------------------------------------------------------------
    
    fluidRow(
      box(title=h3('Linear regression',
                   style='font-size:40px; color:black;',
                   align='center'),
          hr(style="border-top: 1px solid #000000;"),
          
          width = 12,
          
          # goole
          fluidRow(
            box(title=h3("GOOGLE",
                         style='font-size:30px; color:black;',
                         align='center'),
                width = 12,
                
                dataTableOutput('google_all_df'))),
          
          fluidRow(
            box(title=h3("BING",
                         style='font-size:30px; color:black;',
                         align='center'),
                width = 12,
                
                dataTableOutput('bing_all_df'))),
          
          
          fluidRow(
            box(title=h3("GOOGLE",
                         style='font-size:30px; color:black;',
                         align='center'),
                width = 12,
                
                dataTableOutput('google_tf_idf_bert'))),
          
          fluidRow(
            box(title=h3("BING",
                         style='font-size:30px; color:black;',
                         align='center'),
                width = 12,
                
                dataTableOutput('bing_tf_idf_bert'))),
          
          
          fluidRow(
            box(title='GOOGLE glm model summary: PCA',
                width = 6,
                selectizeInput(
                  inputId="google_bert_result_name",
                  label='Dependence',
                  selected='Positive',
                  choices=c('Positive','Negative','Neutral','Informative','NotInformative')),
                verbatimTextOutput('google_model_summary')),
          
          
            box(title='BING glm model summary: PCA',
                width = 6,
                selectizeInput(
                  inputId="bing_bert_result_name",
                  label='Dependence',
                  selected='Positive',
                  choices=c('Positive','Negative','Neutral','Informative','NotInformative')),
                verbatimTextOutput('bing_model_summary'))),
          
          fluidRow(
            box(title='GOOGLE glm model summary: tf_idf',
                width = 6,
                selectizeInput(
                  inputId="google_bert_result_name_tf_idf",
                  label='Dependence',
                  selected='Positive',
                  choices=c('Positive','Negative','Neutral','Informative','NotInformative')),
                verbatimTextOutput('google_model_summary_tf_idf')),
            
            
            
            box(title='BING glm model summary: tf_idf',
                width = 6,
                selectizeInput(
                  inputId="bing_bert_result_name_tf_idf",
                  label='Dependence',
                  selected='Positive',
                  choices=c('Positive','Negative','Neutral','Informative','NotInformative')),
                verbatimTextOutput('bing_model_summary_tf_idf'))))),
    
    # -----------------------------------------------------------------
    
    
    # bayesyan
    
    # -----------------------------------------------------------------
    
    
    fluidRow(
      
      box(title=h2(strong("Bayesian Prior: Probabilities based on article text"),align='center'),
          width = 12,
          
          
          
          box(width = 6,
              title=h3('GOOGLE'),
              
              fluidRow(
                prettyRadioButtons(
                  inputId='fear_or_ex_google_text',
                  label='Emotion',
                  select='Fear',
                  choices=c("Fear",'Excitement'),
                  status = 'primary')),
              
              fluidRow(plotlyOutput('google_word_plus_articles_hist_text'))),
          
          box(width = 6,
              title=h3('BING'),
              
              
              fluidRow(
                prettyRadioButtons(
                  inputId='fear_or_ex_bing_text',
                  label='Emotion',
                  select='Fear',
                  choices=c("Fear",'Excitement'),
                  status = 'primary')),
              
              fluidRow(plotlyOutput('bing_word_plus_articles_hist_text'))))),
    
    fluidRow(
      
      box(title=h2(strong("Bayesian likelihood: probabilities based of tokens(words)"),align='center'),
          width = 12,
          

          
          box(width = 6,
              title=h3('GOOGLE'),
              
          fluidRow(
            prettyRadioButtons(
                inputId='fear_or_ex_google',
                label='Emotion',
                select='Fear',
                choices=c("Fear",'Excitement'),
                status = 'primary')),
              
           fluidRow(plotlyOutput('google_word_plus_articles_hist'))),
          
          box(width = 6,
              title=h3('BING'),
              
              
              fluidRow(
                    prettyRadioButtons(
                      inputId='fear_or_ex_bing',
                      label='Emotion',
                      select='Fear',
                      choices=c("Fear",'Excitement'),
                      status = 'primary')),
              
              fluidRow(plotlyOutput('bing_word_plus_articles_hist'))))),
    
    
    
    # ----------------------------------------------------------------
    # comarison
    
    fluidRow(
      box(width = 12,
         
        box(width = 6, 
            title=h2("GOOGLE"),
          fluidRow(
            box(width = 6,
                prettyRadioButtons(
                  inputId='bayes_fear_google',
                  label='Emotion',
                  selected='Fear',
                  choices=c('Fear','Excitement'),
                  inline=TRUE)),
            box(width = 6,
                checkboxGroupButtons(
                  inputId='bayes_fear_type_google',
                  label='Type',
                  selected='word',
                  choices=c('word','text'),
                  status='primary'))),
          
          fluidRow(plotlyOutput('bayes_comparison_google_hist'))),
        
        box(width = 6, 
            title=h2("BING"),
            fluidRow(
              box(width = 6,
                  prettyRadioButtons(
                    inputId='bayes_fear_bing',
                    label='Emotion',
                    selected='Fear',
                    choices=c('Fear','Excitement'),
                    inline=TRUE)),
              box(width = 6,
                  checkboxGroupButtons(
                    inputId='bayes_fear_type_bing',
                    label='Type',
                    selected='word',
                    choices=c('word','text'),
                    status='primary'))),
            
            fluidRow(plotlyOutput('bayes_comparison_bing_hist')))
          
          )
    )
    
    
  # body ends
  )
  
)


# -----------------------------------------------------------------------------------------
#                                                                                         #
#                                   SERVER                                                #
#                                                                                         #
###########################################################################################






server <- function(input,output,session){
  
  # words sentiment afinn and bing
  # -----------------------------------------------------------------------------
  
  # google
  
  words_sentiment_df_google <- reactive({
    
    if (input$google_sent_lib=='afinn'){
      
      return(google_words_sentiment%>%
               slice_max(abs(total_sum_afinn),n=input$google_n_max_sent) %>%
               dplyr::select(Word=word,Sentiment=total_sum_afinn)%>%
               mutate(Word=reorder(Word,Sentiment)))
    }
    
    return(google_words_sentiment%>%
             slice_max(abs(total_sum_bing),n=input$google_n_max_sent) %>%
             dplyr::select(Word=word,Sentiment=total_sum_bing)%>%
             mutate(Word=reorder(Word,Sentiment)))
  })
  
  output$google_sent_bar <- renderPlotly({
    words_sentiment_df_google() %>% 
      plot_ly(x=~Sentiment,
              y=~Word) %>%
      layout(title=
               list(text=paste("Sentinent scores by ",toupper(input$google_sent_lib),"library",sep=' ')))
    
  })
  
  # data table
  output$google_afinn_table <- renderDataTable(words_sentiment_df_google())
  
  
  # bing
  
  words_sentiment_df_bing <- reactive({
    
    if (input$bing_sent_lib=='afinn'){
      
      return(bing_words_sentiment%>%
               slice_max(abs(total_sum_afinn),n=input$bing_n_max_sent) %>%
               dplyr::select(Word=word,Sentiment=total_sum_afinn)%>%
               mutate(Word=reorder(Word,Sentiment)))
    }
    
    return(bing_words_sentiment%>%
             slice_max(abs(total_sum_bing),n=input$bing_n_max_sent) %>%
             dplyr::select(Word=word,Sentiment=total_sum_bing)%>%
             mutate(Word=reorder(Word,Sentiment)))
  })
  
  output$bing_sent_bar <- renderPlotly({
    words_sentiment_df_bing() %>% 
      plot_ly(x=~Sentiment,
              y=~Word) %>%
      layout(title=
               list(text=paste("Sentinent scores by ",toupper(input$bing_sent_lib),"library",sep=' ')))
    
  })
  
  
  # data table
  
  
  output$bing_afinn_table <- renderDataTable(words_sentiment_df_bing())
  
  # -----------------------------------------------------------------------------
  
  
  # bert resutl comparison for words  
  # -----------------------------------------------------------------------------
  
  # fear
  
  
  fear_words_df <- reactive({
    
    return(bert_words %>%
              filter(name=='Fear' & source %in% input$fear_bing_google))
    
  })
  
  
  output$fear_words_hist <- renderPlotly({
    
    fear_words_df() %>% 
              plot_ly(x=~value,
                      alpha = 0.7,
                      type='histogram',
                      split=~source) %>%
              layout(barmode=input$fear_words_barmode,
                     xaxis=list(title='Probability'),
                     yaxis=list(title='Frequency'))
    
  })
  
  
  
  # excitement
  
  excitement_words_df <- reactive({
    
    return(bert_words %>%
             filter(name=='Excitement' & source %in% input$excitement_bing_google))
    
  })
  
  
  output$excitement_words_hist <- renderPlotly({
    
    excitement_words_df() %>% 
      plot_ly(x=~value,
              alpha = 0.7,
              type='histogram',
              split=~source) %>%
      layout(barmode=input$excitement_words_barmode,
             xaxis=list(title='Probability'),
             yaxis=list(title='Frequency'))
    
  })
  
  
  
  
  
  # -----------------------------------------------------------------------------
  
  



  
  
  
  # nrc sentiment per article
  # -----------------------------------------------------------------------------
  
  # google
  
  nrc_sentiment_google <- reactive({
    return(google_articles%>%dplyr::select(starts_with("nrc_")) %>%
             rename_at(vars(starts_with("nrc_")),
                       ~sub("nrc_*","",.))%>%
             pivot_longer(cols=everything())%>%
             filter(name %in% input$nrc_emotion_google))
             
  })
  
  output$nrc_plot_google <- renderPlotly({
    
    nrc_sentiment_google() %>% 
      plot_ly(alpha = 0.7,
              x=~value,
              split = ~ name,
              type='histogram')%>%
      layout(barmode=input$nrc_google_barmode,
             xaxis=list(title='Value'),
             yaxis=list(title='Frequency'))

  })
  
  observeEvent(input$google_clear_nrc,{
    
    reset('nrc_emotion_google') 
    
  })
  
  
  output$nrc_google_data_table <- renderDataTable(
    google_articles%>%dplyr::select(url,starts_with("nrc_")) %>%
      rename_at(vars(starts_with("nrc_")),
                ~sub("nrc_*","",.)),
    
    options=list(
      scrollX=TRUE,
      scrollY=TRUE)
  )
  
  
  # bing
  

  nrc_sentiment_bing <- reactive({
    return(bing_articles%>%dplyr::select(starts_with("nrc_")) %>%
             rename_at(vars(starts_with("nrc_")),
                       ~sub("nrc_*","",.))%>%
             pivot_longer(cols=everything())%>%
             filter(name %in% input$nrc_emotion_bing))
    
  })
  
  output$nrc_plot_bing <- renderPlotly({
    
    nrc_sentiment_bing() %>% 
      plot_ly(alpha = 0.7,
              x=~value,
              split = ~ name,
              type='histogram')%>%
      layout(barmode=input$nrc_bing_barmode,
             xaxis=list(title='Value'),
             yaxis=list(title='Frequency'))
    
  })
  
  observeEvent(input$bing_clear_nrc,{
    
    reset('nrc_emotion_bing') 
    
  })
  
  output$nrc_bing_data_table <- renderDataTable({
     datatable(bing_articles%>%dplyr::select(url,starts_with("nrc_")) %>%
        rename_at(vars(starts_with("nrc_")),
                  ~sub("nrc_*","",.)),
        

      options=list(
        scrollX=TRUE,
        scrollY=TRUE)
     )
  })
  
  
  
  #  principal component analysis
  # -----------------------------------------------------------------------------
  
  
  pca_df <- reactive({
    
    return(pca_variance%>%
             filter(source %in% input$pca_bing_google &
                    n_pc <= input$n_pc)%>%
             mutate(PrincipalComponent=reorder(PrincipalComponent,n_pc))) 
  })
  
  output$pca_variance_plot <- renderPlotly({
    
    pca_df() %>% plot_ly(x=~PrincipalComponent,
                         y=~get(input$pca_variance),
                         split=~source,
                         type='scatter',
                         mode='lines',
                         marker=list(
                            size=12,
                            line=list(
                              color='rgb(231, 99, 250)',
                              width=2))) %>%
                layout(xaxis=list(title='Principal Component'),
                       yaxis=list(title=input$pca_variance))
    
  })
  
  # data tables
  
  output$google_pca_variance_data_table <- renderDataTable({
    datatable(pca_df()%>%
                filter(source=='GOOGLE')%>%
                dplyr::select(-c(source,n_pc)),
              options=list(scrollX=T,
                           scrollY=T),
              rownames=F)
  })
  
  output$bing_pca_variance_data_table <- renderDataTable({
    datatable(pca_df()%>%
                filter(source=='BING')%>%
                dplyr::select(-c(source,n_pc)),
              
              options=list(scrollX=T,
                           scrollY=T),
              rownames = F)
  })
  
  # -----------------------------------------------------------------
  
  
  
  
  # pca relative importance
  
  # -----------------------------------------------------------------
  
  # google
  
  
  output$google_pca_importance <- renderUI({
    selectizeInput(
      inputId='google_pca_importance',
      label='Principal Component',
      selected='PC1',
      choices=pca_df()%>%dplyr::select(PrincipalComponent)%>%distinct()%>%pull())
  })
  
  google_pca_words_df <- reactive({
    google_words_tf_idf %>% dplyr::select(word,starts_with('PC'))%>%
                        pivot_longer(cols=-word,names_to = 'PC') %>%
                        filter(PC == input$google_pca_importance) %>%
                        arrange(abs(value)) %>%
                        slice_head(n=input$google_pca_importance_top_n) %>%
                        mutate(word=reorder(word,value))
  })
  
  
  
  output$google_words_rel_importance_plot <- renderPlotly({

    google_pca_words_df() %>% plot_ly(x=~word,
                                      y=~value,
                                      color=~word)%>%
                            layout(title=list(text=input$google_pca_importance),
                                   yaxis=list(title='Relateve Frequency'),
                                   xaxis=list(title=list(text='Word',standoff=10L)))
  })
  
  
  # bing
  
  
  output$bing_pca_importance <- renderUI({
    selectizeInput(
      inputId='bing_pca_importance',
      label='Principal Component',
      selected='PC1',
      choices=pca_df()%>%dplyr::select(PrincipalComponent)%>%distinct()%>%pull())
  })
  
  bing_pca_words_df <- reactive({
    bing_words_tf_idf %>% dplyr::select(word,starts_with('PC'))%>%
      pivot_longer(cols=-word,names_to = 'PC') %>%
      filter(PC == input$bing_pca_importance) %>%
      arrange(abs(value)) %>%
      slice_head(n=input$bing_pca_importance_top_n) %>%
      mutate(word=reorder(word,value))
  })
  
  
  
  output$bing_words_rel_importance_plot <- renderPlotly({
    
    bing_pca_words_df() %>% plot_ly(x=~word,
                                      y=~value,
                                      color=~word)%>%
      layout(title=list(text=input$bing_pca_importance),
             yaxis=list(title='Relateve Frequency'),
             xaxis=list(title=list(text='Word',standoff=10L)))
  })
  
  # -----------------------------------------------------------------
  
  
  
  # pca relative importance comparison
  
  # -----------------------------------------------------------------
  
  
  # linear regression
  # -----------------------------------------------------------------
  
  output$google_all_df <- renderDataTable(google_df,
                                          options=list(scrollX=T,
                                                       scrollY=T))
  
  output$bing_all_df <- renderDataTable(bing_df,
                                        options=list(scrollX=T,
                                                     scrollY=T))
  
  # tf idf vs bert
  
  output$google_tf_idf_bert <- renderDataTable(google_tf_idf_bert,
                                          options=list(scrollX=T,
                                                       scrollY=T))
  
  output$bing_tf_idf_bert <- renderDataTable(bing_tf_idf_bert,
                                        options=list(scrollX=T,
                                                     scrollY=T))
  
  
  # linear regression
  
  # google
  
  google_pca_negative <- reactive({
    return(google_df %>% dplyr::select(input$google_bert_result_name,starts_with("PC")))
  })
  
  
  google_model <- reactive({
    return(glm(get(input$google_bert_result_name) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC6 + PC7 + PC8 + PC9 + PC10,
                                   data=google_pca_negative()))
    
    })
  
  output$google_model_summary <- renderPrint({summary(google_model())})
  
  
  # bing
  bing_pca_negative <- reactive({
    return(bing_df %>% dplyr::select(input$bing_bert_result_name,starts_with("PC")))
  })
  
  
  bing_model <- reactive({
    return(glm(get(input$bing_bert_result_name) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC6 + PC7 + PC8 + PC9 + PC10,
               data=bing_pca_negative()))
    
  })
  
  output$bing_model_summary <- renderPrint({summary(bing_model())})
  
  
  
  
  
  # tf_idf_afinn
  # google
  
  google_tf_idf_df<- reactive({
    return(google_df %>% dplyr::select(input$google_bert_result_name_tf_idf,mean_tf,mean_idf,mean_tf_idf,
                                       mean_afinn,mean_bing,mean_sentimentR))
  })
  
  google_model_tf_idf <- reactive({
    return(glm(get(input$google_bert_result_name_tf_idf) ~ mean_tf + mean_idf + mean_tf_idf + mean_afinn + mean_bing + mean_sentimentR,
               data=google_tf_idf_df()))
    
  })
  
  output$google_model_summary_tf_idf <- renderPrint({summary(google_model_tf_idf())})


  
  # bing
  bing_tf_idf_df<- reactive({
    return(bing_df %>% dplyr::select(input$bing_bert_result_name_tf_idf,mean_tf,mean_idf,mean_tf_idf,
                                       mean_afinn,mean_bing,mean_sentimentR))
  })
  
  bing_model_tf_idf <- reactive({
    return(glm(get(input$bing_bert_result_name_tf_idf) ~ mean_tf + mean_idf + mean_tf_idf + mean_afinn + mean_bing + mean_sentimentR,
               data=bing_tf_idf_df()))
    
  })
  
  output$bing_model_summary_tf_idf <- renderPrint({summary(bing_model_tf_idf())})
  
  # 
  # ------------------------------------------------------------
  # beayes
  
  # prior 
  # google
  
  google_word_plus_article_df_text <- reactive({
    return(google_text_fear %>%filter(name==input$fear_or_ex_google_text))
  })
  
  output$google_word_plus_articles_hist_text <- renderPlotly({
    
    google_word_plus_article_df_text() %>% plot_ly(x=~value,
                                              type='histogram')
  })
  
  # bing
  bing_word_plus_article_df_text <- reactive({
    return(bing_text_fear %>%filter(name==input$fear_or_ex_bing_text))
  })
  
  output$bing_word_plus_articles_hist_text <- renderPlotly({
    
    bing_word_plus_article_df_text() %>% plot_ly(x=~value,
                                                   type='histogram')
  })
  
  
  # google
  
  google_word_plus_article_df <- reactive({
    return(google_word_plus_article %>%filter(name==input$fear_or_ex_google))
  })
  
  output$google_word_plus_articles_hist <- renderPlotly({
    
    google_word_plus_article_df() %>% plot_ly(x=~value,
                                              type='histogram')
  })
  
  # bing
  bing_word_plus_article_df <- reactive({
    return(bing_word_plus_article %>%filter(name==input$fear_or_ex_bing))
  })
  
  output$bing_word_plus_articles_hist <- renderPlotly({
    
    bing_word_plus_article_df() %>% plot_ly(x=~value,
                                              type='histogram')
  })
  
  
  
  # ---------------------------------------------------------
  # bayes comparison
  
  # google
  bayes_com_google_df <- reactive({
    return(bert_fear_ex%>%filter(name==input$bayes_fear_google &
                                 source=='GOOGLE' &
                                 type %in% input$bayes_fear_type_google))
  })
  
  
  output$bayes_comparison_google_hist <- renderPlotly({
    bayes_com_google_df() %>% plot_ly(x=~value,
                                      split=~type,
                                      alpha=0.7,
                                      type='histogram') %>%
                              layout(barmode='overlay')
  })
  
  
  # bing
  bayes_com_bing_df <- reactive({
    return(bert_fear_ex%>%filter(name==input$bayes_fear_bing &
                                   source=='BING' &
                                   type %in% input$bayes_fear_type_bing))
  })
  
  
  output$bayes_comparison_bing_hist <- renderPlotly({
    bayes_com_bing_df() %>% plot_ly(x=~value,
                                      split=~type,
                                      alpha=0.7,
                                      type='histogram') %>%
                                  layout(barmode='overlay')
  })
 
}


shinyApp(ui,server)





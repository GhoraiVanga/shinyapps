library(shiny)
library(tidyverse)
library(tm)
library(wordcloud)
library(readr)

server = function(input, output)
{
  value <- reactive(renderText(input$var))
  
  data <- read_csv("zomato.csv")
  str(data)
  output$heist <- renderPlot({
    value <- input$var  
    output$ab <- renderText(input$var)
   # data$rest_type <- as.factor(data$rest_type)
    data$rest_type <- gsub('\\s+', '', data$rest_type)
    data$cuisines<- gsub('\\s+', '',data$cuisines)
    data<- na.omit(data)
    WDLC<- data %>% select(rest_type,cuisines) %>% filter(rest_type==value) 
    ## read data
    text<- WDLC$cuisines
    #text <- readLines("annotation.txt")
    text <- paste(text,collapse = " ")
    Corpus <- Corpus(VectorSource(text))
    ########clean up data
    Corpus <- tm_map(Corpus,tolower)
    Corpus <- tm_map(Corpus,removePunctuation)
    Corpus <- tm_map(Corpus,removeNumbers)
    cleanset <-tm_map(Corpus,removeWords,stopwords("english"))
    cleanset <- tm_map(Corpus,stripWhitespace)
    cleanset <- tm_map(cleanset,PlainTextDocument)
    tdm <- TermDocumentMatrix(Corpus)
    m <-as.matrix(tdm)
    as <- rowSums(m)
    wordFreq <- sort(as, decreasing = TRUE)
    wordcloud(words = names(wordFreq),freq = wordFreq,min.freq = 150,max.words =100,random.order = F,colors = rainbow(20))
    
    
  })
}


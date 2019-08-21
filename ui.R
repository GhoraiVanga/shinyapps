library(shiny)
library(rsconnect)
library(readr)
library(tidyverse)
data <- read_csv("zomato.csv")
str(data)
data$rest_type <- gsub('\\s+', '', data$rest_type)
data$cuisines<- gsub('\\s+', '',data$cuisines)
data <-na.omit(data)
data1 <- data %>% select(rest_type) %>% group_by(rest_type) %>% summarize(count=n()) %>% arrange(desc(count))
data1 <- head(data1,12)
shinyUI(fluidPage(
  titlePanel(sidebarLayout(sidebarPanel(
    selectInput("var","select the variable ",choices = data1$rest_type)),
    mainPanel(plotOutput("heist"),textOutput("ab")))
)))
getwd()

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ngram)
library(tm)
library(SnowballC)
library(stringi)
library(tidyr)
library(stringr)
library(tidyverse)



#let's read the files
two_gram <- readRDS("two_partition.rds")
three_gram <- readRDS("three_partition.rds")
four_gram <- readRDS("four_partition.rds")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#First, let's instruct R to actually match the words
#It applies the 'backoff model' concept



bigram <- function(minicorpus){
  ncount <- length(minicorpus)
  nextword <- as.data.frame(two_gram[two_gram$word1==minicorpus[ncount],][1:3,]$word2) 
  
  ifelse(nrow(nextword)==0, "no match", return(nextword))
  
}


trigram <- function(minicorpus){
  ncount <- length(minicorpus)
  nextword <- as.data.frame(three_gram[three_gram$word1==minicorpus[ncount-1] &
                                        three_gram$word2==minicorpus[ncount],] [1:3,]$word3)

  
  ifelse(nrow(nextword)==0 , bigram(minicorpus), return(nextword))
  
  
}

quadgram <- function(minicorpus){
  ncount <- length(minicorpus)
  nextword <- as.data.frame(four_gram[four_gram$word1==minicorpus[ncount-2] &
                                       four_gram$word2 ==minicorpus[ncount-1] &
                                       four_gram$word3 ==minicorpus[ncount],] [1:3,]$word4)

  
  ifelse(nrow(nextword)==0, trigram(minicorpus), return(nextword))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#Then we instruct R to clean and format the incoming corpus
#R also needs to know call the ngram model to match the words and return the last word



Clean_minicorpus <- function(incoming){
  
  
  
  # turn incoming text into a Corpus first
  minicorpus <- Corpus(VectorSource(incoming), readerControl = list(reader=readPlain, language = "en_US"))
  # clean up this little corpus
  minicorpus <- tm_map(minicorpus, removePunctuation)
  minicorpus <- tm_map(minicorpus, removeNumbers)
  minicorpus <- tm_map(minicorpus, content_transformer(tolower))
  #minicorpus <- tm_map(minicorpus, removeWords, stopwords("english"))
  minicorpus <- tm_map(minicorpus, stripWhitespace)
  minicorpus <- iconv(minicorpus, "latin1", "ASCII", sub="")
  
  minicorpus <- trimws(minicorpus, "l")
  
  #I just want the first column
  minicorpus <- minicorpus[[1]]
  
  #count number of words
  wordcount <- function(str) {
    sapply(gregexpr("\\b\\W+\\b", str,  perl = TRUE), function(x) sum(x>0) ) +1
  }
  ncount <- wordcount(minicorpus[[1]])
  
  #print(ncount)
  
  #chop the incoming corpus into individual words
  minicorpus <- unlist(str_split(minicorpus, boundary("word")))
  
  #print(minicorpus)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  #Call the right ngram
  
  ngrams <- ifelse(ncount ==1, bigram(minicorpus), 
                  ifelse(ncount ==2, trigram(minicorpus), quadgram(minicorpus)))
  
  #print(ngram)
  
  return(ngrams)
  
  
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("lumen"),
  
  # Application title
  titlePanel("Lyrics Predictor"),
  
  # Text box to input lyrics 
  sidebarLayout(
    sidebarPanel(
      textInput("incomingcorpus", "Type few words of your lyrics", value = "Oh baby you!"), width = 4
    ),
    
    # Show the three choices
    mainPanel(
      tableOutput("predictedWord")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  prediction <- reactive({
   
    ifelse(input$incomingcorpus=="" , "Try it! Type some Lyrics =D",
           Clean_minicorpus(input$incomingcorpus))
  })
  
  output$predictedWord <- renderTable({
    prediction()}, bordered = TRUE, caption = "Your top choices of next word are...", caption.placement = "top", colnames=FALSE, width="300"
    
  )
  
  
  
}







# Run the application 
shinyApp(ui = ui, server = server)

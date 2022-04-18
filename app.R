library(shiny)
library(ggplot2)
library(stopwords)
library(tidytext)
library(quanteda.textplots)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}


# task6: add in shinythemes function

ui <- fluidPage(theme =shinythemes::themeSelector(),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    # task2: add in the inputs in the sidebarPanel
    sidebarPanel(

      selectInput("selection", "Choose a book:",
                  choices = books),
      checkboxInput(inputId= "Stop Word", label = "Stop Words:", value = TRUE),
      actionButton("update","Change"),
      hr(),
      h3("Word cloud settings"),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 10, max = 200, value = 100),
      sliderInput("LargeWords",
                  "Size of Largest Words:",
                  min = 1, max = 8, value = 4),
      sliderInput("Small",
                  "Size of Largest Words:",
                  min = .1, max = 4, value = .5),
      hr(),
      h3("Word Count Settings"),
      sliderInput("MinimumWordCounts",
                  "Minimum Words for Counts Charts:",
                  min = 10, max = 100, value = 25),
      sliderInput("Font",
                  "Word Size:",
                  min = 8, max = 30, value = 14),
      
      
    ),
    # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    # task3: add in the outputs in the sidebarPanel
      mainPanel(
        tabsetPanel(
          tabPanel("Word Cloud",
                   plotOutput("cloud")),
          tabPanel("Word Counts",
                   plotOutput("freq")))
      
      )
    )
)
    
  
  # task6: and modify your figure heights

# task5: add in reactivity for getFreq function based on inputs
server <- function(input, output){
  
  freq <- eventReactive(input$update,{
      withProgress({
      setProgress(message = "Preprocessing Corpus...")
      getFreq(input$selection,input$stopwords)
      })
  })
  output$cloud <-renderPlot({
  v <- freq()
  pal <- brewer.pal(8,"Dark2")
  
  v %>% 
    with(
      wordcloud(
        word, 
        n, 
        scale = c(input$Largewords,input$Small),
        random.order = FALSE,
        max.words = input$max, 
        colors=pal,
        max.words = input$max, 
        colors=pal,
        height = "1000px"))
  
})
  output$freq <- renderPlot({
  v <- freq()
  pal <- brewer.pal(8,"Dark2")
  
  v %>% 
    filter(n> input$MinimumWordCounts)%>%
    ggplot(aes(X = reorder(word,n), y=n, height = "600px"))+
    geom_col()+
    coord_flip()+
    theme(text =element_text(size=input$Font))+
    labs(x="xaxis", y= "yaxis", title = "title")
  })




}

shinyApp(ui = ui, server = server)


library(shiny)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   biTable <- readRDS("smallBiTable.rds")
   triTable <- readRDS("smallTriTable.rds")
   quadTable <- readRDS("smallQuadTable.rds")
   
   predFromFreq1 <- function(w1, table){
      input <- paste0("^",tolower(w1)," ")
      values <- table[ngram %like% input]
      if(dim(values)[1] != 0){
         values <- values[order(values$count, decreasing = TRUE)]
         pred <- as.character(values[1,1])
         pred <- tail(strsplit(pred,split=" ")[[1]],1)
         pred
      }else{
         # unknown word
         out <- "Unknown"
      }
   }
   
   predFromFreq2 <- function(w1, w2, table){
      input <- paste0("^",tolower(w1)," ", tolower(w2), " ")
      values <- table[ngram %like% input]
      if(dim(values)[1] != 0){
         values <- values[order(values$count, decreasing = TRUE)]
         pred <- as.character(values[1,1])
         pred <- tail(strsplit(pred,split=" ")[[1]],1)
         pred
      }else{
         # unknown word
         out <- "Unknown"
      }
   }
   
   predFromFreq3 <- function(w1, w2, w3, table){
      input <- paste0("^",tolower(w1)," ", tolower(w2), " ", tolower(w2), " ")
      values <- table[ngram %like% input]
      if(dim(values)[1] != 0){
         values <- values[order(values$count, decreasing = TRUE)]
         pred <- as.character(values[1,1])
         pred <- tail(strsplit(pred,split=" ")[[1]],1)
         pred
      }else{
         # unknown word
         out <- "Unknown"
      }
   }
   
   predictNext <- function(input, biTable, triTable, quadTable){
      splitIn <- tstrsplit(input," ")
      if(length(splitIn)>2){
         check <- tail(unlist(splitIn),3)
         out <- predFromFreq3(check[1],check[2],check[3],quadTable)
         if(out=="Unknown"){
            out <- predFromFreq2(check[2],check[3],triTable)
         }
         if(out=="Unknown"){
            out <- predFromFreq1(check[3],biTable)
         }
      }else if (length(splitIn)==2){
         check <- unlist(splitIn)
         out <- predFromFreq2(check[1],check[2],triTable)
         if(out=="Unknown"){
            out <- predFromFreq1(check[2],biTable)
         }
      }else if (length(splitIn)==1){
         out <- predFromFreq1(input,biTable)
      }
      out
   }
   inputString <- eventReactive(input$run,{
      input$inText
   })
   output$result <- renderText({
      predictNext(inputString(),biTable, triTable, quadTable)
   })
})

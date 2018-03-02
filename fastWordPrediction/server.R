
library(shiny)

# Define server logic
shinyServer(function(input, output, session) {
   # Input reference tables with two variables: V1 = the input word, pred = predicted word
   # This reduces algorithm to a lookup table, fast and small
   biTable <- readRDS("biRefSmall.rds")
   triTable <- readRDS("triRefSmall.rds")
   quadTable <- readRDS("quadRefSmall.rds")

   # Given a string of suitable length and appropriate table, predict a word
   predFromRef <- function(w1, table){
      pred <- table[which(table$V1 == tolower(w1)),2]
      if(identical(pred, character(0))){
         pred <- "Unknown"
      }
      pred
   }
   
   # Employ backoff method to seach tables of length 3, 2 or 1
   predictRefNext <- function(input, biTable, triTable, quadTable){
      splitIn <- unlist(strsplit(input," "))
      if(length(splitIn)>2){
         check <- tail(splitIn,3)
         out <- predFromRef(paste(check[1],check[2],check[3], collapse = " "),quadTable)
         if(out=="Unknown"){
            out <- predFromRef(paste(check[2],check[3], collapse = " "),quadTable)
         }
         if(out=="Unknown"){
            out <- predFromRef(check[3],biTable)
         }
      }else if (length(splitIn)==2){
         out <- predFromRef(paste(splitIn[1],splitIn[2], collapse = " "),triTable)
         if(out=="Unknown"){
            out <- predFromRef(splitIn[2],biTable)
         }
      }else if (length(splitIn)==1){
         out <- predFromRef(input,biTable)
      }
      out
   }
   inputString <- eventReactive(input$run,{
      input$inText
   })
   output$result <- renderText({
      predictRefNext(inputString(),biTable, triTable, quadTable)
   })
})

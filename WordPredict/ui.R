#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
   
   # Application title
   titlePanel("Word Prediction"),
   mainPanel(
      h5("This shiny app predicts your next word."),
      textInput("inText","Type a word or sentence here:"," "),
      actionButton("run","Click this button to predict the next word!"),
      h3(textOutput("result"))
   )
))


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("journal"),

  
  # Application title
  titlePanel("Write your story and we help you with some words"),

  sidebarLayout(
    sidebarPanel(   
      p("Possibilities:"),
      textOutput(outputId = "OutTxt1")
    ),

    # Shows the suggested words when typing in the text input 
    mainPanel(      
        tags$textarea(id="InTxtArea", rows=10, cols=60, "Your telling story begins here ..."),
        tags$style(type="text/css", "#InTxtArea {padding:10px;margin:30px;}")
    )
  )
))

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    navbarPage("Word prediction using n-gram models", theme = shinytheme("superhero"),
               tabPanel("Predictions",
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Enter almost three words to predict next"),
                                helpText("Please, wait until three blank predictions appears on the right panel."),
                                textInput("source", "Enter first words...",value = "")
                                
                            ),
                            mainPanel(
                                h2("Top 3 predicted words"),
                                verbatimTextOutput("word1"),
                                verbatimTextOutput("word2"),
                                verbatimTextOutput("word3"),
                                
                            )
                        )
               ),
               tabPanel("About",
                        h2("About"),
                        h3("Author"),
                        p("Francisco Javier Raya Márquez"),
                        h3("Date"),
                        p(date()),
                        h3("How to use"),
                        p("Enter almost three words in the input box. Top 3 most likely predictions appears in boxes."),
                        p("IMPORTANT: First time you enter to the app, wait until blank predictions appears.")
               )
    ),

))

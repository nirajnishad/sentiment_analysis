library(ggvis)
library(shiny)
library(shinythemes)
langs <- list("All"="all", "English"="en", "French"="fr" , "German"="ge" , "Italian"="it", "Spanish"="sp")
n_tweets <- 100
fluidPage(theme = shinytheme("united"),
          # Application title
          
          titlePanel(title=div(img(src = "sas_logo_4c.jpg", height = 100, width = 400)), windowTitle = "Sentiment Analysis"),
          sidebarLayout(
            # Sidebar with a slider and selection inputs
            sidebarPanel(
              textInput("source1", "Search on Social Media:", value="#india"),
              
              conditionalPanel(
                condition = "input.show_source2 == true",
                textInput("source2", "Compare with:" , value="#hillaryclinton")
              ),
              checkboxInput("show_source2", "Compare"),
              actionButton("plot_feel", "Plot Sentiments"),
              hr(),
              
              numericInput("n_tweets",
                           "Tweets To Fetch:", 1500, min = 100),
            
              selectInput("lang",
                          "Language:", langs)
              
            ),
            
            
          
            mainPanel(
              verbatimTextOutput("twitter_view"),
              h4("Cloud"),
              plotOutput("wordcloud",width = "100%"),
              
              h4("Sentiment Chart"),
              helpText("You can click on each dot to read the social media post."),
              ggvisOutput("plot1"),
              
              h4("Trends"),
              plotOutput("trends"),
              
              h4("Tweets"),
              helpText("Retweets are filtered out from the table below."),
              tabsetPanel(
                tabPanel(
                         dataTableOutput("Table"))
            )
          )
)
)

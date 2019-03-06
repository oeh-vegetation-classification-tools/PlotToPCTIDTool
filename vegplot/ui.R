#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# After setting up the shinyapps token (via rsconnect::setAccountInfo),
# the latest version of the app can be deployed via
#       > library(rsconnect)
#       > deployApp()

library(shiny)
library(DT)

# Define UI for application 
shinyUI(fluidPage(
    # Application title
    headerPanel("NSW vegetation plot allocation tool (prototype v1)"),
    sidebarLayout(
      # the control panel
      sidebarPanel(
        # Input floristic file - can modify down the track to choose/process different data types, header, species match ups etc.
        fileInput('file1', 'Input File: comma separated .csv, first column site names, remaining columns floristic data with PATN label column names',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        actionButton("goMatch", "Analyse data"),
        tags$hr(),
        # number of matches to give back to the user
        sliderInput("topn",
                    "Number of matches to report",
                    min = 1,
                    max = 10,
                    value = 5,
                    step = 1),
        tags$hr(),
        # button to download the example data
        downloadButton("example", "Download example data")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Home",
                   fluidPage(
                     tags$img(src="banner.jpg"),
                     tags$hr(),
                     textOutput("num_sites"),
                     htmlOutput("num_species"),
                     htmlOutput("cores"),
                     htmlOutput("info_text")
                   )),
          tabPanel("Ordination plot",
                   fluidPage(
                     plotOutput("ord_site_plt", width = "600px", height = "600px")
                   )),
          tabPanel("Information",
                   fluidPage(
                     h5("Note that this is not the official NSW Office of Environment and Heritage version - the official version will eventually be hosted under a different domain, but this version will continue to be useable and updated with functionality")
                   ))
        )
      )
    ),
  tags$hr(),
  fluidRow(
    tabsetPanel(
      tabPanel("Char. spp. matches",
               fluidRow(
                 column(12, DT::dataTableOutput("char_table"))
               )),
      tabPanel("Centroid matches",
               fluidRow(
                 column(12, DT::dataTableOutput("cent_table"))
               )),
      tabPanel("Combined matches",
               fluidRow(
                 column(12, DT::dataTableOutput("combined_table"))
               )),
      tabPanel("Download matches",
               fluidRow(
                 # download all of the match data
                 downloadButton("download_char_matches", "Download char. spp. matches"),
                 tags$hr(),
                 downloadButton("download_cent_matches", "Download centroid matches")
               ))
    )
  )
))
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
library(shinydashboard)
library(googleway)
library(shinyjs)


# Define UI for application 
ui<-  htmlTemplate("main.html",
                   
              
                    mainView= mainPanel(
                       tabsetPanel(
                         
                         tabPanel("Information",
                                  fluidPage(
                                    
                                    tags$img(src="EasternNSWStudyRegionMap_wIBRA50.jpg",alt="Eastern NSW Study Region Map with IBRA",style="float:left"),
                                    htmlOutput("info_main_text"),
                                    tags$hr(),
                                    fluidRow(
                                      
                                      # button to launch the userguide
                                     actionButton(inputId='lnkUserGuide', label="User Guide", 
                                                    icon = icon("fas fa-external-link-alt"), 
                                                    onclick ="window.open('http://google.com', '_blank')"),
                                      
                                      
                                      # button to view paper methodology page (pdf)
                                     actionButton(inputId='linkPaperMethodology', label="View to paper on methodology", 
                                                  icon = icon("fas fa-external-link-alt"), 
                                                  onclick ="window.open('http://google.com', '_blank')"),
                                      
                                      # button to go to VIS Flora Survey public site
                                     actionButton(inputId='linkVISFloraSurveyPublic', label="Visit BioNet Flora Survey", 
                                                  icon = icon("fas fa-external-link-alt"), 
                                                  onclick ="window.open('https://www.environment.nsw.gov.au/atlaspublicapp/UI_Modules/YETI_/FloraSearch.aspx', '_blank')"),
                                      
                                      # button to go to Veg Classfication public site
                                     actionButton(inputId='linkVegClassfnPublic', label="Visit BioNet Vegetation Classification", 
                                                  icon = icon("fas fa-external-link-alt"), 
                                                  onclick ="window.open('https://www.environment.nsw.gov.au/NSWVCA20PRapp/LoginPR.aspx', '_blank')"),
                                      
                                      # button to download the example data
                                      downloadButton("linkDownloadSampleData", "Download Sample Data")
                                    )
                                  )),
                         
                         
                         
                         
                         tabPanel("Input",
                                  
                                 verticalLayout(
                                   
                                   sidebarPanel(
                                      # Input floristic file - can modify down the track to choose/process different data types, header, species match ups etc.
                                      fileInput('file1', label='Input File: comma separated .csv, first column site names, remaining columns floristic data with PATN label column names',
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv'),buttonLabel="Browse..."),
                                      actionButton("goMatch", "Analyse data")
                                        ) ,
                             
                                      htmlOutput("uploadInformation", style="padding:20px"),
                                      htmlOutput("dataChecks", style="padding:20px"),
                                      
                                      shinyjs::useShinyjs(),
                                      # button to download the data check report
                                      tags$div(downloadButton("linkDownloadDataCheckReport", "Download data check report"),style="padding:20px"),
                                   
                                      tags$hr(),
                                      htmlOutput("cores", style="padding:20px")
                                
                                  )),
                         
                         tabPanel("Analysis output",
                                 
                                  fluidRow(
                                    
                                    sidebarPanel(
                                    htmlOutput("info_analysis_text"),
                                    tags$hr(),
                                    
                                    
                                    # number of matches to give back to the user
                                    sliderInput("topn",
                                                "Number of matches to report",
                                                min = 1,
                                                max = 10,
                                                value = 5,
                                                step = 1),
                                    tags$hr()
                                    ),
                                    
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
                                  
                                  ),
                         
                         tabPanel("Ordination plot",
                                  fluidPage(
                                    plotOutput("ord_site_plt", width = "600px", height = "600px")
                                  )),
                        
                         tabPanel("Map view",
                                        box(google_mapOutput(outputId = "map", width = "100%"), width = "100%", height="100%")
                                  )
                       )
                     , width = "auto")
             
                   
                   )

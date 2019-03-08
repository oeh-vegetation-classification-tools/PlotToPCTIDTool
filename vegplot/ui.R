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
                                                  step = 1)
                                      ),
                                      
                                      
                                      
                                      ## conditionalPanel() functions for selected tab
                                      conditionalPanel(condition="input.tabselected==1", HTML("<table style='border:none;'>
                                                                                                              <tr>
                                                                                                              <td style='vertical-align:top;width:50%'>
                                                                                                              <p style='text-align:justify;padding:5px;'>
                                                                                                              This analysis uses cover-abundance scores to determine how floristically related each new plot is to existing PCTs.
                                                                                                              
                                                                                                              Each PCT is defined by a specific group of existing plots; the centroid of each group is a collection of features that defines the floristics of the PCT.
                                                                                                              
                                                                                                              The analysis calculates the ecological ‘distance’ between the PCT group centroids and the new plots.
                                                                                                              
                                                                                                              The table presents the PCTs to which the new plot(s) are most closely related. The smaller the number listed in the ‘Match’ columns, the stronger the floristic relationship.
                                                                                                              </p>
                                                                                                              </td>
                                                                                                              <td style='padding:5px;'>
                                                                                                              <table style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <tr>
                                                                                                              <td colspan='2' valign='top' style='border:solid #A6A6A6 1.0pt;background:#e6e6e6;padding:5px;'>
                                                                                                              <p align=center style='text-align:center;'>Key to centroid matches</p>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              <tr>
                                                                                                              <td valign='top' style='width:50%;border:solid #A6A6A6 1.0pt;background:#92D050;padding:5px;'>
                                                                                                              <p>0.0-0.65</p>
                                                                                                              </td>
                                                                                                              <td valign='top' style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <p>Strong match, within threshold</p>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              <tr>
                                                                                                              <td valign=top style='border:solid #A6A6A6 1.0pt;background:#C5E0B4;padding:5px;'>
                                                                                                              <p>0.6501-0.695</p>
                                                                                                              </td>
                                                                                                              <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <p>Moderate match, within threshold</p>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              <tr>
                                                                                                              <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <p>0.69501-1</p>
                                                                                                              </td>
                                                                                                              <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <p>Weak match, outside threshold</p>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              <tr>
                                                                                                              <td valign=top style='border:solid red 1.0pt;padding:5px;'></td>
                                                                                                              <td valign=top style='border-top:none;border-left:
                                                                                                              none;border-bottom:solid #A6A6A6 1.0pt;border-right:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <p>Plot is outside of environmental domain
                                                                                                              for this PCT</p>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              <tr>
                                                                                                              <td valign=top style='border:solid #BFBFBF 1.0pt;
                                                                                                              border-top:none;background:#757575;padding:5px;'></td>
                                                                                                              <td valign=top style='border-top:none;border-left:
                                                                                                              none;border-bottom:solid #A6A6A6 1.0pt;border-right:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <p>Matched PCT cannot be reliably assigned on
                                                                                                              floristic variables alone</p>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              </table>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              </table>")),
                                      
                                      conditionalPanel(condition="input.tabselected==2",  HTML("<table style='border:none;'>
                                                                                                <tr>
                                                                                               <td style='vertical-align:top;width:50%'>
                                                                                               <p style='text-align:justify;padding:5px;'>
                                                                                               This analysis uses species composition (presence-absence) to determine how floristically related each new plot is to existing PCTs.

                                                                                                Each PCT is defined by a specific group of existing plots; the characteristic species for each group is defined by modelling the occurrence of each flora species across all groups.
                                                                                                
                                                                                                The analysis calculates the percentage of species in the new plot(s) that are characteristic species for each PCT.
                                                                                                
                                                                                                The table presents the PCTs which have the highest percentage. The higher the number listed in the ‘Match’ columns, the stronger the floristic relationship.
                                                                                                 </p>
                                                                                               </td>
                                                                                               <td style='padding:5px;'>
                                                                                               <table style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <tr>
                                                                                               <td colspan='2' valign='top' style='border:solid #A6A6A6 1.0pt;background:#e6e6e6;padding:5px;'>
                                                                                               <p align=center style='text-align:center;'>Key to characteristic species matches</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign='top' style='width:50%;border:solid #A6A6A6 1.0pt;background:#92D050;padding:5px;'>
                                                                                               <p>76-100</p>
                                                                                               </td>
                                                                                               <td valign='top' style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Strong match</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;background:#C5E0B4;padding:5px;'>
                                                                                               <p>51-75</p>
                                                                                               </td>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Moderate match</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>0-50</p>
                                                                                               </td>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Weak match</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid red 1.0pt;padding:5px;'></td>
                                                                                               <td valign=top style='border-top:none;border-left:
                                                                                               none;border-bottom:solid #A6A6A6 1.0pt;border-right:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Plot is outside of environmental domain
                                                                                               for this PCT</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid #BFBFBF 1.0pt;
                                                                                               border-top:none;background:#757575;padding:5px;'></td>
                                                                                               <td valign=top style='border-top:none;border-left:
                                                                                               none;border-bottom:solid #A6A6A6 1.0pt;border-right:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Matched PCT cannot be reliably assigned on
                                                                                               floristic variables alone</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               </table>
                                                                                               </td>
                                                                                               </tr>
                                                                                               </table>
                                                                                               ")),
                                      
                                      
                                      conditionalPanel(condition="input.tabselected==3",  HTML("<table style='border:none;'>
                                                                                                <tr>
                                                                                               <td style='vertical-align:top;width:50%'>
                                                                                               <p style='text-align:justify;padding:5px;'>
                                                                                               The table presents the PCTs returned by BOTH Centroid match and Characteristic species match analyses for each plot. 
                                                                                               The PCTs are ordered by the distance to centroid. The PCTs closest to the left have the stronger floristic relationship to the plot.
                                                                                              </p>
                                                                                               </td>
                                                                                               <td style='padding:5px;'>
                                                                                               <table style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <tr>
                                                                                               <td colspan='2' valign='top' style='border:solid #A6A6A6 1.0pt;background:#e6e6e6;padding:5px;'>
                                                                                               <p align=center style='text-align:center;'>Key to combined matches</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign='top' style='width:50%;border:solid #A6A6A6 1.0pt;background:#92D050;padding:5px;'>
                                                                                               <p>Centroid &gt; x AND Combined &gt; y </p>
                                                                                               </td>
                                                                                               <td valign='top' style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Strong match</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;background:#C5E0B4;padding:5px;'>
                                                                                               <p></p>
                                                                                               </td>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Moderate match</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p></p>
                                                                                               </td>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Weak match</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid red 1.0pt;padding:5px;'></td>
                                                                                               <td valign=top style='border-top:none;border-left:
                                                                                               none;border-bottom:solid #A6A6A6 1.0pt;border-right:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Plot is outside of environmental domain
                                                                                               for this PCT</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid #BFBFBF 1.0pt;
                                                                                               border-top:none;background:#757575;padding:5px;'></td>
                                                                                               <td valign=top style='border-top:none;border-left:
                                                                                               none;border-bottom:solid #A6A6A6 1.0pt;border-right:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Matched PCT cannot be reliably assigned on
                                                                                               floristic variables alone</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               </table>
                                                                                               </td>
                                                                                               </tr>
                                                                                               </table>
                                                                                               "))
                                    
                                    , width = "auto"),
                                  
                                  tags$hr(),
                                  
                                  fluidRow(
                                    tabsetPanel(
                                      
                                      tabPanel("Centroid matches", value=1,
                                               fluidRow(
                                                 column(12, DT::dataTableOutput("cent_table"))
                                               )),
                                      tabPanel("Char. spp. matches", value=2,
                                               fluidRow(
                                                 column(12, DT::dataTableOutput("char_table"))
                                               )),
                                      tabPanel("Combined matches", value=3,
                                               fluidRow(
                                                 column(12, DT::dataTableOutput("combined_table"))
                                               )),
                                      tabPanel("Download matches", value=4,
                                               fluidRow(
                                                 # download all of the match data
                                                 tags$div(downloadButton("download_char_matches", "Download char. spp. matches"),
                                                 tags$hr(),
                                                 downloadButton("download_cent_matches", "Download centroid matches"),style="padding:20px")
                                                 
                                               
                                                
                                               ))
                                      , id = "tabselected")
                                    , width = "auto")
                                  
                                  , style='padding:20px;'),
                         
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

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
library(shinyjs)
library(leaflet)
library(RColorBrewer)
library(shinycssloaders)




# Define UI for application 
ui<-  htmlTemplate("main.html",
                   
              
                    mainView= mainPanel(
                      
                      shinyjs::useShinyjs(),
                      
                       tabsetPanel(
                         
                         tabPanel("Information",
                                  fluidPage(
                                    
                                    tags$img(src="PlotToPCTIDToolIntroPage_StudyRegionMapv3.jpg",alt="Eastern NSW Study Region Map with IBRA",style="float:left"),
                                    HTML( "<p style='text-align:justify;padding:10px 10px 1px 10px;'>
                                              This Plot to PCT matching tool is intended to assist in the assignment of standard 400m<sup>2</sup> full floristic survey plots to Plant Community Types (PCT) for the east coast and tablelands of NSW.
                                          </p>
                                          <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                          This tool functions for plots located within the east coast and tablelands region. Qualitative PCTs or PCTs occurring outside the study region cannot be identified using this tool.
                                          </p>
                                          <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                          Information and data on all NSW PCTs is stored in the BioNet Vegetation Classification database. This tool draws on information stored in BioNet.
                                          </p>
                                          <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                          Data imported into this tool is assumed to have been exported from the BioNet Flora Survey database in the correct format. If you haven’t yet done so, please enter plot data into <a href='http://www.bionet.nsw.gov.au/' rel='external' target='_blank' class='external'>&nbsp;BioNet Flora Survey<img alt='external link' src='external-link-alt.png'></a> and then follow the user guide to export data in the format ready for this tool.
                                          </p>
                                          <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                          This tool is designed to assist with PCT allocation only. Allocation of a plot to a PCT requires consideration of floristic, environmental and spatial variables, as well as disturbance and condition of the plot.
                                          </p>
                                          <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                          This tool was developed by OEH and UNSW with funding from the Australian Research Council (ARC) grant number LP150100972.
                                          </p>
                                          <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                          This tool is currently populated with plot to draft PCT assignments as at 22 March 2019. Plot assignments to draft PCTs are managed by the NSW Vegetation Classification Team.
                                          </p>
                                          <p>Contact the <a href='mailto:bionet@environment.nsw.gov.au' rel='external' target='_blank' class='external'>&nbsp;BioNet Team<img alt='external link' src='external-link-alt.png'></a></p>
                                          "),
                                   
                                    tags$hr(),
                                    fluidRow(
                                      
                                      # button to launch the userguide
                                     actionButton(inputId='lnkUserGuide', label="View Plot to PCT ID Tool User Guide", 
                                                    icon = icon("fas fa-external-link-alt"), 
                                                    onclick ="window.open('http://google.com', '_blank')"),
                                      
                                      
                                      # button to view paper methodology page (pdf)
                                     actionButton(inputId='linkPaperMethodology', label="View Eastern NSW Classification Methodology", 
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
                                    
                                  ),style="padding:20px;"),
                         
                         
                         
                         
                         tabPanel("Input",
                                  
                                 verticalLayout(
                                   
                                   sidebarPanel(
                                      # Input floristic file - can modify down the track to choose/process different data types, header, species match ups etc.
                                      fileInput('file1', label='Input File: text file . txt, first column site names, next columns floristic cover abundance data with PATN label scientific name headings, blank column, environmental data if known with headings Latitude, Longitude, Elevation, RainfallAnn, TempAnn. 
                                                                Please note that this tool can accept data with or without environmental data, but will have reduced functionality of the latter is imported. One file type or the other can be imported, not a mix of both',
                                                accept=c('text/plain','text/csv', 
                                                         '.txt','.csv'),buttonLabel="Browse..."),
                                      actionButton("goMatch", "Analyse data")
                                        ) ,
                             
                                      htmlOutput("uploadInformation", style="padding:20px"),
                                      htmlOutput("dataChecks", style="padding:20px"),
                                      
                                    
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
                                                                                                              <td valign='top' style='width:50%;border:solid #A6A6A6 1.0pt;background:#7FFF00;padding:5px;'>
                                                                                                              <p>0.0-0.65</p>
                                                                                                              </td>
                                                                                                              <td valign='top' style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                              <p>Strong match, within threshold</p>
                                                                                                              </td>
                                                                                                              </tr>
                                                                                                              <tr>
                                                                                                              <td valign=top style='border:solid #A6A6A6 1.0pt;background:#8FBC8F;padding:5px;'>
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
                                                                                                
                                                                                                The analysis calculates the percentage of characteristic species (for each PCT) that were observed in the new plot(s).
                                                                                                
                                                                                                The table presents the PCTs to which the highest percentage matches were obtained. The higher the number listed in the ‘Match’ columns, the stronger the floristic relationship.
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
                                                                                               <td valign='top' style='width:50%;border:solid #A6A6A6 1.0pt;background:#7FFF00;padding:5px;'>
                                                                                               <p>76-100</p>
                                                                                               </td>
                                                                                               <td valign='top' style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                               <p>Strong match</p>
                                                                                               </td>
                                                                                               </tr>
                                                                                               <tr>
                                                                                               <td valign=top style='border:solid #A6A6A6 1.0pt;background:#8FBC8F;padding:5px;'>
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
                                                                                               
                                                                                               </table>
                                                                                               </td>
                                                                                               </tr>
                                                                                               </table>
                                                                                               ")),
                                      
                                      
                                      conditionalPanel(condition="input.tabselected==3",  HTML("<table style='border:none;'>
                                                                                               <tr>
                                                                                               <td style='vertical-align:top;width:50%'>
                                                                                               <p style='text-align:justify;padding:5px;'>
                                                                                                  This analysis shows how the plot fits within the known environmental envelopes (Elevation, Average annual rainfall, Average annual temperature) for matched PCTs. 
                                                                                                  This is a guide only, not a definitive analysis. If plots are outside the known environmental thresholds they need to be more closely examined to assess whether they can still be assigned to the matched PCT. 
                                                                                                  You can Use the search bar on the upper right to filter table to specific sites or vegetation types
                                                                                               </p>
                                                                                               </td>
                                                                                             
                                                                                                <td style='padding:5px;'>
                                                                                                  <table style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                     <tr>
                                                                                                     <td colspan='2' valign='top' style='border:solid #A6A6A6 1.0pt;background:#e6e6e6;padding:5px;'>
                                                                                                     <p align=center style='text-align:center;'>Key to environmental thresholds check</p>
                                                                                                     </td>
                                                                                                     </tr>
                                                                                                     <tr>
                                                                                                     <td valign='top' style='width:50%;border:solid #A6A6A6 1.0pt;background:orange;padding:5px;'>
                                                                                                     <p>Below</p>
                                                                                                     </td>
                                                                                                     <td valign='top' style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                     <p>Plot is below the known environmental variable envelope for the PCT</p>
                                                                                                     </td>
                                                                                                     </tr>
                                                                                                     <tr>
                                                                                                     <td valign=top style='border:solid #A6A6A6 1.0pt;background:green;padding:5px;'>
                                                                                                     <p>Within</p>
                                                                                                     </td>
                                                                                                     <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                     <p>Plot is within the known environmental variable envelope for the PCT</p>
                                                                                                     </td>
                                                                                                     </tr>
                                                                                                     <tr>
                                                                                                     <td valign=top style='border:solid #A6A6A6 1.0pt;background:orange;padding:5px;'>
                                                                                                     <p>Above</p>
                                                                                                     </td>
                                                                                                     <td valign=top style='border:solid #A6A6A6 1.0pt;padding:5px;'>
                                                                                                     <p>Plot is above the known environmental variable envelope for the PCT</p>
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
                                                 htmlOutput("PCTName", style="padding:20px"),
                                                 tags$p(actionButton("PCTSubmit","View PCT Profile"), style="padding:20px") ,
                                                 tags$p(),
                                                 column(12, DT::dataTableOutput("cent_table"))
                                               )),
                                      tabPanel("Char. spp. matches", value=2,
                                               fluidRow(
                                                 htmlOutput("PCTName2", style="padding:20px"),
                                                 tags$p(actionButton("PCTSubmit2","View PCT Profile"), style="padding:20px"),
                                                 tags$p(),
                                                 column(12, DT::dataTableOutput("char_table"))
                                               )),
                                      tabPanel("Environmental thresholds", value=3,
                                               fluidRow(
                                                 column(12, DT::dataTableOutput("env_thresholds"))
                                               )),
                                      tabPanel("Download matches", value=4,
                                               fluidRow(
                                                 # download all of the match data
                                                 tags$div(downloadButton("download_cent_matches", "Download centroid matches"),
                                                          tags$hr(),
                                                          downloadButton("download_char_matches", "Download char. spp. matches"),
                                                          tags$hr(),
                                                          downloadButton("download_env_matches", "Download environmental thresholding"),
                                                           tags$hr(),
                                                           downloadButton("PCTProfileData", "Download matched PCT information in tabular form"),
                                                           style="padding:20px")
                                               ))
                                      , id = "tabselected")
                                    , width = "auto")
                                  
                                  , style='padding:20px;'),
                         
                         tabPanel("Ordination plot",
                                  fluidPage(
                                    plotOutput("ord_site_plt", width = "600px", height = "600px")
                                  )),
                        
                         tabPanel("Map view",
                                  fluidPage(
                                    
                                    withSpinner(leafletOutput("map", width = "100%", height = "800px")),
                                       tags$p("Plots are displayed on the map only where they are: stored in BioNet Flora Survey AND listed as publicly available from BioNet Flora Survey AND allocated to a PCT by the NSW Vegetation Classification team; OR uploaded to this Plot to PCT ID tool during the current session.")
                                  )
                         )
                       )
                     , width = "auto")
             
                   
                   )

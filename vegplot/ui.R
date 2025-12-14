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
                         
                         tabPanel("Introduction",
                                  fluidPage(
                                    fluidRow(HTML(paste0("<h3 style='text-align:justify;padding:10px 10px 1px 10px;color:#567621;font-weight:bolder;'>
                                                    Welcome to the Plot to PCT Assignment Tool
                                                  </h3>
                                                <div class='rowI'>
                                                   
                                                   <div class='colI'style='padding:5px;'>
                                                        <p style='text-align:justify;padding:1px 1px 1px 2px;'>
                                                            <span style='color:#567621;'>This tool helps identify NSW Plant Community Types (PCTs)</span> using the standard floristic survey methods required under the <a href='https://www.environment.nsw.gov.au/topics/animals-and-plants/biodiversity-offsets-scheme/about/biodiversity-assessment-method' rel='external' target='_blank' class='external'>NSW Biodiversity Assessment Method (BAM)</a> and promoted by the <a href='https://www.environment.nsw.gov.au/sites/default/files/2024-11/native-vegetation-interim-type-standard-100060.pdf' rel='external' target='_blank' class='external'>NSW Vegetation Survey Standards.</a> 
                                                            </p>
                                                            <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                                            <span style='color:#567621;'>New floristic survey sites collected using standard methods can be evaluated against quantitative Approved PCTs in the coast and tablelands bioregions of eastern NSW</span> using a combination of floristic, environmental and spatial attributes. This tool does not provide definitive answers, but rather assists in the assessment of PCT options. Decisions on PCT identity must also incorporate user knowledge, consideration of site conditions and other relevant factors.
                                                            </p>
                                                            <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                                            <span style='color:#567621;'>A small set of PCTs in the coast and tablelands bioregions of eastern NSW cannot be identified using this tool</span> as they are defined by qualitative data that cannot be incorporated into metric-based assessments. This includes some PCTs that are associated with Threatened Ecological Communities. PCTs occurring outside the coast and tablelands bioregions cannot be identified using this tool.
                                                            </p>
                                                            <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                                            <span style='color:#567621;'>This tool is part of BioNet, the NSW biodiversity data repository.</span> Data on all NSW PCTs are accessible in the <a href='https://vegetation.bionet.nsw.gov.au/LoginPR.aspx?ReturnUrl=%2f' rel='external' target='_blank' class='external'>BioNet Vegetation Classification</a> public application. Standard floristic survey site data are accessed in the Flora surveys module of the <a href='https://atlas.bionet.nsw.gov.au/UI_Modules/YETI_/FloraSearch.aspx' rel='external' target='_blank' class='external'>BioNet Atlas</a> application. 
                                                            </p>
                                                            <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                                            <span style='color:#567621;'>A Plot to PCT Assignment Tool <a href='https://www.environment.nsw.gov.au/publications/plot-pct-assignment-tool-user-guide-eastern-nsw-pct-classification' rel='external' target='_blank' class='externalgreen'>User Guide</a> is available.</span> Details of the classification project underpinning the quantitative PCTs in eastern NSW can be found in the technical documents section of the <a href='https://www.environment.nsw.gov.au/topics/animals-and-plants/biodiversity/nsw-bionet/nsw-plant-community-type-classification' rel='external' target='_blank' class='external'>NSW Plant Community Type classification</a> web page. 
                                                            </p>
                                                            <p style='text-align:justify;padding:1px 10px 1px 2px;'>
                                                            The Plot to PCT Assignment Tool was developed by the NSW Department of Climate Change, Energy, the Environment and Water (DCCEEW) and the University of New South Wales (UNSW) with funding from the Australian Research Council (ARC) grant number LP150100972.
                                                            </p>
                                                            
                                                            <div  style='font-weight: bolder;margin:10px 10px 0px 0px;text-align:left;'>
                                                            App last updated:", textOutput("AppLastUpdated", inline=T) ," &nbsp; PCT data are Eastern NSW PCT Classification version C2.0 <br/>
                                                            <a href='#' data-toggle='popover' style='color:#567621;text-decoration:underline;' title='Disclaimer' 
                                                            data-placement='bottom' data-content='The NSW Department of Climate Change, Energy, the Environment and Water (DCCEEW) has compiled this publication in good faith, 
                                                                exercising all due care and attention. No representation is made about the accuracy, completeness or suitability of the information in this publication for any particular purpose. 
                                                                DCCEEW shall not be liable for any damage which may occur to any person or organisation taking action or not on the basis of this publication. Users should seek appropriate advice when applying the information to their specific needs. 
                                                                This publication may be subject to revision without notice and users should ensure they are using the latest version.'>Disclaimer</a>
                                                            </div>
                                                            <img src='PlotToPCTIDToolIntroPage_StudyRegionMapv7.jpg' alt='Eastern NSW Study Region Map with IBRA'/ >
                                                         
                                                    </div>
                                                    <div class='colI' style='width:60%;padding:5px;'>
                                                        <div style='background-color:#ccd6bc;padding:5px;'>
                                                                  	<h4 style='padding:2px 0px 2px 10px;'>BioNet</h4>
                                                                	<p style='padding:5px 5px 5px 10px;'>The Plot to PCT Assignment Tool is part of BioNet. BioNet is the major repository for NSW biodiversity data. It is the key system for managing native vegetation information including survey data, classification types and descriptions. Contact <a href='mailto:bionet@environment.nsw.gov.au' rel='external' target='_blank'  style='background-color:#ccd6bc;' class='externalg'>bionet@environment.nsw.gov.au</a> for assistance.</p>
                                                                  <hr/> 
                                                                	 <h4 style='padding:2px 0px 2px 10px;'>BAM</h4>
                                                                	<p style='text-align:justify;padding:5px 10px 5px 10px;'>The <a href='https://www.environment.nsw.gov.au/topics/animals-and-plants/biodiversity-offsets-scheme/about/biodiversity-assessment-method' rel='external' target='_blank' class='externalg'>NSW Biodiversity Assessment Method (BAM)</a> is used to assess impacts on biodiversity under the Biodiversity Conservation Act 2016. It requires assessors to collect data using a standard method. A subset of these data are suitable for use in the Plot to PCT Assignment Tool.</p>
                                                              
                                                         </div>
                                                    </div>
                                                  </div>
                                                   
                                                  "))
                                    )
                                    
                                   
                                    
                                  ),style="padding:20px;"),
                         
                         
                         
                         
                         tabPanel("Data Input",
                                  
                              
                                   
                                   fluidRow(
                                     
                                     HTML("<table style='border:none;'>
                                                <tr style='width:100%;'>
                                                <td style='width:70%;vertical-align:top;'>"),
                                     
                                           sidebarPanel(
                                              # Input floristic file - can modify down the track to choose/process different data types, header, species match ups etc.
                                              fileInput('file1', label='Input File: Required format is a comma separated values file (.csv). 
                                                                          The Flora surveys module of the BioNet Atlas application is designed to store and export data ready for direct upload to this tool.',
                                                        accept=c('text/plain','text/csv', 
                                                                 '.txt','.csv'),buttonLabel="Browse..."),
                                              actionButton("goMatch", "Analyse data"),
                                              tags$p(),
                                              htmlOutput("cores")
                                           ) ,   
                                           
                                           
                                           HTML( paste0("
                                                <h3 style='text-align:justify;padding:10px 10px 1px 10px;color:#567621;font-weight:bolder;'>
                                                    Important
                                                </h3>  
                                                <p style='text-align:justify;padding:1px 15px 1px 2px;'>
                                                To use this tool please upload standard floristic survey site data using the browse button to the left.
                                                </p><p style='text-align:justify;padding:1px 15px 1px 2px;'>
                                                 The Flora surveys module of the BioNet Atlas application has been set up to store and export data ready for direct upload to this tool. If you haven’t yet done so, first enter your standard floristic survey data into the Flora surveys module of the BioNet Atlas application and then export it. A quick guide on entering and extracting plot data in BioNet Atlas Flora surveys is <a href='https://www.environment.nsw.gov.au/publications/entering-and-extracting-flora-plot-data-bionet-atlas-use-plot-plant-community-type-assignment-tool' rel='external' target='_blank' class='external'>available.</a> Instructional videos are also <a href='https://vimeo.com/showcase/11306941' rel='external' target='_blank' class='external'>available.</a>
                                                 </p><p style='text-align:justify;padding:1px 15px 1px 2px;'>
                                                Data uploaded to this tool are assumed to have been exported from the Flora surveys module of the BioNet Atlas application using the 'PCT classification taxonomic assignment' and the 'Plot to PCT assignment tool' output format, which automatically applies the required taxonomic treatment, the cover and abundance score conversions, and the truncated scientific name known as a PATN label (usually the first four letters of the genus and the first four letters of the species). Uploading data that haven't had the taxonomic treatments and cover score conversions applied may give unreliable results.
                                                 </p><p style='text-align:justify;padding:1px 15px 1px 2px;'>
                                                As your data are uploaded, some simple data checks will be run. When you see the 'Upload complete' message you can view the results of these checks below. Once you have viewed the data checks, click on the 'Analyse data' button to proceed with analysis. Once the analysis is complete, click to the 'PCT Matching Results' tab in the menu bar above.
                                                 </p><p style='text-align:justify;padding:1px 15px 1px 2px;'>
                                                If you want to view the functions of this tool with some test data, or check the data input format, 

                                                 ",downloadLink("DownloadSampleData2","download sample data here.",class="externalgreen",style="color:#567621;"),"</p> ")),
                                           
                                           htmlOutput("uploadInformation", style="padding:20px;display:block; clear: both;"),
                                           htmlOutput("dataChecks", style="padding:20px"),
                                                   
                                           HTML("</td>
                                                <td style='vertical-align:top;background-color:#ccd6bc;'>
                                                    <h4 style='padding:2px 0px 2px 10px;'>NSW Taxa</h4>
                                                    <p style='padding:5px 5px 5px 10px;'>This tool relies on the use of a specific taxonomic treatment for flora species names. This treatment was applied to the BioNet flora species names list during the eastern NSW PCT classification to provide consistent nomenclature across 50,000 sites in eastern NSW. The treatment is known as the 'PCT classification taxonomic assignment'. It is important that site species data are standardised to this specific treatment for use in this tool. The Flora surveys module of the BioNet Atlas application has this taxonomic treatment functionality built in and available for your use. The BioNet flora species names are available in the Flora sheet within the <a href='https://www.environment.nsw.gov.au/publications/bionet-atlas-flora-survey-upload-datasheet' rel='external' target='_blank' class='externalg'>BioNet Atlas Flora Survey upload spreadsheet.</a></p>
                                                    <hr/> 
                                                     <h4 style='padding:2px 0px 2px 10px;'>Cover and Abundance</h4>
                                                    <p style='text-align:justify;padding:5px 10px 5px 10px;'>This tool is underpinned by standard floristic survey sites collected by a large number of botanists for different purposes over decades and stored within BioNet. Different surveys have used different methods to estimate cover and abundance of plant species within sites. In order to compare sites, cover and abundance data are transformed to a standard schema. The schema used is a 1 to 6 cover abundance score where: 
                                                               <br/> Score 1 = Up to 5% projected foliage cover and uncommon
                                                               <br/> Score 2 = Up to 5% projected foliage cover and common
                                                               <br/> Score 3 = 6-20% projected foliage cover
                                                               <br/> Score 4 = 21-50% projected foliage cover
                                                               <br/> Score 5 = 51-75% projected foliage cover
                                                               <br/> Score 6 = Over 75% projected foliage cover
                                                               <br/> It is important that cover and abundance data for new sites are standardised to this schema. The Flora surveys module of the BioNet Atlas application has this functionality built in and available for your use.</p>
                                                   
                                                </td>
                                              </tr>
                                              </table>")
                                                        
                                       
                                           
                                   ),
                                   
                             
                                     
                                      
                                    
                                      # button to download the data check report
                                      #tags$div(downloadButton("linkDownloadDataCheckReport", "Download data check report", class = ".btn-primary"),style="padding:20px"),
                                   
                                      tags$hr()
                                      
                                   
                                   # button to download the example data
                                   #tags$div(downloadButton("linkDownloadSampleData", "Download Sample Data", class = ".btn-primary"),style="padding:20px")
                                
                                  ,style="padding:20px"),
                         
                         tabPanel("PCT Matching Results",
                                  fluidRow(
                                      # number of matches to give back to the user
                                    tags$p("This section of the tool allows you to assess your sites against the most closely floristically related PCTs. 
                                       It is recommended that you first evaluate your data using the 'Centroid Matches' tab. You can follow the links for each vegetation type to see more details about that type. Supporting information is available in the 'Environmental Thresholds' tab. Your sites can be viewed spatially in relation to previously existing classified sites in the 'Map View' tab. 
                                       If you wish to try an alternative method, or you are obtaining weak results using the centroid matches method, it may be suitable to examine results in the 'Characteristic Species Method' tab in the menu bar above."
                                       ,style="padding:2px 10px 2px 10px;"),
                                  tags$hr(),
                                  tags$div(sliderInput("topn",
                                                  "Number of matches to report",
                                                  min = 1,
                                                  max = 10,
                                                  value = 5,
                                                  step = 1), style="float:left;"),
                                  
                                       style="margin:0px;padding:0px;"),
                                  
                                
                                  
                                  fluidRow(
                                    tabsetPanel(
                                      
                                      tabPanel("Centroid Matches", value=1,
                                               fluidRow(
                                                 
                                                 HTML("<table style='border:none;width:100%;'>
                                                      <tr>
                                                      <td style='vertical-align:top;width:50%;'>
                                                      <p style='text-align:justify;padding:5px;'>
                                                        Centroid matching determines how floristically related each uploaded site is to quantitative PCTs, based on the species present and their cover-abundance scores. Each quantitative PCT is defined by a specific group of existing classified reference sites; the centroid of each group of reference sites is a collection of features that defines the floristics of the PCT. The centroid matching analysis calculates the floristic 'distance' between each uploaded site and PCT centroids - this is called the 'distance to centroid' (see diagram). The table below presents the PCTs to which each uploaded site is most closely related. The smaller the number listed in the 'Distance_to_Centroid' columns the stronger the floristic relationship. A 'distance to centroid' threshold has been set at 0.695. If your site is outside the distance to centroid threshold for any PCT it might, for example, have fewer native plant species than is typical, have inconsistent cover abundance scores, or represent an unclassified assemblage. When evaluating matched PCTs you should check the 'Classification Confidence Level' field in the PCT profile. PCTs with Very Low classification confidence are called placeholder PCTs and are defined from very few reference sites. The centroid matching analysis can be sensitive to this small number of reference sites and can generate a low distance to centroid value that falsely suggests a strong PCT match. Careful consideration is required before attributing sites to placeholder PCTs.
                                                        <br/>You can use the search bar above the table to filter to specific sites or PCTs. Click on a PCT ID code in the table to display the PCT Name then view a profile or map of sites assigned to the PCT.                                                        
                                                      </p>
                                                      </td>
                                                        <td style='padding-left:5px;vertical-align:top;width:20%;'>
                                                             <img src='DistanceToCentroidDiagram2.jpg' alt='Distance To Centroid Diagram'/ >   
                                                      </td>
                                                      <td style='padding:5px;vertical-align:top;width:30%;'>
                                                            <table style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                            <tr>
                                                            <td colspan='2' valign='top' style='color:#5c5c5c; border:solid #7f7f7f 1.0pt;background:#e6e6e6;padding:5px;'>
                                                            <p align=center style='text-align:center;color:#5c5c5c;font-weight: bold;'>Key to centroid matches</p>
                                                            </td>
                                                            </tr>
                                                            <tr>
                                                            <td valign='top' style='width:50%;border:solid #7f7f7f 1.0pt;background:#99b964;padding:5px;'>
                                                            <p>0.0-0.695</p>
                                                            </td>
                                                            <td valign='top' style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                            <p>Site is within the distance to centroid threshold for the PCT</p>
                                                            </td>
                                                            </tr>
                                                            <tr>
                                                            <td valign=top style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                            <p>0.69501-1</p>
                                                            </td>
                                                            <td valign=top style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                            <p>Site is outside the distance to centroid threshold for the PCT</p>
                                                            </td>
                                                            </tr>
                                                            </table>
                                                      </td>
                                                      
                                                      </tr>
                                                      </table>"),
                                                 
                                                 HTML("<table style='border:none;width:100%;'>
                                                         <tr>
                                                         <td style='vertical-align:top;'>"),
                                                 
                                                 htmlOutput("PCTName",inline = T, style="padding:1px; font-weight:bolder;font-size:18px;color:#567621;"),
                                                 actionButton("PCTSubmit","View PCT profile"),
                                                 actionButton("ViewPCTMap","View PCT sites"),
                                                 
                                                 HTML(" </td>
                                                       </tr>
                                                       </table>
                                                       "),
                                                 tags$p(),
                                                 column(12, DT::dataTableOutput("cent_table"))
                                               ),style="padding:20px"),
                                      
                                     
                                      
                                      tabPanel("Environmental Thresholds", value=2,
                                               fluidRow(
                                                 HTML("<table style='border:none;width:100%;'>
                                                      <tr>
                                                      <td style='vertical-align:top;width:50%'>
                                                      <p style='text-align:justify;padding:5px;'>
                                                       This analysis shows how each uploaded site fits within the calculated typical environmental range for matched PCTs. The variables used are elevation (metres above sea level), annual rainfall (millimetres) and annual mean temperature (degrees Celsius). This is a guide only. If an uploaded site is outside the typical environmental range, a closer examination is needed to assess whether the site can still be assigned to the matched PCT. The PCTs to which a site is most closely floristically related are included in this table. PCTs are ranked by the distance to centroid for each site (refer to the 'Centroid Matches' tab). The most closely matched PCT is listed as PCT_Match1 and so on. You can use the search bar above the table to filter to specific sites or PCTs. When evaluating environmental thresholds for matched PCTs you should check the 'Classification Confidence Level' field in the PCT profile. PCTs with Very Low classification confidence are called placeholder PCTs, and are defined from very few reference sites. The typical environmental range may be unreliable for placeholder PCTs. For placeholder PCTs you should assess additional information including data in the 'Map View' tab.
                                                      </p>
                                                      </td>
                                                      
                                                      <td style='padding:5px;vertical-align:top;'>
                                                      <table style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                      <tr>
                                                      <td colspan='2' valign='top' style='border:solid #7f7f7f 1.0pt;background:#e6e6e6;padding:5px;'>
                                                      <p align=center style='text-align:center;font-weight: bold;color:#5c5c5c;'>Key to environmental thresholds check</p>
                                                      </td>
                                                      </tr>
                                                      <tr>
                                                      <td valign='top' style='width:50%;border:solid #7f7f7f 1.0pt;background:white;padding:5px;'>
                                                      <p>Below</p>
                                                      </td>
                                                      <td valign='top' style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                      <p>Site is below the calculated typical environmental variable range for the PCT</p>
                                                      </td>
                                                      </tr>
                                                      <tr>
                                                      <td valign=top style='border:solid #7f7f7f 1.0pt;background:#99b964;padding:5px;'>
                                                      <p>Within</p>
                                                      </td>
                                                      <td valign=top style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                      <p>Site is within the calculated typical environmental variable range for the PCT</p>
                                                      </td>
                                                      </tr>
                                                      <tr>
                                                      <td valign=top style='border:solid #7f7f7f 1.0pt;background:white;padding:5px;'>
                                                      <p>Above</p>
                                                      </td>
                                                      <td valign=top style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                                      <p>Site is above the calculated typical environmental variable range for the PCT</p>
                                                      </td>
                                                      </tr>
                                                      
                                                      </table>
                                                        
                                                      </td>
                                                     
                                                      </tr>
                                                      </table>
                                                      "),
                                                 htmlOutput("EnvDataViewMessage", style="padding:20px"),
                                                 column(12, DT::dataTableOutput("env_thresholds"))
                                               ),style="padding:20px"),
                                      
                                      tabPanel("Map View", value=3,
                                               fluidPage(
                                                 HTML("<br/>This map shows: publicly available sites drawn from the Flora surveys module of the BioNet Atlas application that are classified to a quantitative PCT; sites uploaded to this Plot to PCT Assignment Tool during the current session.<br/>
                                                 This map does not show additional sites that are held in BioNet Atlas but are: not publicly available; not classified to a quantitative PCT; recently collected; collected using methods inconsistent with those used in the development of PCTs.<br/>
                                                 PCT symbols on the map may be difficult to see against the 'Terrain' background. Selecting 'Satellite' background may make some symbols easier to see."),
                                                 htmlOutput("MapViewMessage", style="padding:20px"),
                                                 withSpinner(leafletOutput("map", width = "100%", height = "800px")),
                                                 tags$p(" ",style="padding:20px")
                                               )
                                        ),
                                      
                                      tabPanel("Download PCT Match Results", value=4,
                                               fluidRow(
                                                 # download all of the match data
                                                 tags$div(downloadButton("download_cent_matches", "Download centroid matches"),
                                                          tags$hr(),
                                                          downloadButton("download_combo_data", "Download combined centroid and environmental threshold assessment"),
                                                          tags$hr(),
                                                           downloadButton("PCTProfileData", "Download matched PCT information in tabular form"),
                                                          tags$hr(),
                                                          downloadButton("PCTSppGFData", "Download matched PCT species by growth form group data"),
                                                         
                                                          
                                                          style="padding:20px")
                                               ),style="padding:20px")
                                      
                                      , id = "tabselected")
                                    , width = "auto")
                                  
                                  , style='padding:20px;'),
                         
                                tabPanel("Characteristic Species Method", 
                                     fluidRow(
                                       
                                       
                                    
                                        HTML("<table style='border:none;width:100%;'>
                                            <tr>
                                              <td style='padding:5px;'>"),

                                              HTML("</td>
                                            </tr>
                                             <tr>
                                             <td style='vertical-align:top;width:50%'>
                                              <p style='text-align:justify;padding:5px;'>
                                                It is recommended that you first evaluate your data using the 'PCT Matching Results' tab. The 'Characteristic Species Method' tab provides an alternative if you are obtaining weak results using centroid matching. 
                                                The characteristic species method is still under development.<br/>
                                                Characteristic species have been calculated for each PCT by modelling the occurrence of each flora species across all PCTs. The characteristic species matching analysis calculates the percentage of characteristic species for each PCT that are present in each uploaded site. The table below presents the PCTs which have the highest percentage species match with each site. The larger the number listed in the '%_Char_Spp' columns, the stronger the floristic relationship. The characteristic species matching method only factors in whether species are present or absent at a site, disregarding the relative cover or abundance.<br/>
                                                You can use the search bar above the table to filter to specific sites or PCTs. Click on a PCT ID code in the table to display the PCT Name then view a profile or map of sites assigned to the PCT.<br/>
                                                Note that a percentage species match of zero returns a default match to PCT 3001 (and to 3002, 3003 etc. for sequential PCT matches); where %_Char_Spp is zero, this does not indicate a relationship to these PCTs.<br/>
                                                </p>
                                             </td>
                                             <td style='padding:5px; vertical-align:top;padding-left:30px;'>
                                             <table style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                             <tr>
                                             <td colspan='2' valign='top' style='border:solid #7f7f7f 1.0pt;background:#e6e6e6;padding:5px;'>
                                             <p align=center style='text-align:center;font-weight: bold;color:#5c5c5c;'>Key to characteristic species matches</p>
                                             </td>
                                             </tr>
                                             <tr>
                                             <td valign='top' style='width:50%;border:solid #7f7f7f 1.0pt;background:#99b964;padding:5px;'>
                                             <p>60-100</p>
                                             </td>
                                             <td valign='top' style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                             <p>Good match to PCT</p>
                                             </td>
                                             </tr>
                                             <tr>
                                             <td valign=top style='border:solid #7f7f7f 1.0pt;background:#ccd6bc;padding:5px;'>
                                             <p>25-59</p>
                                             </td>
                                             <td valign=top style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                             <p>Plausible match to PCT</p>
                                             </td>
                                             </tr>
                                             <tr>
                                             <td valign=top style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                             <p>0-24</p>
                                             </td>
                                             <td valign=top style='border:solid #7f7f7f 1.0pt;padding:5px;'>
                                             <p>Unlikely match to PCT</p>
                                             </td>
                                             </tr>
                                             
                                             </table>
                                             </td>
                                             </tr>
                                             </table>
                                             "),
                                        
                                        
                                        HTML("<table style='border:none;width:100%;'>
                                             <tr>
                                             <td style='vertical-align:top;'>"),
                                        
                                         htmlOutput("PCTName2",inline = T, style="padding:1px; font-weight:bolder;font-size:18px;color:#567621;"),
                                        actionButton("PCTSubmit2","View PCT Profile"),
                                        actionButton("ViewPCTMap2","View PCT sites"),
                                        HTML(" </td>
                                             </tr>
                                             </table>
                                             "),
                                        
                                        tags$hr(),
                                          downloadButton("download_char_matches", "Download char. spp. match results"),
                                        tags$p(),
                                      	 column(12, DT::dataTableOutput("char_table"))
                                             ),style="padding:20px")
                        
                        
                       )
                     , width = "auto")
             
                   
                   )

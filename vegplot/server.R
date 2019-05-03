#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
library(dplyr)
library(tidyr)
library(purrr)
library(DT)
library(vegan)
library(parallel)
library(googleway)

source("functions.R")

# Define server logic 

shinyServer(function(input, output) {

  # # RUN EXAMPLE ON SERVER DATA ----------------------------------------------
  
  ## Here i originally had the idea to have the example analysis as just a click-button
  ## but that got unweildly with the reactive dependancies...
  ## how users just download some example data instead
  
  # observeEvent(input$example, {
  #   infile_df <- readRDS("data/AC14_floristic_plots.rds")
  #   char_matches <- calculate_matches(infile_df[,-1], infile_df[,1], char_spp_list)
  #   out_list <- list(sites = infile_df[,1],
  #                    floristics = infile_df[,-1],
  #                    char_matches = char_matches)
  #   output$num_sites <- renderText({paste0("This data has ", length(out_list$sites), " sites.")})
  #   output$num_species <- renderText({paste0("There are ", ncol(out_list$floristics)," species")})
  #   output$info_text <- renderText({
  #     "<br> The table below shows vegetation type matches 
  #     for some example floristic data from the NSW vegetation plot 
  #     database. You can follow the links for each vegetation 
  #     type to see more details about that type 
  #     (incl. floristics and environemtnal characteristics).
  #     <br> <br> N.B. you can't download these results.
  #     <br> <br> N.B. please refresh your page before analysing your own data."
  #   })
  #   make_topn_matches <- reactive({
  #     topn_matches <- get_topn(out_list$char_matches, input$topn)
  #     style_matches(topn_matches)
  #   })
  #   output$table <- DT::renderDataTable({
  #     make_topn_matches()
  #   })
  # })
  
  # example_data <- reactiveValues(data = NULL)
  # 
  # load_example_data <- observeEvent(input$example,{
  #   example_data$data <- readRDS("data/GAP-EAST_plots.rds")
  # })
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  get_example_data <- reactive({
    readRDS("data/GAP-EAST_plots.rds")
  })
  
  output$linkDownloadSampleData <- downloadHandler(
    filename = function() {
      paste("example_floristic_data.csv")
    },
    content = function(file) {
      write.csv(get_example_data(), file, row.names = F)
    }
  )
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  observe({
    if (is.null(input$file1) || input$file1$name == "") {
      shinyjs::hide("linkDownloadDataCheckReport")
    } else {
      shinyjs::show("linkDownloadDataCheckReport")
    }
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # NORMAL APP FUNCTION - USER SUPPLIED DATA --------------------------------
  
  # print out some stats to know we're in business
  check_infile <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    infile_df <- read.csv(inFile$datapath,
                          header = T,
                          stringsAsFactors = F)
    names(infile_df)[1] <- "sites"
    env_present <- all(non_floristic %in% names(infile_df))
    floristics <- select(infile_df, -sites, -one_of(non_floristic))
    out_list <- list(sites = infile_df[,1],
                     floristics = floristics,
                     missing_species = names(floristics)[!names(floristics) %in% colnames(centroids)],
                     env_present = env_present)
                     #infile_df = infile_df)
    return(out_list)
  })
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # # put together the file stats, once file is uploaded
  # # (to start, no file is uploaded - print a promt to do so)
  # output$num_sites <- renderText({
  #   if (!is.null(check_infile())) {
  #     paste0("You uploaded a file with ", length(check_infile()$sites), " sites.")
  #   } else {
  #     "You have not uploaded any data yet (you can download the example data to test drive)."
  #   }
  # })
  # 
  # #num species htmlout
  # output$num_species <- renderUI({
  #   if (!is.null(check_infile())) {
  #     if (length(check_infile()$missing_species) == 0) {
  #       HTML(paste0("There are ", ncol(check_infile()$floristics),
  #                   " species: all species have been matched in the database.",
  #                   "<br> <b>N.B.</b> Up to 200 sites should be <1 minute processing."))
  #     } else {
  #       HTML(paste0("There are ", ncol(check_infile()$floristics),
  #                   " species, <b><mark> and ",length(check_infile()$missing_species),
  #                   " could not be matched: ", paste(check_infile()$missing_species, collapse = ", "), 
  #                   ", so were ignored in analysis.</mark></b>",
  #                   "<br> <b>N.B.</b> Up to 200 sites should be <1 minute processing."))
  #     }
  #   } else {
  #     "Use the upload data box on the left to get started."
  #   }
  # })
  
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # upload information
 
  uploadinfo <- reactive({
    
    HTMLResult <-paste0("<p style='padding:10px'><h4>Upload Information:</h4>
                    You have not uploaded any data yet (you can download the example data to test drive).</p>") 
    
    if (!is.null(check_infile())) {
      
      numplots <- get_numplots(length(check_infile()$sites))
      #numplots<- is.null(numplots) ? numplots : 0

      # place holder at the moment. TODO: needs to be recalculated based on additional data from matrix file.
      # mitch: if we want to handle incomplete environemtnal data, that will have to be done further down the track
              # TODO: include qa/qc of incoming environemtnal data, for the meantime, assume it's complete 
      if (check_infile()$env_present) {
        numplotsWithEnvAndSpatialData <- "Yes"
      } else {
        numplotsWithEnvAndSpatialData <- "No"
      }

      numspecies <- ncol(check_infile()$floristics)
      #numspecies<- is.null(numspecies) ? numspecies : 0




        HTMLResult<- paste0("<p style='padding:10px'><h4>Upload Information:</h4>
                    <table border='1px' style='width:100%;'>
                    <tr>
                    <td style='width:35%'>Number of plots (rows)</td>
                    <td>", numplots ,"</td>
                    </tr>
                    <tr>
                    <td>Number of species (columns)</td>
                    <td>", numspecies ,"</td>
                    </tr>
                    <tr>
                    <td>Was environmental and spatial data detected?</td>
                    <td>", numplotsWithEnvAndSpatialData ,"</td>
                    </tr>
                    </table></p>")
        
      }
      
  
    return(list(uploadresults=HTMLResult))
  })
  
  
  #upload information htmloutput
  output$uploadInformation <- renderUI({
    if (!is.null(check_infile())) {
      HTML(isolate(uploadinfo()$uploadresults))
    }

  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  #dataChecksInfo
  dataChecksInfo <-reactive({
    
    
    if (!is.null(check_infile())) {

      nummissingspecies <- length(check_infile()$missing_species)
     
      missingSpeciesList<-""
      if (nummissingspecies>0){

        missingSpeciesList<-paste0("There are ", ncol(check_infile()$floristics),
                                        " species, <b><mark> and ",length(check_infile()$missing_species),
                                        " could not be matched: ", paste(check_infile()$missing_species, collapse = ", "),
                                        ", so were ignored in analysis.</mark></b>")

      }else{

        missingSpeciesList<-paste0("There are no missing",
                                         " species: all species have been matched in the database.")
      }

      #check if spatial data. If yes then do numplotsOutsideStudy else "N/A. Spatial data not imported"

      # numplotsOutsideStudy = TotalPlots - PlotsInStudyRegion
      numplotsOutsideStudy <-NULL
      numplotsOutsideStudyHTML <-""

      if (is.null(numplotsOutsideStudy)){
        numplotsOutsideStudyHTML <- HTML(paste0("N/A. Spatial data not imported"))
      }else{

        numplotsOutsideStudyHTML <- HTML(paste0(numplotsOutsideStudy))
      }

    }

        return(list(results=paste0("<p style='padding:10px'><h4>Data checks:</h4>
                                    <table border='1px' style='width:100%; padding:5px;'>
                                   <tr>
                                   <td style='width:15%; vertical-align:top'>Species names not found</td>
                                   <td>", missingSpeciesList ,"</td>
                                   </tr>
                                   <tr>
                                   <td>Plots outside study region</td>
                                   <td>", numplotsOutsideStudyHTML ,"</td>
                                   </tr>
                                   </table></p>")))
  })
  
  
  #datachecks ui 
  output$dataChecks <- renderUI({
    
    if (!is.null(check_infile())) {
              HTML(isolate(dataChecksInfo()$results))
      }
    })
  
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # first gather the input data and make the data objects
  # once a file is obtained, calculate the stats
  match_data <- reactiveValues(matches = NULL) # reative storage
  calculate_matches <- observeEvent(input$goMatch, {
    tic <- Sys.time()
    # whack a progress bar up to let people know it's ticking
    progress <- shiny::Progress$new(style = "notification")
    progress$set(message = "Loading data", value = 0.15)
    on.exit(progress$close())
    # Do the calculations
    inFile <- input$file1
    if (is.null(inFile)) {
      showNotification("Please upload some data first. Try downloading the example data if needed.",
                       type = 'error')
      return(NULL)
    }
    infile_df <- read.csv(inFile$datapath,
             header = T,
             stringsAsFactors = F)
    names(infile_df)[1] <- "sites"
    
    if (check_infile()$env_present) {
      env_data <- select(infile_df, sites, one_of(non_floristic), -X)
      floristics <- select(infile_df, -sites, -one_of(non_floristic))
    } else {
      env_data <- NULL
      floristics <- select(infile_df, -sites, -one_of(non_floristic))
    }
    
    progress$set(message = "Matching species", value = 0.25)
    # do the char species / centroid calculations
    #infile_df <- check_infile()$infile_df
    char_matches <- calculate_matches(floristics, infile_df[,1], char_spp_list)
    progress$set(message = "Matching centroids", value = 0.35)
    cent_matches <- calculate_centroids(floristics, infile_df[,1], centroids)
    # make ordination plot
    progress$set(message = "Making plots", value = 0.85)
    ord_sites <- make_sites_ord(select(infile_df, -one_of(non_floristic)))
    # add data to the reactive output
    match_data$matches <- list(char_matches = char_matches,
                               cent_matches = cent_matches,
                               ord_sites = list(ord = ord_sites, sites = infile_df[,1]),
                               mc_cores = attr(cent_matches, "mc_cores"),
                               compute_time = round(as.numeric(difftime(Sys.time(), tic, units = "secs"))),
                               env_data = env_data)
    #match_data$env_data <- list(env_data = env_data)
    progress$set(message = "Compiling matches", value = 0.95)
  })
  
  # info about processing
  output$cores <- renderText({
    if (!is.null(match_data$matches)) {
      
     
      
      paste0("CPU cores used: ", match_data$matches$mc_cores,
             " (analysis took ~", match_data$matches$compute_time, " seconds).")
    } else {
      paste0("Potential CPU cores available: ", detectCores())
    }
  })
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # info on main screen
  # output$info_main_text <- renderText({
  #   
  #     # Text to show once analysis is done
  #     "<p style='text-align:justify;padding:10px 10px 1px 10px;'>
  #       This Plot to PCT matching tool is intended to assist in the assignment of standard 400m2 full floristic survey plots to Plant Community Types (PCT) for the east coast and tablelands of NSW.
  #      </p>
  #       <p style='text-align:justify;padding:1px 10px 1px 2px;'>
  #       This tool functions for plots located within the east coast and tablelands region. Qualitative PCTs or PCTs occurring outside the study region cannot be identified using this tool.
  #       </p>
  #       <p style='text-align:justify;padding:1px 10px 1px 2px;'>
  #       Information and data on all NSW PCTs is stored in the BioNet Vegetation Classification database. This tool draws on information stored in BioNet.
  #       </p>
  #       <p style='text-align:justify;padding:1px 10px 1px 2px;'>
  #       Data imported into this tool is assumed to have been exported from the BioNet Flora Survey database in the correct format. If you haven’t yet done so, please enter plot data into BioNet FS and then follow the user guide to export data in the format ready for this tool.
  #       </p>
  #       <p style='text-align:justify;padding:1px 10px 1px 2px;'>
  #       This tool is designed to assist with PCT allocation only. Allocation of a plot to a PCT requires consideration of floristic, environmental and spatial variables, as well as disturbance and condition of the plot.
  #      </p>
  #       <p style='text-align:justify;padding:1px 10px 1px 2px;'>
  #       This tool was developed by OEH and UNSW with funding from the Australian Research Council (ARC) grant number LP150100972.
  #   </p>"
  #   
  # })
  # 
  
  
  
  # info on analysis
  output$info_analysis_text <- renderText({
    if (!is.null(match_data$matches)) {
      # Text to show once analysis is done
      "<p style='text-align:justify;padding:10px;'>
      The tables below show the top matches for your 
      submitted floristic data to the NSW vegetation plot 
      database. You can follow the links for each vegetation 
      type to see more details about that type 
      (incl. floristics and environmental characteristics).
      Switch between the characteristic species and centroid
      based matches. The combined matches tab shows the 
      types that were matched in both (ordered by distance to
      centroid). The tabs above show more info about the data,
      along with plots.</p>"
    } else {
      if (!is.null(check_infile())) {"<br> Press the analyse button to start!"}
    }
    })
  
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # now we can return the top n matches based on the slider
  style_matches <- reactive({
    topn <- input$topn
    top_char_matches <- get_topn(match_data$matches$char_matches, topn, T)
    top_cent_matches <- get_topn(match_data$matches$cent_matches, topn, F)
    combined_matches <- as.data.frame(t(bind_rows(
      mapply(FUN = match_topn,
             as.data.frame(t(top_cent_matches), stringsAsFactors = F),
             as.data.frame(t(top_char_matches), stringsAsFactors = F),
             MoreArgs = list(topn = topn), SIMPLIFY = F)
      )))
    cent_groups <- top_cent_matches[c(1,grep("group", names(top_cent_matches)))]
    rownames(combined_matches) <- NULL
    names(combined_matches) <- c("Site", 1:topn)
    return(list(char = style_matches_char(reorder_data(top_char_matches)),
                cent = style_matches_cent(reorder_data(top_cent_matches)),
                combined = combined_matches,
                cent_groups = cent_groups))
  })
  
  style_env_thresholds <- reactive({
    if (!is.null(match_data$matches$env_data)) {
      progress <- shiny::Progress$new(style = "notification")
      progress$set(message = "Checking env. thresholds", value = 0.5)
      on.exit(progress$close())
      threshold_results <- check_env_thresholds(style_matches()$cent_groups, env_thresh, match_data$matches$env_data)
      #### do the data table styling here i guess with either drop down or filtering options etc.
      return(list(env_thresholds = threshold_results))
    } else {
      return(NULL)
    }
  })
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  output$char_table <- renderDataTable({
    if (!is.null(match_data$matches)) style_matches()$char
  })
  output$cent_table <- renderDataTable({
    if (!is.null(match_data$matches)) style_matches()$cent
  })
  output$combined_table <- renderDataTable({
    if (!is.null(match_data$matches)) style_matches()$combined
  })
  
  output$env_thresholds <- renderDataTable({
    if (!is.null(match_data$matches$env_data)) style_env_thresholds()$env_thresholds
  })
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # Allow user to download the matches (topn they've decided)
  download_matches <- reactive({
    topn <- input$topn
    return(list(char = get_topn(match_data$matches$char_matches, topn, T),
                cent = get_topn(match_data$matches$cent_matches, topn, F),
                env = check_env_thresholds(style_matches()$cent_groups, env_thresh, match_data$matches$env_data)))
  })
  
  output$download_char_matches <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_char-matches", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(download_matches()$char, file, row.names = F)
    }
  )
  output$download_cent_matches <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_cent-matches", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(download_matches()$cent, file, row.names = F)
    }
  )
  output$download_env_matches <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_env-thresholds", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(download_matches()$env, file, row.names = F)
    }
  )
  
  ### <-- need to add in download for envrionmental thresholds
  
  getDataCheckReport <- reactive({
    return (isolate(dataChecksInfo()$results))
  })
 
  output$linkDownloadDataCheckReport <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_data_check_report", ".html", sep = "")
    },
    content = function(file) {
      write.csv(getDataCheckReport(), file, row.names = F)
    }
  )
  

  # return the ordination plot of sites
  # get_ords <- reactive({
  #   return(match_data$matches$ord_sites)
  # })
  
  output$ord_site_plt <- renderPlot({
    if (!is.null(match_data$matches)) {
      ord_sites <- match_data$matches$ord_sites
      plot(ord_sites$ord, type = "n", display = "sites")
      text(ord_sites$ord$points, labels = ord_sites$sites, cex = 1)
    }
  })
  
  
  
  map_key <- 'AIzaSyCiWLYufriPNDL7DIiWKyeYeKYVSDCPoZ0'
  
  output$map <- renderGoogle_map({
    
    ## different colour palettes
    lstPalette <- list(fill_colour = colorRampPalette(c("red","blue")),
                       stroke_colour = viridisLite::plasma)
    
    google_map(key = map_key, data = tram_stops) %>%
      add_circles(lat = "stop_lat", lon = "stop_lon", fill_colour = "stop_name",
                  stroke_weight = 0.3, stroke_colour = "stop_name", palette = lstPalette, info_window ="stop_id")
    
  })
  
  
})




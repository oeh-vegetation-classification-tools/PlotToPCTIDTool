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


library(dplyr)
library(tidyr)
library(purrr)
library(DT)
library(vegan)
library(parallel)



library(data.table)
library(zoo)
library("jsonlite")
library(DBI)
library(RSQLite)
library(datamart)


library(rgdal)
library(sp)
library(maps)
library("OData")

library(leaflet)
library(RColorBrewer)

library(sqldf)
library(loggit)

setLogFile("www/applog.json")

source("functions.R")

# Define server logic 

shinyServer(function(input, output,session) {

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
  
 
  loggit("INFO","session started", log_detail="session has started", event = "session start",sessionid=isolate(session$token), echo = FALSE)
  
 
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
      
      
      
      numplotsWithEnvData<-0
      numplotsWithSpatialData<-0
      
      # place holder at the moment. TODO: needs to be recalculated based on additional data from matrix file.
      # mitch: if we want to handle incomplete environemtnal data, that will have to be done further down the track
              # TODO: include qa/qc of incoming environemtnal data, for the meantime, assume it's complete 
      inFile <- input$file1
      infile_df <- read.csv(inFile$datapath,
                            header = T,
                            stringsAsFactors = F)
      names(infile_df)[1] <- "sites"
      
      if (check_infile()$env_present) {
        env_data <- select(infile_df, sites, one_of(non_floristic), -X)
        numplotsWithEnvData <-nrow(data.frame(table(env_data$Latitude)))
        numplotsWithSpatialData<-nrow(data.frame(table(env_data$Elevation)))
        
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
                    <td>Number of plots (rows) with environmental data detected?</td>
                    <td>", numplotsWithEnvData ,"</td>
                    </tr>
                    <tr>
                    <td>Number of plots (rows) with spatial data detected?</td>
                    <td>", numplotsWithSpatialData ,"</td>
                    </tr>
                    </table></p>")
        
        
        txtResult<-paste0("Number of plots (rows):", numplots ,", Number of species (columns):", numspecies ,", Number of plots (rows) with environmental data detected:", numplotsWithEnvData ,"
                   , Number of plots (rows) with spatial data detected:", numplotsWithSpatialData)
        
        loggit("INFO","uploadresults", log_detail=txtResult,numplots=numplots, numspecies=numspecies, numplotsWithEnvData=numplotsWithEnvData,numplotsWithSpatialData=numplotsWithSpatialData, event = "upload", sessionid=isolate(session$token), echo = FALSE)
        
        
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
      
      
      
      inFile <- input$file1
      infile_df <- read.csv(inFile$datapath,
                            header = T,
                            stringsAsFactors = F)
      names(infile_df)[1] <- "sites"
      
      if (check_infile()$env_present) {
        
        env_data <- select(infile_df, sites, one_of(non_floristic), -X)
        
       
        wa.map <- readOGR("spatial/EasternNSW_PrimaryStudyArea_Merged.shp", layer="EasternNSW_PrimaryStudyArea_Merged")
        sodo <- wa.map[0]
        proj4string(sodo)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
        
        dat <- data.frame(Longitude =env_data$Longitude ,
                          Latitude =env_data$Latitude,
                          names = env_data$sites)                  
        
        p<-dat
        coordinates(p) <- ~ Longitude + Latitude
        proj4string(p) <- proj4string(sodo)
        
        x<-p[sodo,]
        #plot(sodo)
        #plot(x, col="red" , add=TRUE, lwd=4)
        common <- setdiff(dat$names, x@data$names)  
        #pts<-SpatialPoints(cbind(dat$Longitude,dat$Latitude))
        #plot(pts, col="blue", add=TRUE)
        if (length(common)>0){
          numplotsOutsideStudy<-toString(common) 
        }else{
        numplotsOutsideStudy<-length(common)
        }
        
      } 
      
      
      
      if (is.null(numplotsOutsideStudy)){
        numplotsOutsideStudyHTML <- HTML(paste0("N/A. Spatial data not imported"))
      }else{

        numplotsOutsideStudyHTML <- HTML(paste0(numplotsOutsideStudy))
      }

    }
    
    txtResult<-paste0("Species names not found:", missingSpeciesList ,", Plots outside study region:", numplotsOutsideStudyHTML)
    loggit("INFO","uploadresults", log_detail=txtResult, missingSpeciesList=missingSpeciesList, numplotsOutsideStudy=numplotsOutsideStudy, event = "upload", sessionid=isolate(session$token), echo = FALSE)

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
      based matches. The environmental thresholds tab shows the whether the plot location is within known environment thresholds for matched vegetation types.</p>"
      
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
      return(list(env_thresholds = threshold_results[order(threshold_results$Site_Name),]))
    } else {
      return(NULL)
    }
  })
  
  
  
  
 
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  output$char_table <- renderDataTable({ if (!is.null(match_data$matches)) datatable(array(style_matches()$char)[[1]]$data, selection=list(mode="single",target="cell")) %>%
      formatStyle(grep("match", names(array(style_matches()$char)[[1]]$data)), 
                  backgroundColor = styleInterval(cuts = c(51,76), 
                                                  values = c("white","darkseagreen","chartreuse"))
      ) })
  
  output$cent_table <- renderDataTable({
    if (!is.null(match_data$matches)) datatable(array(style_matches()$cent)[[1]]$data, selection=list(mode="single",target="cell")) %>%
      formatStyle(grep("match", names(array(style_matches()$cent)[[1]]$data)), 
                  backgroundColor = styleInterval(cuts = c(0.65,0.695), 
                                                  values = c("chartreuse","darkseagreen","white"))
      )
  })
  # output$combined_table <- renderDataTable({
  #   if (!is.null(match_data$matches)) style_matches()$combined
  # })
  
  output$env_thresholds <- renderDataTable({
    if (!is.null(match_data$matches$env_data)) style_matches_thresholds(style_env_thresholds()$env_thresholds)
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
      
   
      myvar <- Sys.Date()
      out_string <- paste0("Exported from NSW Plot to PCT ID Tool on",myvar,". Plot to PCT assignment version 22 March 2019\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      fwrite(x = download_matches()$char,
             file= file,
             sep = ',',
             col.names=T,
             append=T)
      
      loggit("INFO","download char_matches", log_detail="download char_matches", event = "download", sessionid=isolate(session$token), echo = FALSE) 
      
    })
  
  
  output$download_cent_matches <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_cent-matches", ".csv", sep = "")
    },
    content = function(file) {
      
   
      myvar <- Sys.Date()
      out_string <- paste0("Exported from NSW Plot to PCT ID Tool on",myvar,". Plot to PCT assignment version 22 March 2019\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      fwrite(x = download_matches()$cent,
             file= file,
             sep = ',',
             col.names=T,
             append=T)
      
      
      loggit("INFO","download centroid_matches", log_detail="download centroid_matches", event = "download", sessionid=isolate(session$token), echo = FALSE) 
      
      #write.csv(download_matches()$cent, file, row.names = F)
    }
    
  )
  
  
  output$download_env_matches <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_env-thresholds", ".csv", sep = "")
    },
    content = function(file) {
      
      myvar <- Sys.Date()
      out_string <- paste0("Exported from NSW Plot to PCT ID Tool on",myvar,". Plot to PCT assignment version 22 March 2019\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      fwrite(x = download_matches()$env,
             file= file,
             sep = ',',
             col.names=T,
             append=T)
      
      #write.csv(download_matches()$env, file, row.names = F)
      
      loggit("INFO","link to download_env_matches", log_detail="link to download_env_matches", event = "download",  sessionid=isolate(session$token), echo = FALSE)  
      
    }
    
  )
  
  
  get_PCTProfile_data <- reactive({
    readRDS("data/GAP-EAST_plots.rds")
  })
  
  output$PCTProfileData <- downloadHandler(
    filename = function() {
      paste("PCTProfile_data", ".csv", sep="")
    },
    content = function(file) {
      # 
   
      myvar <- Sys.Date()
      out_string <- paste0("Exported from NSW Plot to PCT ID Tool on",myvar,". Plot to PCT assignment version 22 March 2019\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
    
      fwrite(x = get_PCTProfile_data(),
             file= file,
             sep = ',',
             col.names=T,
             append=T)
      
      loggit("INFO","download PCTProfile_data", log_detail="download PCTProfile_data", event = "download",  sessionid=isolate(session$token), echo = FALSE)
      
     # write.csv(get_PCTProfile_data(), file, row.names = F)
    },
    contentType="text/csv"
   
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
  
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
 
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  output$PCTName <- renderUI({
    
    if (!is.null(input$cent_table_cell_clicked$col))
    {
      ar<-array(style_matches()$cent)
      columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
      
      if (!columnName==""){
        
        if (substr(columnName,1,nchar(columnName)-1)=="group"){
          shinyjs::show("PCTSubmit")
        pctname<-getPCTName(input$cent_table_cell_clicked$value)
        h4(paste0(" PCT Name ",pctname))
        
        }else{ h4("")
          shinyjs::hide("PCTSubmit")
          }
    }else{
      
      h4("")
      shinyjs::hide("PCTSubmit")
    }
    }else{
      
      h4("")
      shinyjs::hide("PCTSubmit")
    }
    
  })
  
  output$PCTName2 <- renderUI({
    
    if (!is.null(input$char_table_cell_clicked$col))
    {
      ar<-array(style_matches()$char)
      columnName <-colnames(ar[[1]]$data[input$char_table_cell_clicked$col])
      
      if (!columnName==""){
        
        if (substr(columnName,1,nchar(columnName)-1)=="group"){
          shinyjs::show("PCTSubmit2")
          pctname<-getPCTName(input$char_table_cell_clicked$value)
          h4(paste0(" PCT Name ",pctname))
          
        }else{ h4("")
          shinyjs::hide("PCTSubmit2")
          }
      }else{
        
        h4("")
        shinyjs::hide("PCTSubmit2")
      }
    }else{
      
      h4("")
      shinyjs::hide("PCTSubmit2")
    }
    
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  ClickedCentroidPCT <- eventReactive(input$cent_table_cell_clicked,{
    
    
   if (!is.null(input$cent_table_cell_clicked$col))
    {
      ar<-array(style_matches()$cent)
      columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
      
      if (!columnName==""){
        
        if (substr(columnName,1,nchar(columnName)-1)=="group"){
        
        pctprofile<-getPCTProfile(input$cent_table_cell_clicked$value)
        HTML(paste0(pctprofile))
        
      }else{ "" }
    }else{
      
      ""
    }
    }
    
  })
  
 
  observeEvent(input$PCTSubmit,{
    # if (!is.null(input$cent_table_cell_clicked$col))
    # {
    #   ar<-array(style_matches()$cent)
    #   columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
    #   
    #   if (!columnName==""){
    #     
    #     if (substr(columnName,1,nchar(columnName)-1)=="group"){
          showModal(modalDialog(title="PCT Profile",fluidPage(br(),ClickedCentroidPCT()),size="l"))
    #   }
    #   }
    # }
    
  })
  

  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  ClickedCharSppPCT <- eventReactive(input$char_table_cell_clicked,{
    
  
    
    if (!is.null(input$char_table_cell_clicked$col))
    {
      ar<-array(style_matches()$char)
      columnName <-colnames(ar[[1]]$data[input$char_table_cell_clicked$col])
      
      if (!columnName==""){
        
        if (substr(columnName,1,nchar(columnName)-1)=="group"){
          
          pctprofile<-getPCTProfile(input$char_table_cell_clicked$value)
          HTML(paste0(pctprofile))
          
        }else{""}
      }else{
        
        ""
      }
    }
    
  })
  
  
  
  #output$selected2 <- renderText({paste0("PCT Name: ",ClickedCharSppPCT())})
  observeEvent(input$PCTSubmit2,{
    # if (!is.null(input$char_table_cell_clicked$col))
    # {
    #   ar<-array(style_matches()$char)
    #   columnName <-colnames(ar[[1]]$data[input$char_table_cell_clicked$col])
    #   
    #   if (!columnName==""){
    #     
    #     if (substr(columnName,1,nchar(columnName)-1)=="group"){
          showModal(modalDialog(title="PCT Profile data", fluidPage(br(),ClickedCharSppPCT()),size = "l"))
    #     }
    #   }
    # }
    
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  

  
  
  observe({
    
    fp<- file.path(getwd(), "data")
    normalizePath(fp)
    fi<-file.info(list.files(path=fp,pattern="pctdatadb.sqlite$", full.names=TRUE))
    nhours<-as.numeric(difftime(Sys.time(),fi$ctime , units="hours"))
    
    if ((nhours>=4320)||(length(nhours)==0)){
      
      
      ## PCT Data updates -------------------------------------------------------------------------------------------
      pctdata<-retrieveData("https://biodiversity.my.opendatasoft.com/api/odata/pctdata?apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde")
      
      nextlink<-pctdata[["@odata.nextLink"]]
      
      n<-as.integer(length(pctdata$value))
      # Initialize a temporary in memory database and copy a data.frame into it
      con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
      dbExecute(con,"DELETE FROM pctdata")
      for (i in 1:n){
        
        fjs_df<-as.data.frame(pctdata$value[i], stringsAsFactors = F)
        dbWriteTable(con, "pctdata",fjs_df, overwrite=F, append=T)
        
      }
      # clean up
      dbDisconnect(con)
      
      while(!is.null(nextlink))
      {
        pctdata2<-retrieveData(nextlink)
        nextlink<-pctdata2[["@odata.nextLink"]]
        n2<-as.integer(length(pctdata2$value))
        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
        
        for (i2 in 1:n2){
          
          fjs_df<-as.data.frame(pctdata$value[i2], stringsAsFactors = F)
          dbWriteTable(con, "pctdata",fjs_df, overwrite=F, append=T)
          
        }
        # clean up
        dbDisconnect(con)
      }
      
      ## FSurvey data updates -------------------------------------------------------
     
      fjs <- fromJSON("https://biodiversity.my.opendatasoft.com/api/odata/fsdata?apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde")
      
      nextlink<-fjs[["@odata.nextLink"]]
      
      fjs_df<-as.data.frame(fjs$value)
      
      while(!is.null(nextlink))
      {
        
        fsj2<-fromJSON(nextlink)
        nextlink<-fsj2[["@odata.nextLink"]]
        fjs2_df<-as.data.frame(fsj2$value)
        fjs_df<-rbind(fjs_df,fjs2_df)
        
      }
      con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
      dbExecute(con,"DELETE FROM fsdata")
      dbWriteTable(con, "fsdata", fjs_df)
      # clean up
      dbDisconnect(con)
      
      ## End FS data ---------------------------------------------------------------------
      
      
    }
    
    
   
    
    
  })

  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    if (!is.null(match_data$matches)) {
      if (check_infile()$env_present) {
        return(match_data$matches$env_data)
      }
    }
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    pctplotsdata<-pctplots$data

    colfunc <- colorRampPalette(c("AliceBlue", "AntiqueWhite",  "Aquamarine", "Azure", "Beige", "Bisque", "Black", "BlanchedAlmond",
                                  "Blue", "BlueViolet", "Brown", "BurlyWood", "CadetBlue", "Chartreuse", "Chocolate", "Coral", "CornflowerBlue",
                                  "Cornsilk",  "Cyan", "DarkBlue", "DarkCyan", "DarkGoldenRod", "DarkGray", "DarkGrey", "DarkGreen",
                                  "DarkKhaki", "DarkMagenta", "DarkOliveGreen", "DarkOrange", "DarkOrchid", "DarkRed", "DarkSalmon", "DarkSeaGreen",
                                  "DarkSlateBlue", "DarkSlateGray", "DarkSlateGrey", "DarkTurquoise", "DarkViolet", "DeepPink", "DeepSkyBlue", "DimGray",
                                  "DimGrey", "DodgerBlue", "FireBrick", "FloralWhite", "ForestGreen",  "Gainsboro", "GhostWhite", "Gold",
                                  "GoldenRod", "Gray", "Grey", "Green", "GreenYellow", "HoneyDew", "HotPink", "IndianRed",  "Ivory", "Khaki",
                                  "Lavender", "LavenderBlush", "LawnGreen", "LemonChiffon", "LightBlue", "LightCoral", "LightCyan", "LightGoldenRodYellow",
                                  "LightGray", "LightGrey", "LightGreen", "LightPink", "LightSalmon", "LightSeaGreen", "LightSkyBlue", "LightSlateGray",
                                  "LightSlateGrey", "LightSteelBlue", "LightYellow",  "LimeGreen", "Linen", "Magenta", "Maroon", "MediumAquaMarine",
                                  "MediumBlue", "MediumOrchid", "MediumPurple", "MediumSeaGreen", "MediumSlateBlue", "MediumSpringGreen", "MediumTurquoise",
                                  "MediumVioletRed", "MidnightBlue", "MintCream", "MistyRose", "Moccasin", "NavajoWhite", "Navy", "OldLace",  "OliveDrab",
                                  "Orange", "OrangeRed", "Orchid", "PaleGoldenRod", "PaleGreen", "PaleTurquoise", "PaleVioletRed", "PapayaWhip", "PeachPuff",
                                  "Peru", "Pink", "Plum", "PowderBlue", "Purple", "RosyBrown", "RoyalBlue", "SaddleBrown", "Salmon",
                                  "SandyBrown", "SeaGreen", "SeaShell", "Sienna",  "SkyBlue", "SlateBlue", "SlateGray", "SlateGrey", "Snow",
                                  "SpringGreen", "SteelBlue", "Tan", "Thistle", "Tomato", "Turquoise", "Violet", "Wheat", "White", "WhiteSmoke", "Yellow", "YellowGreen"))
    
    colfuncMatched <- colorRampPalette(c("AliceBlue", "AntiqueWhite",  "Aquamarine", "Azure", "Beige", "Bisque", "Black", "BlanchedAlmond",
                                         "Blue", "BlueViolet", "Brown", "BurlyWood", "CadetBlue", "Chartreuse", "Chocolate", "Coral", "CornflowerBlue",
                                         "Cornsilk",  "Cyan", "DarkBlue", "DarkCyan", "DarkGoldenRod", "DarkGray", "DarkGrey", "DarkGreen",
                                         "DarkKhaki", "DarkMagenta", "DarkOliveGreen", "DarkOrange", "DarkOrchid", "DarkRed", "DarkSalmon", "DarkSeaGreen",
                                         "DarkSlateBlue", "DarkSlateGray", "DarkSlateGrey", "DarkTurquoise", "DarkViolet", "DeepPink", "DeepSkyBlue", "DimGray",
                                         "DimGrey", "DodgerBlue", "FireBrick", "FloralWhite", "ForestGreen",  "Gainsboro", "GhostWhite", "Gold",
                                         "GoldenRod", "Gray", "Grey", "Green", "GreenYellow", "HoneyDew", "HotPink", "IndianRed",  "Ivory", "Khaki",
                                         "Lavender", "LavenderBlush", "LawnGreen", "LemonChiffon", "LightBlue", "LightCoral", "LightCyan", "LightGoldenRodYellow"))
    
    
    
    
    colfuncUnmatched <- colorRampPalette(c("LightGray", "LightGrey", "LightGreen", "LightPink", "LightSalmon", "LightSeaGreen", "LightSkyBlue", "LightSlateGray",
                                           "LightSlateGrey", "LightSteelBlue", "LightYellow",  "LimeGreen", "Linen", "Magenta", "Maroon", "MediumAquaMarine",
                                           "MediumBlue", "MediumOrchid", "MediumPurple", "MediumSeaGreen", "MediumSlateBlue", "MediumSpringGreen", "MediumTurquoise",
                                           "MediumVioletRed", "MidnightBlue", "MintCream", "MistyRose", "Moccasin", "NavajoWhite", "Navy", "OldLace",  "OliveDrab",
                                           "Orange", "OrangeRed", "Orchid", "PaleGoldenRod", "PaleGreen", "PaleTurquoise", "PaleVioletRed", "PapayaWhip", "PeachPuff",
                                           "Peru", "Pink", "Plum", "PowderBlue", "Purple", "RosyBrown", "RoyalBlue", "SaddleBrown", "Salmon",
                                           "SandyBrown", "SeaGreen", "SeaShell", "Sienna",  "SkyBlue", "SlateBlue", "SlateGray", "SlateGrey", "Snow",
                                           "SpringGreen", "SteelBlue", "Tan", "Thistle", "Tomato", "Turquoise", "Violet", "Wheat", "White", "WhiteSmoke", "Yellow", "YellowGreen"))
    
    
    
    
    categories<-pctplotsdata$pctid
    RdYlBu <- colorFactor(colfunc(3000), domain = categories)
   

    if ((!is.null(match_data$matches))&&(check_infile()$env_present)) {
      
      
      dt<-style_matches()$cent$x$data %>% select(starts_with("group"))
      allplots<-sqldf("select replace(pctid,'.','_') pctid from pctplotsdata")
      
      SQLString<-""
      for (i in 1:length(dt)){
        
        if (i==length(dt)){
          SQLString<-paste0(SQLString,"select group",i," as pctid from dt")
        }else{
          SQLString<-paste0(SQLString,"select group",i," as pctid from dt union ")
        } 
      }
      
      matchedplots<-sqldf(SQLString)
      unmatchedplots<-setdiff(allplots, matchedplots)
      
      matchedplots<- sqldf("SELECT * from pctplotsdata where replace(pctid,'.','_') in (SELECT pctid FROM matchedplots)")
      unmatchedplots<- sqldf("SELECT * from pctplotsdata where replace(pctid,'.','_') in (SELECT pctid FROM unmatchedplots)")
      
      MatchedCol <-colorFactor(colfuncMatched(3000), domain = matchedplots$pctid)
      UnMatchedCol <-colorFactor(colfuncUnmatched(3000), domain = unmatchedplots$pctid)
      
      
      leaflet(data=filteredData() )%>% addTiles() %>%
        addProviderTiles(providers$Esri.WorldTopoMap)%>%
        fitBounds(~min(filteredData()$Longitude), ~min(filteredData()$Latitude), ~max(filteredData()$Longitude), ~max(filteredData()$Latitude)) %>%
        clearShapes() %>%
        clearMarkers()%>%
        addMarkers(lat = ~filteredData()$Latitude,lng = ~filteredData()$Longitude,layerId = ~filteredData()$sites,
                   popup = ~paste("<b>Site no:</b>",filteredData()$sites,"<br/><b>Lat:</b>",filteredData()$Latitude," <b>Long:</b>",filteredData()$Longitude)) %>%
        
        addCircles(radius= 100, lat = ~matchedplots$lat, lng = ~matchedplots$long, layerId = ~matchedplots$siteno, color =~MatchedCol(matchedplots$pctid), fillColor =~MatchedCol(matchedplots$pctid),opacity = 1,   fillOpacity = 0.7,
                   data = matchedplots, popup = ~paste("<b>Survey name:</b>", matchedplots$surveyname  ,"<br/><b>Site no:</b>",matchedplots$siteno,"<br/><b>Lat:</b>",matchedplots$lat," <b>Long:</b>",matchedplots$long,"<br/><b>PCT:</b>", matchedplots$pctname, "<br/><b>PCT id:</b>", matchedplots$pctid,"<br/><b>PCT assignment category:</b>",matchedplots$pctassignmentcategory ))%>%
      
      addCircles(radius= 50, lat = ~unmatchedplots$lat, lng = ~unmatchedplots$long, layerId = ~unmatchedplots$siteno, color = ~UnMatchedCol(unmatchedplots$pctid),  fillOpacity = 0.5, group = "Unmatched plots",
                 data = unmatchedplots, popup = ~paste("<b>Survey name:</b>", unmatchedplots$surveyname  ,"<br/><b>Site no:</b>",unmatchedplots$siteno,"<br/><b>Lat:</b>",unmatchedplots$lat," <b>Long:</b>",unmatchedplots$long,"<br/><b>PCT:</b>", unmatchedplots$pctname, "<br/><b>PCT id:</b>", unmatchedplots$pctid,"<br/><b>PCT assignment category:</b>",unmatchedplots$pctassignmentcategory))%>%
                     hideGroup("Unmatched plots")%>%
            addLayersControl(
              overlayGroups = c("Unmatched plots"),
              options = layersControlOptions(collapsed = FALSE)
            )
                  
    }else{
      leaflet(data=pctplotsdata)%>% 
        addProviderTiles(providers$Esri.WorldTopoMap)%>%
        clearShapes() %>%
        clearMarkers()%>%
        setView(lat =-33.819775 , lng =150.994018 , zoom = 11) %>%
        addCircles(radius= 100, lat = ~pctplotsdata$lat, lng = ~pctplotsdata$long, layerId = ~pctplotsdata$siteno, color = ~RdYlBu(pctplotsdata$pctid),fillColor =~RdYlBu(pctplotsdata$pctid),   fillOpacity = 0.7,opacity = 1,
                   data = pctplotsdata, popup = ~paste("<b>Survey name:</b>", pctplotsdata$surveyname  ,"<br/><b>Site no:</b>",pctplotsdata$siteno,"<br/><b>Lat:</b>",pctplotsdata$lat,"<b>Long:</b>",pctplotsdata$long,"<br/><b>PCT:</b>", pctplotsdata$pctname, "<br/><b>PCT id:</b>", pctplotsdata$pctid,"<br/><b>PCT assignment category:</b>",pctplotsdata$pctassignmentcategory))
      
    }
  })

  
  pctplots <- reactiveValues(data = NULL)
  observe({


    # Initialize a temporary in memory database and copy a data.frame into it
    con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
    
    SQLQuery<-paste0("SELECT * FROM fsdata WHERE lat is not NULL ORDER BY pctid")
    
    if ((!is.null(match_data$matches))&&(check_infile()$env_present)) {

      SQLQuery<-paste0("SELECT * FROM fsdata WHERE (lat>=",min(filteredData()$Latitude)-0.1,
                                    " AND lat<=",max(filteredData()$Latitude)+0.1,")
                                    AND (long>=",min(filteredData()$Longitude)-0.1," AND long<=",max(filteredData()$Longitude)+0.1,")  ORDER BY pctid")
    }
    
  
    rs <- dbSendQuery(con, SQLQuery)
    pctplots$data <- dbFetch(rs)
    dbHasCompleted(rs)
    dbClearResult(rs)
    # clean up
    dbDisconnect(con)

  })

  
  
  
  observeEvent(input$lnkUserGuide, {
    loggit("INFO","download user guide", log_detail="download user guide", event = "download",  sessionid=isolate(session$token), echo = FALSE)
  })
  
  observeEvent(input$linkPaperMethodology, {
    loggit("INFO","download method paper", log_detail="download method paper", event = "download",  sessionid=isolate(session$token), echo = FALSE)  
  })
  observeEvent(input$linkVISFloraSurveyPublic, {
    loggit("INFO","link to VIS Flora Survey", log_detail="link to VIS Flora Survey", event = "link",  sessionid=isolate(session$token), echo = FALSE)  
  })
  observeEvent(input$linkVegClassfnPublic, {
    loggit("INFO","link to Vegetation Classification", log_detail="link to Vegetation Classification",  sessionid=isolate(session$token), event = "link", echo = FALSE)  
  })
  observeEvent(input$linkDownloadSampleData, {
    loggit("INFO","download download sample csv", log_detail="link to download sample csv", event = "download",  sessionid=isolate(session$token), echo = FALSE)  
  })
  

  
  session$onSessionEnded(function(){    loggit("INFO", "session has ended",log_detail="session has ended", sessionid=isolate(session$token), echo = FALSE)   })


  
  
})




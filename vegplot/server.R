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
library(stringr)
library(future)

 set_logfile("www/applog.json")

source("functions.R")


# options(shiny.sanitize.errors = TRUE)
# #options(shiny.error = browser)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)


# Define server logic 

shinyServer(function(input, output,session) {
  
  
  #session$allowReconnect(TRUE)
  
  
 
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
 
  loggit("INFO","session started", log_detail="session has started", event = "session start",sessionid=isolate(session$token), echo = FALSE)
  
 
  get_example_data <- reactive({
    read.csv("www/ENSWPlotToPCTAssignmentTool_SampleData.csv", check.names = FALSE)
  })
  
  bionetappinfo<-get_appInfo()
  
  output$linkDownloadSampleData <- downloadHandler(
    filename = function() {
      paste("ENSWPlotToPCTAssignmentTool_SampleData.csv")
    },
    content = function(file) {
          write.csv(get_example_data(), file, row.names = F)
    }
  )
  
  output$DownloadSampleData2 <- downloadHandler(
    filename = function() {
      paste("ENSWPlotToPCTAssignmentTool_SampleData.csv")
    },
    content = function(file) {
      write.csv(get_example_data(), file, row.names = F)
    }
  )
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 
  
  observe({
    if (!is.null(match_data$matches)){
      shinyjs::enable("download_cent_matches")
      shinyjs::enable("PCTProfileData")
      shinyjs::enable("download_char_matches")
      shinyjs::enable("linkDownloadDataCheckReport")
      shinyjs::enable("PCTSppGFData")
      
    }else{
      shinyjs::disable("download_cent_matches")
      shinyjs::disable("PCTProfileData")
      shinyjs::disable("download_char_matches")
      shinyjs::disable("linkDownloadDataCheckReport")
      shinyjs::hide("PCTSubmit")
      shinyjs::hide("ViewPCTMap")
      shinyjs::hide("PCTSubmit2")
      shinyjs::hide("ViewPCTMap2")
      shinyjs::disable("PCTSppGFData")
    }
    
    if (!is.null(match_data$matches$env_data)) {
      
      shinyjs::enable("download_combo_data")
    }else{
     
      shinyjs::disable("download_combo_data")
    }
    
   
    
    
    
    
  })
  
  
  
  get_appInfo<- reactive({
    
    fp<- file.path(getwd(), "data")
    normalizePath(fp)
    fidb<-file.info(list.files(path=fp,pattern="pctdatadb.sqlite$", full.names=TRUE))
    fi<-file.info(list.files(path=fp,pattern="bionetapp.info$", full.names=TRUE))
    
    applasteupdated <-as.character(fi$mtime,"%d/%m/%Y")
    dblasteupdated <-as.character(fidb$mtime,"%d/%m/%Y")
    
    list_data <- list(c(applasteupdated), c(dblasteupdated))
    
    # Give names to the elements in the list.
    names(list_data) <- c("AppLastUpdated", "DataLastUpdated")
    
    return(list_data)
  })
  
 
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # NORMAL APP FUNCTION - USER SUPPLIED DATA --------------------------------
  
  # print out some stats to know we're in business
  check_infile <- reactive({
    out_list<-NULL
    tryCatch({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    infile_df <- read.csv(inFile$datapath,
                          header = T,
                          stringsAsFactors = F)
    names(infile_df)[1] <- "sites"
    
    allfiledata<-readLines(inFile$datapath)
    if (nrow(data.frame(str_split(toString(allfiledata[1]),",")))<=1){
      errmsg <- paste(Sys.time(), ", Error: Uploaded data file is not comma separated. Please check acceptable file formats.")
      message(errmsg)
      showNotification(errmsg,duration = 30,type = c("error"))
      write(paste0(errmsg," file:",inFile$name), paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      return(NULL)
    }
    
    
    env_present <- all(non_floristic %in% names(infile_df))
    xcolmissing<-all(c("Latitude","Longitude","Elevation","RainfallAnn","TempAnn") %in% names(infile_df))
    floristics <- select(infile_df, -sites, -one_of(non_floristic))
    sites_floristics<-select(infile_df, -one_of(non_floristic))
    if (env_present==TRUE){
      env_data <- select(infile_df, sites, one_of(non_floristic), -X)
    }
    if ((env_present==FALSE)&&(xcolmissing==TRUE)){
      errmsg <- paste(Sys.time(), ", Error: data is incorrect format. Check sample data for correct format.")
      message(errmsg)
      showNotification(errmsg,duration = 30,type = c("error"))
      write(paste0(errmsg," file:",inFile$name), paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      return(NULL)
    }
    
    # blank cell data
    if (length(which(is.na(sites_floristics)))>0) {
      errmsg <- paste(Sys.time(), ", Error: Cell(s) ",toString(which(is.na(sites_floristics)))," have no data.")
      message(errmsg)
      showNotification(errmsg,duration = 30,type = c("error"))
      write(paste0(errmsg," file:",inFile$name), paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      return(NULL)
    }
    
    # cell data is not numeric
    t<-sapply(floristics, typeof)
    if (length(which(t=="character"))>0) {
      errmsg <- paste(Sys.time(), ", Error: Species cells ",toString(which(t=="character"))," have non-numeric data.")
      message(errmsg)
      showNotification(errmsg,duration = 30,type = c("error"))
      write(paste0(errmsg," file:",inFile$name), paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      return(NULL)
    }
    
    # no species for site
    x<-floristics %>% mutate(sum =rowSums(floristics[1:length(floristics)],na.rm = T))
    if (length(which(x$sum==0))>0) {
      errmsg <- paste(Sys.time(), ", Error: Row(s) ",toString(which(x$sum==0))," have no species for their respective sites.")
      message(errmsg)
      showNotification(errmsg,duration = 30,type = c("error"))
      write(paste0(errmsg," file:",inFile$name), paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      return(NULL)
    }
    
    # cell data has value > 6 check for 'Sites with values outside cover-abundance score range' (range = 0-6 inclusive);
    if (length(which(between(floristics,7,100)))>0) {
      errmsg <- paste(Sys.time(), ", Error: Cell(s) ",toString(which(between(floristics,7,1000)))," have data with values outside cover-abundance score range (range = 0-6 inclusive)")
      message(errmsg)
      showNotification(errmsg,duration = 30,type = c("error"))
      write(paste0(errmsg," file:",inFile$name), paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      return(NULL)
    }
    
   
    
    out_list <- list(sites = infile_df[,1],
                     floristics = floristics,
                     missing_species = names(floristics)[!names(floristics) %in% colnames(centroids)],
                     env_present = env_present
                     )
                     #infile_df = infile_df)
    
    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 30,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
      return(NULL)
    }, finally = {
      #message("Some other message at the end")
    })
    
    return(out_list)
    
    
  })
  
  
 
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # upload information
 
  uploadinfo <- reactive({
    
    tryCatch({
    
    HTMLResult <-paste0("<p style='padding:10px'><h4>Upload Information:</h4>
                    You have not uploaded any data yet (you can download the example data to test drive).</p>") 
    
    if (!is.null(check_infile())) {
      
      numplots <- get_numplots(length(check_infile()$sites))
     
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
      
      numsitestext <- "Number of sites (rows) with spatial and environmental data detected"
      
      if ((numplotsWithEnvData>0)&&(numplotsWithSpatialData==0)){
        numsitestext<-"Number of sites (rows) with spatial data detected"
        
      }

      numspecies <- ncol(check_infile()$floristics)
   
        HTMLResult<- paste0("<p style='padding:10px'><h4>Uploaded data information:</h4>
                    <table border='1px' style='width:100%;'>
                    <tr>
                    <td style='width:55%'>Number of sites (rows)</td>
                    <td>", numplots ,"</td>
                    </tr>
                    <tr>
                    <td>Number of species (columns)</td>
                    <td>", numspecies ,"</td>
                    </tr>
                    <tr>
                    <td>",numsitestext,"</td>
                    <td>", numplotsWithEnvData ,"</td>
                    </tr>
                    </table></p>")
        
        
        txtResult<-paste0("Number of plots (rows):", numplots ,", Number of species (columns):", numspecies ,", Number of plots (rows) with environmental data detected:", numplotsWithEnvData ,"
                   , Number of plots (rows) with spatial data detected:", numplotsWithSpatialData)
        
        loggit("INFO","uploadresults", log_detail=txtResult,Numplots=numplots, Numspecies=numspecies, NumplotsWithEnvData=numplotsWithEnvData,NumplotsWithSpatialData=numplotsWithSpatialData, event = "upload", sessionid=isolate(session$token), echo = FALSE)
        
        
        }
    
    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 30,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
      return(NULL)
    }, finally = {
      #message("Some other message at the end")
    })
    
    return(list(uploadresults=HTMLResult))
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  #upload information htmloutput
  output$uploadInformation <- renderUI({
    if (!is.null(check_infile())) {
      HTML(isolate(uploadinfo()$uploadresults))
    }

  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  #dataChecksInfo
  dataChecksInfo <-reactive({
    
    tryCatch({
      
    
    
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
      
      
      txtResult<-toString(paste0("Species names not found in eastern NSW PCT standardised taxonomy:", missingSpeciesList ,", Sites outside eastern NSW study region:", numplotsOutsideStudyHTML))
      loggit("INFO","uploadresults", log_detail=txtResult, MissingSpeciesList=toString(missingSpeciesList), NumplotsOutsideStudy=toString(numplotsOutsideStudyHTML), event = "upload", sessionid=isolate(session$token), echo = FALSE)
      

    }
    
   
    
    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 30,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
      return(NULL)
    }, finally = {
      #message("Some other message at the end")
    })


    return(list(results=paste0("<p style='padding:10px; clear: both;'><h4>Uploaded data checks:</h4>
                                  <table border='1px' style='width:100%; padding:5px;'>
                                   <tr>
                                   <td style='width:45%; vertical-align:top'>Species names not found in eastern NSW PCT standardised taxonomy</td>
                                   <td>", missingSpeciesList ,"</td>
                                   </tr>
                                   <tr>
                                   <td>Sites outside eastern NSW study region</td>
                                   <td>", numplotsOutsideStudyHTML ,"</td>
                                   </tr>
                                   </table></p>"),
                                    fileresult=txtResult))
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
    
    tryCatch({
    
    tic <- Sys.time()
    match_data$matches<-NULL
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
    
    if ((is.null(check_infile()$env_present))||(is.null(check_infile()$floristics))){
      showNotification("Please check you upload data.",duration = 20,
                       type = 'error')
      return(NULL)
    }
    
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
    ##ord_sites <- make_sites_ord(select(infile_df, -one_of(non_floristic)))
    # add data to the reactive output
    match_data$matches <- list(char_matches = char_matches,
                               cent_matches = cent_matches,
                               ##ord_sites = list(ord = ord_sites, sites = infile_df[,1]),
                               mc_cores = attr(cent_matches, "mc_cores"),
                               compute_time = round(as.numeric(difftime(Sys.time(), tic, units = "secs"))),
                               env_data = env_data)
    
    #match_data$env_data <- list(env_data = env_data)
    
    
   
    
    progress$set(message = "Compiling matches", value = 0.95)
    
    showNotification(paste0("Analysis complete. CPU cores used: ", match_data$matches$mc_cores,
                            " (analysis took ", match_data$matches$compute_time, " seconds)."),duration = 10,type = c("message"))
    
    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 30,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
      return(NULL)
    }, finally = {
      #message("Some other message at the end")
    })
    
  })
  
  # info about processing
  output$cores <- renderText({
    if (!is.null(match_data$matches)) {
      paste0("Analysis complete. CPU cores used: ", match_data$matches$mc_cores,
             " (analysis took ", match_data$matches$compute_time, " seconds).")
    } else {
      paste0("Potential CPU cores available: ", detectCores())
    }
  })
  
  
  # info about application
  output$AppLastUpdated <- renderText({
    
    get_appInfo()$AppLastUpdated
    
  })
  
  # info about data
  output$DataLastUpdated <- renderText({
    
    get_appInfo()$DataLastUpdated
    
  })
 
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # now we can return the top n matches based on the slider
  style_matches <- reactive({
    topn <- input$topn
    top_char_matches <- get_topn(match_data$matches$char_matches, topn, T)
    top_cent_matches <- get_topn(match_data$matches$cent_matches, topn, F)
    combined_matches <- as.data.frame(bind_rows(
      mapply(FUN = match_topn,
             as.data.frame(t(top_cent_matches), stringsAsFactors = F),
             as.data.frame(t(top_char_matches), stringsAsFactors = F),
             MoreArgs = list(topn = topn), SIMPLIFY = F)
      ))
    cent_groups <- top_cent_matches[c(1,grep("PCT_Match", names(top_cent_matches)))]
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
      
      return(list(env_thresholds = threshold_results[order(threshold_results$Site_No),]))
    } else {
      return(NULL)
    }
  })
 
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  output$char_table <- renderDataTable({ 
      if (!is.null(match_data$matches)) {
      ar<-array(style_matches()$char)[[1]]$data %>% rename(Row_No = names(array(style_matches()$char)[[1]]$data)[1])
      names(ar) <- gsub("Distance_to_Centroid", "%_Char_Spp", names(ar))
      datatable(ar, selection=list(mode="single",target="cell"), options=list(columnDefs = list(list(visible=FALSE, targets=c(0)),list(targets=c(1), visible=TRUE, width='20px'))), callback = JS("	
	table.on('mouseenter', 'td', function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
	    	   var idx = table.cell( this ).index().column;
    		var title = table.column( idx ).header();
        var pos = title.innerHTML.search('PCT_Match');
	    	if (pos>=0){
         table.column( idx ).nodes().to$().css({cursor: 'pointer'});
	    	}
	});    ")) %>%
      formatStyle(grep("%_Char_Spp", names(ar)), 
                  backgroundColor = styleInterval(cuts = c(25,60), 
                                                  values = c("white","#ccd6bc","#99b964"))
      )
    }
        
  })
  
  output$cent_table <- renderDataTable({
    if (!is.null(match_data$matches)) {
      ar<-array(style_matches()$cent)[[1]]$data %>% rename(Row_No = names(array(style_matches()$cent)[[1]]$data)[1])
      datatable(ar, selection=list(mode="single",target="cell"), options=list(columnDefs = list(list(visible=FALSE, targets=c(0)),list(targets=c(1), visible=TRUE, width='20px'))), callback = JS("	
	table.on('mouseenter', 'td', function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
	    	   var idx = table.cell( this ).index().column;
    		var title = table.column( idx ).header();
        var pos = title.innerHTML.search('PCT_Match');
	    	if (pos>=0){
         table.column( idx ).nodes().to$().css({cursor: 'pointer'});
	    	}
	});    ") ) %>%
      formatStyle(grep("Distance_to_Centroid", names(array(style_matches()$cent)[[1]]$data)), 
                  backgroundColor = styleInterval(cuts = c(0.695,0.69501), 
                                                  values = c("#99b964","white","white"))
      )
    }
  })
  
  
  output$env_thresholds <- renderDataTable({
    if (!is.null(match_data$matches$env_data)) {
      
      shinyjs::hide("EnvDataViewMessage")
      dtfET<-datatable(style_env_thresholds()$env_thresholds)
      ETdata<-dtfET$x$data %>% rename(Row_No = names(dtfET$x$data)[1], PCT_Match=names(dtfET$x$data)[3], PCT_ID=names(dtfET$x$data)[4])
      ETdata<-transform(ETdata, Row_No = as.numeric(Row_No))
      dta<-style_matches()$cent$x$data
      dt<-style_matches()$cent$x$data %>% select(starts_with("Distance_to_Centroid"))
      
      SQLString<-""
      for (i in 1:length(dt)){
        if (i==length(dt)){
          SQLString<-paste0(SQLString,"select Site_No, Distance_to_Centroid",i," as Distance_to_Centroid, PCT_Match",i," as PCT_ID from dta")
        }else{
          SQLString<-paste0(SQLString,"select Site_No, Distance_to_Centroid",i," as Distance_to_Centroid, PCT_Match",i," as PCT_ID from dta union ")
        } 
      }
      centdta<-sqldf(SQLString)
      dtfinal2<-merge(ETdata,centdta,by.x=c("Site_No","PCT_ID"),by.y=c("Site_No","PCT_ID"))
      
      #reorder columns and hide numbers 0 
      dtfinal2<-dtfinal2[c(3,1,2,4,8,5,6,7,0)]
      setorder(dtfinal2,Site_No,Distance_to_Centroid)
      
      datatable(dtfinal2 , options=list(columnDefs = list(list(visible=FALSE, targets=c(0)  ),list(targets=c(1), visible=TRUE, width='20px'))) ) %>%
        formatStyle(c("Rainfall","Elevation","Temperature"), 
                    backgroundColor = styleEqual(levels = c("Above","Below","Within"), 
                                                 values = c("white","white","#99b964"),default = "white")
        )
      
      
      
    } else{ 
      shinyjs::show("EnvDataViewMessage") 
      
    }
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  # Allow user to download the matches (topn they've decided)
  download_matches <- reactive({
    topn <- input$topn
    redata<- NULL
    
    if (is.null(match_data$matches$env_data))
    { redata<- list(char = get_topn(match_data$matches$char_matches, topn, T),
                    cent = get_topn(match_data$matches$cent_matches, topn, F),
                    env = NULL)}
    else{
    
    redata<- list(char = get_topn(match_data$matches$char_matches, topn, T),
                cent = get_topn(match_data$matches$cent_matches, topn, F),
                env = check_env_thresholds(style_matches()$cent_groups, env_thresh, match_data$matches$env_data))
    }
    
    return(redata)
  })
  
  output$download_char_matches <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_char-matches", ".csv", sep = "")
    },
    content = function(file) {
      
     
        
    #  arCHARDATA<-array(style_matches()$char)[[1]]$data %>% rename(Row_No = names(array(style_matches()$char)[[1]]$data)[1])
      
      
      charmatches<-download_matches()$char
      names(charmatches) <- gsub("Distance_to_Centroid", "%_Char_Spp", names(charmatches))
      charmatches<-reorder_data(charmatches)
   
      myvar <- format(Sys.Date(), format="%d/%m/%Y")
      #Exported from NSW Plot to PCT assignment tool on dd/mm/yyyy. PCT data last updated dd/mm/yyyy
      out_string <- paste0("Exported from NSW Plot to PCT assignment tool on ",myvar,". PCT data last updated ", bionetappinfo$DataLastUpdated  ,"\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      
      
    # reorder_data(arCHARDATA)
      
      withProgress(message = 'Processing....',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      
      future({
      fwrite(x = charmatches,
             file= file,
             sep = ',',
             col.names=T,
             append=T)
      })
      
      loggit("INFO","download char_matches", log_detail="download char_matches", event = "download", sessionid=isolate(session$token), echo = FALSE) 
      
    
      
    })
  
  
  output$download_cent_matches <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_cent-matches", ".csv", sep = "")
    },
    content = function(file) {
   
      myvar <- format(Sys.Date(), format="%d/%m/%Y")
      #Exported from NSW Plot to PCT assignment tool on dd/mm/yyyy. PCT data last updated dd/mm/yyyy
      
      centmatches<-download_matches()$cent
      centmatches<-reorder_data(centmatches)
      out_string <- paste0("Exported from NSW Plot to PCT assignment tool on ",myvar,". PCT data last updated ", bionetappinfo$DataLastUpdated  ,"\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      
      
      withProgress(message = 'Processing....',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      
      future({
      
      fwrite(x = centmatches,
             file= file,
             sep = ',',
             col.names=T,
             append=T)
      })
      
      
      loggit("INFO","download centroid_matches", log_detail="download centroid_matches", event = "download", sessionid=isolate(session$token), echo = FALSE) 
      
      #write.csv(download_matches()$cent, file, row.names = F)
    }
    
  )
  
  

  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  get_PCTProfile_data <- reactive({
    if (!is.null(match_data$matches)){
      
      
      dt<-style_matches()$cent$x$data %>% select(starts_with("PCT_Match"))
      SQLString<-""
      for (i in 1:length(dt)){
        
        if (i==length(dt)){
          SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt")
        }else{
          SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt union ")
        } 
      }
      matchedpcts<-sqldf(SQLString)
      pctdt<-pctprofiles$data
      allpctprofiles<-sqldf("select * from pctdt where pctid in (SELECT pctid FROM matchedpcts)")
      
      #Maximum_Elevation_(m)	Minimum_Elevation_(m)	Median_Elevation_(m)	Maximum_Annual_Rainfall_(mm)	Minimum_Annual_Rainfall_(mm)	
      #Median_Annual_Rainfall_(mm)	Maximum_Annual_Mean_Temperature_(°C)	Minimum_Annual_Mean_Temperature_(°C)	Median_Annual_Mean_Temperature_(°C)
      
      
      allpctprofiles<-allpctprofiles %>% rename(PCT_ID = PCTID, PCT_Name=PCTName, Vegetation_Description=Vegetation_description,
                                                Classification_Confidence_Level=Classification_confidence_level,
                                                Number_of_Primary_Replicates=Number_of_Primary_replicates,
                                                Number_of_Secondary_Replicates=Number_of_Secondary_replicates,
                                                "Maximum_Elevation_(m)"=Elevation_max,
                                                "Minimum_Elevation_(m)"=Elevation_min,
                                                "Median_Elevation_(m)"=Elevation_median,
                                                "Maximum_Annual_Rainfall_(mm)"=Rainfall_max,
                                                "Minimum_Annual_Rainfall_(mm)"=Rainfall_min,
                                                "Median_Annual_Rainfall_(mm)"=Rainfall_median,
                                                "Maximum_Annual_Mean_Temperature_(deg.C)"=Temperature_max,
                                                "Minimum_Annual_Mean_Temperature_(deg.C)"=Temperature_min,
                                                "Median_Annual_Mean_Temperature_(deg.C)"=Temperature_median,
                                                TEC_List=TEC_list,Median_Native_Species_Richness=Median_species_richness)
      
      return(allpctprofiles)
    }
    
  })
  
  output$PCTProfileData <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_pctprofile_data", ".csv", sep = "")
    },
    content = function(file) {
      # 
   
      myvar <- format(Sys.Date(), format="%d/%m/%Y")
      #Exported from NSW Plot to PCT assignment tool on dd/mm/yyyy. PCT data last updated dd/mm/yyyy
      out_string <- paste0("Exported from NSW Plot to PCT assignment tool on ",myvar,". PCT data last updated ", bionetappinfo$DataLastUpdated  ,"\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      withProgress(message = 'Processing....',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      
      future({
          fwrite(x = get_PCTProfile_data(),
                 file= file,
                 sep = ',',
                 col.names=T,
                 append=T)
      })
      
      loggit("INFO","download PCTProfile_data", log_detail="download PCTProfile_data", event = "download",  sessionid=isolate(session$token), echo = FALSE)
      
     # write.csv(get_PCTProfile_data(), file, row.names = F)
    },
    contentType="text/csv"
   
  )
  
  
  get_combo_data <- reactive({
    if (!is.null(match_data$matches$env_data)) {
      dtfET<-datatable(style_env_thresholds()$env_thresholds)
      ETdata<-dtfET$x$data %>% rename(Row_No = names(dtfET$x$data)[1], PCT_Match=names(dtfET$x$data)[3], PCT_ID=names(dtfET$x$data)[4])
      ETdata<-transform(ETdata, Row_No = as.numeric(Row_No))
      dta<-style_matches()$cent$x$data
      dt<-style_matches()$cent$x$data %>% select(starts_with("Distance_to_Centroid"))
      
      SQLString<-""
      for (i in 1:length(dt)){
        if (i==length(dt)){
          SQLString<-paste0(SQLString,"select Site_No, Distance_to_Centroid",i," as Distance_to_Centroid, PCT_Match",i," as PCT_ID from dta")
        }else{
          SQLString<-paste0(SQLString,"select Site_No, Distance_to_Centroid",i," as Distance_to_Centroid, PCT_Match",i," as PCT_ID from dta union ")
        } 
      }
      centdta<-sqldf(SQLString)
      dtfinal2<-merge(ETdata,centdta,by.x=c("Site_No","PCT_ID"),by.y=c("Site_No","PCT_ID"))
      
      pctdt<-pctprofiles$data
      pctprofiles<-sqldf("select pctid AS PCT_ID ,pctname AS PCT_Name from pctdt where pctid in (SELECT PCT_ID FROM centdta)")
      
      dtfinal2<-merge(dtfinal2,pctprofiles,by.x=c("PCT_ID"),by.y=c("PCT_ID"))
      
      #reorder columns and hide numbers 0 and PCT_Match
      dtfinal2<-dtfinal2[c(3,2,1,9,4,8,5,6,7)]
      setorder(dtfinal2,Site_No,PCT_Match)
      
      
      return(dtfinal2)
    }
  })
  
  output$download_combo_data <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_combined_centroid_environmental_PCT_summary_data", ".csv", sep = "")
    },
    content = function(file) {
      # 
      
      myvar <- format(Sys.Date(), format="%d/%m/%Y")
      #Exported from NSW Plot to PCT assignment tool on dd/mm/yyyy. PCT data last updated dd/mm/yyyy
      out_string <- paste0("Exported from NSW Plot to PCT assignment tool on ",myvar,". PCT data last updated ", bionetappinfo$DataLastUpdated  ,"\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      withProgress(message = 'Processing....',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      
      future({
        fwrite(x = get_combo_data(),
               file= file,
               sep = ',',
               col.names=T,
               append=T)
      })
      
      loggit("INFO","download download_combo_data", log_detail="download download_combo_data", event = "download",  sessionid=isolate(session$token), echo = FALSE)
      
      
    },
    contentType="text/csv"
    
  )
  
  #///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  get_PCTSpeciesGrowthforms_data <- reactive({
    if (!is.null(match_data$matches)){
      
      
      dt<-style_matches()$cent$x$data %>% select(starts_with("PCT_Match"))
      SQLString<-""
      for (i in 1:length(dt)){
        
        if (i==length(dt)){
          SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt")
        }else{
          SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt union ")
        } 
      }
      matchedpcts<-sqldf(SQLString)
      
      con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")      
      
      rs <- dbSendQuery(con, paste0("SELECT PCT_ID, Scientific_name, Group_score_median, Group_frequency, GrowthFormGroup,
                        CASE GrowthFormGroup 
                                 WHEN 'Tree (TG)' THEN 1 
                                 WHEN 'Shrub (SG)' THEN 2
                                 WHEN 'Fern (EG)' THEN 3
                                 WHEN 'Grass & grasslike (GG)' THEN 4
                                 WHEN 'Forb (FG)' THEN 5           
                                 ELSE 6
                             END GGroupOrder
                         FROM pctspeciesgrowthforms order by GGroupOrder asc, Group_frequency desc"))
      pctdt <- dbFetch(rs)
      dbHasCompleted(rs)
      dbClearResult(rs)
      
      # clean up
      dbDisconnect(con)
      
      
      # PCT_ID, Group_Score_Median, Group_Frequency, Growth_Form_Group.
      
      allpctsppgfs<-sqldf("select PCT_ID, Scientific_name as Scientific_Name, Group_score_median as Median_Cover_Score, Group_frequency as Species_Frequency, GrowthFormGroup as Growth_Form_Group from pctdt where PCT_ID in (SELECT pctid FROM matchedpcts) order by PCT_ID, GGroupOrder asc, Group_frequency desc")
      
      
      
      return(allpctsppgfs)
    }
    
  })
  
  output$PCTSppGFData <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_pctspeciesgrowthforms_data", ".csv", sep = "")
    },
    content = function(file) {
      # 
      
      myvar <- format(Sys.Date(), format="%d/%m/%Y")
      #Exported from NSW Plot to PCT assignment tool on dd/mm/yyyy. PCT data last updated dd/mm/yyyy
      out_string <- paste0("Exported from NSW Plot to PCT assignment tool on ",myvar,". PCT data last updated ", bionetappinfo$DataLastUpdated  ,"\n", "=================\n")
      cat(out_string, file = file, sep = '\n')
      
      withProgress(message = 'Processing....',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      
      future({
          fwrite(x = get_PCTSpeciesGrowthforms_data(),
                 file= file,
                 sep = ',',
                 col.names=T,
                 append=T)
      })
        
      loggit("INFO","download PCTSppGFData", log_detail="download PCTSppGFData", event = "download",  sessionid=isolate(session$token), echo = FALSE)
      
      
    },
    contentType="text/csv"
    
  )
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  
  
  ### <-- need to add in download for envrionmental thresholds
  
  getDataCheckReport <- reactive({
    return (isolate(dataChecksInfo()$fileresult))
  })
 
  output$linkDownloadDataCheckReport <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file1$name), "_data_check_report", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getDataCheckReport(), file, row.names = F)
    }
   
  )
  
  

 
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  output$PCTName <- renderUI({
    
    if (!is.null(input$cent_table_cell_clicked$col))
    {
      ar<-array(style_matches()$cent)
      columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
      
      if (!columnName==""){
        
        if (substr(columnName,1,nchar(columnName)-1)=="PCT_Match"){
          shinyjs::show("PCTSubmit")
          
          # if (!is.null(match_data$matches$env_data)) {
          #   shinyjs::show("ViewPCTMap")
          # }else{shinyjs::hide("ViewPCTMap")}
          shinyjs::show("ViewPCTMap")
          
        pctname<-getPCTName(input$cent_table_cell_clicked$value)
        span(paste0(" PCT Name: ",pctname))
        
        }else{ span("")
          shinyjs::hide("PCTSubmit")
          shinyjs::hide("ViewPCTMap")
          }
    }else{
      
      span("")
      shinyjs::hide("PCTSubmit")
      shinyjs::hide("ViewPCTMap")
    }
    }else{
      
      span("")
      shinyjs::hide("PCTSubmit")
      shinyjs::hide("ViewPCTMap")
    }
    
  })
  
  output$PCTName2 <- renderUI({
    
    if (!is.null(input$char_table_cell_clicked$col))
    {
      ar<-array(style_matches()$char)
      columnName <-colnames(ar[[1]]$data[input$char_table_cell_clicked$col])
      
      if (!columnName==""){
        
        if (substr(columnName,1,nchar(columnName)-1)=="PCT_Match"){
          shinyjs::show("PCTSubmit2")
          
          # if (!is.null(match_data$matches$env_data)) {
          #   shinyjs::show("ViewPCTMap2")
          # }else{shinyjs::hide("ViewPCTMap2")}
          
          shinyjs::show("ViewPCTMap2")
          
          pctname<-getPCTName(input$char_table_cell_clicked$value)
          span(paste0(" PCT Name: ",pctname))
          
        }else{ span("")
          shinyjs::hide("PCTSubmit2")
          shinyjs::hide("ViewPCTMap2")
          }
      }else{
        
        span("")
        shinyjs::hide("PCTSubmit2")
        shinyjs::hide("ViewPCTMap2")
      }
    }else{
      
      span("")
      shinyjs::hide("PCTSubmit2")
      shinyjs::hide("ViewPCTMap2")
    }
    
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  ClickedCentroidPCT <- eventReactive(input$cent_table_cell_clicked,{
    
    tryCatch({
      
   
       if (!is.null(input$cent_table_cell_clicked$col))
        {
          ar<-array(style_matches()$cent)
          columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
          
          if (!columnName==""){
            
            if ((substr(columnName,1,nchar(columnName)-1)=="PCT_Match")||(substr(columnName,1,nchar(columnName)-2)=="PCT_Match")){
            
            pctprofile<-getPCTProfile(input$cent_table_cell_clicked$value)
          
            
          }else{ "" }
        }else{
          
          ""
        }
       }
      
    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 30,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
      return("")
    }, finally = {
      #message("Some other message at the end")
    })
    
  })
  
 
  observeEvent(input$PCTSubmit,{
    
    # if (!is.null(input$cent_table_cell_clicked$col))
    # {
    #   ar<-array(style_matches()$cent)
    #  # columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
    #   
    #   sitename<-ar[[1]]$data$Site_No[input$cent_table_cell_clicked$row]

      #if (!columnName==""){

    
          showModal(modalDialog(title="PCT Profile",fluidPage(br(),ClickedCentroidPCT()),size="l",easyClose = TRUE))
   
      #}
    #}
    
  })
  
  
  observeEvent(input$ViewPCTMap,{

    # if (!is.null(input$cent_table_cell_clicked$col))
    # {
    #   ar<-array(style_matches()$cent)
    #   columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
    #   
    #   if (!columnName==""){
    #     
    #     if ((substr(columnName,1,nchar(columnName)-1)=="PCT_Match")||(substr(columnName,1,nchar(columnName)-2)=="PCT_Match")){

            # sitename<-ar[[1]]$data$Site_No[input$cent_table_cell_clicked$row]
            # pctid<-input$cent_table_cell_clicked$value
    
           showModal(modalDialog(title="PCT Site Map",withSpinner(fluidPage(tags$script("setTimeout(function(){ var map = document.getElementById('mapView'); map.style.visibility='hidden';}, 500);"),
                       tags$script("setTimeout(function(){ var map = document.getElementById('mapView'); map.style.visibility='visible'; }, 1200);"), leafletOutput("mapView", width = "100%", height = "600px"))),size="l",easyClose = TRUE))

    #     }
    # 
    #   }
    # }

  })
  
  observeEvent(input$ViewPCTMap2,{
    
   

    showModal(modalDialog(title="PCT Site Map",withSpinner(fluidPage(tags$script("setTimeout(function(){ var map = document.getElementById('mapView2'); map.style.visibility='hidden';}, 500);"),
                                                                     tags$script("setTimeout(function(){ var map = document.getElementById('mapView2'); map.style.visibility='visible'; }, 12000);"), leafletOutput("mapView2", width = "100%", height = "600px"))),size="l",easyClose = TRUE))

  })

  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  ClickedCharSppPCT <- eventReactive(input$char_table_cell_clicked,{
    
    tryCatch({
    
          if (!is.null(input$char_table_cell_clicked$col))
          {
            ar<-array(style_matches()$char)
            columnName <-colnames(ar[[1]]$data[input$char_table_cell_clicked$col])
            
            if (!columnName==""){
              
              if ((substr(columnName,1,nchar(columnName)-1)=="PCT_Match")||(substr(columnName,1,nchar(columnName)-2)=="PCT_Match")){
                
                pctprofile<-getPCTProfile(input$char_table_cell_clicked$value)
                HTML(paste0(pctprofile))
                
              }else{""}
            }else{
              
              ""
            }
          }
    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 20,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
      return("")
    }, finally = {
      #message("Some other message at the end")
    })
    
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
    #     if (substr(columnName,1,nchar(columnName)-1)=="PCT_Match"){
          showModal(modalDialog(title="PCT Profile data", fluidPage(br(),ClickedCharSppPCT()),size = "l",easyClose = TRUE))
    #     }
    #   }
    # }
    
  })
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  

  
  ## DATA FILE UPDATES FROM WEB SERVICES
  observe({

    tryCatch({

        # fp<- file.path(getwd(), "data")
        # normalizePath(fp)
        # fi<-file.info(list.files(path=fp,pattern="pctdatadb.sqlite$", full.names=TRUE))
        # nhours<-as.numeric(difftime(Sys.time(),fi$ctime , units="hours"))
        
        
        ## parseQueryString("?updateBioNet=1&bar=pctupdate")
      # ?updateBioNet=1&bar=pctupdate
        query <- parseQueryString(session$clientData$url_search)
        
        updateOneKey<-0
        updateTwoKey<-0
        
        
        if (length(query)>0){
        
          # Ways of accessing the values
          if (as.numeric(query$updateBioNet) == 1) {
            # Do something
            updateOneKey<-1
          }
          if (query[["bar"]] == "pctupdate") {
            # Do something else
            updateTwoKey<-1
          }
        }
        
        if ((updateTwoKey==1)&&(updateOneKey==1)){

           ##if ((nhours>=1000000)||(length(nhours)==0)){
          
          
          progress <- shiny::Progress$new(style = "notification")
          progress$set(message = "Loading flora survey data", value = 0.15)
          on.exit(progress$close())
          
          showNotification(paste0("Loading flora survey data"),duration = 20,type = c("message"))


          ## PCT DATA UPDATES FOR FLORA SURVEY
          
          fjs <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/SystematicFloraSurvey_SiteData?$select=siteID,%20currentClassification,%20currentClassificationDescription,%20surveyName,%20PCTAssignmentCategory,%20decimalLatitude,%20decimalLongitude,%20visitNo,%20ElevationInMeters,%20annualRainfallInMillimeters,%20annualMeanTemperatureInCelsius")
          
          # n<-as.integer(length(pctdata$value))
          # Initialize a temporary in memory database and copy a data.frame into it
          con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
          dbExecute(con,"DELETE FROM fsdata")
          ## for (i in 1:n){
          
          fjs_df<-as.data.frame(fjs$value, stringsAsFactors = F)
          
          names(fjs_df)[names(fjs_df) == "siteID"] <- "siteno"
          names(fjs_df)[names(fjs_df) == "currentClassification"] <- "pctid"
          names(fjs_df)[names(fjs_df) == "currentClassificationDescription"] <- "pctname"
          names(fjs_df)[names(fjs_df) == "PCTAssignmentCategory"] <- "pctassignmentcategory"
          names(fjs_df)[names(fjs_df) == "surveyName"] <- "surveyname"
          names(fjs_df)[names(fjs_df) == "decimalLatitude"] <- "lat"
          names(fjs_df)[names(fjs_df) == "decimalLongitude"] <- "long"
          names(fjs_df)[names(fjs_df) == "visitNo"] <- "replicatenumber"
          names(fjs_df)[names(fjs_df) == "ElevationInMeters"] <- "elevation"
          names(fjs_df)[names(fjs_df) == "annualRainfallInMillimeters"] <- "rainfall"
          names(fjs_df)[names(fjs_df) == "annualMeanTemperatureInCelsius"] <- "temp"
          
          
          dbWriteTable(con, "fsdata",fjs_df, overwrite=F, append=T)
          
        
          
          
          ## pct profile data
          progress$set(message = "Loading classification data", value = 0.25)
          
          showNotification(paste0("Loading classification data"),duration = 20,type = c("message"))
          
          pctjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/VegetationClassification_PCTDefinition?$select=PCTID,PCTName,vegetationDescription,classificationConfidenceLevel,numberOfPrimaryReplicates,numberOfSecondaryReplicates,vegetationFormation,vegetationClass,IBRASubregion,maximumElevationInMeters,minimumElevationInMeters,medianElevationInMeters,maximumAnnualRainfallInMillimeters,minimumAnnualRainfallInMillimeters,medianAnnualRainfallInMillimeters,maximumAnnualMeanTemperatureInCelsius,minimumAnnualMeanTemperatureInCelsius,medianAnnualMeanTemperatureInCelsius,TECAssessed,stateTECFitStatus,medianNativeSpeciesRichness")
          
          
          # Initialize a temporary in memory database and copy a data.frame into it
          ##con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
          dbExecute(con,"DELETE FROM pctprofiledata")
          
          
          pctprofiledt_df<-as.data.frame(pctjson$value, stringsAsFactors = F)
          
          names(pctprofiledt_df)[names(pctprofiledt_df) == "vegetationDescription"] <- "Vegetation_description"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "classificationConfidenceLevel"] <- "Classification_confidence_level"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "numberOfPrimaryReplicates"] <- "Number_of_Primary_replicates"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "numberOfSecondaryReplicates"] <- "Number_of_Secondary_replicates"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "vegetationFormation"] <- "Vegetation_Formation"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "vegetationClass"] <- "Vegetation_Class"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "IBRASubregion"] <- "IBRA_Subregion"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "maximumElevationInMeters"] <- "Elevation_max"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "minimumElevationInMeters"] <- "Elevation_min"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "medianElevationInMeters"] <- "Elevation_median"
          
          names(pctprofiledt_df)[names(pctprofiledt_df) == "maximumAnnualRainfallInMillimeters"] <- "Rainfall_max"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "minimumAnnualRainfallInMillimeters"] <- "Rainfall_min"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "medianAnnualRainfallInMillimeters"] <- "Rainfall_median"
          
          names(pctprofiledt_df)[names(pctprofiledt_df) == "maximumAnnualMeanTemperatureInCelsius"] <- "Temperature_max"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "minimumAnnualMeanTemperatureInCelsius"] <- "Temperature_min"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "medianAnnualMeanTemperatureInCelsius"] <- "Temperature_median"
          
          names(pctprofiledt_df)[names(pctprofiledt_df) == "TECAssessed"] <- "TEC_list"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "stateTECFitStatus"] <- "TEC_Act"
          names(pctprofiledt_df)[names(pctprofiledt_df) == "medianNativeSpeciesRichness"] <- "Median_species_richness"
          
          
          dbWriteTable(con, "pctprofiledata",pctprofiledt_df, overwrite=F, append=T)
          
          
          ## pct tec data
          
          
          
          pcttecjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/VegetationClassification_PCTDefinition?$select=PCTID,TECAssessed,stateTECProfileID,stateTECFitStatus,stateTECDegreeOfFit,countryTECProfileID,countryTECFitStatus,countryTECDegreeOfFit")
          
          
          # Initialize a temporary in memory database and copy a data.frame into it
          ##con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
          dbExecute(con,"DELETE FROM pct_tecdata")
          
          
          pctTECdt_df<-as.data.frame(pcttecjson$value, stringsAsFactors = F)
          
          dbWriteTable(con, "pct_tecdata",pctTECdt_df, overwrite=F, append=T)
          
          
         
          progress$set(message = "Loading TEC data", value = 0.50)
          showNotification(paste0("Loading TEC data"),duration = 20,type = c("message"))
          
          ## tec data only
          tecjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/ThreatenedBiodiversity_EcologicalCommunities?$select=profileID,TECName,stateConservation,countryConservation")
          
          
          # Initialize a temporary in memory database and copy a data.frame into it
          ##con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
          dbExecute(con,"DELETE FROM tecdata")
          
          
          TECdt_df<-as.data.frame(tecjson$value, stringsAsFactors = F)
          
          
          
          dbWriteTable(con, "tecdata",TECdt_df, overwrite=F, append=T)
          
      
          
          ## PCT SPP GF data only
          progress$set(message = "Loading PCT Species growth forms data", value = 0.95)
          showNotification(paste0("Loading PCT Species growth form data"),duration = 20,type = c("message"))
          
          spgfjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/VegetationClassification_PCTGrowthForm?$select=PCTID,scientificName,medianCoverScore,speciesFrequency,primaryGrowthFormGroup")
          
          
          # Initialize a temporary in memory database and copy a data.frame into it
          ##con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadbtest.sqlite")
          dbExecute(con,"DELETE FROM pctspeciesgrowthforms")
          
          
          SPGFdt_df<-as.data.frame(spgfjson$value, stringsAsFactors = F)
          
          names(SPGFdt_df)[names(SPGFdt_df) == "PCTID"] <- "PCT_ID"
          names(SPGFdt_df)[names(SPGFdt_df) == "scientificName"] <- "Scientific_name"
          names(SPGFdt_df)[names(SPGFdt_df) == "medianCoverScore"] <- "Group_score_median"
          names(SPGFdt_df)[names(SPGFdt_df) == "speciesFrequency"] <- "Group_frequency"
          names(SPGFdt_df)[names(SPGFdt_df) == "primaryGrowthFormGroup"] <- "GrowthFormGroup"
          
          
          dbWriteTable(con, "pctspeciesgrowthforms",SPGFdt_df, overwrite=F, append=T)
          
          
          dbExecute(con,"update pctspeciesgrowthforms set group_frequency = group_frequency*100")
          
          
          # clean up
          dbDisconnect(con)

          ## End DATA UPDATES ---------------------------------------------------------------------
          showNotification(paste0("Loading data complete"),duration = 20,type = c("message"))

        }

    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 20,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE)

    }, finally = {
      #message("Some other message at the end")
    })

  })

  
  # envdata supply
  filteredData <- reactive({
    if (!is.null(match_data$matches)) {
      if (check_infile()$env_present) {
        return(match_data$matches$env_data)
      }else
      {return(NULL)}
    }else {return(NULL)}
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    pctplotsdata<-pctplots$data

    
    
    colfuncMatched <- colorRampPalette(c("AliceBlue",  "Aquamarine", "Azure", "#ffd236","#fbb83e","#ec783a","#e54c41","#d04730","#782d49",
                                         "Blue", "BlueViolet", "CadetBlue",  "Coral", "CornflowerBlue",
                                         "Cornsilk",  "Cyan", "DarkBlue", "DarkCyan", "DarkGoldenRod", "DarkMagenta", "DarkOrange", "DarkOrchid", "DarkRed", "DarkSalmon", 
                                         "DarkSlateBlue",   "DarkTurquoise", "DarkViolet", "DeepPink", "DeepSkyBlue", 
                                          "DodgerBlue",  "Gold",
                                         "GoldenRod",  "HotPink", "IndianRed",  
                                         "Lavender", "LavenderBlush",  "LemonChiffon", "LightBlue", "LightCoral", "LightCyan", "LightGoldenRodYellow"))
    
    
    
    
    colfuncUnmatched <- colorRampPalette(c( "LightPink", "LightSalmon", "LightSkyBlue","LightSteelBlue", "LightYellow",  "LimeGreen", "Linen", "Magenta", 
                                            "Maroon", "MediumAquaMarine",
                                           "MediumBlue", "MediumOrchid", "MediumPurple",  "MediumSlateBlue", "MediumTurquoise",
                                           "MediumVioletRed", "MidnightBlue", "MintCream", "MistyRose",   "Navy",   
                                           "Orange", "OrangeRed", "Orchid", "PaleGoldenRod", "PaleTurquoise", "PaleVioletRed", "PapayaWhip", "PeachPuff",
                                            "Pink", "Plum", "PowderBlue", "Purple", "RoyalBlue", "Salmon",
                                             "SkyBlue", "SlateBlue","SteelBlue",  "Thistle", "Tomato", "Turquoise", "Violet", "Yellow", "YellowGreen"))
    
    
    
    
    categories<-pctplotsdata$pctid
    #RdYlBu <- colorFactor(colfunc(3000), domain = categories)
    
  

    if ((!is.null(match_data$matches))&&(check_infile()$env_present)) {
      
      
      shinyjs::hide("MapViewMessage")
      
      
      dt<-style_matches()$cent$x$data %>% select(starts_with("PCT_Match"))
      allplots<-sqldf("select replace(pctid,'.','_') pctid from pctplotsdata")
      
      
      dtfinal<-merge(filteredData(),style_matches()$cent$x$data,by.x="sites",by.y="Site_No")

      
      SQLString<-""
      for (i in 1:length(dt)){
        
        if (i==length(dt)){
          SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt")
        }else{
          SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt union ")
        } 
      }
      
      matchedplots<-sqldf(SQLString)
      unmatchedplots<-setdiff(allplots, matchedplots)
      
      matchedplots<- sqldf("SELECT * from pctplotsdata where replace(pctid,'.','_') in (SELECT pctid FROM matchedplots)")
      unmatchedplots<- sqldf("SELECT * from pctplotsdata where replace(pctid,'.','_') in (SELECT pctid FROM unmatchedplots)")
      
      unmatchedplots<-unmatchedplots %>% filter((unmatchedplots$lat>=min(dtfinal$Latitude)-0.6),(unmatchedplots$lat<=max(dtfinal$Latitude)+0.6),(unmatchedplots$long>=min(dtfinal$Longitude)-0.5),(unmatchedplots$long<=max(dtfinal$Longitude)+0.5))
      
      pctstats<-""
      if ("Distance_to_Centroid1" %in% names(dtfinal)) {pctstats<-paste0("<b>PCT_Match1</b>: ",dtfinal$PCT_Match1," <b>Distance_to_Centroid1:</b> ",dtfinal$Distance_to_Centroid1,"<br/>")}
      if ("Distance_to_Centroid2" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match2:</b> ",dtfinal$PCT_Match2," <b>Distance_to_Centroid2:</b> ",dtfinal$Distance_to_Centroid2,"<br/>")}
      if ("Distance_to_Centroid3" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match3:</b> ",dtfinal$PCT_Match3," <b>Distance_to_Centroid3:</b> ",dtfinal$Distance_to_Centroid3,"<br/>")}
      if ("Distance_to_Centroid4" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match4:</b> ",dtfinal$PCT_Match4," <b>Distance_to_Centroid4:</b> ",dtfinal$Distance_to_Centroid4,"<br/>")}
      if ("Distance_to_Centroid5" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match5:</b> ",dtfinal$PCT_Match5," <b>Distance_to_Centroid5:</b> ",dtfinal$Distance_to_Centroid5,"<br/>")}
      if ("Distance_to_Centroid6" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match6:</b> ",dtfinal$PCT_Match6," <b>Distance_to_Centroid6:</b> ",dtfinal$Distance_to_Centroid6,"<br/>")}
      if ("Distance_to_Centroid7" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match7:</b> ",dtfinal$PCT_Match7," <b>Distance_to_Centroid7:</b> ",dtfinal$Distance_to_Centroid7,"<br/>")}
      if ("Distance_to_Centroid8" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match8:</b> ",dtfinal$PCT_Match8," <b>Distance_to_Centroid8:</b> ",dtfinal$Distance_to_Centroid8,"<br/>")}
      if ("Distance_to_Centroid9" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match9:</b> ",dtfinal$PCT_Match9," <b>Distance_to_Centroid9:</b> ",dtfinal$Distance_to_Centroid9,"<br/>")}
      if ("Distance_to_Centroid10" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match10:</b> ",dtfinal$PCT_Match10," <b>Distance_to_Centroid10:</b> ",dtfinal$Distance_to_Centroid10,"<br/>")}
      
      
      
      MatchedCol <-colorFactor(colfuncMatched(3000), domain = matchedplots$pctid)
      UnMatchedCol <-colorFactor(colfuncUnmatched(3000), domain = unmatchedplots$pctid)
      
      
      EasternNSWStudyRegion <- readOGR("spatial/EasternNSW_PrimaryStudyArea_Merged.shp", layer="EasternNSW_PrimaryStudyArea_Merged")
      proj4string(EasternNSWStudyRegion)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
      
      oehblueicon <- makeAwesomeIcon(icon = "plus-sign", markerColor = "blue",
                                     iconColor = "white", library = "glyphicon",
                                     squareMarker =  TRUE)
      
      groupName<-"Display sites for PCTs that are<br/>not one of your top 10 matches, and are <br/>within 55km distance of your site(s)"
    
      
   m<- leaflet(data=dtfinal )%>% addTiles(group = "Terrain") %>% 
        addScaleBar() %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain")%>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
      clearShapes() %>%
        clearMarkers()%>%
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479")%>%
        
        addPolygons(data = EasternNSWStudyRegion, fill = F, weight = 2, color = "#9932CC") %>%
        
        addAwesomeMarkers(icon=oehblueicon ,lat = dtfinal$Latitude,lng = dtfinal$Longitude,layerId = dtfinal$sites,label = dtfinal$sites, labelOptions = labelOptions(noHide = T, direction = "bottom"),
                   popup = ~paste("<b>Site No:</b>",dtfinal$sites,"<br/><b>Lat:</b>",dtfinal$Latitude," <b>Long:</b>",dtfinal$Longitude,"<br/><b>Elevation (m):</b>",dtfinal$Elevation,"<br/><b>Annual Rainfall (mm):</b>",dtfinal$RainfallAnn,"<br/><b>Annual Mean Temperature (°C):</b>",dtfinal$TempAnn,"<br/>", pctstats) ) %>%
        
        addCircles(radius= 100, lat = ~matchedplots$lat, lng = ~matchedplots$long, layerId = ~matchedplots$siteno, label = ~matchedplots$pctid,  color =~MatchedCol(matchedplots$pctid), fillColor =~MatchedCol(matchedplots$pctid),opacity = 1,   fillOpacity = 0.7,
                   data = matchedplots, popup = ~paste("<b>PCT ID:</b>", matchedplots$pctid,"<br/><b>PCT Name:</b>", matchedplots$pctname, "<br/><b>PCT Assignment Category:</b>",matchedplots$pctassignmentcategory,"<br/><b>Site No:</b>",matchedplots$siteno,"<br/><b>Survey Name:</b>", matchedplots$surveyname  ,"<br/><b>Lat:</b>",matchedplots$lat," <b>Long:</b>",matchedplots$long,"<br/><b>Elevation (m):</b>",matchedplots$elevation,"<br/><b>Annual Rainfall (mm):</b>",matchedplots$rainfall,"<br/><b>Annual Mean Temperature (°C):</b>",matchedplots$temp))%>%
      
      addCircles(radius= 50, lat = ~unmatchedplots$lat, lng = ~unmatchedplots$long, layerId = ~unmatchedplots$siteno, color = ~UnMatchedCol(unmatchedplots$pctid),  fillOpacity = 0.5, group = groupName,
                 data = unmatchedplots, popup = ~paste("<b>PCT ID:</b>", unmatchedplots$pctid,"<br/><b>PCT Name:</b>", unmatchedplots$pctname, "<br/><b>PCT Assignment Category:</b>",unmatchedplots$pctassignmentcategory,"<br/><b>Site No:</b>",unmatchedplots$siteno,"<br/><b>Survey Name:</b>", unmatchedplots$surveyname  ,"<br/><b>Lat:</b>",unmatchedplots$lat," <b>Long:</b>",unmatchedplots$long,"<br/><b>Elevation (m):</b>",unmatchedplots$elevation,"<br/><b>Annual Rainfall (mm):</b>",unmatchedplots$rainfall,"<br/><b>Annual Mean Temperature (°C):</b>",unmatchedplots$temp))%>%
                     hideGroup(groupName)%>%
            addLayersControl(
              baseGroups = c("Terrain", "Satellite"),
              overlayGroups = c(groupName),
              options = layersControlOptions(collapsed = FALSE)
            )
    
    
         if (nrow(dtfinal)>1){
         
            m<- m %>% fitBounds(~min(dtfinal$Longitude), ~min(dtfinal$Latitude), ~max(dtfinal$Longitude), ~max(dtfinal$Latitude))
         
         }
         m
      
      
      
    } 
      else{ 
            shinyjs::show("MapViewMessage") 
            
        }
    
  
    
  })

  pctprofiles <- reactiveValues(data = NULL)
  pctplots <- reactiveValues(data = NULL)
  observe({

    tryCatch({

        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
        
        SQLQuery<-paste0("SELECT * FROM fsdata WHERE lat is not NULL ORDER BY pctid")
        
        rs <- dbSendQuery(con, SQLQuery)
        pctplots$data <- dbFetch(rs)
        dbHasCompleted(rs)
        dbClearResult(rs)
        
        
        SQLQuery<-paste0("SELECT * FROM pctprofiledata ORDER BY pctid")
        rs <- dbSendQuery(con, SQLQuery)
        pctprofiles$data <- dbFetch(rs)
        dbHasCompleted(rs)
        dbClearResult(rs)
        
        # clean up
        dbDisconnect(con)
        
    }
    , error = function(e) {
      showNotification(paste0("Error: ",e),duration = 20,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
      # clean up
      dbDisconnect(con)
    }, finally = {
      #message("Some other message at the end")
      
    })

  })
  
  
  
  output$MapViewMessage <- renderUI({
    
        HTML("Map displayed only when spatial data is provided.")
   
  })
  output$EnvDataViewMessage <- renderUI({
    
    HTML("Environmental thresholds displayed only when site environmental data is loaded.")
    
  })
  
  #///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  output$mapView <- renderLeaflet({
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    
    if (!is.null(input$cent_table_cell_clicked$col))
    {
      ar<-array(style_matches()$cent)
      columnName <-colnames(ar[[1]]$data[input$cent_table_cell_clicked$col])
      
      if (!columnName==""){
        
        
        if ((substr(columnName,1,nchar(columnName)-1)=="PCT_Match")||(substr(columnName,1,nchar(columnName)-2)=="PCT_Match")){
          
          sitename<-ar[[1]]$data$Site_No[input$cent_table_cell_clicked$row]
          pctid<-input$cent_table_cell_clicked$value
          
          pctplotsdata<-pctplots$data 
          
          colfuncMatched <- colorRampPalette(c("#ec783a"))
          
          
          
          categories<-pctplotsdata$pctid
          
          if ((!is.null(match_data$matches))) {      ##   &&(check_infile()$env_present)) {
            
             
            dt<-style_matches()$cent$x$data %>% select(starts_with("PCT_Match"))  
            
            dtFilteredData<-filteredData()
            dtmerged<- NULL
            
            if (is.null(dtFilteredData)){
              dtmerged<-style_matches()$cent$x$data
              dtfinal<-sqldf(paste0("SELECT * from dtmerged WHERE Site_No='",sitename,"'"))
              
            }else{
            
                dtmerged<-merge(filteredData(),style_matches()$cent$x$data,by.x="sites",by.y="Site_No")
                dtfinal<-sqldf(paste0("SELECT * from dtmerged WHERE sites='",sitename,"'"))
            
            }
            
            
            
            
            SQLString<-""
            for (i in 1:length(dt)){
              
              if (i==length(dt)){
                SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt")
              }else{
                SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt union ")
              } 
            }
            
            matchedplots<-sqldf(SQLString)      
            matchedplots<- sqldf(paste0("SELECT * from pctplotsdata where pctid='",pctid,"'"))
            
            pctstats<-""
            if ("Distance_to_Centroid1" %in% names(dtfinal)) {pctstats<-paste0("<b>PCT_Match1</b>: ",dtfinal$PCT_Match1," <b>Distance_to_Centroid1:</b> ",dtfinal$Distance_to_Centroid1,"<br/>")}
            if ("Distance_to_Centroid2" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match2:</b> ",dtfinal$PCT_Match2," <b>Distance_to_Centroid2:</b> ",dtfinal$Distance_to_Centroid2,"<br/>")}
            if ("Distance_to_Centroid3" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match3:</b> ",dtfinal$PCT_Match3," <b>Distance_to_Centroid3:</b> ",dtfinal$Distance_to_Centroid3,"<br/>")}
            if ("Distance_to_Centroid4" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match4:</b> ",dtfinal$PCT_Match4," <b>Distance_to_Centroid4:</b> ",dtfinal$Distance_to_Centroid4,"<br/>")}
            if ("Distance_to_Centroid5" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match5:</b> ",dtfinal$PCT_Match5," <b>Distance_to_Centroid5:</b> ",dtfinal$Distance_to_Centroid5,"<br/>")}
            if ("Distance_to_Centroid6" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match6:</b> ",dtfinal$PCT_Match6," <b>Distance_to_Centroid6:</b> ",dtfinal$Distance_to_Centroid6,"<br/>")}
            if ("Distance_to_Centroid7" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match7:</b> ",dtfinal$PCT_Match7," <b>Distance_to_Centroid7:</b> ",dtfinal$Distance_to_Centroid7,"<br/>")}
            if ("Distance_to_Centroid8" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match8:</b> ",dtfinal$PCT_Match8," <b>Distance_to_Centroid8:</b> ",dtfinal$Distance_to_Centroid8,"<br/>")}
            if ("Distance_to_Centroid9" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match9:</b> ",dtfinal$PCT_Match9," <b>Distance_to_Centroid9:</b> ",dtfinal$Distance_to_Centroid9,"<br/>")}
            if ("Distance_to_Centroid10" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match10:</b> ",dtfinal$PCT_Match10," <b>Distance_to_Centroid10:</b> ",dtfinal$Distance_to_Centroid10,"<br/>")}
            
            
            
            MatchedCol <-colorFactor(colfuncMatched(5), domain = matchedplots$pctid)
            
            
            EasternNSWStudyRegion <- readOGR("spatial/EasternNSW_PrimaryStudyArea_Merged.shp", layer="EasternNSW_PrimaryStudyArea_Merged")
            proj4string(EasternNSWStudyRegion)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
            
            
            oehblueicon <- makeAwesomeIcon(icon = "plus-sign", markerColor = "blue",
                                           iconColor = "white", library = "glyphicon",
                                           squareMarker =  TRUE)
            
            
            if (is.null(dtFilteredData)){
            
                    leaflet(data=dtfinal )%>% addTiles(group = "Terrain") %>% 
                      addScaleBar() %>%
                      addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain")%>%
                      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
                      #fitBounds(~min(dtfinal$Longitude), ~min(dtfinal$Latitude), ~max(dtfinal$Longitude), ~max(dtfinal$Latitude)) %>%
                      clearShapes() %>%
                      clearMarkers()%>%
                      addMeasure(
                        position = "bottomleft",
                        primaryLengthUnit = "meters",
                        primaryAreaUnit = "sqmeters",
                        activeColor = "#3D535D",
                        completedColor = "#7D4479")%>%
                      
                      addPolygons(data = EasternNSWStudyRegion, fill = F, weight = 2, color = "#9932CC") %>%
                      
                      # addAwesomeMarkers(icon = oehblueicon, lat = dtfinal$Latitude,lng = dtfinal$Longitude,layerId = dtfinal$sites,label = dtfinal$sites, labelOptions = labelOptions(noHide = T, direction = "bottom"),
                      #            popup = ~paste("<b>Site No:</b>",dtfinal$sites,"<br/><b>Lat:</b>",dtfinal$Latitude," <b>Long:</b>",dtfinal$Longitude,"<br/><b>Elevation(m):</b>",dtfinal$Elevation,"<br/><b>Rainfall(mm):</b>",dtfinal$RainfallAnn,"<br/><b>Temperature(deg.C):</b>",dtfinal$TempAnn,"<br/>", pctstats) ) %>%
                      # 
                      addCircles(radius= 200, lat = ~matchedplots$lat, lng = ~matchedplots$long, layerId = ~matchedplots$siteno, label = ~matchedplots$pctid,  color =~MatchedCol(matchedplots$pctid), fillColor =~MatchedCol(matchedplots$pctid),opacity = 1,   fillOpacity = 0.7,
                                 data = matchedplots, popup = ~paste("<b>PCT ID:</b>", matchedplots$pctid,"<br/><b>PCT Name:</b>", matchedplots$pctname, "<br/><b>PCT Assignment Category:</b>",matchedplots$pctassignmentcategory,"<br/><b>Site No:</b>",matchedplots$siteno,"<br/><b>Survey Name:</b>", matchedplots$surveyname  ,"<br/><b>Lat:</b>",matchedplots$lat," <b>Long:</b>",matchedplots$long,"<br/><b>Elevation (m):</b>",matchedplots$elevation,"<br/><b>Annual Rainfall (mm):</b>",matchedplots$rainfall,"<br/><b>Annual Mean Temperature (°C):</b>",matchedplots$temp))%>%
                      
                      addLayersControl(
                        baseGroups = c("Terrain", "Satellite"),             
                        options = layersControlOptions(collapsed = FALSE)
                      )
              
            } else
            {
              
              checkpoint<-1
              
                    leaflet(data=dtfinal )%>% addTiles(group = "Terrain") %>% 
                      addScaleBar() %>%
                      addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain")%>%
                      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
                      #fitBounds(~min(dtfinal$Longitude), ~min(dtfinal$Latitude), ~max(dtfinal$Longitude), ~max(dtfinal$Latitude)) %>%
                      clearShapes() %>%
                      clearMarkers()%>%
                      addMeasure(
                        position = "bottomleft",
                        primaryLengthUnit = "meters",
                        primaryAreaUnit = "sqmeters",
                        activeColor = "#3D535D",
                        completedColor = "#7D4479")%>%
                      
                      addPolygons(data = EasternNSWStudyRegion, fill = F, weight = 2, color = "#9932CC") %>%
                      
                      addAwesomeMarkers(icon = oehblueicon, lat = dtfinal$Latitude,lng = dtfinal$Longitude,layerId = dtfinal$sites,label = dtfinal$sites, labelOptions = labelOptions(noHide = T, direction = "bottom"),
                                        popup = ~paste("<b>Site No:</b>",dtfinal$sites,"<br/><b>Lat:</b>",dtfinal$Latitude," <b>Long:</b>",dtfinal$Longitude,"<br/><b>Elevation (m):</b>",dtfinal$Elevation,"<br/><b>Annual Rainfall (mm):</b>",dtfinal$RainfallAnn,"<br/><b>Annual Mean Temperature (°C):</b>",dtfinal$TempAnn,"<br/>", pctstats) ) %>%
                      
                      addCircles(radius= 200, lat = ~matchedplots$lat, lng = ~matchedplots$long, layerId = ~matchedplots$siteno, label = ~matchedplots$pctid,  color =~MatchedCol(matchedplots$pctid), fillColor =~MatchedCol(matchedplots$pctid),opacity = 1,   fillOpacity = 0.7,
                                 data = matchedplots, popup = ~paste("<b>PCT ID:</b>", matchedplots$pctid,"<br/><b>PCT Name:</b>", matchedplots$pctname, "<br/><b>PCT Assignment Category:</b>",matchedplots$pctassignmentcategory,"<br/><b>Site No:</b>",matchedplots$siteno,"<br/><b>Survey Name:</b>", matchedplots$surveyname  ,"<br/><b>Lat:</b>",matchedplots$lat," <b>Long:</b>",matchedplots$long,"<br/><b>Elevation (m):</b>",matchedplots$elevation,"<br/><b>Annual Rainfall (mm):</b>",matchedplots$rainfall,"<br/><b>Annual Mean Temperature (°C):</b>",matchedplots$temp))%>%
                      
                      addLayersControl(
                        baseGroups = c("Terrain", "Satellite"),             
                        options = layersControlOptions(collapsed = FALSE)
                      )
              
            }
                        
            
          } 
          
          
          
        } # PCT_Match
        
        
      }
    }
    
    
  })
  
  #///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  output$mapView2 <- renderLeaflet({
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    
    if (!is.null(input$char_table_cell_clicked$col))
    {
      ar<-array(style_matches()$cent)
      columnName <-colnames(ar[[1]]$data[input$char_table_cell_clicked$col])
      
      if (!columnName==""){
        
        
        if ((substr(columnName,1,nchar(columnName)-1)=="PCT_Match")||(substr(columnName,1,nchar(columnName)-2)=="PCT_Match")){
          
          sitename<-ar[[1]]$data$Site_No[input$char_table_cell_clicked$row]
          pctid<-input$char_table_cell_clicked$value
          
          pctplotsdata<-pctplots$data 
          
          colfuncMatched <- colorRampPalette(c("#ec783a"))
          
          
          
          categories<-pctplotsdata$pctid
          
          if ((!is.null(match_data$matches))) {      ##   &&(check_infile()$env_present)) {
            
            
            dt<-style_matches()$cent$x$data %>% select(starts_with("PCT_Match"))  
            
            dtFilteredData<-filteredData()
            dtmerged<- NULL
            
            if (is.null(dtFilteredData)){
              dtmerged<-style_matches()$cent$x$data
              dtfinal<-sqldf(paste0("SELECT * from dtmerged WHERE Site_No='",sitename,"'"))
              
            }else{
              
              dtmerged<-merge(filteredData(),style_matches()$cent$x$data,by.x="sites",by.y="Site_No")
              dtfinal<-sqldf(paste0("SELECT * from dtmerged WHERE sites='",sitename,"'"))
              
            }
            
            
            
            
            SQLString<-""
            for (i in 1:length(dt)){
              
              if (i==length(dt)){
                SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt")
              }else{
                SQLString<-paste0(SQLString,"select PCT_Match",i," as pctid from dt union ")
              } 
            }
            
            matchedplots<-sqldf(SQLString)      
            matchedplots<- sqldf(paste0("SELECT * from pctplotsdata where pctid='",pctid,"'"))
            
            pctstats<-""
            if ("Distance_to_Centroid1" %in% names(dtfinal)) {pctstats<-paste0("<b>PCT_Match1</b>: ",dtfinal$PCT_Match1," <b>Distance_to_Centroid1:</b> ",dtfinal$Distance_to_Centroid1,"<br/>")}
            if ("Distance_to_Centroid2" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match2:</b> ",dtfinal$PCT_Match2," <b>Distance_to_Centroid2:</b> ",dtfinal$Distance_to_Centroid2,"<br/>")}
            if ("Distance_to_Centroid3" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match3:</b> ",dtfinal$PCT_Match3," <b>Distance_to_Centroid3:</b> ",dtfinal$Distance_to_Centroid3,"<br/>")}
            if ("Distance_to_Centroid4" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match4:</b> ",dtfinal$PCT_Match4," <b>Distance_to_Centroid4:</b> ",dtfinal$Distance_to_Centroid4,"<br/>")}
            if ("Distance_to_Centroid5" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match5:</b> ",dtfinal$PCT_Match5," <b>Distance_to_Centroid5:</b> ",dtfinal$Distance_to_Centroid5,"<br/>")}
            if ("Distance_to_Centroid6" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match6:</b> ",dtfinal$PCT_Match6," <b>Distance_to_Centroid6:</b> ",dtfinal$Distance_to_Centroid6,"<br/>")}
            if ("Distance_to_Centroid7" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match7:</b> ",dtfinal$PCT_Match7," <b>Distance_to_Centroid7:</b> ",dtfinal$Distance_to_Centroid7,"<br/>")}
            if ("Distance_to_Centroid8" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match8:</b> ",dtfinal$PCT_Match8," <b>Distance_to_Centroid8:</b> ",dtfinal$Distance_to_Centroid8,"<br/>")}
            if ("Distance_to_Centroid9" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match9:</b> ",dtfinal$PCT_Match9," <b>Distance_to_Centroid9:</b> ",dtfinal$Distance_to_Centroid9,"<br/>")}
            if ("Distance_to_Centroid10" %in% names(dtfinal)) {pctstats<-paste0(pctstats,"<b>PCT_Match10:</b> ",dtfinal$PCT_Match10," <b>Distance_to_Centroid10:</b> ",dtfinal$Distance_to_Centroid10,"<br/>")}
            
            
            
            MatchedCol <-colorFactor(colfuncMatched(5), domain = matchedplots$pctid)
            
            
            EasternNSWStudyRegion <- readOGR("spatial/EasternNSW_PrimaryStudyArea_Merged.shp", layer="EasternNSW_PrimaryStudyArea_Merged")
            proj4string(EasternNSWStudyRegion)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
            
            
            oehblueicon <- makeAwesomeIcon(icon = "plus-sign", markerColor = "blue",
                                           iconColor = "white", library = "glyphicon",
                                           squareMarker =  TRUE)
            
            
            if (is.null(dtFilteredData)){
              
              leaflet(data=dtfinal )%>% addTiles(group = "Terrain") %>%
                addScaleBar() %>%
                addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain")%>%
                addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
                clearShapes() %>%
                clearMarkers()%>%
                addMeasure(
                  position = "bottomleft",
                  primaryLengthUnit = "meters",
                  primaryAreaUnit = "sqmeters",
                  activeColor = "#3D535D",
                  completedColor = "#7D4479")%>%
                addPolygons(data = EasternNSWStudyRegion, fill = F, weight = 2, color = "#9932CC") %>%
                
                # addAwesomeMarkers(icon = oehblueicon, lat = dtfinal$Latitude,lng = dtfinal$Longitude,layerId = dtfinal$sites,label = dtfinal$sites, labelOptions = labelOptions(noHide = T, direction = "bottom"),
                #            popup = ~paste("<b>Site No:</b>",dtfinal$sites,"<br/><b>Lat:</b>",dtfinal$Latitude," <b>Long:</b>",dtfinal$Longitude,"<br/><b>Elevation(m):</b>",dtfinal$Elevation,"<br/><b>Rainfall(mm):</b>",dtfinal$RainfallAnn,"<br/><b>Temperature(deg.C):</b>",dtfinal$TempAnn,"<br/>", pctstats) ) %>%
                # 
                addCircles(radius= 200, lat = ~matchedplots$lat, lng = ~matchedplots$long, layerId = ~matchedplots$siteno, label = ~matchedplots$pctid,  color =~MatchedCol(matchedplots$pctid), fillColor =~MatchedCol(matchedplots$pctid),opacity = 1,   fillOpacity = 0.7,
                           data = matchedplots, popup = ~paste("<b>PCT ID:</b>", matchedplots$pctid,"<br/><b>PCT Name:</b>", matchedplots$pctname, "<br/><b>PCT Assignment Category:</b>",matchedplots$pctassignmentcategory,"<br/><b>Site No:</b>",matchedplots$siteno,"<br/><b>Survey Name:</b>", matchedplots$surveyname  ,"<br/><b>Lat:</b>",matchedplots$lat," <b>Long:</b>",matchedplots$long,"<br/><b>Elevation (m):</b>",matchedplots$elevation,"<br/><b>Annual Rainfall (mm):</b>",matchedplots$rainfall,"<br/><b>Annual Mean Temperature (°C):</b>",matchedplots$temp))%>%
                
                addLayersControl(
                  baseGroups = c("Terrain", "Satellite"),             
                  options = layersControlOptions(collapsed = FALSE)
                )
              
            }else
            {
              leaflet(data=dtfinal )%>% addTiles(group = "Terrain") %>% 
                addScaleBar() %>%
                addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain")%>%
                addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
                #fitBounds(~min(dtfinal$Longitude), ~min(dtfinal$Latitude), ~max(dtfinal$Longitude), ~max(dtfinal$Latitude)) %>%
                clearShapes() %>%
                clearMarkers()%>%
                addMeasure(
                  position = "bottomleft",
                  primaryLengthUnit = "meters",
                  primaryAreaUnit = "sqmeters",
                  activeColor = "#3D535D",
                  completedColor = "#7D4479")%>%
                
                addPolygons(data = EasternNSWStudyRegion, fill = F, weight = 2, color = "#9932CC") %>%
                
                addAwesomeMarkers(icon = oehblueicon, lat = dtfinal$Latitude,lng = dtfinal$Longitude,layerId = dtfinal$sites,label = dtfinal$sites, labelOptions = labelOptions(noHide = T, direction = "bottom"),
                                  popup = ~paste("<b>Site No:</b>",dtfinal$sites,"<br/><b>Lat:</b>",dtfinal$Latitude," <b>Long:</b>",dtfinal$Longitude,"<br/><b>Elevation (m):</b>",dtfinal$Elevation,"<br/><b>Annual Rainfall (mm):</b>",dtfinal$RainfallAnn,"<br/><b>Annual Mean Temperature (°C):</b>",dtfinal$TempAnn,"<br/>", pctstats) ) %>%
                
                addCircles(radius= 200, lat = ~matchedplots$lat, lng = ~matchedplots$long, layerId = ~matchedplots$siteno, label = ~matchedplots$pctid,  color =~MatchedCol(matchedplots$pctid), fillColor =~MatchedCol(matchedplots$pctid),opacity = 1,   fillOpacity = 0.7,
                           data = matchedplots, popup = ~paste("<b>PCT ID:</b>", matchedplots$pctid,"<br/><b>PCT Name:</b>", matchedplots$pctname, "<br/><b>PCT Assignment Category:</b>",matchedplots$pctassignmentcategory,"<br/><b>Site No:</b>",matchedplots$siteno,"<br/><b>Survey Name:</b>", matchedplots$surveyname  ,"<br/><b>Lat:</b>",matchedplots$lat," <b>Long:</b>",matchedplots$long,"<br/><b>Elevation (m):</b>",matchedplots$elevation,"<br/><b>Annual Rainfall (mm):</b>",matchedplots$rainfall,"<br/><b>Annual Mean Temperature (°C):</b>",matchedplots$temp))%>%
                
                addLayersControl(
                  baseGroups = c("Terrain", "Satellite"),             
                  options = layersControlOptions(collapsed = FALSE)
                )
              
            }
            
            
            
            
          } 
          
          
          
        } # PCT_Match
        
        
      }
    }
    
    
  })
  
  
  #/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  
  observeEvent(input$linkDownloadSampleData, {
    loggit("INFO","download download sample csv", log_detail="link to download sample csv", event = "download",  sessionid=isolate(session$token), echo = FALSE)  
  })
  

  
  session$onSessionEnded(function(){    loggit("INFO", "session has ended",log_detail="session has ended", sessionid=isolate(session$token), echo = FALSE)   })


  
  
  
})




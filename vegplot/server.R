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
library(DT)
library(vegan)
library(parallel)

source("functions.R")

# Define server logic 

shinyServer(function(input, output) {

  # LOAD REQUIRED DATA ------------------------------------------------------
  # this will include data needed for centroid calculations too
  # read char species list
  char_spp_list <- readRDS("data/char_species_list.rds")
  centroids <- readRDS("data/species_centroids.rds")
  
  
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
  
  get_example_data <- reactive({
    readRDS("data/GAP-EAST_plots.rds")
  })
  
  output$example <- downloadHandler(
    filename = function() {
      paste("example_floristic_data.csv")
    },
    content = function(file) {
      write.csv(get_example_data(), file, row.names = F)
    }
  )
  
  
  
  # NORMAL APP FUNCTION - USER SUPPLIED DATA --------------------------------
  
  # print out some stats to know we're in business
  check_infile <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    infile_df <- read.csv(inFile$datapath,
                          header = T,
                          stringsAsFactors = F)
    out_list <- list(sites = infile_df[,1],
                     floristics = infile_df[,-1],
                     missing_species = names(infile_df[,-1])[!names(infile_df[,-1]) %in% colnames(centroids)])
                     #infile_df = infile_df)
    return(out_list)
  })
  
  # put together the file stats, once file is uploaded
  # (to start, no file is uploaded - print a promt to do so)
  output$num_sites <- renderText({
    if (!is.null(check_infile())) {
      paste0("You uploaded a file with ", length(check_infile()$sites), " sites.")
    } else {
      "You have not uploaded any data yet (you can download the example data to test drive)."
    }
  })
  output$num_species <- renderUI({
    if (!is.null(check_infile())) {
      if (length(check_infile()$missing_species) == 0) {
        HTML(paste0("There are ", ncol(check_infile()$floristics),
                    " species: all species have been matched in the database.",
                    "<br> <b>N.B.</b> Up to 200 sites should be <1 minute processing."))
      } else {
        HTML(paste0("There are ", ncol(check_infile()$floristics),
                    " species, <b><mark> and ",length(check_infile()$missing_species),
                    " could not be matched: ", paste(check_infile()$missing_species, collapse = ", "), 
                    ", so were ignored in analysis.</mark></b>",
                    "<br> <b>N.B.</b> Up to 200 sites should be <1 minute processing."))
      }
    } else {
      "Use the upload data box on the left to get started."
    }
  })
  
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
    progress$set(message = "Matching species", value = 0.25)
    # do the char species / centroid calculations
    #infile_df <- check_infile()$infile_df
    char_matches <- calculate_matches(infile_df[,-1], infile_df[,1], char_spp_list)
    progress$set(message = "Matching centroids", value = 0.35)
    cent_matches <- calculate_centroids(infile_df[,-1], infile_df[,1], centroids)
    # make ordination plot
    progress$set(message = "Making plots", value = 0.85)
    ord_sites <- make_sites_ord(infile_df)
    # add data to the reactive output
    match_data$matches <- list(char_matches = char_matches,
                               cent_matches = cent_matches,
                               ord_sites = list(ord = ord_sites, sites = infile_df[,1]),
                               mc_cores = attr(cent_matches, "mc_cores"),
                               compute_time = round(as.numeric(difftime(Sys.time(), tic, units = "secs"))))
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
  
  # info on analysis
  output$info_text <- renderText({
    if (!is.null(match_data$matches)) {
      # Text to show once analysis is done
      "<br>
      The tables below show the top matches for your 
      submitted floristic data to the NSW vegetation plot 
      database. You can follow the links for each vegetation 
      type to see more details about that type 
      (incl. floristics and environmental characteristics).
      Switch between the characteristic species and centroid
      based matches. The combined matches tab shows the 
      types that were matched in both (ordered by distance to
      centroid). The tabs above show more info about the data,
      along with plots."
    } else {
      if (!is.null(check_infile())) {"<br> Press the analyse button to start!"}
    }
    })
  
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
    rownames(combined_matches) <- NULL
    names(combined_matches) <- c("Site", 1:topn)
    return(list(char = style_matches_char(top_char_matches),
                cent = style_matches_cent(top_cent_matches),
                combined = combined_matches))
  })
  
  output$char_table <- renderDataTable({
    if (!is.null(match_data$matches)) style_matches()$char
  })
  output$cent_table <- renderDataTable({
    if (!is.null(match_data$matches)) style_matches()$cent
  })
  output$combined_table <- renderDataTable({
    if (!is.null(match_data$matches)) style_matches()$combined
  })

  # Allow user to download the matches (topn they've decided)
  download_matches <- reactive({
    topn <- input$topn
    return(list(char = get_topn(match_data$matches$char_matches, topn, T),
                cent = get_topn(match_data$matches$cent_matches, topn, F)))
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
})




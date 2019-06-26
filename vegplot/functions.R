#
# This is where the functions for the shiny app sit.
# They are called in the server.R file
#


# data loads - sources to ensure they're in -------------------------------

char_spp_list <- readRDS("data/char_species_list.rds")
centroids <- readRDS("data/species_centroids.rds")
env_thresh <- readRDS("data/env_thresholds.rds")

# a master list of what not to include in floristic data
non_floristic <- c("X","Latitude","Longitude","Elevation","RainfallAnn","TempAnn")



# utils -------------------------------------------------------------------

# check OS - can use mclapply if on linux or mac
get_cores <- function(sites) {
  if (.Platform$OS == "windows") { # this is just so it works if you're testing on a windows machine
    return(1)
  } else {
    if (sites < 200) {
      if (detectCores() > 4) {
        return(4)
      } else {
        return(detectCores())
      }
    } else {
      return(detectCores() - 1)
    }
  }
}



# characteristic species based matching -----------------------------------

## function to match the spcies list - may want to think about how to weight by richness???
species_percentage_match <- function(chars, species) {
  sum(chars %in% species) / length(chars)
}

## function to create the species lists for each site
calculate_matches <- function(floristic_raw, site_labels, group_chars, topn) {
  new_site_species <- apply(X = floristic_raw, MARGIN = 1, FUN = function(x){names(x[x > 0])})
    # loop over the species lists for the new sites and calculate percentage match for each
  char_matches <- matrix(NA, nrow = length(new_site_species), ncol = length(group_chars)) # pre-allocate
  for (i in 1:nrow(char_matches)) { # can make this an other lapply when needed
    #####->>>>> this could be an mclapply call if needed (overhead is not worth it yet)
    char_matches[i,] <- unlist(lapply(group_chars, species_percentage_match, new_site_species[[i]]))
  }
  colnames(char_matches) <- names(group_chars)
  # join together with site info
  data.frame(site_labels, as.data.frame(round(char_matches*100)), stringsAsFactors = F)
}



# distance to centroid based matching -------------------------------------

## function to find distance between two vectors
euc_dist <- function(i, j) {
  sum(abs(i - j)) / sum((i + j))
}

## function to find distance to centroids given a vector of site data and the centroids
dist_to_centroid <- function(site, centres) {
  unlist(lapply(X = as.data.frame(t(centres)),
                FUN = euc_dist, j = site))
}

calculate_centroids <- function(floristic_raw, site_labels, centroids) {
  # find species uploaded not in database, then discard
  missing_species <- names(floristic_raw)[!names(floristic_raw) %in% colnames(centroids)]
  floristic_raw <- floristic_raw[,!names(floristic_raw) %in% missing_species]
  # find species in database not in new data
  absense_species <- colnames(centroids)[!colnames(centroids) %in% names(floristic_raw)]
  # add the absent species in 
  new_dat_absenses <- data.frame(floristic_raw,
                                 as.data.frame(
                                   matrix(data = 0, nrow = nrow(floristic_raw),
                                          ncol = length(absense_species),
                                          dimnames = list(NULL, absense_species))
                                 ))
  new_dat_absenses <- new_dat_absenses[,sort(names(new_dat_absenses))]
  # calculate the distances and hack the matches and group names together
  mc_cores <- get_cores(nrow(floristic_raw))
  cent_matches <- t(bind_rows(
    mclapply(X = as.data.frame(t(new_dat_absenses)),
           FUN = dist_to_centroid,
           centres = centroids,
           mc.cores = mc_cores)
    # lapply(X = as.data.frame(t(new_dat_absenses)),
    #          FUN = dist_to_centroid,
    #          centres = centroids)
  ))
  rownames(cent_matches) <- NULL
  colnames(cent_matches) <- rownames(centroids)
  df_out <- data.frame(site_labels, as.data.frame(round(cent_matches, 3)), stringsAsFactors = F)
  
  
  
  attr(df_out, "mc_cores") <- mc_cores
  
  df_out  
  # cent_matches_top <- data.frame(site_labels,
  #                                round(t(bind_rows(lapply(distances, `[[`, 1))), 3),
  #                                t(bind_rows(lapply(distances, `[[`, 2))))
  # names(cent_matches_top) <- c("Site_Name", paste0("match",1:topn), paste0("group",1:topn))
  # list(distances = cent_matches_top,
  #      missing_species = missing_species)
}



# table output & styling functions ----------------------------------------

## make an abridged one with top n matches
top_n <- function(x, topn, decr) {
  tops <- sort(x, decreasing = decr)[1:topn]
  c(tops, names(tops))
}

## function to reutrn the top n matches
get_topn <- function(matches, topn, decr) {
  matches_top <- data.frame(matches[,1],
                            t(apply(matches[,-1], 1, top_n, topn, decr)),
                            stringsAsFactors = F)
  names(matches_top) <- c("Site_No", paste0("Distance_to_Centroid",1:topn), paste0("PCT_Match",1:topn)) # hacky, but avoids overhead in the top_n function
  matches_top
}

## find the topn matches from both char spp and centroids
match_topn <- function(cent, char, topn) {
  centx <- cent[(topn+2):(topn*2+1)]
  charx <- char[(topn+2):(topn*2+1)]
  matched <- c(cent[1], centx[centx %in% charx])
  matched_out <- character(topn+1)
  names(matched_out) <- c("Site", 1:topn)
  matched_out[1:length(matched)] <- matched
  matched_out
}


## function to style the output allocation table for characteristic species matches
style_matches_char <- function(table) {
  DT::datatable(table) %>%
    formatStyle(grep("Distance_to_Centroid", names(table)), 
              backgroundColor = styleInterval(cuts = c(25,60), 
                                              values = c("white","#ccd6bc","#99b964"))
  )
}

## function to style the output allocation table for distance to centroids
style_matches_cent <- function(table) {
  DT::datatable(table) %>%
    formatStyle(grep("Distance_to_Centroid", names(table)), 
                backgroundColor = styleInterval(cuts = c(0.695,0.69501), 
                                                values = c("#99b964","white","white"))
    )
}

style_matches_thresholds <- function(table) {
  DT::datatable(table) %>%
    formatStyle(c("Rainfall","Elevation","Temperature"), 
                backgroundColor = styleEqual(levels = c("Above","Below","Within"), 
                                             values = c("white","white","#99b964"),default = "white")
    )
}

## function to find threshold status for an individual site + group combo
check_site_thresholds <- function(group, site, env_thresh, env_data) {
  out <- data.frame(Elevation = "N/A", Rainfall = "N/A", Temperature = "N/A")
  if (!group %in% env_thresh$gp) return(out)
  
  group_thresh <- filter(env_thresh, gp == group)
  site_env <- filter(env_data, sites == site)
  
  if (site_env[1,"Elevation"] < group_thresh[1,"T1_elev"]) {
    out$Elevation <- "Below"
  } else if (site_env[1,"Elevation"] > group_thresh[1,"T2_elev"]) {
    out$Elevation <- "Above"
  } else {
    out$Elevation <- "Within"
  }
  
  if (site_env[1,"RainfallAnn"] < group_thresh[1,"T1_rainann"]) {
    out$Rainfall <- "Below"
  } else if (site_env[1,"RainfallAnn"] > group_thresh[1,"T2_rainann"]) {
    out$Rainfall <- "Above"
  } else {
    out$Rainfall <- "Within"
  }
  
  if (site_env[1,"TempAnn"] < group_thresh[1,"T1_tempann"]) {
    out$Temperature <- "Below"
  } else if (site_env[1,"TempAnn"] > group_thresh[1,"T2_tempann"]) {
    out$Temperature <- "Above"
  } else {
    out$Temperature <- "Within"
  }
  
  out
}


## function to determine environmetnal threshold status
check_env_thresholds <- function(cent_groups, env_thresholds, env_data) {
  cent_groups_long <- gather(cent_groups, "Distance_to_Centroid", "PCT_Match", grep("PCT_Match", names(cent_groups)))
  data.frame(cent_groups_long,
             bind_rows(
               map2(.x = cent_groups_long$PCT_Match, .y = cent_groups_long$Site_No,
                    .f = check_site_thresholds, env_thresh = env_thresh, env_data = env_data)
               ))
}


get_numplots <- function(n){
  n
}


##///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
## Reorder columns
reorder_data<-function(matchedData) {
  
  columnnames <- names(matchedData)
  
  colNamesMatch <-array()
  colNamesGroup<-array()
  n=0
  m=0
  for(p in columnnames) {
    
    if ((substr(p,1,nchar(p)-1)=="Distance_to_Centroid")||(substr(p,1,nchar(p)-2)=="Distance_to_Centroid"))
    {
      n<-n+1
      colNamesMatch[n] <- p     		
    }
    
    if ((substr(p,1,nchar(p)-1)=="%_Char_Spp")||(substr(p,1,nchar(p)-2)=="%_Char_Spp"))
    {
      n<-n+1
      colNamesMatch[n] <- p     		
    }
    
    
    if ((substr(p,1,nchar(p)-1)=="PCT_Match")||(substr(p,1,nchar(p)-2)=="PCT_Match"))
    {
      m<-m+1
      colNamesGroup[m] <- p     		
    }  
    
  }
  
  newColNames<-NULL
  for (i in seq(1,length(colNamesMatch),1)){
    
    newColNames <- paste0(newColNames,",",colNamesGroup[i],",",colNamesMatch[i])
    
  }
  newColNames <- paste0(columnnames[1],substr(newColNames,1,nchar(newColNames)))
  newColNames <- unlist(strsplit(newColNames,","))
  
  #print(newColNames)
  matchedData <- matchedData[c(newColNames)]
  matchedData
}
##///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# ordination plot of sites ------------------------------------------------

make_sites_ord <- function(infile) {
  ord <- metaMDS(infile[,-1], trace = 0)
}


getPCTName<- function(pctid) {
  # Initialize a temporary in memory database and copy a data.frame into it
  con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
  
  rs <- dbSendQuery(con, paste0("SELECT pctname FROM pctprofiledata where replace(pctid,'.','_')='",pctid,"' limit 1"))
  d1 <- dbFetch(rs)
  dbHasCompleted(rs)
  dbClearResult(rs)
  
  # clean up
  dbDisconnect(con)
  return(d1$PCTName)
}

getPCTProfile<- function(pctid) {
  # Initialize a temporary in memory database and copy a data.frame into it
  con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")

  rs <- dbSendQuery(con, paste0("SELECT * FROM pctprofiledata where pctid='",pctid,"' limit 1"))
  d1 <- dbFetch(rs)
  dbHasCompleted(rs)
  dbClearResult(rs)
  
  rs <- dbSendQuery(con, paste0("SELECT Scientific_name, Group_score_median, Group_frequency, GrowthFormGroup,
                        CASE GrowthFormGroup 
                                 WHEN 'Tree (TG)' THEN 1 
                                 WHEN 'Shrub (SG)' THEN 2
                                 WHEN 'Fern (EG)' THEN 3
                                 WHEN 'Grass & grasslike (GG)' THEN 4
                                 WHEN 'Forb (FG)' THEN 5           
                                 ELSE 6
                             END GGroupOrder
                         FROM pctspeciesgrowthforms where PCT_ID='",pctid,"' order by GGroupOrder asc, Group_frequency desc"))
  d2 <- dbFetch(rs)
  dbHasCompleted(rs)
  dbClearResult(rs)
  
  # clean up
  dbDisconnect(con)
  
 #PCTID, PCTName, Vegetation_description, Classification_confidence_level, Number_of_Primary_replicates, 
  # Number_of_Secondary_replicates, Vegetation_Formation, Vegetation_Class, IBRA_Subregion, 
  # Elevation_max, Elevation_min, Elevation_median, Rainfall_max, Rainfall_min, Rainfall_median, 
  # Temperature_max, Temperature_min, Temperature_median, TEC_list, TEC_Act, Median_species_richness
  
    pctprofile01<-paste0("<b>PCT ID:</b>",pctid,"<br/>"
                            ,"<b>PCT Name:</b>",d1$PCTName,"<br/>"
                            ,"<b>Classification Confidence Level:</b>",if (is.na(d1$Classification_confidence_level)) "" else d1$Classification_confidence_level,"<br/>"
                            ,"<b>Number of Primary Replicates:</b>",d1$Number_of_Primary_replicates,"<br/>"
                            ,"<b>Number of Secondary Replicates:</b>",d1$Number_of_Secondary_replicates,"<br/>"
                            ,"<b>Vegetation Description:</b>",if (is.na(d1$Vegetation_description)) "" else d1$Vegetation_description,"<br/>"
                            ,"<b>Vegetation Formation:</b>",d1$Vegetation_Formation,"<br/>"
                            ,"<b>Vegetation Class:</b>",d1$Vegetation_Class,"<br/>"
                            ,"<b>IBRA Subregion(s):</b>",d1$IBRA_Subregion,"<br/>"
                            ,"<b>Elevation Max:</b>",d1$Elevation_max,"<br/>"
                            ,"<b>Elevation Min:</b>",d1$Elevation_min,"<br/>"
                            ,"<b>Elevation Median:</b>",d1$Elevation_median,"<br/>"
                            ,"<b>Rainfall Max:</b>",d1$Rainfall_max,"<br/>"
                            ,"<b>Rainfall Min:</b>",d1$Rainfall_min,"<br/>"
                            ,"<b>Rainfall Median:</b>",d1$Rainfall_median,"<br/>"
                            ,"<b>Temperature Max:</b>",d1$Temperature_max,"<br/>"
                            ,"<b>Temperature Min:</b>",d1$Temperature_min,"<br/>"
                            ,"<b>Temperature Median:</b>",d1$Temperature_median,"<br/>"
                            ,"<b>TEC List:</b>",if (is.na(d1$TEC_list)) "" else d1$TEC_list,"<br/>"
                            ,"<b>TEC Act:</b>",if (is.na(d1$TEC_Act)) "" else d1$TEC_Act,"<br/>"
                            ,"<b>Median Native Species Richness:</b>",d1$Median_species_richness,"<br/>"
                            ,"<div><b>Species by Growth Form:</b>")
  
  
  
        headings<- paste0("<div style='overflow:scroll;height:300px;'>
        <table style='border-style:solid; border-width:2px;border-color:black;width;600px;'>
        	<tr style='width:600px;'>
        	<td style='vertical-align:top;font-weight:bolder;padding:2px;margin:2px;'>Scientific name</td>
        	<td style='vertical-align:top;font-weight:bolder;padding:2px;margin:2px;'>Group Score Median</td>
        	<td style='vertical-align:top;font-weight:bolder;padding:2px;margin:2px;'>Group Frequency</td>
        	<td style='vertical-align:top;font-weight:bolder;padding:2px;margin:2px;'>Growth Form Group</td>
        	</tr>")
        
        
       
        
        rowdata<-""
        
        for (i in 1:nrow(d2)){
          
          rowdata<-paste0(rowdata, 
                          "<tr style='width:600px;'>
      				<td style='vertical-align:top;padding:2px;margin:2px;'>",d2$Scientific_name[i],"</td>
      				<td style='vertical-align:top;padding:2px;margin:2px;'>",d2$Group_score_median[i],"</td>
      				<td style='vertical-align:top;padding:2px;margin:2px;'>",d2$Group_frequency[i],"</td>
      				<td style='vertical-align:top;padding:2px;margin:2px;'>",d2$GrowthFormGroup[i],"</td>
      			   </tr>")
        }
        rowdata<-paste0(rowdata,"</table></div></div>")
        
        pctprofile<-HTML(paste0(pctprofile01,headings,rowdata))
  
  
  
  
  return(pctprofile)
}

getPCTSites<- function(pctid) {
  # Initialize a temporary in memory database and copy a data.frame into it
  con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
  
  rs <- dbSendQuery(con, paste0("SELECT * FROM fsdata where replace(pctid,'.','_')='",pctid,"'"))
  d1 <- dbFetch(rs)
  dbHasCompleted(rs)
  dbClearResult(rs)
  
  # clean up
  dbDisconnect(con)
  
  return(d1)
}


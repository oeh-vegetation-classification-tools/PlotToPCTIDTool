#
# This is where the functions for the shiny app sit.
# They are called in the server.R file
#



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
  names(matches_top) <- c("Site_Name", paste0("match",1:topn), paste0("group",1:topn)) # hacky, but avoids overhead in the top_n function
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
    formatStyle(grep("match", names(table)), 
              backgroundColor = styleInterval(cuts = c(50,80), 
                                              values = c("white","darkseagreen","chartreuse"))
  )
}

## function to style the output allocation table for distance to centroids
style_matches_cent <- function(table) {
  DT::datatable(table) %>%
    formatStyle(grep("match", names(table)), 
                backgroundColor = styleInterval(cuts = c(0.65,0.7), 
                                                values = c("chartreuse","darkseagreen","white"))
    )
}



# ordination plot of sites ------------------------------------------------

make_sites_ord <- function(infile) {
  ord <- metaMDS(infile[,-1], trace = 0)
}

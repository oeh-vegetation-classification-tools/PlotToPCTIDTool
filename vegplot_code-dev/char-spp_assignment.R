library(dplyr)



# load characteristic species calculations --------------------------------

group_chars <- unlist(readRDS("group_chars_per_RCP.rds"), recursive = F)
group_chars <- lapply(group_chars, '[[', 1)
## hacky AF but need to rectify the issue in optimus really
names(group_chars) <- unlist(lapply(X = strsplit(names(group_chars), ".", fixed = T), 
                                    FUN = function(x) x[2]))
names(group_chars) <- gsub(x = names(group_chars), pattern = "clusterSolution", replacement = "")



# load floristic data for new sites ---------------------------------------

# floristic_raw <- read.table("new_site_allocation/data/4735_East_NewCA_2359.txt", 
#                             sep = "\t", header = T, stringsAsFactors = F) %>%
#   select(-X) %>%
#   mutate_all(as.integer) %>%
#   mutate_all(funs(ifelse(.>0, 1, 0)))
# 
# site_labels <- read.csv("new_site_allocation/data/4735_East_NewCA_2359_CensusList.csv", 
#                         stringsAsFactors = F) %>%
#   select(SiteNo)
# 
# save(floristic_raw, site_labels, file = "floristic_data.RData")

load("floristic_data.RData")



# match each groups char species to new sites -----------------------------

## function to match the spcies list - may want to think about how to weight by richness???
species_percentage_match <- function(chars, species) {
  sum(chars %in% species) / length(chars)
}

# create the species lists for each site
new_site_species <- apply(X = floristic_raw, MARGIN = 1, FUN = function(x){names(x[x > 0])})

# loop over the species lists for the new sites and calculate percentage match for each
char_matches <- matrix(NA, nrow = length(new_site_species), ncol = length(group_chars)) # pre-allocate
for (i in 1:nrow(char_matches)) { # can make this an other lapply when needed
  if (i %in% seq(1, nrow(char_matches), 50)) print (i)
  char_matches[i,] <- unlist(lapply(group_chars, species_percentage_match, new_site_species[[i]]))
}
colnames(char_matches) <- names(group_chars)
# join together with site info
char_matches_all <- data.frame(site_labels, as.data.frame(char_matches))

# make an abridged one with top n matches
top_n <- function(x, n) {
  tops <- sort(x, decreasing = T)[1:n]
  c(tops, names(tops))
}
char_matches_top <- data.frame(site_labels$SiteNo,
                               as.data.frame(t(apply(char_matches, 1, top_n, 10))))
names(char_matches_top) <- c("SiteNo", paste0("perc_match",1:10), paste0("group",1:10)) # hacky, but avoids overhead in the top_n function

# save off
write.csv(char_matches_all, file = "char_spp_matched.csv")
write.csv(char_matches_top, file = "char_spp_top.csv")









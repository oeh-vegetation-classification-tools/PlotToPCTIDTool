library(dplyr)
library(optimus)


# load data
species_allocs <- readRDS("intermediates/floristics_allocations.rds")



# optimus characteristic species ------------------------------------------

## could do it comparing all groups to all other groups, but that is not really sensible...
## but this is how you would do it:
# char_specs_perclust <- get_characteristic(data = species_binary, clustering = rcp_allocations, 
#                                           family = "binomial", type = "per.cluster")
# saveRDS(char_specs_perclust, file = "char_specs_perclust_all-rcps.rds")


## rather we wand to get characteristic species per PCT, within RCP

# function to calculate the char spp for a given rcp
get_characteristic_RCP <- function(x, data, min.occurence = 5) {
  rcp_data <- filter(data, rcp == x)
  group_allocations <- rcp_data$group
  message(paste0("RCP ",x,": There are ",length(unique(group_allocations))," groups."))
  rcp_data <- rcp_data %>%
    select(-(site:rcp)) %>%
    select(which(colSums(.) > 0)) %>% # remove species not in RCP (and pre-speed up for binary conversion)
    mutate_all(funs(ifelse(.>0, 1, 0))) %>% # convert to binary data
    select(which(colSums(.) > min.occurence)) # remove species not in RCP (and less than certain occurence)
  message("Fitting models...")
  get_characteristic(data = rcp_data, clustering = group_allocations,
                     family = "binomial", type = "per.cluster")
}

# do it
group_chars_per_RCP <- lapply(unique(species_allocs$rcp), get_characteristic_RCP, species_allocs)
names(group_chars_per_RCP) <- unique(species_allocs$rcp)
# save the intermediate data - you can pick up from here, because it takes a fair time to do char spp calcs
saveRDS(group_chars_per_RCP, file = "intermediates/group_chars_per_RCP.rds")




# calculate the char spp to feed to tool ----------------------------------


####
####---> pick up from here if already calculated char spp
####
group_chars_per_RCP <- readRDS("intermediates/group_chars_per_RCP.rds")


# unlist into vector strings of species
char_spp_list <- unlist(group_chars_per_RCP, recursive = F)
char_spp_list <- lapply(char_spp_list, '[[', 1)
## hacky AF but need to rectify the issue in optimus really
names(char_spp_list) <- unlist(lapply(X = strsplit(names(char_spp_list), ".", fixed = T), 
                                    FUN = function(x) x[2]))
names(char_spp_list) <- gsub(x = names(char_spp_list), pattern = "clusterSolution", replacement = "")

# save off into tool input folder
saveRDS(char_spp_list, file = "tool_inputs/char_species_list.rds")



# calculate more stats on char spp for interests sake ---------------------

# out puts go to "/intermediates" folder

bind_and_sort <- function(x) {
  class(x) <- "list" # hmm bind_rows() doesn't seem to detect the optimus object also as a list...
  bind_rows(x, .id = "group") %>%
    arrange(desc(coef_value, daic))
}

# get all info for every char spp in every group
chars_per_RCP <- bind_rows(lapply(group_chars_per_RCP, bind_and_sort), .id = "RCP")
chars_per_RCP$group <- gsub("clusterSolution", "", chars_per_RCP$group)

# now summarise per RCP, taking note of how many groups a species is characteristic for
chars_per_RCP_aggr <- chars_per_RCP %>%
  filter(daic > 0) %>%
  select(RCP, variables) %>%
  group_by(RCP, variables) %>%
  mutate(char_freq = n()) %>%
  distinct() %>% 
  ungroup() %>%
  arrange(RCP, desc(char_freq))
#top_n(n = 1, wt = coef_value)

# find species that are only characteristic in one RCP
unique_species <- names(which(table(chars_per_RCP_aggr$variables) <= 1)) # this number sets how many RCPs the char spp can be in
chars_per_RCP_aggr_unique <- filter(chars_per_RCP_aggr, variables %in% unique_species)


# save to disk
write.csv(chars_per_RCP, file = "intermediates/RCP_char-spp_full.csv", row.names = F)
write.csv(chars_per_RCP_aggr, file = "intermediates/RCP_char-spp_aggregated.csv", row.names = F)
write.csv(chars_per_RCP_aggr_unique, file = "intermediates/RCP_char-spp_aggregated_unique.csv", row.names = F)






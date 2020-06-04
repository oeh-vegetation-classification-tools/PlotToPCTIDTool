library(dplyr)
library(optimus)


# load data
species_allocs <- readRDS("intermediates/floristics_allocations.rds")



# optimus characteristic species ------------------------------------------

## could do it comparing all PCTIDs to all other PCTIDs, but that is not really sensible...
## but this is how you would do it:
# char_specs_perclust <- get_characteristic(data = species_binary, clustering = compset_allocations, 
#                                           family = "binomial", type = "per.cluster")
# saveRDS(char_specs_perclust, file = "char_specs_perclust_all-compsets.rds")


## rather we want to get characteristic species per PCT, within compset

# function to calculate the char spp for a given compset
get_characteristic_compset <- function(x, data, min.occurence = 5) {
  compset_data <- filter(data, CharacteristicSpeciesComparisonSet == x)
  PCTID_allocations <- compset_data$PCTID
  message(paste0("Comparison set - ",x,": There are ",length(unique(PCTID_allocations))," PCTIDs."))
  compset_data <- compset_data %>%
    select(-(site:CharacteristicSpeciesComparisonSet)) %>%
    select(which(colSums(.) > 0)) %>% # remove species not in compset (and pre-speed up for binary conversion)
    mutate_all(funs(ifelse(.>0, 1, 0))) %>% # convert to binary data
    select(which(colSums(.) > min.occurence)) # remove species not in compset (and less than certain occurence)
  message("Fitting models...")
  get_characteristic(data = compset_data, clustering = PCTID_allocations,
                     family = "binomial", type = "per.cluster")
}

# do it
PCTID_chars_per_compset <- lapply(unique(species_allocs$CharacteristicSpeciesComparisonSet), get_characteristic_compset, species_allocs)
names(PCTID_chars_per_compset) <- unique(species_allocs$CharacteristicSpeciesComparisonSet)
# save the intermediate data - you can pick up from here, because it takes a fair time to do char spp calcs
saveRDS(PCTID_chars_per_compset, file = "intermediates/PCTID_chars_per_compset.rds")




# calculate the char spp to feed to tool ----------------------------------


####
####---> pick up from here if already calculated char spp
####
PCTID_chars_per_compset <- readRDS("intermediates/PCTID_chars_per_compset.rds")


# unlist into vector strings of species
char_spp_list <- unlist(PCTID_chars_per_compset, recursive = F)
char_spp_list <- lapply(char_spp_list, '[[', 1)
## hacky AF but need to rectify the issue in optimus really
names(char_spp_list) <- unlist(lapply(X = strsplit(names(char_spp_list), ".", fixed = T), 
                                    FUN = function(x) x[2]))
names(char_spp_list) <- gsub(x = names(char_spp_list), pattern = "clusterSolution", replacement = "")

# save off into tool input folder
saveRDS(char_spp_list, file = "tool_inputs/char_species_list.rds")

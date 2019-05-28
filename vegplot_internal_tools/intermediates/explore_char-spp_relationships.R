library(dplyr)
library(ggplot2)

# load data
char_lists <- readRDS("../tool_inputs/char_species_list.rds")
floristics_allocations <- readRDS("floristics_allocations.rds")


## functions to match the spcies list - may want to think about how to weight by richness???
# species_percentage_match <- function(chars, species) {
#   sum(chars %in% species) / length(chars)
# }
# species_n_match <- function(chars, species) {
#   sum(chars %in% species)
# }
# 
# ## function to create the species lists for each site
# calculate_matches <- function(floristic_raw, group_chars) {
#   new_site_species <- apply(X = floristic_raw, MARGIN = 1, FUN = function(x){names(x[x > 0])})
#   # loop over the species lists for the new sites and calculate percentage match for each
#   char_matches <- matrix(NA, nrow = length(new_site_species), ncol = length(group_chars)) # pre-allocate
#   char_nums <- matrix(NA, nrow = length(new_site_species), ncol = length(group_chars)) # pre-allocate
#   print("% matching...")
#   for (i in 1:nrow(char_matches)) { # can make this an other lapply when needed
#     char_matches[i,] <- unlist(lapply(group_chars, species_percentage_match, new_site_species[[i]]))
#   }
#   print("n matching...")
#   for (i in 1:nrow(char_nums)) { # can make this an other lapply when needed
#     char_nums[i,] <- unlist(lapply(group_chars, species_n_match, new_site_species[[i]]))
#   }
#   colnames(char_matches) <- names(group_chars)
#   colnames(char_nums) <- names(group_chars)
#   # join together with site info
#   list(char_matches, char_nums)
# }
# 
# 
# char_spp_results <- calculate_matches(select(floristics_allocations, -(site:rcp)), char_lists)
# 
# saveRDS(char_spp_results, file = "intermediates/char-spp_analysis.rds")

char_spp_results <- readRDS("char-spp_analysis.rds")


# explore results

return_allocated_match <- function(x, matches, group) {
  matches[x,group[x]]
}

allocation_percs <- unlist(
  lapply(1:nrow(floristics_allocations),
         return_allocated_match,
         as.data.frame(char_spp_results[[1]]),
         floristics_allocations$group)
)

allocation_nums <- unlist(
  lapply(1:nrow(floristics_allocations),
         return_allocated_match,
         as.data.frame(char_spp_results[[2]]),
         floristics_allocations$group)
)



ggplot(rbind(data.frame(match = "percentage", value = allocation_percs), 
                  data.frame(match = "count", value = allocation_nums)),
       aes(y = value)) +
  geom_violin(aes(x = match)) + facet_wrap(~match, scales = "free")


quantile(allocation_percs, c(0.01, 0.05,0.1), na.rm = T)




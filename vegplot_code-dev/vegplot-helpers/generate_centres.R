library(dplyr)
library(tibble)

## FUNCTION TO CALCULATE CENTRES
calculate_centres <- function(data, groups) {
  centres <- data.frame(data, group = groups) %>%
    group_by(group) %>%
    summarise_all(funs(mean)) %>%
    as.data.frame()
  row.names(centres) <- centres$group
  as.matrix(centres[,-1])
}



# load floristic data and assignments for all plots in database -----------

floristics_allocations <- readRDS("floristics_allocations.rds")

floristics <- floristics_allocations %>%
  select(-site:-init_groups)

groups <- floristics_allocations %>%
  select(group)



# calculate centres -------------------------------------------------------

# sort the columns by species name
floristics <- floristics[,sort(names(floristics))]

# calculate centroids
centroids <- calculate_centres(floristics, groups)


# save for use in allocation tool
saveRDS(centroids, file = "species_centroids.rds")

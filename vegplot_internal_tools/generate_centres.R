library(dplyr)
library(tibble)

## FUNCTION TO CALCULATE CENTRES
calculate_centres <- function(data, groups) {
  centres <- data.frame(data, PCTID = groups) %>%
    group_by(PCTID) %>%
    summarise_all(mean) %>%
    as.data.frame()
  row.names(centres) <- centres$PCTID
  as.matrix(centres[,-1])
}



# load floristic data and assignments for all plots in database -----------

floristics_allocations <- readRDS("intermediates/floristics_allocations.rds")

floristics <- floristics_allocations %>%
  select(-site:-CharacteristicSpeciesComparisonSet)

groups <- floristics_allocations %>%
  select(PCTID)



# calculate centres -------------------------------------------------------

# sort the columns by species name
floristics <- floristics[,sort(names(floristics))]

# calculate centroids
centroids <- calculate_centres(floristics, groups)


# save for use in allocation tool
saveRDS(centroids, file = "tool_inputs/species_centroids.rds")

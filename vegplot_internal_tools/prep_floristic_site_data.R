library(dplyr)

# set the working directory to the "/vegplot_internal_tools" folder
# setwd("~/PlotToPCTIDTool/vegplot_internal_tools")

# load allocation data ----------------------------------------------------

## this needs to need to point it to the allocations file - needs to have at least columns named:
#     - PCTID (the allocations)
#     - Site.no
#     - PCTAssignmentCategory
#     - CharacteristicSpeciesComparisonSet
#     - it can have others but they will be ignored not required for the floristics prep)

allocations <- read.csv("raw_data/EasternNSWClassification_Version1.1_SiteToPCTID_ALL_WithEnvVars&VegFormation.csv", stringsAsFactors = F) %>%
  filter(Site.no != "") %>%
  filter(PCTAssignmentCategory == "Primary") %>%
  rename(site = Site.no) %>%
  select(site, PCTID, CharacteristicSpeciesComparisonSet)

# checks
message("column names: ", names(allocations))
message("number of unique PCTs: ", length(unique(allocations$PCTID)))
message("number of unique sites: ", length(unique(allocations$site)))



# load floristic data -----------------------------------------------------

# this needs to point to the floristic data - makes sense to process and save it as an .rds file (memory efficient)

## if you need to process the floristic data from a .csv, you can use the code below, noting:
#     - there should be one column with the site names (called Site.no) before the species data start
#     - the code below assumes the first:last species are Abilovat:Zygoiodo
#     - after you've done this once for a particular .csv file, you can comment out the code from the csv read to the rds save

# floristic_export <- read.csv("E/OEH Work/Veg Classn Analysis/MasterDataCopies/allflordata50882x4695_19May2020_sitesxspecies_V1.1.csv")
# # check complete rows/cols (assuming there's just one column called Site.no before hte first species column)
# sum(colSums(floristic_export[,-1]) == 0) # check for any empty species
# sum(rowSums(floristic_export[,-1]) == 0) # check for any empty sites
# # save the data as an .rds file
# saveRDS(floristic_export, "raw_data/east_nsw_floristics_v1.1.rds")

# otherwise we just load the .rds floristic data
species_raw <- readRDS("raw_data/east_nsw_floristics_v1.1.rds") %>%
  select(Site.no, Abilovat:Zygoiodo) %>%
  rename(site = Site.no)



# join data and save off --------------------------------------------------

# just a helper function to find absent species columns, while preserving any 
my_colSums <- function(x) {
  if (!is.numeric(x)) return(T)
  sum(x, na.rm = T) > 0
}

# join species and allocs to get matching data, noting:
#     - the allocation input controls the final selection of sites and their component species
species_allocs <- allocations %>%
  inner_join(species_raw, "site") %>%
  select_if(my_colSums)
# save as .rds file
saveRDS(species_allocs, file = "intermediates/floristics_allocations.rds")
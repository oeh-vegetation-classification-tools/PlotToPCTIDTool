library(dplyr)

# set the working directory to the "/vegplot_internal_tools" folder
# setwd("~/PlotToPCTIDTool/vegplot_internal_tools")

# load allocation data ----------------------------------------------------

## this needs to need to point it to the allocations file - needs to have at least columns named:
#     - PCTID (the allocations)
#     - SiteName
#     - PCTAssignmentCategory
#     - it can have others but they will be ignored not required)

allocations <- read.csv("raw_data/EasternNSWClassification_Version1.1_SiteToPCTID_ALL_WithEnvVars.csv", stringsAsFactors = F) %>%
  filter(SiteName != "") %>%
  filter(PCTAssignmentCategory == "Primary") %>%
  rename(site = SiteName) %>%
  select(SiteName, PCTID)

# checks
message("column names: ", names(allocations))
message("number of unique PCTs: ", length(unique(allocations$PCTID)))
message("number of unique sites: ", length(unique(allocations$SiteName)))



# load floristic data -----------------------------------------------------

# this needs to point to the floristic data - makes sense to process and save it as an .rds file (memory efficient)

## if you need to process the floristic data from a .csv, can use the code below, noting:
#     - there should be one column with the site names (called SiteName) before the species data start
#     - the code below assumes the first:last species are Abilovat:Zygoiodo

# floristic_export <- read.csv("~/data/allflordata49827_27Feb2019_sitesxspecies.csv")
# # check compelte rows/cols (assuming there's just one column called SiteName before hte first species column)
# sum(colSums(floristic_export[,-1]) == 0) # check for any empty species
# sum(rowSums(floristic_export[,-1]) == 0) # check for any empty sites
# # save the data as an .rds file
# saveRDS(floristic_export, "east_nsw_floristics_v1.1.rds")

# otherwise we just load the .rds florsitc data
species_raw <- readRDS("raw_data/east_nsw_floristics_v1.1.rds") %>%
  select(SiteName, Abilovat:Zygoiodo) %>%
  rename(site = SiteName)



# join data and save off --------------------------------------------------

# join species and allocs to get matching data, noting:
#     - the allo
species_allocs <- inner_join(allocations, species_raw, "site")
# save as .rds file
saveRDS(species_allocs, file = "intermediates/floristics_allocations.rds")
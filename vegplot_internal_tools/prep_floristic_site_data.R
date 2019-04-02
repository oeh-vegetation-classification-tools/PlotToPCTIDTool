library(dplyr)

# set the working directory to the "/vegplot_internal_tools" folder
# setwd("~/PlotToPCTIDTool/vegplot_internal_tools")

# load allocation data ----------------------------------------------------

# need to point it to the allocations file - needs to have at lease columns named: group and site (can have others but not required)

allocations <- read.csv("raw_data/PlotAssignMasterRound4_V37_PrimaryOnly_SentMitch22March2019.csv", stringsAsFactors = F) %>%
  filter(group != "") %>%
  mutate(group = gsub(x = group, pattern = ".", replacement = "_", fixed = T)) %>%
  mutate(rcp = unlist(lapply(strsplit(group, "_"), function(x) x[1]))) %>%
  rename(site = SiteName) %>%
  select(site, group, rcp)

# checks
names(allocations); sort(table(allocations$rcp))



# load floristic data -----------------------------------------------------

# need to point it to the floristic data - makes sense to process and save it as an .rds file (memory efficient)

##---> if you need to process the floristic data fro ma .csv, can use something like:
# floristic_export <- read.csv("~/data/allflordata49827_27Feb2019_sitesxspecies.csv")
# # check compelte rows/cols (assuming there's just one column called SiteName before hte first species column)
# sum(colSums(floristic_export[,-1]) == 0) # check for any empty species
# sum(rowSums(floristic_export[,-1]) == 0) # check for any empty sites
# # save the data as an .rds file
# saveRDS(floristic_export)

# otherwise we just load the .rds florsitc data
species_raw <- readRDS("raw_data/east_nsw_floristics_feb19.rds") %>%
  select(SiteName, Abilovat:Zygoiodo) %>%
  rename(site = SiteName)



# join data and save off --------------------------------------------------

# join species and allocs to get matching data
species_allocs <- inner_join(allocations, species_raw, "site")
# save as .rds file
saveRDS(species_allocs, file = "intermediates/floristics_allocations.rds")
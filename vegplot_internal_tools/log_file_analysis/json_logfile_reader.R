library(jsonlite)
library(loggit)
library(rdrop2)
library(dplyr)
library(tidyr)

source("json_logfile_reader_functions.R")

# just check one out
eg_logfile <- read_table("log_0a2cf75571b20a2bac934144584672fd.json")
eg_logfile[[6]]
# try reading json 
read_json("log_0a2cf75571b20a2bac934144584672fd.json")
## rubbish
# read with {loggit}
a_logfile <- loggit::read_logs("log_0a2cf75571b20a2bac934144584672fd.json")
## ding ding ding!


## OK grab files from dropbox directly
## could skip and download manually to same directory
drop_auth(rdstoken = "../../vegplot/www/drop_token.rds") # check the token works

download_folder(path = "PlotToPCTAssignmentTool_ActivityLogFiles/",
                local_path = "PlotToPCTAssignmentTool_ActivityLogFiles/", 
                dtoken = drop_auth(rdstoken = "../../vegplot/www/drop_token.rds"))

# Now we want to iterate over all logfiles 
# do it for all .json files, eventhoug hthey're not in .json format really
log_files <- dir(path = "PlotToPCTAssignmentTool_ActivityLogFiles/",
                 pattern = "*json", full.names = T)

#log_files_list <- lapply(log_files, read_logs)

log_files_df <- bind_rows(
  lapply(log_files, read_logs)
)

# extract missing species lists
cols_to_keep <- c("timestamp","log_lvl","log_msg","log_detail","event","sessionid",
                  "Numplots","Numspecies","NumplotsWithEnvData","NumplotsWithSpatialData")

log_files_species <- log_files_df %>%
  filter(grepl("Species names not found", log_detail, fixed = T)) %>%
  unite("missing_spp", !(any_of(cols_to_keep))) %>%
  mutate(missing_spp = gsub("NA", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("_", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("N/A", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("MissingSpeciesList", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("so were ignored in analysis.", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("</mark>", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("</b>0", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("Spatial data not imported", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("0", "", .$missing_spp)) %>%
  mutate(missing_spp = gsub("all species could be found.", "", .$missing_spp))

log_files_main <- log_files_df %>%
  filter(grepl("Number of plots", log_detail, fixed = T)) %>%
  unite("other_info", !(any_of(cols_to_keep))) %>%
  mutate(other_info = gsub("NA", "", .$other_info)) %>%
  mutate(other_info = gsub("_", "", .$other_info)) %>%
  mutate(other_info = gsub("N/A", "", .$other_info))


# now you're ready to do something with the data!
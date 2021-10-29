library(jsonlite)
library(loggit)
library(rdrop2)
library(dplyr)

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

log_files_df <- bind_rows(
  lapply(log_files, read_logs)
)

# now you're ready to do something with the data!
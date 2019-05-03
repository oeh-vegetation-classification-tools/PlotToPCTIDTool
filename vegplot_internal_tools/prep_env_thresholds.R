raw_thresholds <- read.csv("raw_data/envoutlierthresholds_5Mar2019.csv", stringsAsFactors = F)

raw_thresholds$gp <- gsub(".", "_", raw_thresholds$gp, fixed = T)

raw_thresholds$freq <- NULL

saveRDS(raw_thresholds, file = "tool_inputs/env_thresholds.rds")

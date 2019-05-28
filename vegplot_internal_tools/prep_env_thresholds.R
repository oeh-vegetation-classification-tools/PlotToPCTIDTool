library(dplyr)

env_range <- read.csv("raw_data/envoutlierthresholds_5Mar2019.csv", stringsAsFactors = F) %>%
  select(-freq) %>%
  rename(group = gp)
env_range$group <- gsub(".", "_", env_range$group, fixed = T)


env_minmax <- read.csv("raw_data/RCPGrpEnvKnownRanges_FromPlotAssignMasterRound4_V37_PrimSecOnly_SentMitch23May2019.csv", stringsAsFactors = F) %>%
  select(-Elevation_Med,-RainfallAnn_Med,-TempAnn_Med) %>%
  rename(group = Row.Labels)
env_minmax$group <- gsub(".", "_", env_minmax$group, fixed = T)


env_thresh <- inner_join(env_range, env_minmax, by = "group")


saveRDS(env_thresh, file = "tool_inputs/env_thresholds.rds")

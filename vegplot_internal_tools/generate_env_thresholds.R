library(dplyr)

# set the working directory to the "/vegplot_internal_tools" folder
# setwd("~/PlotToPCTIDTool/vegplot_internal_tools")

# load allocation + env data ----------------------------------------------

## this needs to need to point it to the allocations file - needs to have at least columns named:
#     - PCTID (the allocations)
#     - Site.no
#     - PCTAssignmentCategory
#     - ElevationInMeters
#     - annualRainfallInMillimeters
#     - annualMeanTemperatureInCelsius
#     - it can have others but they will be ignored not required for the generate env thresholds)


site_env <- read.csv("raw_data/EasternNSWClassification_Version1.1_SiteToPCTID_ALL_WithEnvVars&VegFormation.csv", stringsAsFactors = F) %>%
  filter(PCTAssignmentCategory %in% c("Primary","Secondary")) %>%
  rename(site = Site.no) %>%
  filter(site != "") %>%
  select(site, PCTID, ElevationInMeters, annualRainfallInMillimeters, annualMeanTemperatureInCelsius) %>%
  mutate_at(vars(ElevationInMeters, annualRainfallInMillimeters, annualMeanTemperatureInCelsius), as.numeric)

T1_calc <- function(x) {
  t1 <- quantile(x, 0.25, na.rm = T) - (3 * (quantile(x, 0.5, na.rm = T) - quantile(x, 0.25, na.rm = T)))
  if (t1 < 0) {t1 <- 0}
  as.numeric(t1)
}

T2_calc <- function(x) {
  t2 <- quantile(x, 0.75, na.rm = T) + (3 * (quantile(x, 0.75, na.rm = T) - quantile(x, 0.5, na.rm = T)))
  as.numeric(t2)
}


env_thresholds <- site_env %>%
  group_by(PCTID) %>%
  summarise(T1_tempann = T1_calc(annualMeanTemperatureInCelsius),
            T1_rainann = T1_calc(annualRainfallInMillimeters),
            T1_elev = T1_calc(ElevationInMeters),
            T2_tempann = T2_calc(annualMeanTemperatureInCelsius),
            T2_rainann = T2_calc(annualRainfallInMillimeters),
            T2_elev = T2_calc(ElevationInMeters)
)


saveRDS(env_thresholds, file = "tool_inputs/env_thresholds.rds")

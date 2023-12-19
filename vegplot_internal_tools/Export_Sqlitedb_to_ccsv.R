# EM recording method for exporting data from the pctdatadb.sqlite file in order to examine and check it

con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")

pctprofiledata <- dbReadTable(con, "pctprofiledata")

write.csv(pctprofiledata, "C:/Users/magaree/PlotToPCTID_CheckSqliteFile/pctprofiledata.csv")

pctspeciesgrowthforms <- dbReadTable(con, "pctspeciesgrowthforms")

write.csv(pctspeciesgrowthforms, "C:/Users/magaree/PlotToPCTID_CheckSqliteFile/pctspeciesgrowthforms.csv")

TECData <- dbReadTable(con, "TECData")

write.csv(TECData, "C:/Users/magaree/PlotToPCTID_CheckSqliteFile/TECData.csv")

PCT_TECData <- dbReadTable(con, "PCT_TECData")

write.csv(PCT_TECData, "C:/Users/magaree/PlotToPCTID_CheckSqliteFile/PCT_TECData.csv")

fsdata <- dbReadTable(con, "fsdata")

write.csv(fsdata, "C:/Users/magaree/PlotToPCTID_CheckSqliteFile/fsdata.csv")
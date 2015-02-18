
require("activityinfo")

# Load required functions:
source("matchLevel.R")

hh <- read.csv(file="qis/round01/HH-2013_05_20.csv", stringsAsFactors=FALSE)

vwc <- hh[!duplicated(hh$psu), ]
vwc$divisionId <- matchLevel(1523, vwc$district, field = "parentId")
vwc$districtId <- matchLevel(1523, vwc$district)
vwc$upazilaId <- matchLevel(1534, vwc$upazila)
vwc$locationId <- sapply(1:nrow(vwc), function(i) generateId())
# saveRDS(vwc, "qis/round01/vwc.rds")


hh.merged <- merge(hh, vwc[, c("psu", "locationId")], all.x=TRUE)
hh.merged$siteId <- sapply(1:nrow(hh), function(i) generateId())
hh.merged$periodId <- sapply(1:nrow(hh), function(i) generateId())
# saveRDS(hh.merged, "qis/round01/hh.rds")


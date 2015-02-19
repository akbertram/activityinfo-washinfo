
require("activityinfo")

hh <- read.table(
  file = file.path("qis", "round03", "frame", "R03HH_MailMergeData.csv"),
  header = TRUE,
  sep=",",
  stringsAsFactors = FALSE,
  fill = TRUE,
  quote = "" # required to deal with the quote in "Cox's Bazar"
  )

# remove records with no barcode:
hh <- hh[!is.na(hh$Barcode),]
cat("There are", nrow(hh), "records in the households sample frame.\n")

schema <- getDatabaseSchema(2188)

getActivity <- function(id) {
  for (activity in schema$activities) {
    if (activity$id == id) return(activity)
  }
  stop("no activity with id ", id, " found in schema for database ", schema$id)
}
hh.form <- getActivity(12950)

cat("...Downloading records from ActivityInfo...\n")
sites <- getSitesDataFrame(hh.form)[, c("Barcode", "locationId")]
cat("There are", length(unique(sites$Barcode)), "household records in ActivityInfo.\n")

# merge names of administrative levels into the AI records:
if (!exists("location.table")) {
  source("locations.R")
}
sites <- merge(sites, location.table, by = "locationId")

# To-do:
# 2) check if current data in AI is actually round 3 data for households
# 3) if every record matches in (1), then add missing records from "hh" to AI.

# match districts and upazilas in "hh" to admin levels in AI:
source("matchLevel.R")
hh$divisionId <- matchLevel(1523, hh$District, field = "parentId")
hh$districtId <- matchLevel(1523, hh$District)
hh$upazilaId <- matchLevel(1534, hh$Upazila)

# for each record in the household sample frame, check if the VWC can be matched to a location in AI:
hh$locationId <- rep(NA, nrow(hh))
for (i in seq(nrow(hh))) {
  # first find all locations in the given adminstrative area:
  jj <- which(location.table$divisionId == hh$divisionId[i] & 
                location.table$districtId == hh$districtId[i] &
                location.table$upazilaId == hh$upazilaId[i])
  if (length(jj)) {
    location.names <- location.table$name[jj]
    k <- amatch(toupper(hh$VWC[i]), toupper(location.names), maxDist = 10)
    hh$locationId[i] <- ifelse(is.na(k), NA, location.table$locationId[jj][k])
  }
}
cat(sum(is.na(hh$locationId)), "household records cannot be linked to a location in AI.\n")

hh$in.AI <- hh$Barcode %in% sites$Barcode
cat("There are", sum(!hh$in.AI), "out of", sum(!duplicated(hh$Barcode)),
    "household records for round 3 missing in AI.\n")

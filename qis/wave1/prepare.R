


getAdminTable <- function(levelId) {
  
  entities <- getAdminLevel(levelId)
  data.frame(name = activityinfo:::extractField(entities, "name"),
             id = activityinfo:::extractField(entities, "id"),
             parentId = activityinfo:::extractField(entities, "parentId"),
             stringsAsFactors = FALSE)
  
}

matchLevel <- function(levelId, names, field = "id") {
  
  adminTable <- getAdminTable(levelId)
  
  uniqueNames <- names
  matches <- amatch(toupper(uniqueNames), table = toupper(adminTable$name), maxDist=10)

  if(any(is.na(matches))) {
    cat("Could not match:\n", paste(uniqueNames[is.na(matches)], collapse="\n"), "\n")
  }
  
  cat(paste(sort(unique(sprintf("%s => %s", uniqueNames, adminTable$name[matches]))), collapse="\n"))
  
  adminTable[[field]][matches]
}

hh <- read.csv(file="qis/wave1/HH-2013_05_20.csv", stringsAsFactors=FALSE)

vwc <- hh[!duplicated(hh$psu), ]
vwc$divisionId <- matchLevel(1523, vwc$district, field = "parentId")
vwc$districtId <- matchLevel(1523, vwc$district)
vwc$upazilaId <- matchLevel(1534, vwc$upazila)
vwc$locationId <- sapply(1:nrow(vwc), function(i) generateId())
saveRDS(vwc, "qis/wave1/vwc.rds")


hh.merged <- merge(hh, vwc[, c("psu", "locationId")], all.x=TRUE)
hh.merged$siteId <- sapply(1:nrow(hh), function(i) generateId())
hh.merged$periodId <- sapply(1:nrow(hh), function(i) generateId())
saveRDS(hh.merged, "qis/wave1/hh.rds")



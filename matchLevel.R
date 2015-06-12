
require("stringdist")

getAdminTable <- function(levelId) {
  
  entities <- getAdminLevelEntities(levelId)
  data.frame(name = activityinfo:::extractField(entities, "name"),
             id = activityinfo:::extractField(entities, "id"),
             parentId = activityinfo:::extractField(entities, "parentId"),
             stringsAsFactors = FALSE)
  
}

findLevel <- function(country, level.name) {
  levels <- getAdminLevels(country)
  for(level in levels) {
    if(identical(level$name, level.name)) {
      return(level)
    }
  }
  stop(sprintf("No such level '%s': existing levels are: %s", level.name, 
               paste(collapse = ", ", sapply(levels, function(level) level$name))))
}

lookupKey <- function(district, upazilla) {
  key <- toupper(paste(district, upazilla, sep="|"))
  key <- gsub(pattern = "\\s+$", x = key, replacement = "")
  key <- gsub(pattern = "\\s+", x = key, replacement = " ")
  key <- gsub(pattern = "^\\s*", x = key, replacement = "")
}


mergeAdminLevels <- function(x, districtColumn, upazillaColumn, rename = list()) {
  
  # Build a lookup table for district/upazilla pairs
  district <- getAdminTable(1523)
  upazilla <- getAdminTable(1534)
  names(district) <- c("district", "E1523", "E1267")
  names(upazilla) <- c("upazilla", "E1534", "E1523")
  adminLookup <- merge(upazilla, district)
  adminLookup$key <- lookupKey(adminLookup$district, adminLookup$upazilla)

  # Prepare the list of district/upazillas from the import
  districtColumn <- as.character(districtColumn)
  upazillaColumn <- as.character(upazillaColumn)
  
  # Apply synonyms 
  for(from in names(rename)) {
    to = rename[from]
    districtColumn[ districtColumn == from ] <- to
    upazillaColumn[ upazillaColumn == from ] <- to
  }
  
  # Find all the unique upazila/districts in the import source
  # and match to the lookup table
  sourceKey <- lookupKey(districtColumn, upazillaColumn)
  uniqueSourceKey <- unique(sourceKey)

  matches <- amatch(uniqueSourceKey, table = adminLookup$key, maxDist=10)
  names(matches) <- uniqueSourceKey
  
  if(any(is.na(matches))) {
    stop(sprintf("Could not match:\n %s\n", paste(uniqueSourceKey[is.na(matches)], collapse="\n")))
  }
  
  # Find the indexes within the lookup table for each source row
  adminIndex <- matches[ sourceKey ]
  for(id in c("E1267", "E1523", "E1534")) {
    x[[id]] <- adminLookup[[id]][ adminIndex ]
  }
  x
}

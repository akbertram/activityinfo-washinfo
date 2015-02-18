
require("stringdist")

getAdminTable <- function(levelId) {
  
  entities <- getAdminLevelEntities(levelId)
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

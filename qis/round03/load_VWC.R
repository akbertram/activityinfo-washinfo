
# Add area attribute
sites <- getSites(23588)


rematch <- read.csv("qis//round03/vwcs-rematch.csv")

for(i in 1:nrow(rematch)) {
  siteId <- rematch$SiteId[i]
  barcode <- as.character(rematch$barcode[i], 1, 1)
  if(!is.na(barcode)) {
    changes <- list()
    if(substr(barcode,1,1) == '1') {
      changes$ATTRIB2147467877 <- TRUE
    } else {
      changes$ATTRIB2147467878 <- TRUE
    }
    changes$I2146411757 <- barcode
    
    executeCommand("UpdateSite", siteId=siteId, changes=changes)
    }
}

rematch.admin <- mergeAdminLevels(rematch, rematch$District, rematch$Upazila, 
                                  rename = c('Lalmonirhat Sadar' = 'Patgram'))


for(i in 1:length(sites)) {
  rematchIndex <- which(rematch.admin$SiteId == sites[[i]]$id)
  if(length(rematchIndex) == 1) {
    properties <- unclass(rematch.admin[rematchIndex, c("E1267", "E1523", "E1534")])
    properties$id <- sites[[i]]$location$id
    properties$longitude <- rematch.admin$longitude[rematchIndex]
    properties$latitude <- rematch.admin$latitude[rematchIndex]
    if(!is.na(properties$longitude) && !is.na(properties$latitude)) {
      executeCommand("CreateLocation", properties = properties)
    }
  }
}
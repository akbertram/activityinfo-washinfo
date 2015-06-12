library(activityinfo)

# First fetch the original records from the version 3.0 deployment
activityInfoRootUrl("https://build42-dot-ai-production.appspot.com")
activityInfoLogin("shahinur.is@brac.net", "testing123")
sites <- getResource("sites", activity=1590734674)

# Now fetch the sites that have been pushed to AI 2.8
activityInfoRootUrl("https://www.activityinfo.org")
activityInfoLogin()
db <- getDatabaseSchema(2188)
hhForm <- db$activities[[2]]
hh <- getSitesDataFrame(hhForm)

# Now create the list of site attachments
# The QIS 3 household survey had two images: latrines, and water points

nz <- function(x) if(is.null(x)) NA else x

images <- rbind(
  data.frame(list(
    barcode =  sapply(sites, function(s) nz(s$indicatorValues$`1604516018`)),
    cuid = sapply(sites, function(s) nz(s$indicatorValues$`1886116666`)),
    fileName = "waterpoint.jpg"), stringsAsFactors = FALSE),
  data.frame(list(
    barcode = sapply(sites, function(s) nz(s$indicatorValues$`1604516018`)),
    cuid = sapply(sites, function(s) nz(s$indicatorValues$`1876323691`)),
    fileName = "latrine.jpg"), stringsAsFactors = FALSE))
  
# Merge the SiteId from the barcode
hh.barcodes <- with(hh, data.frame(siteId=siteId, barcode=barcode, stringsAsFactors = FALSE))
attachments <- merge(images, hh.barcodes)

# create a new uuid for the image
library(uuid)
attachments$blobId <- replicate(toupper(UUIDgenerate()), n = nrow(attachments))


# Upload images as attachments

library(httr)
for(i in 28:nrow(attachments)) {
  with(attachments[i, ], {
  sourceFile <-  file.path("qis", "round03", "images", attachments$cuid[i])
  
  if(!file.exists(sourceFile)) {
    cat(sprintf("Could not find blob %s for site %d (barcode %s)\n", cuid, siteId, barcode ))
  } else {
    imageFile =  sprintf("/tmp/%s-%s", barcode, fileName)
    file.copy(sourceFile, imageFile)
    
    cat(sprintf("Uploading %s...\n", imageFile))
    
  
    POST(url = "https://www.activityinfo.org/ActivityInfo/attachment",
      activityinfo:::activityInfoAuthentication(),
      encode = "multipart",
      query = list(siteId = siteId, blobId = blobId),
      body = list(file = upload_file(imageFile)))
  
    }
  })
}



                     
                     

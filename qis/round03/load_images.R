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

# Divide into chunks to be uploaded in parallel
attachments$chunk <- as.integer(cut(1:nrow(attachments), breaks = 20))

write.csv(file="qis/round03/images.csv", attachments)

writeLines(con="qis/round03/upload_images.sh", 
           sprintf("Rscript upload_images.R %d > chunk%d.log &\n", unique(attachments$chunk), unique(attachments$chunk)))

                     
                     

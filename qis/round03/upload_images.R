
library(httr)
library(activityinfo)

chunkNumber <- as.integer(commandArgs(trailingOnly = TRUE))

cat(sprintf("Starting chunk #%d\n", chunkNumber))

# Upload images as attachments

attachments <- read.csv("images.csv")

# Skip already uploaded rows
attachments <- attachments[203:nrow(attachments), ]

attachments <- subset(attachments, chunk == chunkNumber)

for(i in 1:nrow(attachments)) {
  with(attachments[i, ], {
    sourceFile <-  file.path("images", attachments$cuid[i])
    
    if(!file.exists(sourceFile)) {
      cat(sprintf("Could not find blob %s for site %d (barcode %s)\n", cuid, siteId, barcode ))
    } else {
      imageFile =  sprintf("/tmp/%s-%s", barcode, fileName)
      file.copy(sourceFile, imageFile)
      
      cat(sprintf("Uploading %s... (%s)\n", imageFile, blobId))
      
      
      POST(url = "https://www.activityinfo.org/ActivityInfo/attachment",
           activityinfo:::activityInfoAuthentication(),
           encode = "multipart",
           query = list(siteId = siteId, blobId = as.character(blobId)),
           body = list(file = upload_file(imageFile)))
      
    }
  })
}

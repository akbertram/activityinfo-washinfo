
schoolLocationTypeId <- 51312L

schools <- read.csv("qis/round03/raw_data/R03_Schools_20150515.csv")
schools$Barcode <- as.character(schools$Barcode)

schools.meta <- read.csv("qis/round03/frame/R03School_MailMergeData.csv")
schools.meta$ID <- as.character(schools.meta$ID)

schools <-  merge(schools, schools.meta, by.x="Barcode", by.y="ID")
schools$locationId <- generateId(n = nrow(schools))

schools <- mergeAdminLevels(schools, 
                 schools$District, schools$Upazila, 
                 rename = list(Companigonj = 'Companiganj',
                               Gaffargaon = 'Gaffargaon'))

schools$locationId <- generateId(n = nrow(schools))
schools$siteId <- generateId(n = nrow(schools))
schools$reportingPeriodId <- generateId(n = nrow(schools))

# Create location objects first
for(i in 27:nrow(schools)) {
  location <- c(schools[i, c("E1267", "E1523", "E1534")])
  location$longitude <- schools$Latitude.Longitude...Longitude[i]
  location$latitude <- schools$Latitude.Longitude...Latitude[i]
  location$name <- gsub(as.character(schools$Name_School[i]), pattern="^\\s+", replacement = "")
  location$id <- schools$locationId[i]
  location$locationTypeId <- schoolLocationTypeId
  
  if(nchar(location$name) > 50) {
    location$name <- substr(location$name, 1, 50)
  }
  
  cat(sprintf("%d: %s\n", i, location$name))
  executeCommand("CreateLocation", properties = location)
}

# Now create the site objects
schoolSites <- schools[, c("locationId", 
                         "reportingPeriodId",
                         "Barcode", 
                         "Type.of.school.", "PSU", "Remarks", 
                         grep(names(schools), pattern = "^SS*", value=T))]
schoolSites$id <- schools$siteId
schoolSites$Remarks <- as.character(schoolSites$Remarks)
schoolSites$Area <- factor(ifelse(schools$PSU %in% c(41, 42), "Area I", "Area II"))



# Add standard binary codings
for(var in names(schoolSites)) {
  if("A" %in% levels(schoolSites[[var]])) {
    schoolSites[[paste("b", var, sep = "")]] <- factor(ifelse(schoolSites[[var]] %in% c("A", "B", "C"), 
                                                              "At or Above Standard", "Below standard"))
  }
}

sites <- prepareForm(schoolSites, databaseId = 2188, name = "Schools", locationTypeId = schoolLocationTypeId)
loadForm(sites)





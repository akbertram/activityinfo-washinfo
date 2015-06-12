
rscLocationTypeId <- 51313L

rsc <- read.csv("qis/round03/raw_data/R03_RSC_20150515.csv")
rsc$Barcode <- as.character(rsc$Barcode)

rsc.meta <- read.csv("qis/round03/frame//R03RSC_MailMergeData.csv")
rsc.meta$ID <- as.character(rsc.meta$ID)

rsc <-  merge(rsc, rsc.meta, by.x="Barcode", by.y="ID")

rsc <- mergeAdminLevels(rsc, rsc$District, rsc$Upazila, rename = c('Lalmonirhat Sadar' = 'Patgram'))

rsc$locationId <- generateId(n = nrow(rsc))
rsc$siteId <- generateId(n = nrow(rsc))
rsc$reportingPeriodId <- generateId(n = nrow(rsc))

# Create location objects first
for(i in 1:nrow(rsc)) {
  location <- c(rsc[i, c("E1267", "E1523", "E1534")])
  location$name <- as.character(rsc$Name_RSC[i])
  location$id <- rsc$locationId[i]
  location$axe <- as.character(rsc$PSU[i])
  location$locationTypeId <- rscLocationTypeId
  location$longitude <- rsc$Latitude.Longitude...Longitude
  location$latitude <- rsc$Latitude.Longitude...Latitude
  
  if(nchar(location$name) > 50) {
    location$name <- substr(location$name, 1, 50)
  }
  
  cat(sprintf("%d: %s\n", i, location$name))
  executeCommand("CreateLocation", properties = location)
}

# Now create the site objects
rscSites <- rsc[, c("locationId", 
                         "reportingPeriodId",
                         "Barcode", 
                         "Furthest.distance.from.RSC.to.households.of.the.union",
                         "Travel.time..to.and.from..from.RSC.to.ultra.poor.households.of.the.union.",
                         "Support.received.from.the.BRAC.WASH.Programme.",
                         "RSC01.Performance.of.RSC.",
                         "Comment..if.any..",
                         "PSU", "PSU_RSC",
                         "loan",
                         "Orientation",
                         "Loan...orientation.",
                         "Remarks")]
rscSites$id <- rsc$siteId
rscSites$Area <- factor(ifelse(rsc$PSU %in% c(10, 14), "Area I", "Area II"))



# Add standard binary codings
for(var in names(rscSites)) {
  if("A" %in% levels(rscSites[[var]])) {
    rscSites[[paste("b", var, sep = "")]] <- factor(ifelse(rscSites[[var]] %in% c("A", "B", "C"), 
                                                              "At or Above Standard", "Below standard"))
  }
}

sites <- prepareForm(rscSites, databaseId = 2188, name = "rscs", locationTypeId = rscLocationTypeId)
loadForm(sites)






schema <- getDatabaseSchema(databaseId=1736)
hh.form <- schema$activities[[1]]



for(i in 1:nrow(hh.merged)) {
  
  row <- hh[i, ]
  
  site <- list(
    id = row$siteId,
    reportingPeriodId = row$periodId,
    locationId = row$locationId,
    date1 = substr(row$X_CREATION_DATE, 1, 10),
    date2 = substr(row$X_CREATION_DATE, 1, 10),
    partnerId = schema$partners[[1]]$id,
    activityId = hh.form$id)
  
  
  site[[paste("ATTRIB", switch(row$wealth, up=252806784, pp=1668522016, np=1208930133), sep="")]] <- TRUE
  site[[paste("ATTRIB", switch(row$HH01, A=627335107, B=43841142, C=660019566, D=1562988226, E=1149579133), sep="")]] <- TRUE
  
  createSite(site)
  
  cat(sprintf("Created site %i\n", i))
}

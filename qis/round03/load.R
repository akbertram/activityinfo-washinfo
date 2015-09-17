

# Merge HH with VWC
# Standardize keys by removing punctuation, spaces, and case
hh.weighted$vwc.key <- gsub(toupper(hh.weighted$vwc), pattern="[^A-Z1-9]", replacement = "")
location.table$vwc.key <-  gsub(toupper(location.table$name), pattern="[^A-Z1-9]", replacement = "")
matched <- hh.weighted$vwc.key %in% location.table$vwc.key

hh <- merge(hh.weighted, location.table[, c("vwc.key", "locationId")], all.x=T)
hh <- hh[!duplicated(hh$barcode), ]




# Define common order of levels
wealthLevels <- c("UP", "PP", "NP")

score <- function(df, scoreCode) {
  factor(find(df, scoreCode), levels = toupper(letters[1:6]))
}

find <- function(df, prefix) {
  columnName <- grep(names(df), pattern = paste(prefix, ".", sep=""), fixed=TRUE, value=TRUE)
  if(length(columnName) == 0) {
    stop(prefix)
  }
  df[[columnName]]
}


# Define the area variable

hh.clean <- data.frame(
  id = replicate(n = nrow(hh), expr = generateId()),
  locationId = hh$locationId,
  barcode = as.character(hh$barcode),
  reportingPeriodId = replicate(n = nrow(hh), expr = generateId()),
  wealth = factor(hh$wealth, levels = wealthLevels),
  wealth.reported = factor(hh$economic.status.during.data.collection, levels = wealthLevels),
  area = factor(ifelse(hh$psu %in% c(14,15), "Area I", "Area II")),
  hhma = find(hh, "hhma"),
  hhfa = find(hh, "hhfa"),
  hhmado = find(hh, "hhmado"),
  hhfado = find(hh, "hhfado"),
  hhmh5 = find(hh, "hhmh5"),
  hhfm5 = find(hh, "hhfh5"),
  hhmsp = find(hh, "hhmsp"),
  hhfsp = find(hh, "hhfsp"),
  hhsx = factor(find(hh, "hhsx"), levels=c("female", "male")),
  hh01.old = score(hh, "hh01.old"),
  hh01.new = score(hh, "hh01.new"),
  hh02.old = score(hh, "hh02old"),
  hh02.new = score(hh, "hh02new"),
  hh03.old = score(hh, "hh03old"),
  hh03.new = score(hh, "hh03new"),
  hh03a = score(hh, "hh03a"),
  hh03b = score(hh, "hh03b"),
  hh04 = score(hh, "hh04"),
  hh04a = score(hh, "hh04a"),
  hh05 = score(hh, "hh05"),
  hh06 = score(hh, "hh06"),
  hh07.dne = score(hh, "hh07.dne"),
  hh07.ftr = score(hh, "hh07.ftr"),
  remarks = hh$remarks,
  stringsAsFactors = FALSE)

for(x in grep(pattern = "^hh0", x = names(hh.clean), value = TRUE)) {
  hh.clean[[paste("b", x, sep = "")]] <- factor(ifelse(hh.clean[[x]] %in% c("A", "B", "C"), "At or Above Standard", "Below standard"))
}

vwc.clean <- data.frame(
  id = replicate(n = nrow(vwc), expr = generateId()),
  locationId = vwc$locationId,
  

sites <- prepareForm(databaseId = 2188, name = "QIS3_9", locationTypeId=51036, data = hh.clean)
loadForm(sites)






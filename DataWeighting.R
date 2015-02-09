

# Load household results
hh <- read.csv("HH QIS 3.csv")

hh$code <- as.character(hh$Barcode)
hh$valid.code <- !is.na(hh$code) & nchar(hh$code) == 10
hh$vwc.code <- substr(hh$code, 1, 8)
hh$wealth <- as.character(hh$Economic.status.during.data.collection)
hh$wealth[ hh$wealth == "" ] <- NA

# Consider only those with valid codes
hh.valid <- subset(hh, valid.code & !is.na(hh$wealth))
hh.counts <- melt( with(hh.valid, table(vwc.code, wealth)), value.name="sampled")
hh.counts$vwc.code <- as.character(hh.counts$vwc.code)

# Get the number of hh per wealth category per VWC
vwc.frame <- read.csv("VWC_Frame.csv")
vwc.frame <- with(vwc.frame, data.frame(vwc.code = as.character(ID),
                                        UP = Ultra.poor,
                                        PP = Poor,
                                        NP = Non.Poor,
                                        stringsAsFactors = FALSE))
vwc.pop <- melt(vwc.frame)
names(vwc.pop) <- c("vwc.code", "wealth", "population")

# Combine vwc populations and sample sizes
weights <- merge(hh.counts, vwc.pop, all.x=TRUE)
weights$ip <- with(weights, sampled / population)
weights$weight <- 1 / weights$ip

# Add to hh frame
hh <- merge(hh, weights, all.x=TRUE)

write.csv(hh, "HH QIS 3 weighted 20141007.csv")

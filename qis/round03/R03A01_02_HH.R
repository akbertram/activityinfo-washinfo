# change working directory:
wd <- setwd(file.path("qis", "round03"))

base.name <- "R03A01_02_HH"
raw <- read.table(file = file.path("raw_data", paste(base.name, ".csv", sep = "")),
                  sep = ",",
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  fill = TRUE,
                  quote = "\""
)
frame <- read.table(file = file.path("frame", "R03HH_MailMergeData.csv"),
                    sep = ",",
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    fill = TRUE,
                    quote = "\"" # required to deal with the quote in "Cox's Bazar"
)

# merge survey data with sample frame (somehow the order() function is required
# to make all records in 'raw' link to a record in 'frame'):
hh <- merge(raw[order(raw$Barcode),], frame, by = "Barcode", all.x = TRUE)

# change names from ABC_DEFG to abc.defg
names(hh) <- gsub(x=tolower(names(hh)), pattern="_", replace=".")


# create a list by splitting the table per VWC:
vwc.list <- split(hh, hh$vwc)

# wealth categories:
wealth <- data.frame(
  code = c("UP", "PP", "NP"),
  category = c("ultra.poor", "poor", "non.poor"),
  stringsAsFactors = FALSE
)

# calculate the weights per VWC:
res <-lapply(vwc.list, function(vwc) {
  vwc$weight <- NA
  vwc$surveyed <- NA
  # first check for missing numbers in the 'ultra.poor', 'poor' and 'non.poor' columns:
  for (i in seq(nrow(wealth))) {
    category <- wealth$category[i]
    code <- wealth$code[i]
    
    if (all(is.na(vwc[[category]]))) {
      # replace the NA with an estimate:
      other.codes <- setdiff(wealth$code, code)
      other.categories <- setdiff(wealth$category, category)
      factor <- sum(vwc$wealth %in% other.codes)/sum(vwc[1, other.categories])
      estimate <- ceiling(sum(vwc$wealth == code)/factor)
      vwc[[category]] <- rep(estimate, nrow(vwc))
      cat("The VWC with name '", vwc$name.of.vwc[1],
          "' is missing the number of ", category,
          " households. This has been set to ", estimate, "\n", sep = "")
      
    }
  }
  
  # now that missing numbers are replaced with estimates, we add a column with
  # the population size (i.e. the number of households in this context):
  vwc$population <- rowSums(vwc[wealth$category])
  
  # then calculate the weights:
  for (i in seq(nrow(wealth))) {
    category <- wealth$category[i]
    code <- wealth$code[i]
    
    fraction <- vwc[[category]][1]/vwc[["population"]][1]
    jj <- which(vwc$wealth == code)
    vwc$surveyed[jj] <- length(jj)
    vwc$weight[jj] <- fraction/length(jj)
  }
  vwc
})

# write a single VWC to CSV for checking purposes:
write.table(res[["Akhilpur"]][c("barcode", "vwc", "wealth", "ultra.poor", "poor", "non.poor", "population", "surveyed", "weight")],
            file = paste(base.name, "Akhilpur.csv", sep = "_"),
            row.names = FALSE,
            col.names = TRUE,
            quote = TRUE,
            sep = ",")

# put the household table back together:
hh.weighted <- do.call(rbind, res)


write.table(hh.weighted,
            file = paste(base.name, "weighted.csv", sep = "_"),
            row.names = FALSE,
            col.names = TRUE,
            quote = TRUE,
            sep = ",")

# set working directory back to original:
setwd(wd)

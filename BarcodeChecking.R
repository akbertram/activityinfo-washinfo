# To check barcodes, Which are double and which are invalid
# Save those lists in "QIS Cleaning Info"

# Returns the 'values' data frame with those records (i.e. rows) removed that
# a) do not have a barcode or
# b) have a barcode which doesn't exist in the sample frame.

BarcodeChecking <- function(values, sampleframe, round, survey) {

    # Remove enteries which have no barcode:
    values <- values[!is.na(values$Barcode), ]
    
    # write the number of records remaining in the log in the "QIS Cleaning
    # Info" subdirectory
    RowInData <- nrow(values)
    line <-  paste("After deleting records without barcode",
                   RowInData, "records remain.", sep =" ")
    cat(line, "\n", file = cleanfilename, append = TRUE)

    # Check the number of duplicated barcode values and write a list in the
    # "QIS Cleaning Info" subdirectory
    filename <- file.path("QIS Cleaning Info",
                          sprintf("R%02d%s_Duplicates.tsv", round, survey))
    # write the number of records of which the barcode appears more than once
    # and write it away in the log in the "QIS Cleaning Info" subdirectory

    duplicate.rows <- which(duplicated(values$Barcode))

    barcodes <- aggregate(list(count = values$Barcode),
                          by = list(barcode = values$Barcode),
                          FUN = length)

    if (max(barcodes$count > 1)) {
      # sort by count in decreasing order:
      barcodes <- barcodes[order(barcodes$count, decreasing=TRUE),]
      
      write.table(barcodes[barcodes$count > 1,],
                  file = filename,
                  quote = FALSE,
                  sep = "\t",
                  row.names = FALSE,
                  col.names = TRUE)
    } else {
      cat("There are no duplicate barcodes.\n", file = filename, append = FALSE)
    }
    RowInData <- length(duplicate.rows)

    line <- paste(RowInData,
                  " of the records have a barcode which appears more than once",
                  " in the data set. They should be unique.", sep = "")
    cat(line, "\n", file = cleanfilename, append = TRUE)

    # Check if there are Barcode number in dataset (values) which are not in
    # the sample frame (i.e. these are considered invalid)
    InvalidBC.idx <- !(values$Barcode %in% sampleframe$ID)
    nInvalidBC <- sum(InvalidBC.idx) # number of records with an invalid barcode
    if (nInvalidBC > 0) {
        InvalidBC <- data.frame(invalid.barcodes = unique(values$Barcode[InvalidBC.idx]))
        filename <- file.path("QIS Cleaning Info",
                              sprintf("R%02d%s_InvalidBC.tsv", round, survey))
        write.table(InvalidBC,
                    file = filename,
                    quote = FALSE,
                    sep = "\t",
                    row.names = FALSE,
                    col.names = FALSE)

        # write the number of records with barcodes which do not appear in the sample frame
        # and write it away in the log in the "QIS Cleaning Info" subdirectory
        line <-  paste(nInvalidBC,
                       " of the records in the data set have a barcode ",
                       "which doesn't exist in the sample frame.", sep = "")
        # remove those records with a barcode that's not in the sampleframe:
        values <- values[!InvalidBC.idx,]
    } else {
        line <- paste("All records in the data set have a barcode ",
                      "which exists in the sample frame.", sep = "")
    }

    write(line, file = cleanfilename, append = TRUE)

    # return the data with only the correct barcodes:
    return(values)
}

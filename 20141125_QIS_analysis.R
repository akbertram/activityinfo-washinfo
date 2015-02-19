# Script to get the QIS data from Activity Info, clean it and write it away for analysis

library(activityinfo) # For more info to install and use check at https://github.com/bedatadriven/activityinfo-R
library(knitr)
library(reshape2) # required for calculation of weights for round 3 HH survey

setwd("~/Google Drive/LASA/BRAC WASH DATA collection 2014/QIS analysis") #KB's working directory
# setwd("E:/Google Drive/QIS analysis") #CV's working directory #CV's working directory
# External functions
source("BarcodeChecking.R")
source("RenameVariables.R")
source("CheckDataValidity.R")
source("DatasetMerging.R")
source("RSCstatistics.R")
source("VWCstatistics.R")
source("SanitizeStrings.R")

# Declare variables
rounds <- 3L # Which rounds will be prepared for analysis, can be a list such as c(1,2,3)
areas <- c(1,2) # Which area will be included, can be a list such as c(1,2,3)
surveys <- c("HH") # Which surveys will be prepared for analysis, can be a list such as
# surveys <- c("HH","VWC","RSC","School")



# Declare the locations in AI where the data is.
# Currently only round 3 is avaialable
forms <- list(
  round01 = NA,
  round02 = NA,
  round03 = list(
    database.id = 2050188410,
    form.ids = list(
      HH = 1590734674,
      RSC = 74998114,
      School = 1813277707,
      VWC = 1247114012)
    )
  )

activityInfoRootUrl("https://build42-dot-ai-production.appspot.com")

# A function to extract the activity/form from which we want to export the data from AI:
extractActivity <- function(schema, form.id) {
    activities <- schema$activities # activity = form
    for (i in seq(length(activities))) {
        if (activities[[i]]$id == form.id) {
            return(activities[[i]])
        }
    }
    invisible()
}

# Getting the data per round ans save it locally as RAW data

for (round in rounds) {
    schema <- getDatabaseSchema(forms[[round]]$database.id)
    database <- forms[[round]]
    # Getting the data per survey
    for (survey in surveys) {
        survey.id <- database$form.ids[[survey]]
        activity <- extractActivity(schema, survey.id) 
        values <- getSitesDataFrame(activity)
        values <- SanitizeStrings(values)
        filename <- file.path("QIS RAW Data",
                              sprintf("R%02d%s_RAW.csv", round, survey))
        write.csv(values,
                  file = filename,
                  quote = TRUE,
                  row.names = FALSE)
        # Note how many records and log this information in the "QIS Cleaning
        # Info" subdirectory
        RowInData <- nrow(values)
        line <-  paste("The original data counted", RowInData, "records.", sep =" ")
        
        cleanfilename <- file.path("QIS Cleaning Info",
                                   sprintf("R%02d%s_Log.txt", round, survey))
        cat(line, "\n", file = cleanfilename, append=FALSE)
        
        # load sample frame and note how many records there are in the file
        sampleframefile <- file.path("Sample Frames",
                                 sprintf("R%02d%s_MailMergeData.csv", round, survey))
        sampleframe <- read.csv(file = sampleframefile,
                                sep=",",
                                header=TRUE,
                                fill = TRUE)

        sampleframe <- subset(sampleframe, !is.na(ID))

        RowInData <- nrow(sampleframe)
        line <- paste("The sample frame used, expects", RowInData,
                      "records.", sep =" ")

        cat(line, "\n", file=cleanfilename, append=TRUE)

        # Check the barcode for the data set of each survey (variable values)
        # and compare with ID of the sample frame:
        values <- BarcodeChecking(values, sampleframe, round, survey)
        # Rename the variables of the data set of each survey (variable values)
        values <- RenameVariables(survey, round, values)
        # Check the Validity of the data and generate flags for each variable
        values <- CheckDataValidity(survey, round, values)        
        # Merge the data values of the survey with the sample frame and save
        # the final file for further processing
        DataSet <- DatasetMerging(round, survey, values, sampleframe)
        # rm(values, sampleframe) #Clean the global enviroment

        # calculate weights for round 3 HH survey:
        # !!! Error !!! => Error in round == 3 : comparison (1) is possible only for atomic and list types
        if (round == 3L & survey == "HH") {
            hh <- DataSet
            hh$code <- as.character(hh$Barcode)
            hh$valid.code <- !is.na(hh$code) & nchar(hh$code) == 10
            hh$vwc.code <- substr(hh$code, 1, 8)
            hh$wealth <- hh$WealthCll
            # ensure that there are no empty cells in the 'wealth' column:
            hh$wealth[hh$wealth == ""] <- NA
            # Consider only those with valid codes
            hh.valid <- subset(hh, valid.code & !is.na(hh$wealth))

            # group sum per VWC and wealth code (UP, PP, NP):
            hh.counts <- melt(with(hh.valid, table(vwc.code, wealth)),
                              id.vars = "vwc.code",
                              value.name = "sampled")
            vwc.frame <- with(hh.valid, data.frame(vwc.code = vwc.code,
                                                   UP = Ultra.poor,
                                                   PP = Poor,
                                                   NP = Non.Poor,
                                                   stringsAsFactors = FALSE))
            vwc.frame <- subset(vwc.frame, !duplicated(vwc.frame))
            vwc.pop <- melt(vwc.frame, id.vars = "vwc.code")
            names(vwc.pop) <- c("vwc.code", "wealth", "population")
            # Combine vwc populations and sample sizes
            weights <- merge(hh.counts, vwc.pop, all.x=TRUE)
            # inclusion probability:
            weights$ip <- with(weights, sampled / population)
            # weight = inverse of the inclusion probability
            weights$weight <- 1 / weights$ip

            # Add to hh frame (join on 'wealth' and 'vwc.code' columns!)
            DataSet <- merge(hh.valid, weights, all.x=TRUE)
#             write.csv(DataSet,
#                       file = file.path
#                         ("QIS Work Data",
#                         sprintf("R%02d%s_Merged_with_weights.csv",
#                                 round,
#                                 survey)),
#                       quote = TRUE,
#                       row.names = FALSE)
            # Split data set for area 1 & 2
            area <- "A01"
            write.csv(DataSet01 <- subset(DataSet,as.numeric(DataSet$PSU)<20),
                      file = file.path
                      ("QIS Work Data",
                       sprintf("R%02d%s%s_Merged_with_weights.csv",
                               round,
                               area,
                               survey)),
                      quote = TRUE,
                      row.names = FALSE)
            area <- "A02"
            write.csv(subset(DataSet,as.numeric(DataSet$PSU)>20),
                      file = file.path
                      ("QIS Work Data",
                       sprintf("R%02d%s%s_Merged_with_weights.csv",
                               round,
                               area,
                               survey)),
                      quote = TRUE,
                      row.names = FALSE)
        }
    }
    
    # create function HH <- function(round, values, barcodes) {...}
}

# -------------- Calculate statistics for the data set: frequency tables and plots ------------------------
# This part of the script is for the preliminary results that Ingebourg need. But can be reused for the KnitR later on

# RSC
# Select BRAC WASH I area PSU == 10 | 11 or BRAC WASH II area PSU 20 | 21
# area <- "area1"
# RSCstatistics(DataSet, survey, area, round)

# VWC
# Select BRAC WASH I area PSU == 14 | 15 or BRAC WASH II area PSU 25 | 26
# area <- "area2"
# VWCstatistics(DataSet, survey, area, round)

# School
# For this purpose we use the psu variable.  BRAC1  <=  50 > psu >= 50  => BRAC2
# area <- "area1"
# Schoolstatistics(DataSet, survey, area, round)

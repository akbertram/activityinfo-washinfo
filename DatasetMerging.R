# IRC BRAC WASH DATA ANALYSIS
# This function merge the validated data from the survey with the sample frame
# The mergin is based on the Barcode and the ID
# The merged data set is then saved in the "QIS Work Data folder"
# The file generated here is the one should be used for further analysis
# It is a generic function for all surveys
# Version 0.1 date: 20141113 by Carlos Velez email:carlos_velez@yahoo.com

DatasetMerging <- function(round, survey, values, sampleframe){
  
  # Merge both datasets (inner join, therefore non-matching barcodes are lost)
  DataSet <- merge(values, sampleframe, by.x="Barcode", by.y="ID")

  # Save a text file with the data validated and merged
  
  filename <- file.path("QIS Work Data",
                        sprintf("R%02d%s_Merged.csv", round, survey))
  if (round==3){
              write.csv(DataSet,
                        file = filename,
                        quote = TRUE,
                        row.names = FALSE)
                }

  return(DataSet)
}

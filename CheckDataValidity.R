# IRC BRAC WASH DATA ANALYSIS
# This function is use to check validity of de variables collected in the QIS surveys
# It is a generic function for all surveys
# If data is inconsisten value is replaced by NA and  
# a column with flags per variable is created, thus:
#   - if record is verified the flag is the letter "v"
#   - if record is modified the flag is the letter "m"
#   - if record is inconsidte the flag is the letter "i"
# Current version of the vector is based on the survey for Round 3
#
# Version 0.1 date: 20141116 by Carlos Velez email:carlos_velez@yahoo.com
# Version 0.2 date: 20141118 added the script for validation of HH data set

# <<<<<<<<<<<<<<<<<<<<<<<<  Lines only needed for testing and developing the script  >>>>>>>>>>>>>>>>>>>>>>>
# setwd("E:/Google Drive/QIS analysis") #CV's working directory
# # 
# round <- 3 # Which rounds will be prepared for analysis, can be a list such as c(1,2,3)
# areas <- 1 # Which area will be included, can be a list such as c(1,2,3)
# survey <- "VWC" # Which surveys will be prepared for analysis, can be a list such as
# # #surveys <- c("HH","VWC","RSC","School")
# # 
# fname <- "QIS RAW Data/R03VWC_RAW_test.csv" #R03HH_RAW.csv R03RSC_RAW_test.csv
# values <- read.csv(fname, sep=",", header=TRUE, fill = TRUE)
# # 
# # # # # load sample frame and note how many records there are in the file
# # # filename <- paste("Sample Frames/R", sprintf("%02d", round),survey, "_MailMergeData", ".csv" , sep ="")
# # # sampleframe <- read.csv(filename, sep=",", header=TRUE, fill = TRUE)
# # # sampleframe <- subset(sampleframe, ID!="NA" ) # delete all lines without a barcode
# # 
# # # source("BarcodeChecking.R")
# # # BarcodeChecking(round, survey)  # variable are not really required as they ar global
# # # I just simplify the barcode checking to eliminate the variabels that does not have it
# values <- subset(values, Barcode!="NA" )
# source("RenameVariables.R")
# values <- RenameVariables(survey, round, values)

#HERE I HAVE A DOUBT. WILL THE DATA ALWAYS HAVE THE SAME COLUMNS.  IF SO WE CAN CODE THE VECTOR OF COLUMNS INSIDE
# THE PROBLEM WITH THIS IS THAT THE VECTOR WILL BE HIDEND AND IF FOR SOME REASON COLUMN CHANGE ERROR WILL BE PRODUCED
# <<<<<<<<<<<<<<<<<<<<<<<<  Lines only needed for testing and developing the script  >>>>>>>>>>>>>>>>>>>>>>>


#   ------------------- CHECK VALIDITY OF THE RECORDS FOR THE VARIBLES ----------------------------------------

CheckDataValidity <- function(survey, round, values){
  
  # call function that loop though the column and validate the records
  source("DataValidation.R")
  
  #survey must be one of the following character strings: "HH","VWC","RSC","School"
  switch(survey,

         # ............................ Valitadion of RSC data  ...................................................  
         RSC = {
           
           # Question RSC01 
           ValidValues  <- c("A", "B", "C", "D", "E")
           values$flagRSCO1 <- NA
           VarValidated <- DataValidation(values$RSC01, values$flagRSCO1, ValidValues)
           values$RSC01     <-  VarValidated[[1]]
           values$flagRSCO1 <-  VarValidated[[2]]
           
           # Question BRACsupport
           ValidValues  <- c("A", "B", "C", "D")
           values$flagRSupport <- NA
           VarValidated <- DataValidation(values$BRACsupport, values$flagRSupport, ValidValues)
           values$BRACsupport  <-  VarValidated[[1]]
           values$flagRSupport <-  VarValidated[[2]]           

         },
         # ............................ Valitadion of VWC data  ...................................................         
         VWC = {
           # Question Year
           ValidValues  <- c(2006:2014)
           values$flagYear <- NA
           VarValidated <- DataValidation(values$Year, values$flagYear, ValidValues)
           values$Year     <-  VarValidated[[1]]
           values$flagYear <-  VarValidated[[2]]           

           # Question NoMaleStart, NoFemaleStart, NoMaleNow, NoFemaleNow
           ValidValues  <- c(1:9)
           
           values$flagNoMaleStart <- NA
           VarValidated <- DataValidation(values$NoMaleStart, values$flagNoMaleStart, ValidValues)
           values$NoMaleStart     <-  VarValidated[[1]]
           values$flagNoMaleStart <-  VarValidated[[2]]           
       
           values$flagNoFemaleStart <- NA
           VarValidated <- DataValidation(values$NoFemaleStart, values$flagNoMaleStart, ValidValues)
           values$NoFemaleStart     <-  VarValidated[[1]]
           values$flagNoFemaleStart <-  VarValidated[[2]]           
       
           values$flagNoMaleNow <- NA
           VarValidated <- DataValidation(values$NoMaleNow, values$flagNoMaleNow, ValidValues)
           values$NoMaleNow     <-  VarValidated[[1]]
           values$flagNoMaleNow <-  VarValidated[[2]]       
         
           values$flagNoFemaleNow <- NA
           VarValidated <- DataValidation(values$NoFemaleNow, values$flagNoFemaleNow, ValidValues)
           values$NoFemaleNow     <-  VarValidated[[1]]
           values$flagNoFemaleNow <-  VarValidated[[2]]
           
           # Question VWC01A, VWC01B, VWC01C
           ValidValues  <- c("A", "B", "C", "D", "E", "F")
           
           values$flagVWCO1A <- NA
           VarValidated <- DataValidation(values$VWC01A, values$flagVWCO1A, ValidValues)
           values$VWC01A     <-  VarValidated[[1]]
           values$flagVWCO1A <-  VarValidated[[2]]
           
           values$flagVWCO1B <- NA
           VarValidated <- DataValidation(values$VWC01B, values$flagVWCO1B, ValidValues)
           values$VWC01B     <-  VarValidated[[1]]
           values$flagVWCO1B <-  VarValidated[[2]]           
       
           values$flagVWCO1C <- NA
           VarValidated <- DataValidation(values$VWC01C, values$flagVWCO1C, ValidValues)
           values$VWC01C     <-  VarValidated[[1]]
           values$flagVWCO1C <-  VarValidated[[2]]
           
           #  Question VWC02, VWC03Men, VWC03Women, VWC03Agreed 
           ValidValues  <- c("A", "B", "C", "D", "E")
           
           values$flagVWCO2 <- NA
           VarValidated <- DataValidation(values$VWC02, values$flagVWCO2, ValidValues)
           values$VWC02     <-  VarValidated[[1]]
           values$flagVWCO2 <-  VarValidated[[2]]
           
           values$flagVWCO3Men <- NA
           VarValidated <- DataValidation(values$VWC03Men, values$flagVWCO3Men, ValidValues)
           values$VWC03Men     <-  VarValidated[[1]]
           values$flagVWCO3Men <-  VarValidated[[2]]           
         
           values$flagVWCO3Women <- NA
           VarValidated <- DataValidation(values$VWC03Women, values$flagVWCO3Women, ValidValues)
           values$VWC03Women     <-  VarValidated[[1]]
           values$flagVWCO3Women <-  VarValidated[[2]]            
         
           values$flagVWCO3Agreed <- NA
           VarValidated <- DataValidation(values$VWC03Agreed, values$flagVWCO3Agreed, ValidValues)
           values$VWC03Agreed     <-  VarValidated[[1]]
           values$flagVWCO3Agreed <-  VarValidated[[2]] 
           
         },
         # ............................ Valitadion of School data  ...................................................         
         School = {
           ValidValues  <- c("A", "B", "C", "D", "E")
           
           #  Question SS02 
           values$flagSSO2 <- NA
           VarValidated <- DataValidation(values$SS02, values$flagSSO2, ValidValues)
           values$SS02     <-  VarValidated[[1]]
           values$flagSSO2 <-  VarValidated[[2]]
           
           #  Question SS03 
           values$flagSSO3 <- NA
           VarValidated <- DataValidation(values$SS03, values$flagSSO3, ValidValues)
           values$SS03     <-  VarValidated[[1]]
           values$flagSSO3 <-  VarValidated[[2]]
           
           #  Question SS04 
           #add column with the flag for SS01 and replace the non valid records
           values$flagSSO4 <- NA
           VarValidated <- DataValidation(values$SS04, values$flagSSO4, ValidValues)
           values$SS04     <-  VarValidated[[1]]
           values$flagSSO4 <-  VarValidated[[2]]
         },
         # ............................ Valitadion of HH data  ...................................................         
         HH = {
           #  Question "ChildMale5+", "ChildFemale5+", "ChildMaleTill5", "ChildFemaleTill5"
           # Valid values for Children counts 
           ValidValues  <- c(1:25)
           
           # Variable : ChildMale5plus
           values$flagChildMale5plus <- NA
           VarValidated <- DataValidation(values$ChildMale5plus, values$flagChildMale5plus, ValidValues)
           values$ChildMale5plus     <-  VarValidated[[1]]
           values$flagChildMale5plus <-  VarValidated[[2]]
           
           # Variable : ChildFemale5plus
           values$flagChildFemale5plus <- NA
           VarValidated <- DataValidation(values$ChildFemale5plus, values$flagChildFemale5plus, ValidValues)
           values$ChildFemale5plus     <-  VarValidated[[1]]
           values$flagChildFemale5plus <-  VarValidated[[2]]
           
           # Variable : ChildMaleTill5
           values$flagChildMaleTill5 <- NA
           VarValidated <- DataValidation(values$ChildMaleTill5, values$flagChildMaleTill5, ValidValues)
           values$ChildMaleTill5     <-  VarValidated[[1]]
           values$flagChildMaleTill5 <-  VarValidated[[2]]
           
           # Variable : ChildFemaleTill5
           values$flagChildFemaleTill5 <- NA
           VarValidated <- DataValidation(values$ChildFemaleTill5, values$flagChildFemaleTill5, ValidValues)
           values$ChildFemaleTill5     <-  VarValidated[[1]]
           values$flagChildFemaleTill5 <-  VarValidated[[2]]
           
           #  Question "HH01old", "HH01new", "HH02old", "HH02new", "HH03old"  
           # Valid value of the HH01old....HHO3old
           ValidValues  <- c("A", "B", "C", "D", "E")
           
           # Variable : HH01old
           values$flagHH01old <- NA
           VarValidated <- DataValidation(values$HH01old, values$flagHH01old, ValidValues)
           values$HH01old     <-  VarValidated[[1]]
           values$flagHH01old <-  VarValidated[[2]]
           
           # Variable : HH01new
           values$flagHH01new <- NA
           VarValidated <- DataValidation(values$HH01new, values$flagHH01new, ValidValues)
           values$HH01new     <-  VarValidated[[1]]
           values$flagHH01new <-  VarValidated[[2]]
           
           # Variable : HH02old
           values$flagHH02old <- NA
           VarValidated <- DataValidation(values$HH02old, values$flagHH02old, ValidValues)
           values$HH02old     <-  VarValidated[[1]]
           values$flagHH02old <-  VarValidated[[2]]
           
           # Variable : HH02new
           values$flagHH02new <- NA
           VarValidated <- DataValidation(values$HH02new, values$flagHH02new, ValidValues)
           values$HH02new     <-  VarValidated[[1]]
           values$flagHH02new <-  VarValidated[[2]]
           
           # Variable : HH03old
           values$flagHH03old <- NA
           VarValidated <- DataValidation(values$HH03old, values$flagHH03old, ValidValues)
           values$HH03old     <-  VarValidated[[1]]
           values$flagHH03old <-  VarValidated[[2]]
           
           #  Question "HH03new"
           #add column with the flag for "HH03new" and replace the non valid records for NA
           
           # Valid value for HH03new
           ValidValues  <- c("A", "B", "C", "D", "E", "F")
           
           # Variable : HH03new
           values$flagHH03new <- NA
           VarValidated <- DataValidation(values$HH03new, values$flagHH03new, ValidValues)
           values$HH03new     <-  VarValidated[[1]]
           values$flagHH03new <-  VarValidated[[2]]
           
           
           #  Question "HH03A"          
           # Valid value for HH03A
           ValidValues  <- c("A", "B", "C")
           
           # Variable : HH03A
           values$flagHH03A <- NA
           VarValidated <- DataValidation(values$HH03A, values$flagHH03A, ValidValues)
           values$HH03A     <-  VarValidated[[1]]
           values$flagHH03A <-  VarValidated[[2]]
           
           #  Question "HH03B"         
           # Valid value for HH03B
           ValidValues  <- c("A", "B", "C", "D")
           
           # Variable : HH03B
           values$flagHH03B <- NA
           VarValidated <- DataValidation(values$HH03B, values$flagHH03B, ValidValues)
           values$HH03B     <-  VarValidated[[1]]
           values$flagHH03B <-  VarValidated[[2]]
           
           
           #  Question HH04            
           # Valid value for HH04
           ValidValues  <- c("A", "B", "C", "D", "E", "F")
           
           # Variable : HH04
           values$flagHH04 <- NA
           VarValidated <- DataValidation(values$HH04, values$flagHH04, ValidValues)
           values$HH04     <-  VarValidated[[1]]
           values$flagHH04 <-  VarValidated[[2]]
           
           # NOTICE THAT IN NEW DATA BASE THERE ARE NOT THE SAME VARIABLES FOR CHILD COUNTS!!!
           # Modify the records of HH04 if the family does not have kids
           # for (i in 1:length(values$HH04)){
           #   kids <- values$MEMBER_NO_CHILD_MALE[i] + values$MEMBER_NO_CHILD_FEMALE[i]
           #   if (kids==0){
           #     if (is.element(values$HH04[i], c("B","C"))){
           #       values$HH04[i] <- "A"
           #       values$flagHHO4[i] <- "m"
           #     }
           #   }
           # }
           
           # Question HH04A          
           # Valid value for HH04A
           ValidValues  <- c("yes", "no", "Yes", "No", "YES", "NO")
           
           # Variable : HH04A
           values$flagHH04A <- NA
           VarValidated <- DataValidation(values$HH04A, values$flagHH04A, ValidValues)
           values$HH04A     <-  VarValidated[[1]]
           values$flagHH04A <-  VarValidated[[2]]
           
           
           # Question HH05 and HH06            
           # Valid value for HH05
           ValidValues  <- c("A", "B", "C", "D", "E")
           
           # Variable : HH05
           values$flagHH05 <- NA
           VarValidated <- DataValidation(values$HH05, values$flagHH05, ValidValues)
           values$HH05     <-  VarValidated[[1]]
           values$flagHH05 <-  VarValidated[[2]]
           
           # Variable : HH06
           values$flagHH06 <- NA
           VarValidated <- DataValidation(values$HH06, values$flagHH06, ValidValues)
           values$HH06     <-  VarValidated[[1]]
           values$flagHH06 <-  VarValidated[[2]]
           
           
           #  Question HH07done            
           # Valid value for HH07done
           ValidValues  <- c("A", "B", "C", "D", "E", "F")
           
           # Variable : HH07done
           values$flagHH07done <- NA
           VarValidated <- DataValidation(values$HH07done, values$flagHH07done, ValidValues)
           values$HH07done     <-  VarValidated[[1]]
           values$flagHH07done <-  VarValidated[[2]]
           
           #  Question HH07future          
           # Valid value for HH07future
           ValidValues  <- c("A", "B", "C", "D", "E")
           
           # Variable : HH07future
           values$flagHH07future <- NA
           VarValidated <- DataValidation(values$HH07future, values$flagHH07future, ValidValues)
           values$HH07future     <-  VarValidated[[1]]
           values$flagHH07future <-  VarValidated[[2]]
           
           },
{stop("enter a valid value for the survey")}
           

  )# end of switch


  #   ------------------- PRINT AND READ DATA SET WITH THE MODIFICATIONS ----------------------------------------
  # This part of the script print the data set that has being modfied in a tab separated format
  # User can read in text editor and the file can be used to do more statistics later on
  # Data is saved in a folder "Data_Modified"
  
  Mfilename <- paste("QIS Modified data/",sprintf("%02d", round),survey, "_Modif", ".csv" , sep ="") # <= EDIT NAME OF FILE
  write.csv(values, file = Mfilename, quote = FALSE,
            row.names = FALSE)
  
  # Read Modified data set to be used for statistical calculation
  # By doing so, the data read as Factor has the correct clases (i.e. "A", "B"..."E")
#   values <- read.csv(Mfilename, sep=",", header=TRUE, fill = TRUE, quote = "")
   return(values)

} # End of function

# if(round==3 & survey=="School"){
#     # to be added
# }
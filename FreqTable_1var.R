# This function calculate the frequency table for 1 variable and format the table
# Results qre saved in the "FreqTable" folder
# Version 0.2 date: 20141012
# Author: Carlos Velez email:carlos_velez@yahoo.com

FreqTable_1var <- function(x, var1, survey, area, round){
  varfreq <- table(x)
  varx <- as.matrix(varfreq)
  coltot <- apply(varx,2,sum)
  Pct <- round(sweep(varx, 2, coltot, "/")*100,2)
  varxPct <- cbind(varx, Pct)
  colnames(varxPct) <- c("Freq", "Perc(%)")
  tot <- apply(varxPct,2,sum)
  vartab <- rbind(varxPct, tot)
  names(dimnames(vartab)) <- c(var1, "")
  vartab <- ftable(vartab)
  filename <- paste("FreqTables/", survey,"r",sprintf("%02d", round),area,"_",var1,".tsv",sep="")
  write.ftable(vartab, file = filename, quote = FALSE, sep = "\t")
  print(vartab)
}
# This function calculate the frequency table for 2 variables and format the table
# Results are saved in the "FreqTable" folder
# Version 0.1 date: 20141112
# Author: Carlos Velez email:carlos_velez@yahoo.com

FreqTable_2var <- function(x, y, var1, var2, survey, area, round){
  Varfreq <- table(x,y)
  Varx <- as.matrix(Varfreq)
  coltot <- apply(Varx,2,sum)
  Pct <- round(sweep(Varx, 2, coltot, "/")*100,2)
  VarxPct <- cbind(Varx, Pct)
  tot <- apply(VarxPct,2,sum)
  Vartab <- rbind(VarxPct, tot)
  names(dimnames(Vartab)) <- c(var1, var2)
  Varftab <- ftable(Vartab)
  filename <- paste("FreqTables/",survey,"r",sprintf("%02d", round),area,"_",var1,"_",var2,".tsv",sep="")
  write.ftable(Varftab, file = filename, quote = FALSE, sep = "\t")
  print(Varftab)  
}
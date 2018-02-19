library("tabulizer", lib.loc="~/R/win-library/3.4")
library("xlsx", lib.loc="~/R/win-library/3.4")
library("tidyr", lib.loc="~/R/win-library/3.4")
library("plyr", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("stringr", lib.loc="~/R/win-library/3.4")

Name <- ""
ExcelName <- paste0(Name,".xls")
MFurl2 <- ""
Pagecount <- get_n_pages(MFurl2)
MFJap <- extract_tables(MFurl2, guess = F, encoding = "UTF-8")
MFPageno <- c()
MFJap <- MFJap[MFPageno]
Data <- MFJap

#Deletes first 6 rows of data as these typically are the messy text fields in .pdfs
Tidy <- function(x){
  as.data.frame(x)
  x <- x[-c(1:6),]
  x
}
Data <- lapply(Data,Tidy)
Data

#Removes special characters - Need to write this function...


#seperates out the columns
colseperate <- function(x){
  for (i in seq_along(x)){
    if(ncol(x[[i]]) == 1){
      colnames(x[[i]]) <- c("Col1") 
      NewNames <- c("Col1","Col2","Col3")
      #add if statement
      if(str_detect(x[[i]]," ") == TRUE){
        x[[i]] <- x[[i]] %>% separate("Col1", NewNames, sep = "\\s", extra = "merge")
        
      }
    } else if (ncol(x[[i]]) == 2){ 
      colnames(x[[i]]) <- c("p","z")
      NewNames2 <- c("Col1","Col2") 
      NewNames3 <- c("Col2","Col3")
      NewNames4 <- c("Col3","Col4")
      
      
      if(str_detect(x[[i]][2,2]," ") == TRUE){
        x[[i]] <- x[[i]] %>% separate("z", NewNames4, sep = "\\s", extra = "merge")
        
        
      } else if (str_detect(x[[i]][2,1]," ") == TRUE) {
        x[[i]] <- x[[i]] %>% separate("p", NewNames2, sep = "\\s", extra = "merge")
        
      }
    }
  }
  if (str_detect(x[[i]][,1]," ") == TRUE) {
    x[[i]] <- x[[i]] %>% separate("p", NewNames2, sep = "\\s", extra = "merge")
    
  } 
  x
}
Data <- lapply(Data,as.data.frame)
Data2 <- colseperate(Data)

##Assigns each list to global environment
for (i in seq(Data2))
  assign(paste0("df", i), Data2[[i]])


#call cols all same
cnames <- c("col1","col2","col3")
colnames(df1) <- cnames
colnames(df2) <- cnames
colnames(df3) <- cnames
colnames(df4) <- cnames

## Rbind all df
Export <- rbind(df1,df2)
Export <- rbind(Export,df3)
Export <- rbind(Export,df4)

write.xlsx(Export, file=ExcelName, sheetName="Statements", row.names=FALSE)
rm(list=ls())

## remove columns
df4 <- subset(df4,select = -3)

##add columns
df4 <- cbind(df1,"x")



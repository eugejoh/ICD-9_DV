# ----------------------------------------------------------------------------------
# Adaptation of BS845 Final Code to Read-in Original STATA NHAMCS Files
# ----------------------------------------------------------------------------------
# Call Libraries
library(devtools)
library(foreign)
library(readstata13)
library(survey)
library(ggplot2)
library(data.table)
library(MASS)
library(plyr)
library(data.tree)
library(treemap)
#install_github("timelyportfolio/d3treeR")
library(d3treeR)


rm(list=ls())
ls()
cat("\014") # clear console in R Studio
R.Version()$version.string
sessionInfo()

####################################
# FUNCTION TO SETWD BY A FUNCTION #
####################################
# edit of the function to read in the files after setting the working directory

set.direct <- function(z){
  z <-  readline('Enter File Path (e.g. "C:/Users/Directory") : ')
  if(is.null(z)){ #if there is no input for readline
    stop("Empty Input")}
  else{
    setwd(z)}
  filenames <- list.files(pattern="ed+.*dta")
  message("File names within the selected working directory")
  print(filenames)
  ed.list <<- list() # empty list that will be filled with dataframes from working directory
  for (i in 1:length(filenames)){ # for loop that based on the number of files in the folder with filenames criterion
    ed.list[[i]]<<-read.dta13(filenames[i],generate.factors = T) ## creating "ed" a list of STATA 13 files based on file name
    names(ed.list)[i]<<-sub(pattern = "(.*)\\..*$", replacement = "\\1",filenames[i]) # Google regex to understand syntax
    message(names(ed.list[i])," (dataframe dimensions: rows x cols)")
    if ( T==all(grepl("[[:upper:]]", names(ed.list[[i]]))) ){ #visual check of which dataframes have upper/lower-case column names
      print("Uppercase Column Variables")}
    else {
      print("Lowercase Column Variables")
      names(ed.list[[i]])<<-toupper(names(ed.list[[i]]))
    }
    print(dim(ed.list[[i]]))
    #assign(paste0("summary.",names(ed.list[i])),summary(ed.list[[i]]),envir = .GlobalEnv) #this is not necessary for this analysis
  }
}
# Use this path:
# /Users/eugenejoh/Documents/BU Graduate School/BU SPH/Practicum Summer/Online Dropbox Folder/STATA Files
ls()
# ----------------------------------------------------------------------------------
# Save NHAMCS FILES FOR LATER USE TO AVOID READ-IN TIME
# ----------------------------------------------------------------------------------
# Select 2005 NHAMCS ED data
ed05 <- ed.list$ed05
ed06 <- ed.list$ed06
ed07 <- ed.list$ed07
ed08 <- ed.list$ed08
ed09 <- ed.list$ed09

myvars <- c("AGE","SEX","DIAG1","REGION","PATWT","CPSUM","CSTRATM") #required subset variables

names(ed05) <- toupper(names(ed05))
ed05 <- ed05[,myvars]
write.csv(ed05,file="ed05s.csv",row.names = FALSE)

names(ed06) <- toupper(names(ed06))
ed06 <- ed06[,myvars]
write.csv(ed06,file="ed06s.csv",row.names = FALSE)

names(ed07) <- toupper(names(ed07))
ed07 <- ed07[,myvars]
write.csv(ed07,file="ed07s.csv",row.names = FALSE)

names(ed08) <- toupper(names(ed08))
ed08 <- ed08[,myvars]
write.csv(ed08,file="ed08s.csv",row.names = FALSE)

names(ed09) <- toupper(names(ed09))
ed09 <- ed09[,myvars]
write.csv(ed09,file="ed09s.csv",row.names = FALSE)
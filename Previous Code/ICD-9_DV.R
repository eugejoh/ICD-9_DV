# Visualizing ICD-9 Code Classification using Trees in ggplot2 (ggtree?)
#https://www.datascienceriot.com/how-to-upgrade-r-without-losing-your-packages/kris/

# ----------------------------------------------------------------------------------
# Adaptation of BS845 Final Code
# ----------------------------------------------------------------------------------
#BS845_mod_FINAL
library(devtools)
library(foreign)
library(readstata13)
library(survey)
library(ggplot2)
library(data.table)
library(MASS)

rm(list=ls())
ls()
cat("\014") # clear console in R Studio
R.Version()$version.string
sessionInfo()

####################################
# FUNCTION TO SETWD BY A FUNCTION #
####################################
# edit of the function to read in the files after setting the working directory

set.directory <- function(z){
  z <-  readline('Enter File Path (e.g. "C:/Users/Directory") : ')
  if(is.null(z)){
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
    assign(paste0("summary.",names(ed.list[i])),summary(ed.list[[i]]),envir = .GlobalEnv)
  }
}
# Use this path:
# /Users/eugenejoh/Documents/BU Graduate School/BU SPH/Practicum Summer/Online Dropbox Folder/STATA Files
ls()

ed05 <- ed.list$ed05
ed06 <- ed.list$ed06
getwd()
write.csv(ed05,file="ed05.csv",row.names = FALSE)

# ----------------------------------------------------------------------------------
# Read-in Reference for ICD-9 Codes from ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2010 'DINDEX11.ZIP'
# ----------------------------------------------------------------------------------
setwd("~/GitHub/ICD-9_DV/APPNDX10")
icd9.3 <- readLines("Dc_3d10.txt") #In readLines("Dc_3d10.RTF") : incomplete final line found on 'Dc_3d10.RTF'
# fix by: http://stackoverflow.com/questions/5990654/incomplete-final-line-warning-when-trying-to-read-a-csv-file-into-r


# ----------------------------------------------------------------------------------
# PARSE ICD-9-CM codes by group and classification using REGEX
# ----------------------------------------------------------------------------------
# Group - ie. Other bacterial diseases (030-041)
# Classification - ie. "030 Leprosy
head(icd9.3,50)
tail(icd9.3,50) # SKIP to APPPLICATION OF REGEX... etc.

# http://www.rexegg.com/regex-anchors.html
# separate diagnoses by \t and line
# distinguish between grouping and classification... all groupings start with non-numeric
# 

# remove title of text file "Appendix E\u2028List of Three-Digit Categories"
icd9.3 <- icd9.3[-1]

grep("^[^\\(]+",icd9.3)
icd9.3[grep("\\([0-9]..-[0-9]..\\)",icd9.3)] #captures rows with parantheses with 3 digit pairs with a "-" in between closed by parantheses

icd9.3[grep("1+\\.",icd9.3)] #captures anything with string that contains "1."
icd9.3[grep("^1\\.+?",icd9.3)] #captures from the beginning only character "1."
icd9.3[grep("^1.\\.+?",icd9.3)] #captures from beginning character "1x." x-any other character
icd9.3[grep("^[0-9]+\\.",icd9.3)] #captures from beginning character with any digit from 0 to 9, followed by a '.'
# ^ - beginning of string
# [0-9] - any digit
# [0-9]+ - of any length (non-greedy)
# \\. - seelcts period (special character requires \\)
# + match quantity of 'digit period'

# ----------------------------------------------------------------------------------
# Extract Group titles
icd9.3[grep("^[0-9]+\\.",icd9.3)] #"1.\tCATEGORY NAME IN CAPS

# ----------------------------------------------------------------------------------
# Extract Subgroup titles
icd9.3[grep("^[A-z]{2}",icd9.3)] #captures any string with sequence that starts with atleast 2 letters in a row, BUT this captures "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES" need to exclude
icd9.3[grep("\\([0-9]{3}-[0-9]{3}\\)",icd9.3)] #captures rows with parantheses with 3 digit pairs with a "-" in between closed by parantheses

# ----------------------------------------------------------------------------------
# Extract Classification
dig3 <- icd9.3[grep("(^[0-9]{3}\t)|(^[0-9]{3}\\s)",icd9.3)] #captures any string that starts with 3 digit sequence and is followed by \t BUT excludes V and E classifications
v3 <- icd9.3[grep("^V[0-9]{2}\t",icd9.3)] #captures any string that starts with a capital V and is followed by two digits and "\t"
e3 <- icd9.3[grep("^E[0-9]{3}\t",icd9.3)]#captures any string that starts with a capital V and is followed by two digits and "\t"
# Apply the three above with OR "|" to grep to extract all
icd9.3[grep("(^[0-9]{3}\t)|(^V[0-9]{2}\t)|(E[0-9]{3}\t)",icd9.3)] 

length(icd9.3[grep("(^[0-9]{3}\t)|(^V[0-9]{2}\t)|(E[0-9]{3}\t)",icd9.3)])
length(dig3)+length(v3)+length(e3) #they are equal

# ----------------------------------------------------------------------------------
# Replacement of "\t" in Classification and white space before "()" in Subgroups with "__" to use as string splitting identifier
gsub("^([0-9]{3})\t","\\1__",head(icd9.3,35)) #replaces all \t with __ but retains the first group in the regex expression (any 3 digit sequence at start of string)

gsub("\\s(\\([0-9]{3}-[0-9]{3}\\))","__\\1",icd9.3) #replaces white space "\\s" before front parantheses\\()
icd9.3[grep("\\([0-9]{3}-[0-9]{3}\\)",icd9.3)] #captures rows with parantheses with 3 digit pairs with a "-" in between closed by parantheses


# ----------------------------------------------------------------------------------
# APPLICATION OF REGEX TO CREAT ICD-9 DICTIONARY BY GROUP, SUBGROUP, CLASSIFICATION
# ----------------------------------------------------------------------------------
# remove title of text file "Appendix E\u2028List of Three-Digit Categories"
icd9.3 <- icd9.3[-1] #only do once

# GROUP
icd9.3g <- icd9.3[grep("^[0-9]+\\.",icd9.3)]
icd9.3g <- gsub("\\.\t","__",icd9.3g) #removal of ".\t"
# obtain Vxxx Groups (if wanted)
#icd9.3[grep("^[A-z]{4,}|V[0-9]{2}",icd9.3)] #extracts all that start with 4 or greater letters or contains a "V" followed by 2 digits
icd9.3[grep("V[0-9]{2}\\)$",icd9.3)] #extracts all subtitles for V classifications
icd9.3gv <- icd9.3[grep("SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING",icd9.3)]

diag.g <- as.data.frame(stringr::str_split_fixed(icd9.3g,"__",2),stringsAsFactors = F)
names(diag.g) <- c("Group","Name")
diag.g <- diag.g[c("Name","Group")] #switch the columns so that ICD-9 dictionary is [,1]-Name then [,2]-Code
head(diag.g)

# Obtain ranges for groups, 999 is the max value (3 digits)
groupn <- grep("\\.\t",icd9.3,value=T)
groupn
groupi <- abs(grep("\\.\t",icd9.3,value=F)) #index of group titles in icd9.3
groupi
icd9.3[groupi-1]
icd9.3[groupi+2]

# upper bound of range
up.g <- gsub("[^0-9]","",icd9.3[groupi-1]) #extract only numbers (removes all letters, "/t")
# lower bound of range
low.g <- gsub("[^0-9]","",icd9.3[groupi+2]) #same as above

up.g <- c(gsub("[^0-9]","",icd9.3[groupi-1]),"999") #addition of max value (999) to upper bound

# replace "800" in second index with max value
# replace(gsub("[^0-9]","",icd9.3[groupi+2]),gsub("[^0-9]","",icd9.3[groupi+2])=="800","999")
diag.g <-data.frame(diag.g,low.g,up.g, stringsAsFactors = F)
names(diag.g) <- c("Name","Group","Start","End")
head(diag.g)
str(diag.g)

# SUBGROUP
icd9.3sg <- icd9.3[grep("\\([0-9]{3}-[0-9]{3}\\)",icd9.3)]
icd9.3sg <- gsub("\t"," ",icd9.3sg) #removes any lingering "\t"
icd9.3sg <- gsub("\\s\\(([0-9]{3}-[0-9]{3})\\)","__\\1",icd9.3sg) #replaces white space "\\s" and parantheses before front parantheses\\()
diag.sg <- as.data.frame(
  stringr::str_split_fixed(icd9.3sg,"__",2),
  stringsAsFactors = F)

diag.sg <- as.data.frame(
  cbind(
    diag.sg,
    stringr::str_split_fixed(diag.sg[,2],"-",2),
  stringsAsFactors = F))
names(diag.sg) <- c("Name","Range","Start","End")
head(diag.sg,4)
str(diag.sg)

# CLASSIFICATION
icd9.3c <- icd9.3[grep("(^[0-9]{3}\t)|(^V[0-9]{2}\t)|(^[0-9]{3}\\s)",icd9.3)]
icd9.3c <- gsub("\t","__",icd9.3c)
icd9.3c <- gsub("^(V[0-9]{2})\\s","\\1__",icd9.3c)
icd9.3c <- gsub("^([0-9]{3})\\s","\\1__",icd9.3c)

diag.c <- as.data.frame(stringr::str_split_fixed(icd9.3c,"__",2),stringsAsFactors = F)
names(diag.c) <- c("Code","Name")
diag.c <- diag.c[c("Name","Code")]
head(diag.c)
diag.c[1:50,2]

# NEED TO CONSIDER EXTRA CODES IN THE NHAMCS
# ----------------------------------------------------------------------------------
# V990- = noncodable diagnosis, insufficient information for coding, illegible diagnosis
# V991- = left before being seen, patient walked out, not seen by doctor, left against
# medical advice
# V992- = transferred to another facility, sent to see specialist
# V993- = HMO will not authorize treatment
# V997- = entry of "none," "no diagnosis," "no disease," "healthy" 
# 0000 = blank
# ----------------------------------------------------------------------------------
# ADDITION OF EXTRA CODES FROM ABOVE
Code <- c("V990-", #names need to match in data.frame() and rbind() steps
          "V991-",
          "V992-",
          "V993-",
          "V997-",
          "0000")
Name <- c("Noncodable diagnosis, insufficient information for coding, illegible diagnosis",
          "Left before being seen, patient walked out, not seen by doctor, left against medical advice",
          "Transferred to another facility, sent to see specialist",
          "HMO will not authorize treatment",
          "Entry of 'none,' 'no diagnosis,' 'no disease,' 'healthy'",
          "Blank")
#df1 <- data.frame(Code,Name)

diag.c <- rbind(diag.c,data.frame(Code,Name))

# example test codes
#vector that contains ICD-9 codes of interest
# CREATE FUNCTION FOR READING IN ICD-9 CODES FROM SPREADSHEET FILE
icd9<-read.table(file="/Users/eugenejoh/Documents/BU Graduate School/BU SPH/2016 Fall/BS 845/Final Project/Data/icd9codes_final.txt",sep = ",",header=T,colClasses=c(rep("character",2)))
# /Users/eugenejoh/Documents/BU Graduate School/BU SPH/Practicum Summer/Online Dropbox Folder/STATA Files

icd9<-icd9[-25,1] #removes MRSA diagnosis
icd9.list<-sort(gsub("\\.","",icd9)) #removes period, \\. is for special characters... "." removes any character and sorts by numberical value

# ----------------------------------------------------------------------------------
# Subset Data and Apply Survey Design
myvars <- c("VMONTH", "VYEAR", "AGE","SEX","AGER","RESIDNCE","RACE","ETHNIC","DIAG1","DIAG2","DIAG3","REGION","PATWT","CPSUM","CSTRATM") #15 variables
ed05 <- ed05[,myvars] #smaller dataset with columns of interest to reduce processing time

# After subset, we apply the survey design weights 
library(survey)
wt<-svydesign(data=ed05, # dataset from ed.list by appropriate year
              weights=~PATWT, #weights=PATWT #check by edw; names(ed.list[[1]])[140]
              ids=~CPSUM, #ids=CPSUM #check by names(ed.list[[1]])[278]
              strata=~CSTRATM, #strata=CSTRATM #check by names(ed.list[[1]])[277]
              nest=T) #forces nesting in the design

# Extract Estimates of # Visits for each diagnosis
sum(svytable(~DIAG1,wt)) #estimated 115,322,815 visits to ED in 2005
sum(svytable(~DIAG2,wt))
sum(svytable(~DIAG3,wt))

newtab <- svytable(~DIAG1,wt) #0000 are blank ()
ed05.df <- as.data.frame(newtab, stringsAsFactors = F)
names(ed05.df)
head(ed05.df)

# ----------------------------------------------------------------------------------
# NOTES, pg. 19 of ed05 #ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHAMCS/doc05.pdf

# MATCH DIAGNOSES
# test vector with first 20 codes from ed05
test1.df <- ed05.df[1:15,] #test set
test1.df

ids <- "^008"

gindex <- grep(ids,test1.df[,1]) #extract all strings that start with 008
gindex #index number that matches "^008"


test1.df[gindex,]
test1.df$newcol <- "-"
test1.df$newcol[gindex] # fill in new column based on index of 'ids' with values of 

test1.df$newcol[gindex] <- diag.c$Name[grep(ids,diag.c[,2])]

diag.c[,2][8]
grep(ids,diag.c[,2]) # ids %in% diag.c[,2]

diag.c$Name[grep(ids,diag.c[,2])] # pulls diagnosis name based on 'ids'


# ----------------------------------------------------------------------------------
# FINAL WORKING LOOP
# ----------------------------------------------------------------------------------
# ed05.df$d3_name <- "-"
# for (k in paste0("^",diag.c[,2])){
#   gindex <- grep(k,ed05.df[,1])
#   ed05.df$d3_name[gindex] <- diag.c$Name[grep(k,diag.c[,2])]
# }
# 
# head(ed05.df$d3_name)

# ADDING 3 digit code/classification to ed05.df (based on diag.c
ed05.df$Code <- "-"
for (k in paste0("^",diag.c[,2])){ #codes
  gindex <- grep(k,ed05.df[,1]) #matches code with DIAG1 column
  ed05.df$Code[gindex] <- diag.c$Code[grep(k,diag.c[,2])]
  ed05.df$Code.n <- as.numeric(ed05.df$Code) #some values are coered to NA
  ed05.df$Code.n[which(is.na(ed05.df$Code.n))] <- 1000 #treat all NA's wih value 1000
}

# ADDING Group number/name to ed05.df (based on diag.g)

ed05.df$Group <- "-"

# ADDING Subgroup name to ed05.df (based on diag.sg)

ed05.df$Subgroup <- "-"

# ----------------------------------------------------------------------------------
# Use of dplyr 'join()' for classification of diagnoses
# ----------------------------------------------------------------------------------
head(ed05.df)
head(diag.c)
head(diag.g)
head(diag.sg)

library(plyr)
head(join(ed05.df,diag.c,by="Code")) #join ed05.df with names from diag.c from plyr package

# https://stackoverflow.com/questions/35636315/replacing-values-in-a-dataframe-based-on-lookup-table
# code below accomplishes the same thing
df <- head(ed05.df,50)
new <- df
new[] <- diag.c$Name[match(unlist(df$Code), diag.c$Code)]
new[] <- sapply(df$Code, function(x) diag.c$Name[match(x, diag.c$Code)])

# ----------------------------------------------------------------------------------
# Use of data.table to complete "overlap joins" *requires numeric/integer types
# ----------------------------------------------------------------------------------
# Adding numeric values for ranges
# Groups
diag.g$Start.n <- as.numeric(diag.g$Start) #format for data.table
diag.g$End.n <- as.numeric(diag.g$End)
head(diag.g)

# Subgroups
diag.sg$Start.n <- as.numeric(diag.sg$Start) #format for data.table
diag.sg$End.n <- as.numeric(diag.sg$End)
head(diag.sg)

# https://stackoverflow.com/questions/24480031/roll-join-with-start-end-window
library(data.table)
# ICD-9 Group
dt.g <- data.table(diag.g)
dt.g <- dt.g[,c(1,2,5,6)]
d1 <- dt.g[,c(2,3,4)]

df <- head(ed05.df,50)
dt.ed05 <- data.table(df) 
d2 <- dt.ed05
setkey(d1,Group,start,end)

d1[d2][between(Code.n, start, end)]

foverlaps(d2,d1,by.x=names(d1),type="within",nomatch=0L)

#setkey(dt.g)
setkey(d1,start,end)

d2[, Code.n2 := Code.n]
#dt.ed05[, Code2 := Code.n]
d2

foverlaps(dt.ed05, dt.g, by.x=names(dt.g), type="within", nomatch = 0L)

foverlaps(d2, d1, by.x = names(d2), type = "within", mult = "all", nomatch = 0L)
# ----------------------------------------------------------------------------------
# CHECK IF THERE ARE ANY MISSING
# ----------------------------------------------------------------------------------
miss <- which(ed05.df$Code=="-")
table(ed05.df$Code)
ed05.df[miss,]

ggplot(ed05.df,aes(x=ed05.df$Code, y=ed05.df$Freq)) +
  geom_bar(stat="identity") + coord_flip()

# ----------------------------------------------------------------------------------
# DATA.TREE and TREEMAP Package Use to transform data
# ----------------------------------------------------------------------------------
# install.packages("data.tree")
# install.packages("treemap")
library(data.tree)
library(treemap)
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#introduction
data(GNI2014)
head(GNI2014)

GNI2014$pathString <- paste("world", 
                            GNI2014$continent, 
                            GNI2014$country, 
                            sep = "/")

head(GNI2014)
class(GNI2014)
population <- as.Node(GNI2014)
population
class(population)
# follow the same data.frame guidelines using the Group, Subgroup and Classification
# use of diag.g, diag.sg, diag.c

dim(diag.g) #17 groups
dim(diag.sg) #111 subgroups
dim(diag.c) #1014 classifications

# Use loops to create updated ed05.df with columns for group, subgroup and classification
# convert ed05.df into data.tree type using as.Node function
ed05.t <- as.Node(ed05.df)

# ----------------------------------------------------------------------------------
# IDEA OF GGTREE DATA SET UP
# ----------------------------------------------------------------------------------
#source("https://bioconductor.org/biocLite.R") #install ggtree package
#biocLite("ggtree")
library(ggtree)

beast_file <- system.file("examples/MCC_FluA_H3.tree", package="ggtree")
beast_tree
get.fields(beast_tree)

head(fortify(beast_tree))

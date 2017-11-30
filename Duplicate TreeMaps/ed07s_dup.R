#ICD-9 Data Visualization using Regular Expression and TreeMaps (May 24 2017)
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

# ----------------------------------------------------------------------------------
# Run READ-IN_NHAMCS_0509.R *only once
# ----------------------------------------------------------------------------------
# Read-in only ED 2007 Data
setwd("~/GitHub/ICD-9_DV")
ed07 <- read.csv("ed07s.csv")

# ----------------------------------------------------------------------------------
# Read-in Reference for ICD-9 Codes from ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2010 'DINDEX11.ZIP'
# ----------------------------------------------------------------------------------
setwd("~/GitHub/ICD-9_DV/APPNDX10")
icd9.3 <- readLines("Dc_3d10.txt") #In readLines("Dc_3d10.RTF") : incomplete final line found on 'Dc_3d10.RTF'
# fix by: http://stackoverflow.com/questions/5990654/incomplete-final-line-warning-when-trying-to-read-a-csv-file-into-r
head(icd9.3,10)
# ----------------------------------------------------------------------------------
# APPLICATION OF REGEX TO CREATE ICD-9 LOOKUP TABLE BY GROUP, SUBGROUP, CLASSIFICATION
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
names(diag.g) <- c("Group.n","Group")
diag.g <- diag.g[c("Group","Group.n")] #switch the columns so that ICD-9 dictionary is [,1]-Name then [,2]-Code
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
names(diag.g) <- c("Group","Group.n","Start","End")
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
names(diag.sg) <- c("Subgroup","Range","Start","End")
head(diag.sg,4)
str(diag.sg)

# CLASSIFICATION
icd9.3c <- icd9.3[grep("(^[0-9]{3}\t)|(^V[0-9]{2}\t)|(^[0-9]{3}\\s)",icd9.3)]
icd9.3c <- gsub("\t","__",icd9.3c)
icd9.3c <- gsub("^(V[0-9]{2})\\s","\\1__",icd9.3c)
icd9.3c <- gsub("^([0-9]{3})\\s","\\1__",icd9.3c)

diag.c <- as.data.frame(stringr::str_split_fixed(icd9.3c,"__",2),stringsAsFactors = F)
names(diag.c) <- c("Code","Classification")
diag.c <- diag.c[c("Classification","Code")]
head(diag.c)
diag.c[1:50,2]

# ----------------------------------------------------------------------------------
# Subset Data and Apply Survey Design
myvars <- c("VMONTH", "VYEAR", "AGE","SEX","AGER","RESIDNCE","RACE","ETHNIC","DIAG1","DIAG2","DIAG3","REGION","PATWT","CPSUM","CSTRATM") #15 variables
ed07 <- ed07[,myvars] #smaller dataset with columns of interest to reduce processing time

# After subset, we apply the survey design weights 
library(survey)
wt<-svydesign(data=ed07, # dataset from ed.list by appropriate year
              weights=~PATWT, #weights=PATWT #check by edw; names(ed.list[[1]])[140]
              ids=~CPSUM, #ids=CPSUM #check by names(ed.list[[1]])[278]
              strata=~CSTRATM, #strata=CSTRATM #check by names(ed.list[[1]])[277]
              nest=T) #forces nesting in the design

# Extract Estimates of # Visits for each diagnosis
sum(svytable(~DIAG1,wt)) #estimated 115,322,815 visits to ED in 2007

newtab <- svytable(~DIAG1,wt) # only considering DIAG1
ed07.df <- as.data.frame(newtab, stringsAsFactors = F)
names(ed07.df)
head(ed07.df)

# ----------------------------------------------------------------------------------
# Loop Regex pattern search
# ----------------------------------------------------------------------------------
# ADDING 3 digit code/classification to ed07.df (based on diag.c)
ed07.df$Code <- "-"
for (k in paste0("^",diag.c[,2])){ #codes
  gindex <- grep(k,ed07.df[,1]) #matches code with DIAG1 column
  ed07.df$Code[gindex] <- diag.c$Code[grep(k,diag.c[,2])]
  ed07.df$Code.n <- as.numeric(ed07.df$Code) #some values are coered to NA
  ed07.df$Code.n[which(is.na(ed07.df$Code.n))] <- 1000 #treat all NA's wih value 1000
}

# ----------------------------------------------------------------------------------
# Use of dplyr 'join()' for classification of diagnoses
# ----------------------------------------------------------------------------------
head(join(ed07.df,diag.c,by="Code")) #join ed07.df with names from diag.c from plyr package

ed07.df <- join(ed07.df,diag.c,by="Code")
head(ed07.df)
str(ed07.df)
# ----------------------------------------------------------------------------------
# Data Preparation/Setup for foverlaps() from data.table
# Adding numeric values for ranges because foverlaps() only works with numeric, integer values see documentation
# Groups
diag.g$Start.n <- as.numeric(diag.g$Start) #format for data.table
diag.g$End.n <- as.numeric(diag.g$End)
head(diag.g)

# Subgroups
diag.sg$Start.n <- as.numeric(diag.sg$Start) #format for data.table
diag.sg$End.n <- as.numeric(diag.sg$End)
head(diag.sg)

# ----------------------------------------------------------------------------------
# Use of data.table to complete "overlap joins"
# ----------------------------------------------------------------------------------
library(data.table)
# https://stackoverflow.com/questions/24480031/roll-join-with-start-end-window
# ----------------------------------------------------------------------------------
# foverlaps for Groups
dt.g <- data.table(diag.g) #rename these 
setkey(dt.g,Start.n,End.n)

dt.ed07 <- data.table(ed07.df)
dt.ed07[, Code.n2 := Code.n] #create a range of zero for overlap
dt.ed07
setcolorder(dt.ed07, c("DIAG1","Freq","Code","Classification","Code.n","Code.n2"))
names(dt.ed07)
setkey(dt.ed07)

# Apply foverlaps to original dt.ed07
dt.ed07 <- foverlaps(dt.ed07, dt.g, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all", nomatch = 0L) #removes V and Blanks, only1931 rows
# ----------------------------------------------------------------------------------
# foverlaps for Subgroups
dt.sg <- data.table(diag.sg)
setkey(dt.sg,Start.n,End.n)

dt.ed07
setkey(dt.ed07)

dt.ed07 <- foverlaps(dt.ed07, dt.sg, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all", nomatch = 0L) #removes V and Blanks, only1931 rows

dt.ed07 <- dt.ed07[order(Group.n)] #order the data.table by Group number (Group.n)
head(dt.ed07,10)
# we want to keep Subgroup, Range, Group, Group.n, DIAG1, Freq, Code, Classification
names(dt.ed07)
dt.ed07[,which(!grepl("Subgroup|Range|^Group$|Group.n|DIAG1|Freq|^Code$|Classification",names(dt.ed07))):=NULL]
names(dt.ed07)
setcolorder(dt.ed07,c("Group","Group.n","Subgroup","Range","Classification","Code","DIAG1","Freq"))
head(dt.ed07) #check order
str(dt.ed07) #check column classes

# Convert back to data.frame
df.ed07 <- as.data.frame(dt.ed07) #getAnywhere("as.data.frame.data.table")
str(df.ed07)

# ----------------------------------------------------------------------------------
# DATA.TREE and TREEMAP Package Use to transform data
# ----------------------------------------------------------------------------------
# BY CODE
df.ed07$pathString <- paste("2007",
                            df.ed07$Group.n,
                            df.ed07$Range,
                            df.ed07$Code,
                            sep = "/")
head(df.ed07)

ICD9.code <- as.Node(df.ed07,pathName="pathString")
ICD9.code
print(ICD9.code,"Freq")

# ----------------------------------------------------------------------------------
# BY NAMES
df.ed07$pathString1 <- paste("2007",
                             df.ed07$Group,
                             df.ed07$Subgroup,
                             df.ed07$Classification,
                             sep = "/")
ICD9.name <- as.Node(df.ed07,pathName="pathString1")
ICD9.name
print(ICD9.name,"Freq")

# ----------------------------------------------------------------------------------
# Visualize using TreeMap
# ----------------------------------------------------------------------------------
setwd("~/GitHub/ICD-9_DV")

pal1 <- c("#a97975", #bottom right (bottom-left third)
          "#de9fff", #pregnancy
          "#ff5f8b", #bottom right (bottom-right third)
          "#019a51", #blood forming organs
          "#E55252", #circulatory
          "#99B4F9", #digestive
          "#CEFEFD", #genitourinary
          "#EE8484", #musculoskeleta
          "#17957e", #nervrous CEFEFD
          "#FFCCA2", #respiratory FFCCA2 FFD8B7
          "#FEF5CE", #skin cutaneous
          "#A5D6FA", #endocrine
          "#EAFAA5", #infectious diseases
          "#DDCCF3", #injury
          "#FFFFFF", #mental disorders
          "#B9B9B9", #bottom right (top third)
          "#F6EBBE") #symptoms
#grid::grid.raster(pal1, interpolate = FALSE) #check color palette
png(filename="ICD-9_tree_2007.png",width=1300, height=850,unit="px")
tree.n <- treemap(df.ed07,
                  index=c("Group","Subgroup","Classification"), #grouping for each
                  vSize="Freq", #area of rectangles by NHAMCS estimates
                  type="index", #see documentation
                  title="NHAMCS Emergency Department Visits in 2007",
                  overlap.labels = 0.5, ##see documentation 
                  palette=pal1, #custom palette from IWantHue
                  border.col = c("#101010","#292929","#333333"), #set border colors
                  fontsize.title = 40, #set font size
                  fontsize.labels = c(30, 24, 16), #set size for each grouping
                  lowerbound.cex.labels=.4, ##see documentation
                  fontcolor.labels = c("#000000","#292929","#333333"), #set color
                  fontface.labels = c(2,4,1), # bold, bold-italic, normal
                  fontfamily.labels = c("sans"), #sans font
                  inflate.labels=F, #see documentation
                  align.labels = c("center","center"), #align all labels in center
                  bg.labels=230 # 0 and 255 that determines the transparency
)
dev.off() #3c0120

# INTERACTIBE PLOT
tree.c <- treemap(df.ed07,
                  index=c("Group.n","Range","Code"), #grouping for each
                  vSize="Freq", #area of rectangles by NHAMCS estimates
                  type="index", #see documentation
                  title="NHAMCS Emergency Department Visits in 2007",
                  overlap.labels = 0.5, ##see documentation 
                  palette=pal1, #custom palette from IWantHue
                  border.col = c("#101010","#292929","#333333"), #set border colors
                  fontsize.title = 40, #set font size
                  fontsize.labels = c(30, 24, 16), #set size for each grouping
                  lowerbound.cex.labels=.4, ##see documentation
                  fontcolor.labels = c("#000000","#292929","#333333"), #set color
                  fontface.labels = c(2,4,1), # bold, bold-italic, normal
                  fontfamily.labels = c("sans"), #sans font
                  inflate.labels=F, #see documentation
                  align.labels = c("center","center"), #align all labels in center
                  bg.labels=230 # 0 and 255 that determines the transparency
)
inter <- d3tree2(tree.c,
                 rootname="Groups",
                 height=700,width=1000)
inter
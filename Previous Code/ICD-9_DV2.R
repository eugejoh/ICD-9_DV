# ICD-9_DV Edit 2 (MAY 24 2017)
# ----------------------------------------------------------------------------------


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

ed05 <- ed.list$ed05
ed06 <- ed.list$ed06

# ----------------------------------------------------------------------------------
# Read-in only ED 2005 Data
setwd("~/GitHub/ICD-9_DV")
#getwd()
#write.csv(ed05,file="ed05.csv",row.names = FALSE)
ed05.h <- read.csv("ed05.csv")
sum(dim(ed05)==dim(ed05.h))
sum(names(ed05)==names(ed05.h))
ed05 <- ed05.h
rm(ed05.h)
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

# NEED TO CONSIDER EXTRA CODES IN THE NHAMCS *this is ignored after data.table transformation
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
# Loop Regex pattern search
# ----------------------------------------------------------------------------------
# ADDING 3 digit code/classification to ed05.df (based on diag.c)
ed05.df$Code <- "-"
for (k in paste0("^",diag.c[,2])){ #codes
  gindex <- grep(k,ed05.df[,1]) #matches code with DIAG1 column
  ed05.df$Code[gindex] <- diag.c$Code[grep(k,diag.c[,2])]
  ed05.df$Code.n <- as.numeric(ed05.df$Code) #some values are coered to NA
  ed05.df$Code.n[which(is.na(ed05.df$Code.n))] <- 1000 #treat all NA's wih value 1000
}

# ----------------------------------------------------------------------------------
# Use of dplyr 'join()' for classification of diagnoses
# ----------------------------------------------------------------------------------
library(plyr)
head(join(ed05.df,diag.c,by="Code")) #join ed05.df with names from diag.c from plyr package

ed05.df <- join(ed05.df,diag.c,by="Code")
head(ed05.df)
str(ed05.df)
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

dt.ed05 <- data.table(ed05.df)
dt.ed05[, Code.n2 := Code.n] #create a range of zero for overlap
dt.ed05
setcolorder(dt.ed05, c("DIAG1","Freq","Code","Classification","Code.n","Code.n2"))
names(dt.ed05)
setkey(dt.ed05)

# foverlaps(dt.ed05, dt.g, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all") #inputs NA for non-matches (ie. 0000 and V--- codes), retains 2062 rows
# foverlaps(dt.ed05, dt.g, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all", nomatch = 0L) #removes V and Blanks, only 1931 rows
# tail(dt.ed05,50)

# Apply foverlaps to original dt.ed05
dt.ed05 <- foverlaps(dt.ed05, dt.g, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all", nomatch = 0L) #removes V and Blanks, only1931 rows
# ----------------------------------------------------------------------------------
# foverlaps for Subgroups
dt.sg <- data.table(diag.sg)
dt.sg
setkey(dt.sg,Start.n,End.n)

dt.ed05
setkey(dt.ed05)

foverlaps(dt.ed05, dt.sg, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all", nomatch = 0L) #removes V and Blanks, only1931 rows
tail(dt.ed05,50)

foverlaps(dt.ed05, dt.sg, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all", nomatch = 0L) #removes V and Blanks, only1931 rows

dt.ed05 <- foverlaps(dt.ed05, dt.sg, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "within", mult="all", nomatch = 0L) #removes V and Blanks, only1931 rows

head(dt.ed05,10)
dt.ed05 <- dt.ed05[order(Group.n)] #order the data.table by Group number (Group.n)
head(dt.ed05,10)
# we want to keep Subgroup, Range, Group, Group.n, DIAG1, Freq, Code, Classification
names(dt.ed05)
dt.ed05[,which(!grepl("Subgroup|Range|^Group$|Group.n|DIAG1|Freq|^Code$|Classification",names(dt.ed05))):=NULL]
names(dt.ed05)
# setnames(dt.ed05,"Name","Subgroup")
# setnames(dt.ed05,"Group","Group.n")
# setnames(dt.ed05,"i.Name","Group")
# setnames(dt.ed05,"Classification","Code")
head(dt.ed05)
setcolorder(dt.ed05,c("Group","Group.n","Subgroup","Range","Classification","Code","DIAG1","Freq"))
head(dt.ed05)
str(dt.ed05)

# Convert back to data.frame
df.ed05 <- as.data.frame(dt.ed05) #getAnywhere("as.data.frame.data.table")
head(dt.ed05)
head(df.ed05)
str(df.ed05)

# ----------------------------------------------------------------------------------
# DATA.TREE and TREEMAP Package Use to transform data
# ----------------------------------------------------------------------------------
library(data.tree)
library(treemap)
# ----------------------------------------------------------------------------------
# BY CODE
df.ed05$pathString <- paste("2005",
                            df.ed05$Group.n,
                            df.ed05$Range,
                            df.ed05$Code,
                            sep = "/")
head(df.ed05)

ICD9.code <- as.Node(df.ed05,pathName="pathString")
ICD9.code
print(ICD9.code,"Freq")

# Visualize using D3 Network
# Codes
#install.packages("networkD3")
library(networkD3)

ICD9net.code <- ToDataFrameNetwork(ICD9.code,"Freq")
simpleNetwork(ICD9net.code)

ICD9netlist.code <- ToListExplicit(ICD9.code, unname = TRUE)
radialNetwork(ICD9netlist.code)

# ----------------------------------------------------------------------------------
# BY NAMES
df.ed05$pathString1 <- paste("2005",
                             df.ed05$Group,
                             df.ed05$Subgroup,
                             df.ed05$Classification,
                             sep = "/")
ICD9.name <- as.Node(df.ed05,pathName="pathString1")
ICD9.name
print(ICD9.name,"Freq")

# Visualize using D3 Network
ICD9net.name <- ToDataFrameNetwork(ICD9.name,"Freq")
simpleNetwork(ICD9net.name)

ICD9netlist.name <- ToListExplicit(ICD9.name, unname = TRUE)
radialNetwork(ICD9netlist.name)

# ----------------------------------------------------------------------------------
# Visualize using TreeMap
# ----------------------------------------------------------------------------------
# useful for two continous variables with multiple categorical variables
library(RColorBrewer)
treemap(df.ed05,
        index=c("Group.n","Subgroup","Classification"),
        vSize="Freq",
        title="TreeMap of NHAMCS Emergency Department Visits in 2005",
        overlap.labels = 0.7,
        palette=brewer.pal(10,"Set3")) # too cluttered

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
png(filename="ICD-9_tree_2005.png",width=1300, height=850,unit="px")
treemap(df.ed05,
        index=c("Group","Subgroup","Classification"), #
        vSize="Freq",
        type="index",
        #vColor="Group.n",
        title="Emergency Department Visits in 2005 based on NHAMCS",
        overlap.labels = 0.5,
        palette=pal1,
        border.col = c("#101010","#292929","#333333"),
        fontsize.title = 40,
        fontsize.labels = c(30, 24, 16),
        lowerbound.cex.labels=.4,
        fontcolor.labels = c("#000000","#292929","#333333"),
        fontface.labels = c(2,4,1),
        fontfamily.labels = c("sans"),
        inflate.labels=F,
        align.labels = c("center","center"),
        bg.labels=230 # 0 and 255 that determines the transparency
)
dev.off() #3c0120
# Interactive Treemap
devtools::install_github("timelyportfolio/d3treeR")
library(d3treeR)
tree1 <- treemap(df.ed05,
                 index=c("Group","Subgroup","Classification"), #
                 vSize="Freq",
                 type="index",
                 #vColor="Group.n",
                 title="Emergency Department Visits in 2005 based on NHAMCS",
                 overlap.labels = 0.7,
                 palette=pal1,
                 border.col = c("#101010","#292929","#333333"),
                 fontsize.title = 40,
                 fontsize.labels = c(30, 24, 16),
                 lowerbound.cex.labels=1,
                 fontcolor.labels = c("#000000","#292929","#333333"),
                 fontface.labels = c(2,4,1),
                 fontfamily.labels = c("sans"),
                 inflate.labels=F,
                 align.labels = c("center","center"),
                 bg.labels=230 # 0 and 255 that determines the transparency
)
inter <- d3tree2(tree1,
                 rootname="Groups",
                 height=700,width=1000)
inter

# Visualize using Bubbles
devtools::install_github("jcheng5/bubbles@6724e43f5e")
library(scales)
library(bubbles)
library(RColorBrewer)

df.ed05$Group.n <- as.numeric(df.ed05$Group.n)  #convert Group.n to numeric type
df.ed05 <- df.ed05[order(df.ed05$Group.n),] #order all the data by Group.n

as.vector(by(df.ed05$Freq,df.ed05$Group.n,sum)) #sums by group number #ordered from 1-17
# visualize 
# bubble size by Freq
# bubbles by Subgroup
# bubble color by Group

# custom palette
count.col <- function(y){ #y ranges for 1-17 (range of unique Group.n values)
    x<-length(
      unique(
        df.ed05$Range[
          which(
            df.ed05$Group.n == unique(df.ed05$Group.n)[y]
          )]
      )
    )
  return(paste0("'",rep(pal1[y],x),sep="'",collapse = ","))
}

for (i in 1:length(unique(df.ed05$Group.n))) print(count.col(i))

paste0("'",rep(pal1[1],23),sep="'",collapse = ",")

pal3 <- c('#a97975','#a97975','#a97975','#a97975','#a97975','#a97975','#a97975','#a97975','#a97975','#a97975','#a97975',
          '#de9fff','#de9fff','#de9fff','#de9fff','#de9fff','#de9fff','#de9fff','#de9fff','#de9fff',
          '#ff5f8b','#ff5f8b','#ff5f8b',
          '#019a51',
          '#E55252','#E55252','#E55252','#E55252',
          '#99B4F9','#99B4F9','#99B4F9','#99B4F9','#99B4F9','#99B4F9',
          '#CEFEFD','#CEFEFD','#CEFEFD','#CEFEFD','#CEFEFD','#CEFEFD','#CEFEFD',
          '#EE8484','#EE8484','#EE8484','#EE8484','#EE8484','#EE8484',
          '#17957e','#17957e','#17957e','#17957e','#17957e','#17957e','#17957e',
          '#FFCCA2','#FFCCA2','#FFCCA2','#FFCCA2','#FFCCA2','#FFCCA2',
          '#FEF5CE','#FEF5CE','#FEF5CE','#FEF5CE','#FEF5CE',
          '#A5D6FA','#A5D6FA','#A5D6FA',
          '#EAFAA5','#EAFAA5','#EAFAA5','#EAFAA5',
          '#DDCCF3',
          '#E6E6E6',
          '#B9B9B9','#B9B9B9','#B9B9B9',
          '#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE','#F6EBBE'
          )

pal2 <- c('#258c89','#258c89','#258c89','#258c89','#258c89','#258c89','#258c89','#258c89','#258c89','#258c89','#258c89',
          '#ef4e51','#ef4e51','#ef4e51','#ef4e51','#ef4e51','#ef4e51','#ef4e51','#ef4e51','#ef4e51',
          '#75ffbd','#75ffbd','#75ffbd',
          "#d355c1",
          '#88dbff','#88dbff','#88dbff','#88dbff',
          '#a5a1ff','#a5a1ff','#a5a1ff','#a5a1ff','#a5a1ff','#a5a1ff',
          '#69c189','#69c189','#69c189','#69c189','#69c189','#69c189','#69c189',
          '#fdd7ff','#fdd7ff','#fdd7ff','#fdd7ff','#fdd7ff','#fdd7ff',
          '#F1F861','#F1F861','#F1F861','#F1F861','#F1F861','#F1F861','#F1F861',
          '#d19ea6','#d19ea6','#d19ea6','#d19ea6','#d19ea6','#d19ea6',
          '#436CBD','#436CBD','#436CBD','#436CBD','#436CBD',
          '#ffd1d1','#ffd1d1','#ffd1d1',
          '#e2ffd0','#e2ffd0','#e2ffd0','#e2ffd0',
          "#bfa5a7",
          "#d5ffff",
          '#cfa37d','#cfa37d','#cfa37d',
          'FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050','FFA050')


bubbles(value = as.vector(by(df.ed05$Freq,df.ed05$Subgroup,sum)),
        label = unique(df.ed05$Range),
        color = pal3)

bub1 <- bubbles(value = as.vector(by(df.ed05$Freq,df.ed05$Subgroup,sum)),
                label = unique(df.ed05$Range),
                color = pal2)

# Use ggtree Visualization
# https://guangchuangyu.github.io/ggtree/documentation/
class(ICD9.code)
class(ICD9.name)
#need to convert ICD9 from Node to phylo using ?as.phylo.Node

ICD9.name.p <- as.phylo.Node(ICD9.code,heightAttribute = "Freq")
GetPhyloNr(ICD9.code,type = "node")


# ----------------------------------------------------------------------------------
# Subset Test for Clusters
# ----------------------------------------------------------------------------------
test1 <- head(df.ed05,300)
test1$pathString1 <- paste("2005",
                           test1$Group.n,
                             test1$Range,
                             test1$Code,
                             sep = "/")
test1.n <- as.Node(test1,pathName="pathString1")
test1.n

test1.list <- ToListExplicit(test1.n, unname = TRUE)
radialNetwork(test1.list)

print(test1.n,"Freq")
as.phylo(test1.n)
as.phylo.Node(test1.n) # https://stackoverflow.com/questions/15343338/how-to-convert-a-data-frame-to-tree-structure-object-such-as-dendrogram
# https://joey711.github.io/phyloseq/plot_tree-examples.html#radial_tree

# ----------------------------------------------------------------------------------
# Visualize Using igraph
#install.packages("igraph")
library(igraph)
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
# http://kateto.net/networks-r-igraph

plot(as.igraph(test1.n),directed=T)

class(test1.n)

# Visualize Using sunburstR
#http://www.buildingwidgets.com/blog/2015/7/2/week-26-sunburstr

#-------------------------------------------------------------------------------
# My comments are in similar blocks!
#-------------------------------------------------------------------------------
#setwd("/Users/max/Desktop/Illustration")
setwd("/Users/max/Desktop/Illustration") # ADAM's

#install.packages("divDyn")

library(divDyn)

#dat <- read.csv("pbdb_data_final_v3.csv", sep=";" )
dat <- read.csv("pbdb_data_final_v3.csv", sep=";" )# ADAM's
seddat <- read.csv2("weathering_data.csv", sep=";" )

dat <- dat[dat$family=="Cypraeidae",]

data(stages)
data(tens)

data(keys)

# the 'stg' entries (lookup)
stgMin <- categorize(dat[ ,"early_interval"], keys$stgInt)
stgMax <- categorize(dat[ ,"late_interval"], keys$stgInt)

# convert to numeric
stgMin <- as.numeric(stgMin)
stgMax <- as.numeric(stgMax)

# empty container
dat$stg <- rep(NA, nrow(dat))

# select entries, where
stgCondition <- c(
  # the early and late interval fields indicate the same stg
  which(stgMax==stgMin),
  # or the late_intervar field is empty
  which(stgMax==-1))

# in these entries, use the stg indicated by the early_interval
dat$stg[stgCondition] <- stgMin[stgCondition] 

#-------------------------------------------------------------------------------
# This does not make any sense to me, you are trying to read in a pdf
# document as a csv!
#-------------------------------------------------------------------------------

## url <- "https://github.com/divDyn/ddPhanero/blob/master/doc/1.0.1/dd_phanero.pdf"
## temp <- read.csv(file = url)


# a. categorize interval names to bin numbers
# categorize is the new function of the package
binMin<-categorize(dat[,"early_interval"],keys$binInt)
binMax<-categorize(dat[,"late_interval"],keys$binInt)
# convert to numeric
binMin<-as.numeric(binMin)
binMax<-as.numeric(binMax)
# b. resolve max-min interval uncertainty
# final variable (empty)
dat$bin <- rep(NA, nrow(dat))
# use entries, where
binCondition <- c(
  # the early and late interval fields indicate the same bin
  which(binMax==binMin),
  # or the late_interval field is empty
  which(binMax==-1))
# in these entries, use the bin indicated by the early_interval
dat$bin[binCondition] <- binMin[binCondition]

table(dat$stg)

sum(table(dat$stg))

# which is a
sum(table(dat$stg))/nrow(dat)

# omit unbinned
dats <- dat[!is.na(dat$stg),]

#-------------------------------------------------------------------------------
# The dataset does not have any paleozoic records, the lowest stg number is
# 69, so this is actually not necessary!
#-------------------------------------------------------------------------------
# omit Paleozoic 
dats <- dats[dats$stg > 66,]


bsFull <- binstat(dats, tax="genus", bin="stg", 
                  coll="collection_no", ref="reference_no")

bsFull$occs

bs <- binstat(dats, tax="genus", bin="stg", 
              coll="collection_no", ref="reference_no", duplicates=FALSE)
bs$occs


tsplot(stages, boxes="sys", boxes.col="systemCol", 
       shading="series", xlim=c(100, 0), ylim=c(0,450))


tsplot(stages, boxes="sys", boxes.col="systemCol", 
       shading="series", xlim=c(100, 0), ylim=c(0,450), ylab="Number occurrences")
lines(stages$mid, bs$occs)

tp <- function(...) tsplot(stages, boxes="sys", boxes.col="systemCol",
                           shading="series", xlim=78:95, ...)


tp(ylim=c(0,200), ylab="Number of collections") 
lines(stages$mid, bs$colls)

# The divDyn function call
dd <- divDyn(dats, tax="genus", bin="stg")

#-------------------------------------------------------------------------------
#  The warning here is there because there is a genus entry which is "".
# These are usually poorly identifiable entries, that are given at different
# taxonomic ranks (e.g. family, order, etc).
# For this reason, I recommend removing some occurrences before you start the analyses
#-------------------------------------------------------------------------------

# taxonomic ranks of identification
table(dats$identified_rank)

# you only want these
dats <- dats[dats$identified_rank=="genus" | dats$identified_rank=="subgenus" | dats$identified_rank=="species", ]

# now function runs without warnings
dd <- divDyn(dats, tax="genus", bin="stg")





?divDyn

# simple diversity
tp(ylim=c(0,50), ylab="richness")    
lines(stages$mid, dd$divRT, lwd=2)


#-------------------------------------------------------------------------------
# You can ignore the warning here!
#-------------------------------------------------------------------------------

cor.test(dd$divRT, bs$occs, method="spearman")


## 
##  Spearman's rank correlation rho
## 
## data:  dd$divRT and bs$occs
## S = 6167.7, p-value = 0.0007426
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.5002229



cr50 <- subsample(dats, tax="genus", bin="stg", 
                  q=50, coll="collection_no", duplicates=FALSE, iter=300)


tp(ylim=c(0,35), ylab="Subsampled richness")   
lines(stages$mid, cr50$divRT, lwd=2)

sqsDD <- subsample(dats, tax="genus", bin="stg", q=0.5, type="sqs",
                   duplicates=FALSE, coll="collection_no", iter=300)

tp(ylim=c(0,20), ylab="Subsampled richness (SQS, 0.5)")    
lines(stages$mid, sqsDD$divRT, lwd=2)

# turnover rates
tp(ylim=c(0,1), ylab="per capita turnover rates")   
lines(stages$mid, dd$extPC, lwd=2, col="red")
lines(stages$mid, dd$oriPC, lwd=2, col="green")
lines(ages$mid, Organic.Carbon.Weathering.Flux.Forgw.$oriPC, lwd=2,col="blue")
legend("topleft", legend=c("extinction", "origination"), 
       col=c("red", "green"), lwd=2, cex=0.9, x.intersp=0.7, y.intersp=0.7, bty="o")
     


#-------------------------------------------------------------------------------
# Plotting: I recommend plotting the processed data. You can do it with these
# You can find more about this in the Climate and Earth System Data class' material
# (vectors)


library(chronosphere)
ne <- fetch("NaturalEarth")



# for faster plotting, consider plotting only the collections (less duplicates of coordinates)
# Normalizing data for collection coordinates (every row is one collection), with the
# basic longitude and latitude information
colls <- unique(dats[, c("collection_no", "lng", "lat")])

# this should work...
points(colls$lng, colls$lat, pch=16,col="red",cex=0.5)



#.. but it won't because these two columns are character vectors and not numerics
str(colls)

# I have to admit that I do not recognize this format "30.049.999". I thought that these
# were degrees, minutes and seconds, but 999 does not make sense that way either, so I think
# these might be there because of corrupted data.

#################################################################################
# IMPORTANT
#################################################################################
# Also, there are some DATES in the paleolng and paleolat columns, which can only happen if you
# open and save the data in MS Excel or another spreadsheet software package. DO NOT DO THIS!
# this process has compromised the downloaded dataset beyond repair.
#
# I recommend you to re-download the dataset, or use the one
# in chronosphere, which you can access with:
# pbdb <- fetch("pbdb", ver="20250519") # specific version from 2025-05-19
#################################################################################
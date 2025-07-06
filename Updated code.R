#setwd("/Users/max/Desktop/Illustration")
setwd("/Users/max/Desktop/Illustration") 

#install.packages("divDyn")

library(divDyn)

#dat <- read.csv("pbdb_data_final_v3.csv", sep=";" )
dat <- read.csv("pbdb_data_final_v3.csv", sep=";" )

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

# omit Paleozoic 
dats <- dats[dats$stg > 66,]

# Taxonomic ocurrence binning
bsFull <- binstat(dats, tax="genus", bin="stg", 
                  coll="collection_no", ref="reference_no")

bsFull$occs

# Ocurrence collection binning
bs <- binstat(dats, tax="genus", bin="stg", 
              coll="collection_no", ref="reference_no", duplicates=FALSE)
bs$occs

# Generate a plot 
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


#Ignore the following warning

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
legend("topleft", legend=c("extinction", "origination"), 
       col=c("red", "green"), lwd=2, cex=0.6, x.intersp=0.7, y.intersp=0.7, bty="o")

# Cross comparison between temperature and turnover components
sExt <- scale(dd$extPC)
sOri <- scale(dd$oriPC)
sdiv <- scale(dd$divRT)
sT <- scale(stages$T)

#Temperature and Extinction turnover comparison
tsplot(stages, boxes="sys", shading="sys", boxes.col="systemCol",, xlim=78:95, ylim=c(-2, 2), 
       ylab="Temperature and turnover")
lines(stages$mid, sT, col = "blue")
lines(stages$mid, sExt, col = "red")
legend("topleft", legend=c("Global temperatue", "extinction"), 
       col=c("blue", "red"), lwd=2, cex=0.6, x.intersp=0.7, y.intersp=0.7, bty="o")

# Temperature and Extinction turnover correlation test
acf(sT[75:95])
acf(sExt[75:94])
     
  cor.test(diff(sT[75:94]),diff(sExt[75:94]))

#Temperature and Origination turnover comparison
  tsplot(stages, boxes="sys", shading="sys", boxes.col="systemCol",, xlim=78:95, ylim=c(-3, 3), 
         ylab="Temperature and turnover")
  lines(stages$mid, sT, col = "blue")
  lines(stages$mid, sOri, col = "green")
  legend("topleft", legend=c("Global temperatue", "Origination"), 
         col=c("blue", "green", "red"), lwd=2, cex=0.6, x.intersp=0.7, y.intersp=0.7, bty="o")

# Temperature and Origination turnover correlation test
  acf(sT[75:95])
  acf(sOri[75:94])
  
  cor.test(diff(sT[75:94]),diff(sOri[75:94]))

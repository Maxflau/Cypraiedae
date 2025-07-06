#setwd("/Users/max/Desktop/Illustration")
setwd("/Users/max/Desktop/Illustration") # ADAM's
library(divDyn)
data(stages)
library(chronosphere)
dat <- datasets()
View(dat)


song <- fetch(src="REF-song2019temperature")
i <- 0
stages$top <- c(stages$bottom[-1],0)
temp <- NULL

for (i in 1:length(stages$stg)){
  Min <- stages$bottom[i]
  Max <- stages$top[i]
  ind <- which(song$`Age.(Ma)`<= Min & song$`Age.(Ma)`>= Max)# stage bin
  t <- mean(song[ind,"T-2010(℃)"])# mean temperature bin
  temp <-c(temp, t)# temperature bin
}
 
names(temp) <- stages$stage
temp

stages$T <- temp

str(stages)

#ts plot
tsplot(stages, boxes="sys", shading="sys", boxes.col="systemCol",, xlim=78:95, ylim=c(15, 35), ylab="T°C")
points(stages$mid, stages$T, col="blue", pch =20)
lines(stages$mid, stages$T, col = "blue")

sT <- scale(stages$T)





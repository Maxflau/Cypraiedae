library(divDyn)
library(chronosphere)
dat <- datasets()
View(dat)
fetch(src="REF-song2019temperature")


song <- fetch(src="REF-song2019temperature")

str(song)

plot(song[["Age.(Ma)"]], song[['T-2010(℃)']])

age <- song[["Age.(Ma)"]]

T <- song[['T-2010(℃)']]

data(stages)
#define a structured container for our data (vector of value)
Tbin <- rep(NA,nrow(stages))

#Temperature for every stages (rep function)
for (i in 1:nrow(stages)){
    Max <- stages$bottom[i]
    Min <- stages$top[i]
    #when is the age lower or equal to the top/Max
    #3<=5
    Max_conditions <- age <= Max
    #when is the age higher or equal to the bottom/Min
    Min_conditions <- Min < age
    Both <- Max_conditions&Min_conditions
    #sum(Both)
    #T[Both]
    Tbin[i] <- mean(T[Both])
}

lines(stages$mid,Tbin, col="red", lwd=3)



#bin to put Temperature in the stage data

song <- fetch(src="REF-song2019temperature")



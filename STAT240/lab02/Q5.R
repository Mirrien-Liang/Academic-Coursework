library(tidyverse)
poke = read.csv(file = "pokemon_2019.csv")
poke$Type_1=as.factor(poke$Type_1)

pokenew = poke[1,]
# pokenew = poke[1,]*NA

# names(pokenew)
pokenew$Number <- 722
pokenew$Name <- "Mirrien"
pokenew$Type_1 <- "Student"
pokenew$Type_2 <- "Poison"
pokenew$HP <- 500
pokenew$Attack <- 500
pokenew$Defense <- 500
pokenew$Sp_Atk <- 500
pokenew$Sp_Def <- 500
pokenew$Speed <- 500
pokenew$Total <- sum(pokenew[6:11])
pokenew$Generation <- 6
pokenew$isLegendary <- "True"
pokenew$Color <- "White"
pokenew$hasGender <- "True"
pokenew$Height_m <- 1.75
pokenew$Weight_kg <- 50
pokenew$hasMegaEvolution <- "True"
pokenew$Catch_Rate <- 0
pokenew$Body_Style <- "Dark_meat"


pokemonextra = rbind(pokenew, poke)

# plot(pokemonextra[,"Type_1"], las=2) # Error here but will work with some modifications

# is.data.frame(poke)
# is.data.frame(pokemonextra)
# is.numeric(poke[,"Type_1"])
# is.numeric(pokemonextra[,"Type_1"])
# is.factor(poke[,"Type_1"])
# is.factor(pokemonextra[,"Type_1"])
poke[1:5,"Type_1"]
pokemonextra[1:5,"Type_1"]
unique(poke[,"Type_1"])
pokemonextra[,"Type_1"] = factor(pokemonextra[,"Type_1"])
pokemonextra[1:5,"Type_1"]

# as.numeric(poke[,"Type_1"])
# factor(poke[,"Attack"])
# you don't need to give the input argument 'levels' to factor
# but if you do it will give the levels nicer names.

levels(pokemonextra[,"Type_1"])
levels(poke[,"Type_1"])

plot(pokemonextra[,"Type_1"], las=2) # Now it should work




################################
# Q5a:
plot((filter(pokemonextra,(Height_m > 1)&(Weight_kg > 1)))[,"Type_1"],
     horiz = TRUE, # in the question, it requires a horizontal plot
                   # but the original "horizontal" plot on pg.7 is not
                   # horizontal. If it requires to remake what was given
                   # on pg.7, then delete "horiz" argument.
     las=2,
     main = "Counts of Type_1 of Pokemon Taller\nthan 1 m and Heavier than 1 kg",
     xlab="Counts")

# 

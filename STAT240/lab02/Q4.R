library(tidyverse)

poke = read.csv(file = "pokemon_2019.csv")
# head(poke)

# plot(poke[3],horiz=TRUE,las=2) # Error due to encoding errors, e.g., line 32



##############################
# Q4a


poke %>%
  select(Name,Height_m,isLegendary) %>%
  filter((Height_m > 2)&(isLegendary == "True"))


##############################
# Q4b

poke_1 <- poke %>%
  filter((Body_Style == "head_arms")|(Body_Style == "serpentine_body"))

plot(poke_1$Attack,poke_1$Defense,
     main = "Attack vs Defense of Head_arms and Serpentine_body Pokemon",
     xlab = "Attack",
     ylab = "Defense",
     cex = 0.5)

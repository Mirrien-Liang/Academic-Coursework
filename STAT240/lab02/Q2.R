library(tidyverse)

# Define parameters
radius = seq(from = 1,to = 7,length = 10000)
volume = 4/3*pi*radius^3



# Q2a
par(mfrow=c(1,1))
plot(radius,volume,main="The volume of a sphere with 10,000 radii in points",
     xlab="Radius",
     ylab="Volume",
     ylim=c(0,1500),xlim=c(-1,9),
     lwd = 1,
     col = "black",
     type = "p",
     cex = 0.1)



# Q2c
df <- data.frame(Radius=radius, Volume=volume)
head(df)
ggplot(df,aes(x=Radius,y=Volume)) + geom_point() + 
  labs(title = "The volume of a sphere with 10,000 radii in points") +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5))

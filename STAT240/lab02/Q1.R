
radius = c(1,3,5,7)
volume = 4/3*pi*radius^3

par(mfrow=c(2,2))
plot(radius,volume,main="The volume of a sphere with different radii in points",
     xlab="Radius",
     ylab="Volume",
     ylim=c(0,1500),xlim=c(-1,9),
     lwd = 1,
     col = "black",
     type = "p")
plot(radius,volume,main="The volume of a sphere with different radii in line",
     xlab="Radius",
     ylab="Volume",
     ylim=c(0,1500),xlim=c(-1,9),
     lwd = 2,
     col = "grey",
     type = "l")
plot(radius,volume,main="The volume of a sphere with different radii in both points and lines",
     xlab="Radius",
     ylab="Volume",
     ylim=c(0,1500),xlim=c(-1,9),
     lwd = 3,
     col = "blue",
     type = "b")
plot(radius,volume,main="The volume of a sphere with different radii without plotting",
     xlab="Radius",
     ylab="Volume",
     ylim=c(0,1500),xlim=c(-1,9),
     lwd = 4,
     col = "red",
     type = "n")

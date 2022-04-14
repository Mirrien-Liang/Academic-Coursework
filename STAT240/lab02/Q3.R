

##########################################
# Define parameters
x <- seq(f=-10,t=10,length=100)

y_1 = x^2
y_2 = 2^x

plot(x,y_1,
     main = "The graphs of x^2 and 2^x",
     xlab = "Value of x",
     ylab = "Value of y",
     ylim=c(0,150),xlim=c(-10,10),
     col = 1,
     type = "l")
lines(x,y_2,
      col = 2,
      lty = 2)
legend(-10,150,
       legend=c("x^2","2^x"),
       col = 1:2,
       pch = c(NA,NA),
       lty = c(1,2))

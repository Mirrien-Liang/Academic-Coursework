library(RSQLite)

dbcon <- dbConnect(SQLite(),dbname='lab03.sqlite')
dbListTables(dbcon)

#################################################

# Q2a:

library(rworldmap)
library(rworldxtra)

worldmap = getMap(resolution = "high")
NrthAm = worldmap[which(worldmap$REGION =="North America"),]

plot(NrthAm, xlim=c(-123.35,-122.65),
     ylim=c(49,49.35), main = "Pokemon in Vancouver")




loc1 = "SELECT latitude,longitude FROM Vanpoke"
location = dbGetQuery(dbcon, loc1)


library(MASS)

points(location$longitude,location$latitude,cex=0.5,col="gray35")


#################################################

# Q2b:

f1 <- kde2d(location$longitude, location$latitude,n=200)

contour(f1$x,f1$y,f1$z,
        drawlabels=FALSE,
        nlevels=10,
        col=c("black","darkblue","darkgreen","darkred","red"),
        add=TRUE,
        lwd = 2)
# Reference:

# https://stackoverflow.com/questions/37899785/contour-plot-density-of-sites-
# with-latitude-and-longitude-locations
# https://stats.stackexchange.com/questions/31726/
# scatterplot-with-contour-heat-overlay
# https://www.r-bloggers.com/2014/09/5-ways-to-do-2d-histograms-in-r/





#################################################

# Q2c:

# In three sentences or fewer, answer these questions: Where are the
# peaks of this two dimensional density plot? Why are the peaks
# in those locations?

# According to the density plot in the previous question, based on
# the color of contour lines and a general presentation of points,
# the peaks appear at (1) Vancouver City Hall, (2) Burnaby City Hall,
# (3) somewhere around the City of Lougheed Shopping Centre, and
# (4) somewhere around Lonsdale Quay Market in North Vancouver.



#################################################


# Q2d:

library(ggmap)
 

qmplot(x=longitude, y=latitude,
       data = location,
       maptype = "toner-lite",
       color = I("black"),
       alpha = I(.2)) +
  geom_density_2d(color = "black") +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon",
                  alpha = .4,
                  color = NA,
                  contour = TRUE) +
  scale_fill_gradient2("Density",
                       low = "white",
                       mid = "yellow",
                       high = "red",
                       midpoint = 10)

# Reference:
# https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html
# https://cran.r-project.org/web/packages/ggmap/readme/README.html
# https://rpubs.com/nickbearman/r-google-map-making
# https://biostats.w.uib.no/creating-a-2d-density-plot/



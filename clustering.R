##' ---
##' title: "Cluster Analysis - Polarization in US Congress Over Time"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
## setwd("ENTER HERE)  # Set the working directory if needed

## Load the data
congress <-  read.csv("congress.csv")
head(congress)

## Subset the data by party
rep <- subset(congress, party == "Republican")
dem <- subset(congress, party == "Democrat")

## Now subset by Congress
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

##' Draw the scatterplot
xlab <- "economic dimension"
ylab <- "racial dimension"
lim <- c(-1.5, 1.5)

## Scatterplot for the 80th Congress
plot(dem80$dwnom1, dem80$dwnom2, pch = 16, col = "blue", 
     xlim = lim, ylim = lim, 
     xlab = xlab, ylab = ylab, 
     main = "80th Congress \n (1947 - 1949)")  # Democrats
points(rep80$dwnom1, rep80$dwnom2, pch = 17, col = "red")  # Republicans
text(-0.75, 1, "Democrats")
text(1, -1, "Republicans")

## Scatterplot for the 112th Congress
plot(dem112$dwnom1, dem112$dwnom2, pch = 16, col = "blue",
     xlim = lim, ylim = lim, xlab = xlab, ylab = ylab,
     main = "112th Congress \n (2011 - 2013)")
points(rep112$dwnom1, rep112$dwnom2, pch = 17, col = "red")

## Party median for each congress
dem.median <- tapply(dem$dwnom1, dem$congress, median)
rep.median <- tapply(rep$dwnom1, rep$congress, median)

## Democrats
plot(names(dem.median), dem.median, col = "blue", type = "l",
     xlim = c(80, 115), ylim = c(-1, 1), xlab = "Congress",
     ylab = "Economic Dimension")
## Republicans
lines(names(rep.median), rep.median, col = "red")
text(110, -0.6, "Democratic\n Party")
text(110, 0.85, "Republican\n Party")

##' Clustering
## Subset the data by Congressional session
session80 <- subset(congress, congress == 80)
session112 <- subset(congress, congress == 112)

##' Create a matrix of DW Nominate Scores
dwnom80 <- cbind(session80$dwnom1, session80$dwnom2)
dwnom112 <- cbind(session112$dwnom1, session112$dwnom2)

##' Run K-Means Clustering Algorithm
k80two.out <- kmeans(dwnom80, centers = 2, nstart = 5)
k112two.out <- kmeans(dwnom112, centers = 2, nstart = 5)

##' Check the Centroids from the K-Means Clustering
k80two.out$centers 
k112two.out$centers

##' Check the Clusters
table(party = session80$party, cluster = k80two.out$cluster)
table(party = session112$party, cluster = k112two.out$cluster)

##' Plot the results
plot(dwnom80, col = k80two.out$cluster, main = "80th Congress")
plot(dwnom112, col = k112two.out$cluster, main = "112th Congress")

## kmeans with 4 clusters
k80four.out <- kmeans(dwnom80, centers = 4, nstart = 5)
k112four.out <- kmeans(dwnom112, centers = 4, nstart = 5)

palette(c("red4", "red", "blue", "blue4" )) 

## plotting the results using the labels and limits defined earlier
palette(c("blue", "red", "red4", "blue4" ))
plot(dwnom80, col = k80four.out$cluster, xlab = xlab, ylab = ylab,
     xlim = lim, ylim = lim, main = "80th Congress \n (1947 - 1949)")
## plotting the centroids
points(k80four.out$centers, pch = 16, cex = 2)
## 112th congress
palette(c("blue4", "red", "red4", "blue" ))
plot(dwnom112, col = k112four.out$cluster, xlab = xlab, ylab = ylab,
     xlim = lim, ylim = lim, main = "112th Congress \n (2011 - 2013)")
points(k112four.out$centers, pch = 16, cex = 2)

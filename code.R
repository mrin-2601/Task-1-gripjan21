           # installing packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(NbClust)
library(permute)
library(lattice)
library(vegan)
      # setting the working directory
getwd()
iris <- read.csv("Iris.csv")

view(iris)

str(iris)

summary(iris)

dataset <- select(class,c(1,2,3,4))

       # scatterplot on the data 

scatter <- ggplot(data = iris, aes(x = PetalLengthCm, y = PetalWidthCm)) 
plot <- scatter + geom_point(aes(color = Species, shape = Species)) +
  theme_bw() +
  xlab("PetalLengthCm") + ylab("PetalWidthCm") +
  ggtitle("Petal Length-Width") +
  theme(plot.title = element_text(hjust = 0.5))

       # within group sum of squares, plot to determine optimal number of clusters

wss <- (nrow(dataset)-1)*sum(apply(dataset,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dataset,
                                     centers = i)$withinss)
plot(1:15, wss, type="b", xlab = "Number of Cluster",
     ylab = "within groups sum of squares", col = "blue")

      # Nbclust function for determining the best number of clusters

par(mar = c(2,2,2,2))
irisData <- iris[,1:4]
totalwss <- c()
nb <- NbClust(irisData, method = "kmeans")

       # histogram approach denoting various indices with different no. of clusters voted 

hist(nb$Best.nc[1,], breaks = 15, main = "Number of Clusters", col = c("blue", "red", "orange", "green"))






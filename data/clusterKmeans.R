# Clustering with k-means
####################################################################
set.seed(345)

## Data set - Let's create an artificial 2D dataset:
# the cclust library contains some clustering functions, including k-means
library (cclust)
n1 <- 30
n2 <- 40
n3 <- 50

# cluster 1
x1 <- rnorm (n1,1,0.5)
y1 <- rnorm (n1,1,0.5)
# cluster 2
x2 <- rnorm (n2,2,0.5)
y2 <- rnorm (n2,6,0.7)
# cluster 3
x3 <- rnorm (n3,7,1)
y3 <- rnorm (n3,7,1)
# data
x <- rbind (cbind(x1,y1), cbind(x2,y2), cbind(x3,y3))
c <- c(rep("4", n1), rep("2", n2), rep("3", n3))
D <- data.frame (x,color=c)
plot(D$x1,D$y1)
## clusters:
plot(D$x1,D$y1,col=as.vector(D$color))
# K-means, one for cluster
k <- 3

## k-means with a maximum of 100 iterations
kmeans.3 <- cclust (x,k,iter.max=100,method="kmeans",dist="euclidean")
## plot initial and final prototypes (cluster centers)
points(kmeans.3$initcenters, pch=19)
points(kmeans.3$centers, pch=19)

## draw arrows to see the process
arrows (kmeans.3$initcenters[,1], kmeans.3$initcenters[,2], kmeans.3$centers[,1], kmeans.3$centers[,2])

## plot and paint the clusters (according to the computed assignments)
plot(D$x1,D$y1,col=(kmeans.3$cluster+1))

## plot the cluster centers
points(kmeans.3$centers,col=seq(1:kmeans.3$ncenters)+1,cex=2,pch=19)

# We measure clustering quality by the Calinski-Harabasz
# where n is the number of data points and k is the number of clusters
(CH.3 <- clustIndex(kmeans.3,x, index="calinski"))

k <- 5
## execute k-means with a maximum of 100 iterations
kmeans.5 <- cclust (x,k,iter.max=100,method="kmeans",dist="euclidean")

## plot initial and final prototypes (centers)
points(kmeans.5$initcenters, pch=19)
points(kmeans.5$centers, pch=19)

## draw arrows to see the process
arrows (kmeans.5$initcenters[,1], kmeans.5$initcenters[,2], kmeans.5$centers[,1], kmeans.5$centers[,2])

## plot and paint the clusters
plot(D$x1,D$y1,col=(kmeans.5$cluster+1))

## plot the cluster centers
points(kmeans.5$centers,col=seq(1:kmeans.5$ncenters)+1,cex=2,pch=19)

# clustering quality as measured by the Calinski-Harabasz index
(CH.5 <- clustIndex(kmeans.5,x, index="calinski"))

# notice CH.3 > CH.5, so K=3 is better according to C-H


# Clustering with k-means and E-M
###########################################################################

library(MASS)
library(ggplot2)
library(RColorBrewer)
set.seed(333)

# Function to generate gaussian data:
generate.data <- function(N, K, prior.mean, prior.var)
{
  p <- length(prior.mean)  
  # generate random mixture centres from the prior
  mu_k <- mvrnorm(K, mu=prior.mean, Sigma=diag(prior.var, 2))
  
  # generate mixture coefficients
  pi_k <- runif(K)
  pi_k <- pi_k/sum(pi_k)
  
  # generate the data
  obs <- matrix(0, nrow=N, ncol=p)
  z <- numeric(N)
  sigma_k <- matrix(0, nrow=K, ncol=p)
  
  for (i in 1:K)
    sigma_k[i,] <- runif(p)
  
  for (i in 1:N)
  {
    # draw the observation from a component according to coefficient
    z[i] <- sample(1:K, 1, prob=pi_k)
    # draw the observation from the corresponding mixture location
    obs[i,] <- mvrnorm(1, mu=mu_k[z[i],], Sigma=diag(sigma_k[z[i],],p))
  }
  list(locs=mu_k, z=z, obs=obs, coefs=pi_k)
}

# Function to plot 2D data from a mixture
plot.mixture <- function(locs, z, obs)
{
  stopifnot(dim(obs)[2]==2)
  z <- as.factor(z)
  df1 <- data.frame(x=obs[,1], y=obs[,2], z=z)
  df2 <- data.frame(x=locs[,1], y=locs[,2])
  p <- ggplot()
  p <- p + geom_point(data=df1, aes(x=x, y=y, colour=z), shape=16, size=2, alpha=0.75)
  p <- p + geom_point(data=df2, aes(x=x, y=y), shape=16, size=3)
  p <- p + theme(legend.position="none")
  p
}

# Function to plot 2D data as a scatter plot
plot.data <- function(dat)
{
  stopifnot(dim(dat)[2]==2)
  df1 <- data.frame(x=dat[,1], y=dat[,2])
  p <- ggplot()
  p <- p + geom_point(data=df1, aes(x=x, y=y), size=2, alpha=0.75)
  p
}

## Generate the data:
n <- 1000
k <- 5
centre <- c(0,0)
dispersion <- 10

## generate 2D data as a mixture of 5 Gaussians, each axis-aligned 
#(therefore the two variables are independent) with different variances; the centers and coefficients of the mixture are chosen randomly
d <- generate.data (n,k,centre,dispersion)

## components of the mixture
plot.mixture(d$locs, d$z, d$obs)

## compute 2D kernel density
z <- kde2d(d$obs[,1], d$obs[,2], n=50)

colors <- rev(brewer.pal(11, "RdYlBu"))

## what the clustering method "sees"
plot(d$obs, xlab="x", ylab="y", pch=19, cex=.4)

## Contour plot of the unconditional density:
contour(z, drawlabels=FALSE, nlevels=22, col=colors, add=TRUE)
abline(h=mean(d$obs[,2]), v=mean(d$obs[,1]), lwd=2)

## a simpler way of plotting the data
plot.data(d$obs)

######################################
## let us try first with k-means (k=2)
k <- 2
kmeans2.2 <- cclust (d$obs,k,iter.max=100,method="kmeans",dist="euclidean")

plot(d$obs[,1],d$obs[,2],col=(kmeans2.2$cluster+1))
points(kmeans2.2$centers,col=seq(1:kmeans2.2$ncenters)+1,cex=2,pch=19)

#Calinski-Harabasz index
(CH2.2 <- clustIndex(kmeans2.2,d$obs, index="calinski"))

#k-means = 5
k <- 5
do.kmeans <- function (what.k)
{
  r <- cclust (d$obs,what.k,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,d$obs, index="calinski"))
}
max (r <- replicate (100, do.kmeans(5)))
plot(d$obs[,1],d$obs[,2],col=(kmeans2.5$cluster+1))
points(kmeans2.5$centers,col=seq(1:kmeans2.5$ncenters)+1,cex=2,pch=19)

#Calinski-Harabasz index
(CH2.5 <- clustIndex(kmeans2.5,d$obs, index="calinski"))
# at least CH2.5 >> CH2.2 ... so C-H does a good job

# This may take a while for large datasets
res <- vector("numeric", 10)
for (k in 2:10)
  res[k] <- max (r <- replicate (100, do.kmeans(k)))

res[1] <- NA
plot(res, type="l", axes = FALSE, xlab="k", ylab="Calinski-Harabasz")

axis(side = 1, at = 2:10)
axis(side = 2, at = seq(0,5000,500))
grid(9, 6, lwd = 2)

## k-means + Calinski-Harabasz is better for 5 clusters

######################################
library(Rmixmod)

## This method performs E-M for mixture densities, including mixtures of Gaussians
## we can specify which family of gaussians we intend to fit in this case:
## "diagonal" for the diagonal family 
fammodel <- mixmodGaussianModel (family="diagonal", equal.proportions=FALSE)
z <- mixmodCluster (data.frame(d$obs), models = fammodel, nbCluster = 5)
summary(z)

# centers
(means <- z@bestResult@parameters@mean)
# if you want "hard" assignments
(found.clusters <- z@bestResult@partition)

## the estimated covariance matrices for each cluster and probabilities
(z@bestResult@parameters@variance)
(z@bestResult@likelihood)
z@bestResult@proba

## Let's have a look at the first 5 data points:
z@bestResult@proba[1:5,]

## This is a graphical summary of the clustering
plot(d$obs[,1],d$obs[,2],col=(found.clusters+1))
points(means,col="black",cex=2,pch=19)

## Suppose now we do not know the truth, but we still wish to fit general gaussians,
## we can change the family to general because we don't know in theory
fammodel <- mixmodGaussianModel (family="general", equal.proportions=FALSE)
z <- mixmodCluster (data.frame(d$obs),models = fammodel, nbCluster = 5)
summary(z)

means <- z@bestResult@parameters@mean
found.clusters <- z@bestResult@partition

plot(d$obs[,1],d$obs[,2],col=(found.clusters+1))
points(means,col="black",cex=2,pch=19)

## It also works well because the data is gaussian, let's compare the estimated centers:
means
# with the truth
d$locs

## or the estimated coefficients with the truth 
sort(z@bestResult@parameters@proportions)
sort(d$coefs)


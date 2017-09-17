# SQL Server Connect
library(RODBC)
con <- odbcConnect("..", uid = "..", pwd = "..")
dat = sqlQuery(con,"select * from ..")

# Clustering Tendency
library(factoextra)
library(clustertend)
## Data Prepairation
data(iris)
traindat <- iris
traindat$Species <- NULL
random <- apply(traindat, MARGIN = 2, 
                function(x){runif(length(x), min(x), (max(x)))})
random <- as.data.frame(random)
traindat <- scale(traindat)
random <- scale(random)
## Visulization
fviz_pca_ind(prcomp(traindat), title = "PCA - Iris data", habillage = iris$Species,
             palette = "jco", geom = "point", ggtheme = theme_classic(),
             legend = "bottom")
fviz_pca_ind(prcomp(random), title = "PCA - Random data", geom = "point", 
             ggtheme = theme_classic())
## Hopkins Statistic
set.seed(123)
hopkins(traindat, n = nrow(traindat) - 1)
hopkins(random, n = nrow(random) - 1)
## VAT Approach
fviz_dist(dist(traindat), show_labels = FALSE) + labs(title = "Iris data")
fviz_dist(dist(random), show_labels = FALSE) + labs(title = "Random data")

# Estimating the Optimal Number of Clusters
data(iris)
traindat <- iris
traindat$Species <- NULL
library(NbClust)
nb <- NbClust(traindat, distance = "euclidean", method = "kmeans")
library("factoextra")
fviz_nbclust(nb)

# Validation
library(clValid)
clm <- c("hierarchical","kmeans","diana","fanny","som","model","sota","pam","clara","agnes")
vali <- c("internal", "stability", "biological")
compare <- clValid(traindat, nClust = 2:10, clMethods =  clm, validation = vali, 
                   maxitems = 600, metric = "euclidean", method = "average")
optimalScores(compare)

# Clustering
# K-Means
data(iris)
traindat <- iris
traindat$Species <- NULL
kmclu <- kmeans(traindat, centers = 3, iter.max = 300, nstart = 20)
library("factoextra")
fviz_cluster(kmclu, data = traindat, ellipse.type = "t", geom = "point", 
             pointsize = 1, ggtheme = theme_classic())
table(iris$Species, kmclu$cluster)

# PAM
data(iris)
traindat <- iris
traindat$Species <- NULL
library(cluster)
pamclu <- pam(traindat, k = 3)
fviz_cluster(pamclu, data = traindat, ellipse.type = "t", geom = "point", 
             pointsize = 1, ggtheme = theme_classic())
table(iris$Species, pamclu$clustering)

# CLARA
data(iris)
traindat <- iris
traindat$Species <- NULL
library(cluster)
claraclu <- clara(traindat, k = 3)
fviz_cluster(claraclu, data = traindat, ellipse.type = "t", geom = "point", 
             pointsize = 1, ggtheme = theme_classic())
table(iris$Species, claraclu$clustering)

# Affinity Propagation (AP)
data(iris)
traindat <- iris
traindat$Species <- NULL
library(apcluster)
apclu <- apcluster(negDistMat(r=2), traindat)
# PCA for data visulization
irispca <- princomp(traindat, cor = T)
ptdat <- predict(irispca)[,c(1,2)]
plot(apclu, ptdat)
table(iris$Species, apclu@idx)

# Mean Shift
data(iris)
traindat <- iris
traindat$Species <- NULL
## Transpose of the data so that the features are on the horizontal lines
traindat <- t(traindat)
library(MeanShift)
options( mc.cores=2 )
msclu <- msClustering(traindat, h = 0.8, multi.core=TRUE)
traindat <- iris
traindat$Species <- NULL
irispca <- princomp(traindat, cor = T)
ptdat <- predict(irispca)[,c(1,2)]
plot(ptdat, col = msclu$labels, cex=0.8, pch=16)
table(iris$Species, msclu$labels)

# Spectral Clustering
data(iris)
traindat <- iris
traindat$Species <- NULL
traindat <- as.matrix(traindat)
library(kernlab)
spclu <- specc(traindat, centers = 3)
irispca <- princomp(traindat, cor = T)
ptdat <- predict(irispca)[,c(1,2)]
plot(ptdat, col = spclu)
table(iris$Species, spclu@.Data)

# Hierarchical Clustering
data(iris)
traindat <- iris
traindat$Species <- NULL
## Calculate the distances
traindat <- dist(traindat)
## Clustering
hclu <- hclust(traindat, method = "ward.D2")
library("factoextra")
fviz_dend(hclu, k = 3, cex = 0.5, k_colors = c(1, 2, 3), color_labels_by_k = TRUE,
          rect = TRUE)
hresult <- cutree(hclu, k = 3)
fviz_cluster(list(data = traindat, cluster = hresult), palette = c(1, 2, 3), 
             ellipse.type = "convex", show.clust.cent = FALSE, 
             ggtheme = theme_minimal())
table(iris$Species, hresult)

# Agglomerative Clustering
data(iris)
traindat <- iris
traindat$Species <- NULL
library(cluster)
agclu <- agnes(traindat, method = "average")
agresult <- cutree(agclu, k = 3)
fviz_cluster(list(data = traindat, cluster = agresult), palette = c(1, 2, 3), 
             ellipse.type = "convex", show.clust.cent = FALSE, 
             ggtheme = theme_minimal())
table(iris$Species, agresult)

# DBSCAN
data(iris)
traindat <- iris
traindat$Species <- NULL
library(fpc)
dbclu <- dbscan(traindat, eps = 0.6, MinPts = 4)
irispca <- princomp(traindat, cor = T)
ptdat <- predict(irispca)[,c(1,2)]
plot(dbclu, ptdat)
dbresult <- dbclu$cluster
table(iris$Species, dbresult)

# Gaussian Mixtures
data(iris)
traindat <- iris
traindat$Species <- NULL
library(mclust)
mc <- Mclust(traindat, G = 3)
irispca <- princomp(traindat, cor = T)
ptdat <- predict(irispca)[,c(1,2)]
mclust2Dplot(ptdat, parameters = mc$parameters, z = mc$z, what = "classification",
             main = T)
mcresult <- mc$classification
table(iris$Species, mcresult)

# Bootstrap P-value
library(pvclust)
result <- parPvclust(cl=NULL, traindat, method.hclust = "average",
           method.dist = "correlation", nboot = 1000,
           iseed = NULL)
clusters <- pvpick(result)
clusters

# Clustering
Unsupervised learning part for the credit project.

## SQL Server Connect
The data is acquired from SQL Server. We will need to set up the ODBC connect mannualy, and connect through R.
```
library(RODBC)
# Connect to SQL Server
con <- odbcConnect("..", uid = "..", pwd = "..")
# Get the data using SQL Language
dat = sqlQuery(con,"select * from ..")
```
The above code should return a data frame in R.

## Assessing Clustering Tendency
Before applying any clustering method on the data, it’s important to evaluate whether the data contains meaningful clusters (i.e.: non-random structures) or not. This process is defined as *the assessing of clustering tendency* or *the feasibility of the clustering analysis*.

A big issue, in cluster analysis, is that clustering methods will return clusters even if the data does *NOT* contain any clusters. In other words, if you blindly apply a clustering method on a data set, it will divide the data into clusters because that is what it supposed to do.

### Required R packages
* *factoextra* for data visualization
* *clustertend* for statistical assessment clustering tendency
```
library(factoextra)
library(clustertend)
```

### Data Preparation
For the purpose of illustration, we will use two data sets:
* the built-in R data set *iris*.
* a *random data set* generated from the iris data set.
```
# Prepairing data iris
data(iris)
traindat <- iris
traindat$Species <- NULL
# Prepairing random data
random <- apply(traindat, MARGIN = 2, 
                function(x){runif(length(x), min(x), (max(x)))})
random <- as.data.frame(random)
# Scale the two data sets
traindat <- scale(traindat)
random <- scale(random)
```
Note that both data sets contains 150 rows and 4 columns.

### Visual Inspection of the Data
We start by visualizing the data to assess whether they contains any meaningful clusters.

As the data contain more than two variables, we need to reduce the dimensionality in order to plot a scatter plot. This can be done using principal component analysis (PCA) algorithm. After performing PCA, we visualize the output.

```
# For iris data
fviz_pca_ind(prcomp(traindat), title = "PCA - Iris data", habillage = iris$Species,
             palette = "jco", geom = "point", ggtheme = theme_classic(), legend = "bottom")
# For random data
fviz_pca_ind(prcomp(random), title = "PCA - Random data", geom = "point", ggtheme = theme_classic())
```
The visualizations can provide us a general idea of whether the data can be clustered. If the data does not seem to form groups, clustering analysis might not be as much meaningful.

### Methods for Assessing Clustering Tendency
#### Statistical Methods
The *Hopkins statistic* is used to assess the clustering tendency of a data set by measuring the probability that a given data set is generated by a uniform data distribution. In other words, it tests the spatial randomness of the data.

The null and the alternative hypotheses are defined as follow:

* **Null hypothesis**: the data set is uniformly distributed (i.e., no meaningful clusters)
* **Alternative hypothesis**: the data set is not uniformly distributed (i.e., contains meaningful clusters)

We can use 0.5 as the threshold to reject the alternative hypothesis. That is, if the value of Hopkins statistic > 0.5, then it is unlikely that D has statistically significant clusters.

Put in other words, If the value of Hopkins statistic is close to zero, then we can reject the null hypothesis and conclude that the dataset D is significantly a clusterable data.

References：

<http://www.sthda.com/english/articles/29-cluster-validation-essentials/95-assessing-clustering-tendency-essentials/>

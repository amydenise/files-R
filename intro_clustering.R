library(ISLR)

#########
#EXPLORE
#########
dim(USArrests) # = 50 x 4
head(USArrests) #= Murder  Assault  UrbanPop  Rape
  #Arrests per 100,000 for Murder, Assault, & Rape
  #UrbanPpop = % of population living in urban area

states=row.names(USArrests)
states

#calculate column (=2) means and variances
apply(USArrests, 2, mean)  # 2 = columns
apply(USArrests, 2, var)   

##########
#Calculate principcal components on scaled variables  (defaults to mean 0, scale = T --> sd = 1)
#########
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center        #column means
pr.out$scale         #column SDs
pr.out$rotation      #Loadings
dim(pr.out$x)        #Scores

##########
#Produce biplot
##########
biplot(pr.out, scale=0)   #mirror of 10.1
pr.out$rotation=-pr.out$rotation  #flip signs on loadings
pr.out$x=-pr.out$x                #flip signs on scores
biplot(pr.out, scale=0)

###########
#PVE  - % variation explained = square of sd
##########
pr.out$sdev          #sd of each PC
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", 
     ylab="Proportion of Variance Explained", 
     ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1),type='b')    #cumsum = cumulative sum


##########################
#Clustering
########################

# K-Means Clustering

############
#Generate Data
###########
set.seed(2)
#2 clusers: first 25 have mean sheft relative to next 25
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

#############
#K means clustering with k = 2 (true)  & plot
#############
km.out=kmeans(x,2,nstart=20)
km.out$cluster     #cluster assignments

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

#############
#K means clustering with k = 3 (true k = 2) & plot
#############
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

############
#Compare nstarts = 1 to = 20
#############
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss

km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

# Hierarchical Clustering
#Using complete, single, and average linkage clusingering with
#Euclidean distance as the dissimilarity measure

#############
#Hierarchical clustering & plots
#############
#dist(x) = 50x50 inter-observation Euclidean distance matrix
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

#################
#Use cutree to determine the cluster labels for each
#observation associated with a given cut of the dendogram
#################
cutree(hc.complete, 2)  #2 clusters
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)    #4 clusters


#############
#Hierarchical clustering (with scaled features)& plot
#############
xsc=scale(x)      #scale x first
par(mfrow=c(1,1))
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

x=matrix(rnorm(30*3), ncol=3)  #3 dimensional data
dd=as.dist(1-cor(t(x)))     #correlation based distance
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")

##############
#NCI60 Data Example
##############

# The NCI60 data
#6,830 gene expression measurements on 64 cancer cell lines

#####
#Data import and exploration
#####
library(ISLR)
nci.labs=NCI60$labs     #cancer type labels
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

#exploring scale of variables
par(mfrow = c(2,2))
hist(nci.data[,1])
hist(nci.data[,100])
hist(nci.data[,1000])
hist(nci.data[,2000])

range(nci.data)

#####
# PCA on the NCI60 Data
#####
pr.out=prcomp(nci.data, scale=TRUE)

#write a function that assigns a distinct color to each element of a numeric vector
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

#plot principal component score vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")

#understand PVE
summary(pr.out)
plot(pr.out)

#plot the PVE of each PC (scree) + cumulative PVE
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

#######
# Clustering the Observations of the NCI60 Data
#######
sd.data=scale(nci.data)

#perform h-clustering using complete, single, and average linkage with
#Euclidean distance as the dissiminarity measure
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")

hc.out=hclust(dist(sd.data))  #proceed using complete linkage
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)   #comparison of true labels and cluster assignment

par(mfrow=c(1,1))             #plot the cut that produced these clusters
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

hc.out      #cluster object

#compare h-clustering with cut to have 4 clusters with 4-means clustering
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)

#perform H-clustering on the frist 5 PC Score Vectors
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)
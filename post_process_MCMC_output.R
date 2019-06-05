#libraries
library(tidyverse)
library(coda)
library(mcclust)
library(alluvial)
library(cluster)
#code used to post process clusters
#.rds file produced from slice sampler is a list containing K, alpha and z for each iteration
#load .rds file
output<- readRDS("slice_output.rds")
#extract K
output_k<-output$K
output_k<-as.mcmc(output_k)
#traceplot, autocorrelation and effective sample size for K
traceplot(output_k)
effectiveSize(output_k)
autocorr(output_k)
#if you have run more than one chain you can combine them and calculate GR stat
combined_chains<- mcmc.list(chain1_k, chain2_k, chain3_k)
traceplot(combined_chains)
gelman.diag(combined_chains)
gelman.plot(combined_chains)

##can repeat the above for the chains for alpha

#create the posterior similarity matrix using z
#get z from .rds file

output_z<-output$z
#calculate posterior similarity matrix
sim.mat<-comp.psm(t(output_z))
#if using PEAR method to find optimal clusterings
pear<-maxpear(sim.mat, t(output_z), method = "draws")
#Note: if your sample size is very large this will take a long time - perhaps use hierarchical method
#If have run multiple chains can compare the optimal clusters across the chains using an alluvial plot

labels1<-pear1$cl
labels2<-pear2$cl
labels3<-pear3$cl

labels1<-as.factor(labels1)
labels2<-as.factor(labels2)
labels3<-as.factor(labels3)

data<-cbind(labels1, labels2, labels3)
data<-as.data.frame(data)

data<- data %>%
  group_by(labels1, labels2, labels3)%>%
  tally()

alluvial(data[, 1:3], freq = data$n)

#if your sample size is very large you can use the hierarchical clustering method to find the optimal clustering
#convert similarity matrix into dissimilarity matrix
dissim_mat<- 1- sim_mat
dist_mat<-as.dist(dissim_mat)
#use hierarchical clustering to cluster the distmat.
cluster_result<- hclust(dist_mat, method="complete")
#cut the dendrogram into different number  clusters and compare silhouette widths
#choose number of clusters with with largest width

average<- vector("list", 0)

for(k in 2:20){
  result<- silhouette(cutree(cluster_result, k = k), dist = dist_mat)
  average[k]<- mean(result[,3])
}
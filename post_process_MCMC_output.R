#libraries
library(tidyverse)
library(coda)
library(mcclust)
library(alluvial)
library(cluster)
#code used to post process clusters
#.rds file produced from slice sampler is a list containing K, alpha and z for each iteration
#load .rds file
output<- readRDS("MCMC_traces_ext-4.rds")
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
#combine PSM  for each chain in a list
ist_PSM<- list(sim_mat1, sim_mat2, sim_mat3)

#calculate average for each element to join the chains
mean_PSM<- apply(simplify2array(list_PSM), 1:2, mean)

#calculate dissimilarity matrix to use with PAm
mean_dissim<- 1 - mean_PSM
mean_dissim<-as.dist(mean_dissim) 

#use PAM method to method to find optimal clusterings
#get PAM clusters
pam_mean<-pam(mean_dissim, k=3)
cl_pam_mean<- pam_mean$clustering

#get silhouette widths for range of clsuter sizes using PAM
av.sil.Width <- function(k) {
  pr.pam <- pam(mean_dissim, k = k)
  return(pr.pam$silinfo$avg.width)
}
k <- 2:20
avg.width <- apply(as.matrix(k), 1, FUN = av.sil.Width)
plot(k, avg.width, xlab = "number of clusters", ylab = "average silhouette", 
     type = "b", col = "red", main = "Average silhouette vs. number of clusters")


#Note: if your sample size is very large this will take a long time 

